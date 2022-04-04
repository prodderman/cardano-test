{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Faucet.UserSide where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import           Ledger              hiding (singleton)
import           Ledger.Ada
import qualified Ledger.Constraints  as Constraints
import           Ledger.Value        as Value
import           Playground.Contract
import           Plutus.Contract     as Contract
import qualified PlutusTx
import           PlutusTx.AssocMap   as PMap
import           PlutusTx.Prelude    hiding (Semigroup (..), unless)
import qualified Prelude             as P
import           Text.Printf         (printf)

import qualified Faucet.Policy       as Policy
import qualified Faucet.Validator    as Validator

data FundParams = FundParams
  { fCurrencySymbol :: CurrencySymbol
  , fValidator      :: Validator
  , fAmount         :: Integer
  } deriving (P.Show, Generic, ToJSON, FromJSON)

data GetParams = GetParams
  { gCurrencySymbol :: CurrencySymbol
  , gAddress        :: Validator
  , gApiKey         :: Validator.ApiKey
  } deriving
   (P.Show, Generic, ToJSON, FromJSON)

type FaucetSchema =
  Endpoint "fundFaucet" FundParams .\/
  Endpoint "getSomeAda" GetParams

fundFaucet :: FundParams -> Contract w FaucetSchema Text ()
fundFaucet (FundParams cs validator amount) = do
  Contract.logInfo @P.String $ printf "funding the faucet"
  faucetInfo <- findFaucet validator cs
  case faucetInfo of
    Nothing -> throwError "faucet not found"
    Just (oref, txOut, datum) -> do
      let faucetBalance = _ciTxOutValue txOut
          newBalance = lovelaceValueOf amount + faucetBalance
      when (faucetBalance `lt` newBalance) $ throwError "wrong balance"
      let lookups = Constraints.otherScript validator <>
                    Constraints.unspentOutputs (Map.singleton oref txOut)
          tx = Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toBuiltinData Validator.Fund)) <>
               Constraints.mustPayToOtherScript (validatorHash validator) (Datum $ PlutusTx.toBuiltinData datum) newBalance
      ledgerTx <- submitTxConstraintsWith @Validator.FaucetType lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      Contract.logInfo @P.String $ printf "faucet has funded"

getSomeAda :: GetParams -> Contract w FaucetSchema Text ()
getSomeAda (GetParams cs validator apiKey) = do
  Contract.logInfo @P.String $ printf "getting from the faucet"
  faucetInfo <- findFaucet validator cs
  now <- currentTime
  pkh <- ownPubKeyHash
  case faucetInfo of
    Nothing -> Contract.logInfo @P.String $ printf "faucet not found"
    Just (oref, txOut, datum) -> do
      let faucetBalance = _ciTxOutValue txOut
          apiKeyHash = sha3_256 apiKey
          valueToConsume = if apiKeyHash == Validator.apiKeyHash datum
            then Validator.allowedAmountWithRightApiKey
            else Validator.allowedAmount
          datum' = datum { Validator.book = PMap.insert pkh now (Validator.book datum) }
          change = faucetBalance - valueToConsume
          lookups = Constraints.otherScript validator <>
                    Constraints.unspentOutputs (Map.singleton oref txOut)
          tx = Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toBuiltinData $ Validator.Use apiKey)) <>
               Constraints.mustPayToOtherScript (validatorHash validator) (Datum $ PlutusTx.toBuiltinData datum') change <>
               Constraints.mustValidateIn (interval (now - 1000) (now + 1000))
      Contract.logInfo @P.String $ printf "old datum %s" (P.show datum)
      Contract.logInfo @P.String $ printf "new datum %s" (P.show datum')
      ledgerTx <- submitTxConstraintsWith @Validator.FaucetType lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx

findFaucet :: Validator -> CurrencySymbol -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Validator.ConsumeDatum))
findFaucet validator cs = do
  utxos <- Contract.utxosAt $ scriptAddress validator
  let faucetInfo = [ (oref, txOut)
            | (oref, txOut) <- Map.toList utxos
            , Value.valueOf (_ciTxOutValue txOut) cs Policy.faucetTokenName == 1
            ]
  pure $ case faucetInfo of
      [(oref, txOut)] -> case _ciTxOutDatum txOut of
          Left _ -> Nothing
          Right (Datum datum) -> case PlutusTx.fromBuiltinData datum of
            Nothing -> Nothing
            Just d  -> Just (oref, txOut, d)
      _ -> Nothing

endpoints :: Contract (Last Validator.Faucet) FaucetSchema Text ()
endpoints = awaitPromise (getSomeAda' `select` fundFaucet') >> endpoints
  where
    getSomeAda' = endpoint @"getSomeAda" getSomeAda
    fundFaucet' = endpoint @"fundFaucet" fundFaucet
