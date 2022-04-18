{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Faucet.OffChain where

import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Maybe
import           Data.Text
import           Data.Void              (Void)
import           Ledger                 hiding (singleton)
import           Ledger.Ada
import qualified Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Playground.Contract
import           Plutus.Contract        as Contract
import           Plutus.Contract.Wallet (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.AssocMap      as PMap
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import qualified Prelude                as P
import           Text.Printf

import           Data.Monoid
import qualified Faucet.Policy          as Policy
import qualified Faucet.Validator       as Validator

data FaucetParams = NewFaucet Validator.ApiKey  | ExistingFaucet Validator.Faucet

startFaucet :: FaucetParams -> Contract w s Text Validator.Faucet
startFaucet (NewFaucet apiKey) = do
  cs <- mintForFaucet
  phk <- ownPubKeyHash
  let faucet = Validator.Faucet cs phk apiKey
      emptyBook = PMap.empty :: PMap.Map PubKeyHash POSIXTime
      tx = Constraints.mustPayToTheScript (Validator.ConsumeDatum emptyBook) $ assetClassValue (Policy.faucetAsset cs) 1
  ledgerTx <- submitTxConstraints (Validator.typedValidator faucet) tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  pure faucet

startFaucet (ExistingFaucet faucet) = do
  faucetInfo <- findFaucet faucet
  case faucetInfo of
    Nothing -> throwError "faucet does not exist"
    Just _  ->  pure faucet

mintForFaucet :: Contract w s Text CurrencySymbol
mintForFaucet = do
  utxo <- getUnspentOutput
  txOut <- fromJust <$> txOutFromRef utxo
  let cs =  Policy.curSymbol utxo
      val = Value.singleton cs Policy.faucetTokenName 1
      lookups = Constraints.mintingPolicy (Policy.policy utxo) P.<> Constraints.unspentOutputs (Map.singleton utxo txOut)
      tx      = Constraints.mustMintValue val P.<> Constraints.mustSpendPubKeyOutput utxo
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  pure cs

data FundParams = FundParams
  { fFaucet :: Validator.Faucet
  , fAmount :: Integer
  } deriving (P.Show, Generic, ToJSON, FromJSON)

data SendParams = SendParams
  { gFaucet  :: Validator.Faucet
  , gAddress :: PubKeyHash
  , gApiKey  :: Maybe Validator.ApiKey
  } deriving
   (P.Show, Generic, ToJSON, FromJSON)

type FaucetSchema =
  Endpoint "fundFaucet" FundParams .\/
  Endpoint "sendAda" SendParams

fundFaucet :: FundParams -> Contract w FaucetSchema Text ()
fundFaucet (FundParams faucet amount) = do
  Contract.logInfo @P.String $ printf "funding the faucet"
  faucetInfo <- findFaucet faucet
  case faucetInfo of
    Nothing -> throwError "faucet not found"
    Just (oref, txOut, datum) -> do
      let validator = Validator.validator faucet
          faucetBalance = _ciTxOutValue txOut
          newBalance = lovelaceValueOf amount + faucetBalance
      when (faucetBalance `lt` newBalance) $ throwError "wrong balance"
      let lookups = Constraints.otherScript validator <>
                    Constraints.unspentOutputs (Map.singleton oref txOut)
          tx = Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toBuiltinData Validator.Fund)) <>
               Constraints.mustPayToOtherScript (validatorHash validator) (Datum $ PlutusTx.toBuiltinData datum) newBalance
      ledgerTx <- submitTxConstraintsWith @Validator.FaucetType lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      Contract.logInfo @P.String $ printf "faucet has funded"

sendAda :: SendParams -> Contract w FaucetSchema Text ()
sendAda (SendParams faucet addr apiKey) = do
  Contract.logInfo @P.String $ printf "getting from the faucet"
  faucetInfo <- findFaucet faucet
  now <- currentTime
  case faucetInfo of
    Nothing -> Contract.logInfo @P.String $ printf "faucet not found"
    Just (oref, txOut, datum) -> do
      let validator = Validator.validator faucet
          faucetBalance = _ciTxOutValue txOut
          valueToConsume = case apiKey of
            Nothing -> Validator.allowedAmount
            Just ak -> if ak == Validator.fApiKey faucet
              then Validator.allowedAmountWithRightApiKey
              else Validator.allowedAmount
          datum' = datum { Validator.book = PMap.insert addr now (Validator.book datum) }
          change = faucetBalance - valueToConsume
          lookups = Constraints.otherScript validator <>
                    Constraints.unspentOutputs (Map.singleton oref txOut)
          tx = Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toBuiltinData $ Validator.Use addr apiKey)) <>
               Constraints.mustPayToOtherScript (validatorHash validator) (Datum $ PlutusTx.toBuiltinData datum') change <>
               Constraints.mustPayToPubKey addr valueToConsume <>
               Constraints.mustValidateIn (interval (now - 5000) (now + 5000))
      Contract.logInfo @P.String $ printf "old datum %s" (P.show datum)
      Contract.logInfo @P.String $ printf "new datum %s" (P.show datum')
      ledgerTx <- submitTxConstraintsWith @Validator.FaucetType lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx

findFaucet :: Validator.Faucet -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Validator.ConsumeDatum))
findFaucet faucet@Validator.Faucet {..} = do
  utxos <- Contract.utxosAt $ Validator.scrAddress faucet
  let faucetInfo = [ (oref, txOut)
            | (oref, txOut) <- Map.toList utxos
            , Value.valueOf (_ciTxOutValue txOut) fCurrencySymbol Policy.faucetTokenName == 1
            ]
  pure $ case faucetInfo of
      [(oref, txOut)] -> case _ciTxOutDatum txOut of
          Left _ -> Nothing
          Right (Datum datum) -> case PlutusTx.fromBuiltinData datum of
            Nothing -> Nothing
            Just d  -> Just (oref, txOut, d)
      _ -> Nothing

runFaucet :: FaucetParams -> Contract (Last Validator.Faucet) FaucetSchema Text ()
runFaucet params = do
  faucet <- startFaucet params
  tell $ Last $ Just faucet
  forever endpoints
    where
      endpoints = selectList [sendAda', fundFaucet']
      sendAda' = endpoint @"sendAda" sendAda
      fundFaucet' = endpoint @"fundFaucet" fundFaucet

