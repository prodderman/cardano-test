{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Faucet.OffChain where

import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 hiding (singleton)
import           Ledger.Ada
import qualified Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Playground.Contract
import           Plutus.Contract        as Contract
import           Plutus.Contract.Wallet (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.AssocMap      as PlutusMap
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import qualified Prelude                as P
import           Text.Printf            (printf)

import qualified Faucet.Policy          as Policy
import qualified Faucet.Validator       as Validator


data FundParams = FundParams
  { fFaucet :: Validator.Faucet
  , fAmount :: Integer
  } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

data GetParams = GetParams
  { gFaucet :: Validator.Faucet
  , gApiKey :: Validator.ApiKey
  } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

type FaucetSchema =
  Endpoint "startFaucet" Validator.Faucet .\/
  Endpoint "fundFaucet" FundParams .\/
  Endpoint "getSomeAda" GetParams

createFaucet :: Validator.ApiKey -> Contract w FaucetSchema Text Validator.Faucet
createFaucet apiKey = do
  cs <- mintForFaucet
  Contract.logInfo @P.String $ printf "faucet currency symbol %s" (P.show cs)
  pure $ Validator.Faucet
              { Validator.currencySymbol = cs
              , Validator.apiKey = apiKey
              }

startFaucet :: Validator.Faucet -> Contract w FaucetSchema Text ()
startFaucet faucet@(Validator.Faucet cs _) = do
  let emptyBook = PlutusMap.empty :: PlutusMap.Map PubKeyHash POSIXTime
      tx = Constraints.mustPayToTheScript emptyBook $ assetClassValue (Policy.faucetAsset cs) 1
  ledgerTx <- submitTxConstraints (Validator.typedValidator faucet) tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx

fundFaucet :: FundParams -> Contract w FaucetSchema Text ()
fundFaucet (FundParams faucet amount) = do
  Contract.logInfo @P.String $ printf "funding the faucet"
  faucetInfo <- findFaucet faucet
  case faucetInfo of
    Nothing -> Contract.logInfo @P.String $ printf "faucet not found"
    Just (oref, txOut, datum) -> do
      let v = lovelaceValueOf amount <> assetClassValue (Policy.faucetAsset $ Validator.currencySymbol faucet) 1
          validator = Validator.validator faucet
          lookups = Constraints.otherScript validator <>
                    Constraints.unspentOutputs (Map.singleton oref txOut)
          tx = Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toBuiltinData Validator.Fund)) <>
               Constraints.mustPayToOtherScript (validatorHash validator) (Datum $ PlutusTx.toBuiltinData datum) v
      ledgerTx <- submitTxConstraintsWith @Validator.FaucetType lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      Contract.logInfo @P.String $ printf "faucet has funded"

getSomeAda :: GetParams -> Contract w FaucetSchema Text ()
getSomeAda (GetParams faucet apiKey) = do
  Contract.logInfo @P.String $ printf "getting from the faucet"


mintForFaucet :: Contract w FaucetSchema Text CurrencySymbol
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


findFaucet :: Validator.Faucet -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Validator.ConsumeDatum))
findFaucet params@(Validator.Faucet cs _) = do
  utxos <- Contract.utxosAt (Validator.scrAddress params)
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

endpoints :: Validator.ApiKey -> Contract (Last Validator.Faucet) FaucetSchema Text ()
endpoints apiKey = do
  faucet <- createFaucet apiKey
  tell $ Last $ Just faucet
  start faucet
  where
    start :: Validator.Faucet -> Contract (Last Validator.Faucet) FaucetSchema Text a
    start faucet = do
      awaitPromise $ startFaucet' `select` getSomeAda' `select` fundFaucet'
      start faucet
    startFaucet' = endpoint @"startFaucet" startFaucet
    getSomeAda' = endpoint @"getSomeAda" getSomeAda
    fundFaucet' = endpoint @"fundFaucet" fundFaucet
