{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Faucet.AdminSide where

import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Maybe
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 hiding (singleton)
import qualified Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Playground.Contract
import           Plutus.Contract        as Contract
import           Plutus.Contract.Wallet (getUnspentOutput)
import qualified PlutusTx
import           PlutusTx.AssocMap      as PMap
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import qualified Prelude                as P

import           Data.Monoid
import qualified Faucet.Policy          as Policy
import qualified Faucet.Validator       as Validator

data FaucetParams = NewFaucet Validator.ApiKey | ExistingFaucet Validator.Faucet

startFaucet :: FaucetParams -> Contract w s Text (Validator, Validator.Faucet)
startFaucet (NewFaucet apiKey) = do
  cs <- mintForFaucet
  let faucet = Validator.Faucet cs apiKey
      emptyBook = PMap.empty :: PMap.Map PubKeyHash POSIXTime
      apiKeyHash = sha3_256 apiKey
      tx = Constraints.mustPayToTheScript (Validator.ConsumeDatum emptyBook apiKeyHash) $ assetClassValue (Policy.faucetAsset cs) 1
  ledgerTx <- submitTxConstraints (Validator.typedValidator faucet) tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  pure (Validator.validator faucet, faucet)

startFaucet (ExistingFaucet faucet) = do
  validator <- findFaucet faucet
  case validator of
    Nothing -> throwError "faucet does not exist"
    Just v  -> pure (v, faucet)

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

findFaucet :: Validator.Faucet -> Contract w s Text (Maybe Validator)
findFaucet params@(Validator.Faucet cs _) = do
  let validator = Validator.validator params
  utxos <- Contract.utxosAt $ Validator.scrAddress params
  let faucetInfo = [ (oref, txOut)
            | (oref, txOut) <- Map.toList utxos
            , Value.valueOf (_ciTxOutValue txOut) cs Policy.faucetTokenName == 1
            ]
  pure $ case faucetInfo of
      [(_, txOut)] -> case _ciTxOutDatum txOut of
          Left _ -> Nothing
          Right (Datum datum) -> case (PlutusTx.fromBuiltinData datum :: Maybe Validator.ConsumeDatum) of
            Nothing -> Nothing
            Just _  -> Just validator
      _ -> Nothing

runFaucet :: FaucetParams -> Contract (Last (Validator, Validator.Faucet)) (Endpoint "" ()) Text ()
runFaucet params = do
  faucet <- startFaucet params
  tell $ Last $ Just faucet
