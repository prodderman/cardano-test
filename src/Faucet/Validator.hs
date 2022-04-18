{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Faucet.Validator where

import           Ledger               hiding (singleton)
import           Ledger.Ada           (lovelaceValueOf)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (assetClassValueOf, geq)
import           Playground.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap    as PMap
import           PlutusTx.Prelude
import qualified Prelude              as P

import qualified Faucet.Policy        as Policy

type ApiKey = BuiltinByteString

data Faucet = Faucet
  { fCurrencySymbol :: !CurrencySymbol
  , fOwner          :: !PubKeyHash
  , fApiKey         :: !ApiKey
  }
  deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

newtype ConsumeDatum = ConsumeDatum
  { book       :: PMap.Map PubKeyHash POSIXTime
  } deriving (P.Show)

instance Eq ConsumeDatum where
    {-# INLINABLE (==) #-}
    a == b = book a == book b

data FaucetAction = Fund | Use PubKeyHash (Maybe ApiKey) deriving P.Show

PlutusTx.makeLift ''Faucet
PlutusTx.unstableMakeIsData ''Faucet
PlutusTx.unstableMakeIsData ''FaucetAction
PlutusTx.unstableMakeIsData ''ConsumeDatum

{-# INLINABLE allowedAmount #-}
allowedAmount :: Value
allowedAmount = lovelaceValueOf 5_000_000

{-# INLINABLE allowedAmountWithRightApiKey #-}
allowedAmountWithRightApiKey :: Value
allowedAmountWithRightApiKey = lovelaceValueOf 10_000_000

{-# INLINABLE waitingTime #-}
waitingTime :: POSIXTime
waitingTime = POSIXTime (60 * 1000)

{-# INLINABLE mkValidator #-}
mkValidator :: Faucet -> ConsumeDatum -> FaucetAction -> ScriptContext -> Bool
mkValidator Faucet {..} datum action ctx =
  traceIfFalse "Wrong signature" signedByOwner &&
  traceIfFalse "Token missing from input" inputHasToken &&
  traceIfFalse "Token missing from output" outputHasToken &&
  case action of
    Fund -> traceIfFalse "Datum changed" $ outputDatum == datum &&
            traceIfFalse "Faucet is not funded" hasFunded
    Use addr ak ->
            traceIfFalse "Wrong time range" rangeValid &&
            traceIfFalse "It is not yet time" (canConsume addr) &&
            traceIfFalse "Wrong amount" (rightAmount ak) &&
            traceIfFalse "Not all signers are included in datum" (newAddrIncluded addr) &&
            traceIfFalse "Deadlines for all signers are not correct" (isDeadlineValid addr) &&
            traceIfFalse "There can't be extra signers" (thereAreNoExtraRecords addr)
  where
    txInfo = scriptContextTxInfo ctx
    range = txInfoValidRange txInfo

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Just i  -> txInInfoResolved i
        Nothing -> traceError "faucet input missing"

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one faucet output"

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (Policy.faucetAsset fCurrencySymbol) == 1

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (Policy.faucetAsset fCurrencySymbol) == 1

    inputBalance :: Value
    inputBalance = txOutValue ownInput

    outputBalance :: Value
    outputBalance = txOutValue ownOutput

    outputDatum :: ConsumeDatum
    outputDatum = case getDatum of
      Nothing -> traceError "datum is missing"
      Just d  -> d
      where
        findDatumByHash = (`findDatum` txInfo)
        getDatum = do
          Datum d <- txOutDatum ownOutput >>= findDatumByHash
          PlutusTx.fromBuiltinData d

    signedByOwner :: Bool
    signedByOwner = txSignedBy txInfo fOwner

    rangeValid :: Bool
    rangeValid = toTime - fromTime <= 10000
      where
        fromTime = case ivFrom range of
          LowerBound (Finite t) _ -> t
          _                       -> traceError "Invalid time range"
        toTime = case ivTo range of
          UpperBound (Finite t) _ -> t
          _                       -> traceError "Invalid time range"

    hasFunded :: Bool
    hasFunded = outputBalance `geq` inputBalance

    canConsume :: PubKeyHash -> Bool
    canConsume addr = case getDeadline addr $ book datum of
      Just dl -> (dl + waitingTime) `before` range
      Nothing -> True

    newAddrIncluded :: PubKeyHash -> Bool
    newAddrIncluded addr = addr `PMap.member` book outputDatum

    isDeadlineValid :: PubKeyHash ->  Bool
    isDeadlineValid addr = case getDeadline addr (book outputDatum) of
      Just dl -> dl `Ledger.member` range
      Nothing -> traceError "Address is not specified"

    thereAreNoExtraRecords :: PubKeyHash ->  Bool
    thereAreNoExtraRecords addr = length newRecord == 1 && addr `elem` newRecord
      where
        newRecord = filter (\x ->  (not . elem x) inputAddresses || x == addr) outAddresses
        inputAddresses = PMap.keys $ book datum
        outAddresses = PMap.keys $ book outputDatum

    rightAmount :: Maybe ApiKey -> Bool
    rightAmount mApiKey = case mApiKey of
      Nothing -> inputBalance - outputBalance == allowedAmount
      Just ak | ak == fApiKey && inputBalance - outputBalance == allowedAmountWithRightApiKey -> True
              | ak /= fApiKey && inputBalance - outputBalance == allowedAmount -> True
              | otherwise -> False

    getDeadline addr b = addr `PMap.lookup` b

data FaucetType
instance Scripts.ValidatorTypes FaucetType where
    type instance DatumType FaucetType = ConsumeDatum
    type instance RedeemerType FaucetType = FaucetAction

typedValidator :: Faucet -> Scripts.TypedValidator FaucetType
typedValidator faucet = Scripts.mkTypedValidator @FaucetType
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode faucet)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ConsumeDatum @FaucetAction

validator :: Faucet -> Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: Faucet -> Ledger.Address
scrAddress = scriptAddress . validator
