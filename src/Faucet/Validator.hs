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
type ApiKeyHash = BuiltinByteString

newtype Faucet = Faucet { fCurrencySymbol :: CurrencySymbol }
  deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

data ConsumeDatum = ConsumeDatum
  { book       :: PMap.Map PubKeyHash POSIXTime
  , apiKeyHash :: ApiKeyHash
  } deriving (P.Show)

instance Eq ConsumeDatum where
    {-# INLINABLE (==) #-}
    a == b = (book a == book b) && (apiKeyHash a == apiKeyHash b)

data FaucetAction = Fund | Use ApiKey deriving P.Show

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
  traceIfFalse "token missing from input" inputHasToken &&
  traceIfFalse "token missing from output" outputHasToken &&
  traceIfFalse "api key changed" apiKeyHashTheSame &&
  case action of
    Fund -> traceIfFalse "datum changed" $ outputDatum == datum &&
            traceIfFalse "faucet is not funded" hasFunded
    Use ak -> traceIfFalse "it is not yet time" canConsume &&
              traceIfFalse "wrong amount" (rightAmount ak) &&
              traceIfFalse "not all signers are included in datum" allSignersIncluded &&
              traceIfFalse "deadlines for all signers are not correct" signersDeadlinesValid &&
              traceIfFalse "there can't be extra signers" thereAreNoExtraSigners
  where
    txInfo = scriptContextTxInfo ctx
    range = txInfoValidRange txInfo
    signers = txInfoSignatories txInfo
    ownApiKeyHash = apiKeyHash datum

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

    rangeValid :: Bool
    rangeValid = False

    apiKeyHashTheSame :: Bool
    apiKeyHashTheSame = apiKeyHash datum == apiKeyHash outputDatum

    hasFunded :: Bool
    hasFunded = outputBalance `geq` inputBalance

    canConsume :: Bool
    canConsume = all (\deadline -> (deadline + waitingTime) `before` range) $ getSignersDeadlinesFromDatum $ book datum

    allSignersIncluded :: Bool
    allSignersIncluded = all (`PMap.member` book outputDatum) signers

    signersDeadlinesValid :: Bool
    signersDeadlinesValid = all (`Ledger.member` range) $ getSignersDeadlinesFromDatum $ book outputDatum

    thereAreNoExtraSigners :: Bool
    thereAreNoExtraSigners = all (`elem` signers) newSigners
      where
        newSigners = filter (\x ->  (not . elem x) (PMap.keys $ book datum)) (PMap.keys $ book outputDatum)

    rightAmount :: ApiKey -> Bool
    rightAmount apiKey
      | apiKeyHash == ownApiKeyHash && inputBalance - outputBalance == allowedAmountWithRightApiKey = True
      | apiKeyHash /= ownApiKeyHash && inputBalance - outputBalance == allowedAmount = True
      | otherwise = False
      where
        apiKeyHash = sha3_256 apiKey

    getSignersDeadlinesFromDatum b = [deadline | (pkhInBook, deadline) <- PMap.toList b, signerPkh <- signers, pkhInBook == signerPkh]

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
