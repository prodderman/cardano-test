{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Faucet.Validator where

import           Ledger                 hiding (singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Playground.Contract
import           Plutus.V1.Ledger.Value (assetClassValueOf, geq)
import qualified PlutusTx
import           PlutusTx.AssocMap      (Map)
import           PlutusTx.Prelude       hiding (Semigroup (..), unless)
import qualified Prelude                as P

import qualified Faucet.Policy          as Policy

data Faucet = Faucet
  { currencySymbol :: !CurrencySymbol
  , apiKey         :: !ApiKey
  }  deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

type ApiKey = PlutusTx.Prelude.BuiltinByteString
type ConsumeDatum = Map PubKeyHash POSIXTime
data FaucetAction = Fund | Use PlutusTx.Prelude.BuiltinByteString deriving P.Show

PlutusTx.makeLift ''Faucet
PlutusTx.unstableMakeIsData ''Faucet
PlutusTx.unstableMakeIsData ''FaucetAction

{-# INLINABLE mkValidator #-}
mkValidator :: Faucet -> ConsumeDatum -> FaucetAction -> ScriptContext -> Bool
mkValidator (Faucet cs _) dat action ctx =
  traceIfFalse "token missing from input" inputHasToken &&
  traceIfFalse "token missing from output" outputHasToken &&
  case action of
    Fund -> traceIfFalse "datum changed" $ outputDatum == Just dat &&
            traceIfFalse "faucet is not funded" hasFunded
    Use apiKey -> False
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one faucet output"

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (Policy.faucetAsset cs) == 1

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (Policy.faucetAsset cs) == 1

    outputDatum :: Maybe ConsumeDatum
    outputDatum = do
      Datum datum <- txOutDatum ownOutput >>= findDatumByHash
      PlutusTx.fromBuiltinData datum
      where
        findDatumByHash = (`findDatum` txInfo)

    hasFunded :: Bool
    hasFunded = outputValue `geq` inputValue
      where
        inputValue = txOutValue ownInput
        outputValue = txOutValue ownOutput


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
