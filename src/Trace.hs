{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Trace where

import           Data.Functor          (void)
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Plutus.Trace.Emulator as Emulator
import qualified Prelude               as P
import           Wallet.Emulator

import           Faucet.OffChain
import           Faucet.Validator

runTrace :: P.IO ()
runTrace = runEmulatorTraceIO trace

trace :: EmulatorTrace ()
trace = do
    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        rightApiKey = "right api key"
        -- wrongApiKey = "wrong api key"

    creator <- activateContractWallet wallet1 P.$ runFaucet (NewFaucet rightApiKey)
    void P.$ Emulator.waitNSlots 1
    faucet  <- getFaucet creator

    callEndpoint @"fundFaucet" creator P.$ FundParams faucet 90_000_000
    void P.$ Emulator.waitNSlots 1
    callEndpoint @"sendAda" creator P.$ SendParams faucet (walletPubKeyHash wallet2) (Just rightApiKey)
    void P.$ Emulator.waitNSlots 100
    callEndpoint @"sendAda" creator P.$ SendParams faucet (walletPubKeyHash wallet2) (Just rightApiKey)
    void P.$ Emulator.waitNSlots 1
    -- callEndpoint @"getSomeAda" h3 P.$ GetParams cs rightApiKey
    -- void P.$ Emulator.waitNSlots 100
    -- callEndpoint @"getSomeAda" h3 P.$ GetParams cs rightApiKey
    -- void P.$ Emulator.waitNSlots 1

  where
      getFaucet :: ContractHandle (Last Faucet) FaucetSchema Text -> EmulatorTrace Faucet
      getFaucet h = do
          l <- observableState h
          case l of
              Last Nothing       -> waitNSlots 1 P.>> getFaucet h
              Last (Just faucet) -> P.pure faucet
