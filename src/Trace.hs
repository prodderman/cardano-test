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
import           Ledger
import           Plutus.Trace.Emulator as Emulator
import qualified Prelude               as P
import           Wallet.Emulator

import           Faucet.AdminSide
import           Faucet.UserSide
import           Faucet.Validator
import           Playground.Contract

runTrace :: P.IO ()
runTrace = runEmulatorTraceIO trace

trace :: EmulatorTrace ()
trace = do
    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        wallet3 = knownWallet 3
        wallet4 = knownWallet 4
        rightApiKey = "right api key"
        wrongApiKey = "wrong api key"
    creator <- activateContractWallet wallet1 P.$ runFaucet (NewFaucet rightApiKey)
    h1 <- activateContractWallet wallet1 endpoints
    h2 <- activateContractWallet wallet2 endpoints
    h3 <- activateContractWallet wallet3 endpoints
    h4 <- activateContractWallet wallet4 endpoints
    void P.$ Emulator.waitNSlots 1
    Faucet cs <- getFaucet creator

    callEndpoint @"fundFaucet" h1 P.$ FundParams cs 90_000_000
    void P.$ Emulator.waitNSlots 1
    callEndpoint @"fundFaucet" h2 P.$ FundParams cs 90_000_000
    void P.$ Emulator.waitNSlots 1
    callEndpoint @"getSomeAda" h4 P.$ GetParams cs wrongApiKey
    void P.$ Emulator.waitNSlots 1
    callEndpoint @"getSomeAda" h3 P.$ GetParams cs rightApiKey
    void P.$ Emulator.waitNSlots 100
    callEndpoint @"getSomeAda" h3 P.$ GetParams cs rightApiKey
    void P.$ Emulator.waitNSlots 1

  where
      getFaucet :: ContractHandle (Last Faucet) (Endpoint "" ()) Text -> EmulatorTrace Faucet
      getFaucet h = do
          l <- observableState h
          case l of
              Last Nothing       -> waitNSlots 1 P.>> getFaucet h
              Last (Just faucet) -> P.pure faucet
