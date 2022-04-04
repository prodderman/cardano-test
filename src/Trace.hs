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
import           Faucet.OffChain
import           Faucet.Validator
import           Plutus.Trace.Emulator as Emulator
import           Prelude               (Applicative (pure), IO, Monad ((>>)),
                                        ($))
import           Wallet.Emulator

runTrace :: IO ()
runTrace = runEmulatorTraceIO trace

trace :: EmulatorTrace ()
trace = do
    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        wallet3 = knownWallet 3
        wallet4 = knownWallet 4
        rightApiKey = "right api key"
        wrongApiKey = "wrong api key"
    h1 <- activateContractWallet wallet1 $ endpoints rightApiKey
    h2 <- activateContractWallet wallet2 $ endpoints rightApiKey
    h3 <- activateContractWallet wallet3 $ endpoints rightApiKey
    h4 <- activateContractWallet wallet4 $ endpoints rightApiKey
    void $ Emulator.waitNSlots 1
    faucet <- getFaucet h1
    callEndpoint @"startFaucet" h1 faucet
    void $ Emulator.waitNSlots 1

    callEndpoint @"fundFaucet" h1 $ FundParams faucet 90_000_000
    void $ Emulator.waitNSlots 2
    callEndpoint @"fundFaucet" h4 $ FundParams faucet 90_000_000
    void $ Emulator.waitNSlots 1

    callEndpoint @"getSomeAda" h2 $ GetParams faucet rightApiKey
    void $ Emulator.waitNSlots 1
    callEndpoint @"getSomeAda" h3 $ GetParams faucet wrongApiKey
    void $ Emulator.waitNSlots 5

  where
      getFaucet :: ContractHandle (Last Faucet) FaucetSchema Text -> EmulatorTrace Faucet
      getFaucet h = do
          l <- observableState h
          case l of
              Last Nothing       -> waitNSlots 1 >> getFaucet h
              Last (Just faucet) -> pure faucet
