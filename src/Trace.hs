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
        faucetApiKey = "some api key"
    h1 <- activateContractWallet wallet1 $ endpoints faucetApiKey
    h2 <- activateContractWallet wallet2 $ endpoints faucetApiKey
    h3 <- activateContractWallet wallet3 $ endpoints faucetApiKey
    void $ Emulator.waitNSlots 1
    faucet <- getFaucet h1
    callEndpoint @"startFaucet" h1 faucet
    void $ Emulator.waitNSlots 1
    callEndpoint @"fundFaucet" h1 FundParams {fFaucet = faucet, fAmount = 10_000_000}
    void $ waitNSlots 5

  where
      getFaucet :: ContractHandle (Last Faucet) FaucetSchema Text -> EmulatorTrace Faucet
      getFaucet h = do
          l <- observableState h
          case l of
              Last Nothing       -> waitNSlots 1 >> getFaucet h
              Last (Just faucet) -> pure faucet
