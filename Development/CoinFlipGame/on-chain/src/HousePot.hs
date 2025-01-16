-- HighLowGame.hs

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}  
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module HousePot where

import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..), 
                          TokenName (..), TxInfo (..), TxOut (..), txOutAddress, txOutValue, 
                          PubKeyHash, TxInInfo(..))
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, txInfoOutputs, txInfoInputs)
import PlutusLedgerApi.V1.Value (valueOf, lovelaceValueOf, Lovelace)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Builtins (serialiseData)

-- Parameters for the HousePot validator
data HousePotParams = HousePotParams
    { hppCoinFlipGameScriptHash :: PlutusTx.BuiltinByteString
    -- ^ The script hash of the CoinFlipGame validator
    , hppHouseAddress :: PubKeyHash
    -- ^ The house's public key hash for receiving change
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''HousePotParams
PlutusTx.makeIsDataIndexed ''HousePotParams [('HousePotParams, 0)]

-- Simple redeemer for betting
data HousePotRedeemer = Bet
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''HousePotRedeemer
PlutusTx.makeIsDataIndexed ''HousePotRedeemer [('Bet, 0)]

{-# INLINEABLE housePotTypedValidator #-}
housePotTypedValidator :: HousePotParams -> BuiltinData -> HousePotRedeemer -> ScriptContext -> Bool
housePotTypedValidator params _ Bet ctx@(ScriptContext txInfo _) =
    PlutusTx.and
        [ -- Check that CoinFlipGame script is used as input
          hasCoinFlipGameInput
        , -- Check that player's bet matches house deduction
          correctBetAmounts
        , -- Check that outputs are correct (CoinFlipGame gets combined amount, house gets change)
          correctOutputs
        ]
    where
        hasCoinFlipGameInput :: Bool
        hasCoinFlipGameInput = 
            case PlutusTx.find
                (\(TxInInfo _ txOut) -> 
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash -> 
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash) 
                                PlutusTx.== hppCoinFlipGameScriptHash params
                        Nothing -> False
                )
                (txInfoInputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError "CoinFlipGame input not found"

        -- Get the player's bet amount from their input UTxO
        getPlayerBetAmount :: Lovelace
        getPlayerBetAmount =
            case PlutusTx.find
                (\(TxInInfo _ txOut) ->
                    case toScriptHash (txOutAddress txOut) of
                        Just _ -> False  -- Skip script UTxOs
                        Nothing -> True  -- This should be the player's input
                )
                (txInfoInputs txInfo) of
                Just (TxInInfo _ txOut) -> lovelaceValueOf (txOutValue txOut)
                Nothing -> PlutusTx.traceError "Player input not found"

        -- Get the amount deducted from house (difference between input and change output)
        getHouseBetAmount :: Lovelace
        getHouseBetAmount =
            let houseInput = case PlutusTx.find 
                    (\(TxInInfo _ txOut) -> 
                        -- Find the house UTxO being spent (our own input)
                        case toScriptHash (txOutAddress txOut) of
                            Just _ -> True  -- This is our script UTxO
                            Nothing -> False
                    ) (txInfoInputs txInfo) of
                    Just (TxInInfo _ txOut) -> lovelaceValueOf (txOutValue txOut)
                    Nothing -> PlutusTx.traceError "House input not found"
                
                houseChange = case PlutusTx.find
                    (\txOut -> 
                        -- Find the change output going back to house address
                        toPubKeyHash (txOutAddress txOut) PlutusTx.== Just (hppHouseAddress params)
                    ) (txInfoOutputs txInfo) of
                    Just txOut -> lovelaceValueOf (txOutValue txOut)
                    Nothing -> PlutusTx.traceError "House change output not found"
            in houseInput PlutusTx.- houseChange  -- The difference is the house's bet

        -- Check that the amount deducted from house matches player's bet
        correctBetAmounts :: Bool
        correctBetAmounts =
            let playerBet = getPlayerBetAmount
                houseBet = getHouseBetAmount
            in houseBet PlutusTx.== playerBet

        -- Verify that outputs are correct
        correctOutputs :: Bool
        correctOutputs =
            let playerBet = getPlayerBetAmount
                totalBet = playerBet PlutusTx.+ playerBet  -- Combined bets
            in case PlutusTx.find
                (\txOut ->
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash ->
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                PlutusTx.== hppCoinFlipGameScriptHash params
                                PlutusTx.&& lovelaceValueOf (txOutValue txOut) PlutusTx.== totalBet
                        Nothing -> False
                )
                (txInfoOutputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError "CoinFlipGame output not found with correct amount"

{-# INLINEABLE housePotUntypedValidator #-}
housePotUntypedValidator ::
    HousePotParams ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    PlutusTx.BuiltinUnit
housePotUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( housePotTypedValidator
            params
            datum
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

housePotValidatorScript ::
    HousePotParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
housePotValidatorScript params =
    $$(PlutusTx.compile [||housePotUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params


