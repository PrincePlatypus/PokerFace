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

module VRFHolder where

import GHC.Generics (Generic)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, txInfoOutputs, txInfoInputs, txInfoValidRange, txSignedBy)
import PlutusLedgerApi.V1.Value (valueOf, lovelaceValueOf)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Builtins (serialiseData)

-- Parameters for the VRFHolder validator
data VRFHolderParams = VRFHolderParams
    { vhpCoinFlipGameScriptHash :: PlutusTx.BuiltinByteString
    -- ^ The script hash of the CoinFlipGame validator
    , vhpHousePubKey :: PubKeyHash
    -- ^ The house's public key for signing VRF
    }
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''VRFHolderParams
PlutusTx.makeIsDataIndexed ''VRFHolderParams [('VRFHolderParams, 0)]

-- Datum to store VRF information
data VRFHolderDatum = VRFHolderDatum
    { vhdGameTxId :: PlutusTx.BuiltinByteString
    -- ^ Transaction ID of the CoinFlipGame UTxO
    , vhdVRFResult :: Integer
    -- ^ The VRF result
    , vhdVRFGenerationTime :: POSIXTime
    -- ^ When the VRF was generated
    , vhdSubscriptionId :: Integer
    -- ^ Chainlink subscription ID
    }
    deriving stock (Generic)

PlutusTx.makeIsDataIndexed ''VRFHolderDatum [('VRFHolderDatum, 0)]
PlutusTx.makeLift ''VRFHolderDatum

-- Simple redeemer for submitting VRF
data VRFHolderRedeemer = SubmitVRF
    deriving stock (Generic)
    deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''VRFHolderRedeemer
PlutusTx.makeIsDataIndexed ''VRFHolderRedeemer [('SubmitVRF, 0)]

{-# INLINEABLE vrfHolderTypedValidator #-}
vrfHolderTypedValidator :: 
    VRFHolderParams -> 
    VRFHolderDatum -> 
    VRFHolderRedeemer -> 
    ScriptContext -> 
    Bool
vrfHolderTypedValidator params datum SubmitVRF ctx@(ScriptContext txInfo _) =
    PlutusTx.and
        [ -- Check that CoinFlipGame UTxO is used as input and matches txId
          hasCoinFlipGameInput (vhdGameTxId datum)
        , -- Check that VRF is generated 3 blocks after game start
          correctVRFTiming
        , -- Check that house signed the transaction
          txSignedBy txInfo (vhpHousePubKey params)
        , -- Check VRF generation time is 1 minute after game start
          correctVRFGenerationTime
        , -- Check that CoinFlipGame UTxO is preserved in output
          preservesCoinFlipGameOutput
        ]
    where
        -- Helper to find CoinFlipGame input and verify its txId
        hasCoinFlipGameInput :: PlutusTx.BuiltinByteString -> Bool
        hasCoinFlipGameInput expectedTxId =
            case PlutusTx.find
                (\(TxInInfo txInRef txOut) ->
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash ->
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                PlutusTx.== vhpCoinFlipGameScriptHash params
                                PlutusTx.&& PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData txInRef) 
                                    PlutusTx.== expectedTxId
                        Nothing -> False
                )
                (txInfoInputs txInfo) of
                Just _ -> True
                Nothing -> PlutusTx.traceError "CoinFlipGame input not found or wrong txId"

        -- Check that VRF is generated 3 blocks after game start
        correctVRFTiming :: Bool
        correctVRFTiming = 
            case PlutusTx.find
                (\(TxInInfo txInRef txOut) ->
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash ->
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                PlutusTx.== vhpCoinFlipGameScriptHash params
                        Nothing -> False
                )
                (txInfoInputs txInfo) of
                Just (TxInInfo txInRef _) ->
                    -- Get the lower bound of the valid range
                    case ivFrom (txInfoValidRange txInfo) of
                        LowerBound (Finite startTime) _ ->
                            -- Check if at least 3 blocks have passed (assuming 20 sec per block)
                            vhdVRFGenerationTime datum PlutusTx.>= (startTime PlutusTx.+ 60000)  -- 60 seconds
                        _ -> False
                Nothing -> False

        -- Check VRF generation time is 1 minute after game start
        correctVRFGenerationTime :: Bool
        correctVRFGenerationTime =
            case PlutusTx.find
                (\(TxInInfo txInRef txOut) ->
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash ->
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                PlutusTx.== vhpCoinFlipGameScriptHash params
                        Nothing -> False
                )
                (txInfoInputs txInfo) of
                Just (TxInInfo txInRef _) ->
                    -- Get the lower bound of the valid range
                    case ivFrom (txInfoValidRange txInfo) of
                        LowerBound (Finite startTime) _ ->
                            -- Check if VRF was generated exactly 1 minute after
                            vhdVRFGenerationTime datum PlutusTx.== (startTime PlutusTx.+ 60000)  -- 60 seconds
                        _ -> False
                Nothing -> False

        -- Find game start time from CoinFlipGame UTxO
        findGameStartTime :: POSIXTime
        findGameStartTime =
            -- Implementation depends on how you store start time in CoinFlipGame
            -- This is a placeholder for the actual implementation
            0 -- TODO: Implement game start time retrieval

        -- Check that CoinFlipGame UTxO is preserved unchanged in output
        preservesCoinFlipGameOutput :: Bool
        preservesCoinFlipGameOutput =
            case PlutusTx.find
                (\(TxInInfo _ txOut) ->
                    case toScriptHash (txOutAddress txOut) of
                        Just scriptHash ->
                            PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                PlutusTx.== vhpCoinFlipGameScriptHash params
                        Nothing -> False
                )
                (txInfoInputs txInfo) of
                Just (TxInInfo _ originalTxOut) ->
                    case PlutusTx.find
                        (\txOut ->
                            case toScriptHash (txOutAddress txOut) of
                                Just scriptHash ->
                                    PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData scriptHash)
                                        PlutusTx.== vhpCoinFlipGameScriptHash params
                                Nothing -> False
                        )
                        (txInfoOutputs txInfo) of
                        Just newTxOut -> 
                            -- Compare all relevant fields
                            txOutValue originalTxOut PlutusTx.== txOutValue newTxOut PlutusTx.&&
                            txOutDatum originalTxOut PlutusTx.== txOutDatum newTxOut
                        Nothing -> False
                Nothing -> False

{-# INLINEABLE vrfHolderUntypedValidator #-}
vrfHolderUntypedValidator ::
    VRFHolderParams ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    PlutusTx.BuiltinUnit
vrfHolderUntypedValidator params datum redeemer ctx =
    PlutusTx.check
        ( vrfHolderTypedValidator
            params
            (PlutusTx.unsafeFromBuiltinData datum)
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

vrfHolderValidatorScript ::
    VRFHolderParams ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
vrfHolderValidatorScript params =
    $$(PlutusTx.compile [||vrfHolderUntypedValidator||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

