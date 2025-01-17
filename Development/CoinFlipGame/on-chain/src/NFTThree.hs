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

module NFTThree where

import GHC.Generics (Generic)


import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (TokenName(..), flattenValue)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext (..), TxInfo (..), TxOut(..), txOutAddress)
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusLedgerApi.V1.Address (toScriptHash)
import PlutusLedgerApi.V1.Time (POSIXTime(..))
import PlutusLedgerApi.V1.Interval (Extended(..), LowerBound(..), UpperBound(..), ivFrom, ivTo, contains, to, from)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx

-- Now we only need PubKeyHash as param since we'll use tx time
type ScriptIdentityParams = PubKeyHash
type ScriptIdentityRedeemer = ()

-- Token names stay the same
{-# INLINEABLE coinFlipTokenName #-}
coinFlipTokenName :: TokenName
coinFlipTokenName = TokenName "CoinFlip"

{-# INLINEABLE vrfHolderTokenName #-}
vrfHolderTokenName :: TokenName
vrfHolderTokenName = TokenName "VRFHolder"

{-# INLINEABLE housePotTokenName #-}
housePotTokenName :: TokenName
housePotTokenName = TokenName "HousePot"

{-# INLINEABLE scriptIdentityTypedPolicy #-}
scriptIdentityTypedPolicy ::
    ScriptIdentityParams ->
    ScriptIdentityRedeemer ->
    ScriptContext ->
    Bool
scriptIdentityTypedPolicy pkh _redeemer ctx =
    PlutusTx.and
        [ PlutusTx.traceIfFalse "Unauthorized: Transaction not signed by correct key" 
            (txSignedBy txInfo pkh)
        , PlutusTx.traceIfFalse "Invalid: Must mint exactly three tokens"
            mintedExactlyThreeTokens
        , PlutusTx.traceIfFalse "Invalid: Transaction outside minting window"
            validateMintingWindow
        ]
  where
    txInfo = scriptContextTxInfo ctx
    
    -- Get the transaction's validity start time and ensure minting happens within 10 seconds
    validateMintingWindow = 
        case ivFrom (txInfoValidRange txInfo) of
            LowerBound (Finite startTime) _ -> 
                -- Ensure tx validity window is at most 10 seconds
                case ivTo (txInfoValidRange txInfo) of
                    UpperBound (Finite endTime) _ ->
                        let validWindow = endTime PlutusTx.<= (startTime PlutusTx.+ 10000)
                            startTimeInt = getPOSIXTime startTime
                            endTimeInt = getPOSIXTime endTime
                        in PlutusTx.traceIfFalse 
                            ("Invalid time window - Start: " PlutusTx.<> PlutusTx.show startTimeInt 
                             PlutusTx.<> " End: " PlutusTx.<> PlutusTx.show endTimeInt)
                            validWindow
                    _ -> PlutusTx.traceError "Invalid: Missing upper bound for validity interval"
            _ -> PlutusTx.traceError "Invalid: Missing lower bound for validity interval"
    
    mintedExactlyThreeTokens = case flattenValue (txInfoMint txInfo) of
        [(cs1, tn1, q1), (cs2, tn2, q2), (cs3, tn3, q3)] ->
            let conditions = 
                    [ (cs1 PlutusTx.== ownCurrencySymbol ctx, "Invalid currency symbol for token 1")
                    , (cs2 PlutusTx.== ownCurrencySymbol ctx, "Invalid currency symbol for token 2")
                    , (cs3 PlutusTx.== ownCurrencySymbol ctx, "Invalid currency symbol for token 3")
                    , (q1 PlutusTx.== 1, "Invalid quantity for token 1")
                    , (q2 PlutusTx.== 1, "Invalid quantity for token 2")
                    , (q3 PlutusTx.== 1, "Invalid quantity for token 3")
                    , (tn1 PlutusTx.== coinFlipTokenName, "Invalid token name: expected CoinFlip")
                    , (tn2 PlutusTx.== vrfHolderTokenName, "Invalid token name: expected VRFHolder")
                    , (tn3 PlutusTx.== housePotTokenName, "Invalid token name: expected HousePot")
                    ]
            in PlutusTx.all (\(condition, msg) -> PlutusTx.traceIfFalse msg condition) conditions
        xs -> PlutusTx.traceError 
              ("Invalid number of minted tokens: " PlutusTx.<> PlutusTx.show (PlutusTx.length xs))

{-# INLINEABLE scriptIdentityUntypedPolicy #-}
scriptIdentityUntypedPolicy ::
    ScriptIdentityParams ->
    BuiltinData ->
    BuiltinData ->
    PlutusTx.BuiltinUnit
scriptIdentityUntypedPolicy pkh redeemer ctx =
    PlutusTx.check
        ( scriptIdentityTypedPolicy
            pkh
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

scriptIdentityPolicyScript ::
    ScriptIdentityParams ->
    CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
scriptIdentityPolicyScript pkh =
    $$(PlutusTx.compile [||scriptIdentityUntypedPolicy||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh

