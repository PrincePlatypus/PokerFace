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
        [ txSignedBy txInfo pkh  -- Only authorized key can mint
        , mintedExactlyThreeTokens  -- Must mint exactly our three tokens
        , validateMintingWindow     -- Must mint within 10 seconds of tx start
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
                        endTime PlutusTx.<= (startTime PlutusTx.+ 10000)  -- 10 seconds in milliseconds
                    _ -> False
            _ -> False
    
    mintedExactlyThreeTokens = case flattenValue (txInfoMint txInfo) of
        [(cs1, tn1, q1), (cs2, tn2, q2), (cs3, tn3, q3)] ->
            PlutusTx.and
                [ cs1 PlutusTx.== ownCurrencySymbol ctx
                , cs2 PlutusTx.== ownCurrencySymbol ctx
                , cs3 PlutusTx.== ownCurrencySymbol ctx
                , q1 PlutusTx.== 1
                , q2 PlutusTx.== 1
                , q3 PlutusTx.== 1
                , tn1 PlutusTx.== coinFlipTokenName
                , tn2 PlutusTx.== vrfHolderTokenName
                , tn3 PlutusTx.== housePotTokenName
                ]
        _ -> False

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

