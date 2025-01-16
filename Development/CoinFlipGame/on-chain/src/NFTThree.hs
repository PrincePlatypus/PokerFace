

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
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext (..), TxInfo (..), TxOut(..), OutputDatum(..), txOutAddress)
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusLedgerApi.V1.Address (toScriptHash)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

-- Parameters for the minting policy
data ScriptIdentityParams = ScriptIdentityParams
    { sipHousePkh :: PubKeyHash  -- House's public key hash
    }
    deriving stock (Generic)

PlutusTx.makeLift ''ScriptIdentityParams
PlutusTx.makeIsDataIndexed ''ScriptIdentityParams [('ScriptIdentityParams, 0)]

-- Token names for each script
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
    () ->  -- Redeemer not used
    ScriptContext ->
    Bool
scriptIdentityTypedPolicy params _ ctx =
    PlutusTx.and
        [ -- Only house can mint
          txSignedBy txInfo (sipHousePkh params)
        , -- Exactly three tokens must be minted
          validateMintedTokens
        , -- Each token must go to a script address
          validateTokenDestinations
        ]
  where
    txInfo = scriptContextTxInfo ctx

    -- Validate that exactly one of each token is minted
    validateMintedTokens :: Bool
    validateMintedTokens =
        case flattenValue (txInfoMint txInfo) of
            [(cs, tn1, q1), (cs', tn2, q2), (cs'', tn3, q3)] ->
                PlutusTx.and
                    [ cs PlutusTx.== ownCurrencySymbol ctx
                    , cs' PlutusTx.== ownCurrencySymbol ctx
                    , cs'' PlutusTx.== ownCurrencySymbol ctx
                    , q1 PlutusTx.== 1
                    , q2 PlutusTx.== 1
                    , q3 PlutusTx.== 1
                    , PlutusTx.and
                        [ tn1 PlutusTx.== coinFlipTokenName PlutusTx.|| 
                          tn1 PlutusTx.== vrfHolderTokenName PlutusTx.|| 
                          tn1 PlutusTx.== housePotTokenName
                        , tn2 PlutusTx.== coinFlipTokenName PlutusTx.|| 
                          tn2 PlutusTx.== vrfHolderTokenName PlutusTx.|| 
                          tn2 PlutusTx.== housePotTokenName
                        , tn3 PlutusTx.== coinFlipTokenName PlutusTx.|| 
                          tn3 PlutusTx.== vrfHolderTokenName PlutusTx.|| 
                          tn3 PlutusTx.== housePotTokenName
                        , tn1 PlutusTx./= tn2
                        , tn2 PlutusTx./= tn3
                        , tn1 PlutusTx./= tn3
                        ]
                    ]
            _ -> False

    -- Validate that tokens go to script addresses
    validateTokenDestinations :: Bool
    validateTokenDestinations =
        let outputs = txInfoOutputs txInfo
        in PlutusTx.and $ PlutusTx.map
            (\out -> case toScriptHash (txOutAddress out) of
                Just _ -> True
                Nothing -> False)
            outputs

{-# INLINEABLE scriptIdentityUntypedPolicy #-}
scriptIdentityUntypedPolicy ::
    ScriptIdentityParams ->
    BuiltinData ->
    BuiltinData ->
    PlutusTx.BuiltinUnit
scriptIdentityUntypedPolicy params redeemer ctx =
    PlutusTx.check
        ( scriptIdentityTypedPolicy
            params
            (PlutusTx.unsafeFromBuiltinData redeemer)
            (PlutusTx.unsafeFromBuiltinData ctx)
        )

scriptIdentityPolicyScript ::
    ScriptIdentityParams ->
    CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
scriptIdentityPolicyScript params =
    $$(PlutusTx.compile [||scriptIdentityUntypedPolicy||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

