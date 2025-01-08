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

module HighLowGame where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (ScriptContext (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx

data HighLowGameParams = HighLowGameParams
  { -- Add any necessary game parameters here
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''HighLowGameParams
PlutusTx.makeIsDataSchemaIndexed ''HighLowGameParams [('HighLowGameParams, 0)]

data HighLowGameDatum = HighLowGameDatum
  { -- Add any necessary datum fields here
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''HighLowGameDatum
PlutusTx.makeIsDataSchemaIndexed ''HighLowGameDatum [('HighLowGameDatum, 0)]

data HighLowGameRedeemer = HighLowGameRedeemer
  { -- Add any necessary redeemer fields here
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''HighLowGameRedeemer
PlutusTx.makeIsDataSchemaIndexed ''HighLowGameRedeemer [('HighLowGameRedeemer, 0)]

{-# INLINEABLE highLowGameTypedValidator #-}
highLowGameTypedValidator ::
  HighLowGameParams ->
  HighLowGameDatum ->
  HighLowGameRedeemer ->
  ScriptContext ->
  Bool
highLowGameTypedValidator _params _datum _redeemer _ctx = True

{-# INLINEABLE highLowGameUntypedValidator #-}
highLowGameUntypedValidator ::
  HighLowGameParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
highLowGameUntypedValidator params datum redeemer ctx =
  PlutusTx.check
    ( highLowGameTypedValidator
        params
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

highLowGameValidatorScript ::
  HighLowGameParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
highLowGameValidatorScript params =
  $$(PlutusTx.compile [||highLowGameUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

