{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import HighLowGame
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

highLowGameParams :: HighLowGameParams
highLowGameParams =
  HighLowGameParams
    {
    }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "high-low-game-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[HighLowGameParams, HighLowGameDatum, HighLowGameRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "High Low Game Validator"
    , preambleDescription = Just "Blueprint for a Plutus script validating High Low Game transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "High Low Game Validator"
    , validatorDescription = Just "Plutus script validating High Low Game transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @HighLowGameParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the High Low Game validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @HighLowGameRedeemer
          }
    , validatorDatum =
        Just
          ( MkArgumentBlueprint
              { argumentTitle = Just "Datum"
              , argumentDescription = Just "Datum for the High Low Game validator"
              , argumentPurpose = Set.fromList [Spend]
              , argumentSchema = definitionRef @HighLowGameDatum
              }
          )
    , validatorCompiled = do
        let script = highLowGameValidatorScript highLowGameParams
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV2 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
