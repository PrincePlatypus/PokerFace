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

import HousePot
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

housePotParams :: HousePotParams
housePotParams =
    HousePotParams
        { hppCoinFlipGameScriptHash = error "Replace with CoinFlipGame script hash"
        , hppHouseAddress = error "Replace with house public key hash"
        }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
    MkContractBlueprint
        { contractId = Just "house-pot-validator"
        , contractPreamble = myPreamble
        , contractValidators = Set.singleton myValidator
        , contractDefinitions = deriveDefinitions @[HousePotParams, HousePotRedeemer]
        }

myPreamble :: Preamble
myPreamble =
    MkPreamble
        { preambleTitle = "House Pot Validator"
        , preambleDescription = Just "Blueprint for a Plutus script managing house bets in CoinFlip game"
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV2
        , preambleLicense = Just "MIT"
        }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
    MkValidatorBlueprint
        { validatorTitle = "House Pot Validator"
        , validatorDescription = Just "Plutus script validating house bet transactions"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @HousePotParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Redeemer"
                , argumentDescription = Just "Redeemer for the house pot validator"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @HousePotRedeemer
                }
        , validatorDatum = Nothing  -- HousePot doesn't use datum
        , validatorCompiled = do
            let script = housePotValidatorScript housePotParams
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

