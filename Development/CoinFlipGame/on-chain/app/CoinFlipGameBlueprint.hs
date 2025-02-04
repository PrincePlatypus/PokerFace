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

import CoinFlipGame
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

coinFlipGameParams :: CoinFlipGameParams
coinFlipGameParams =
    CoinFlipGameParams
        { cfgVRFHolderScriptHash = error "Replace with VRFHolder script hash"
        , cfgHousePotScriptHash = error "Replace with HousePot script hash"
        , cfgHousePubKey = error "Replace with house public key hash"
        }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
    MkContractBlueprint
        { contractId = Just "coin-flip-game-validator"
        , contractPreamble = myPreamble
        , contractValidators = Set.singleton myValidator
        , contractDefinitions = deriveDefinitions @[CoinFlipGameParams, CoinFlipGameDatum, CoinFlipGameRedeemer]
        }

myPreamble :: Preamble
myPreamble =
    MkPreamble
        { preambleTitle = "CoinFlip Game Validator"
        , preambleDescription = Just "Blueprint for a Plutus script managing CoinFlip game logic"
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV2
        , preambleLicense = Just "MIT"
        }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
    MkValidatorBlueprint
        { validatorTitle = "CoinFlip Game Validator"
        , validatorDescription = Just "Plutus script validating CoinFlip game transactions"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @CoinFlipGameParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Redeemer"
                , argumentDescription = Just "Redeemer for the CoinFlip game validator"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @CoinFlipGameRedeemer
                }
        , validatorDatum =
            Just $ MkArgumentBlueprint
                { argumentTitle = Just "Datum"
                , argumentDescription = Just "Datum containing game state information"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @CoinFlipGameDatum
                }
        , validatorCompiled = do
            let script = coinFlipGameValidatorScript coinFlipGameParams
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
