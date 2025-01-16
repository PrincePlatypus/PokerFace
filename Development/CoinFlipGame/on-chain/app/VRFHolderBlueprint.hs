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

import VRFHolder
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

vrfHolderParams :: VRFHolderParams
vrfHolderParams =
    VRFHolderParams
        { vhpCoinFlipGameScriptHash = error "Replace with CoinFlipGame script hash"
        , vhpHousePubKey = error "Replace with house public key hash"
        }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
    MkContractBlueprint
        { contractId = Just "vrf-holder-validator"
        , contractPreamble = myPreamble
        , contractValidators = Set.singleton myValidator
        , contractDefinitions = deriveDefinitions @[VRFHolderParams, VRFHolderDatum, VRFHolderRedeemer]
        }

myPreamble :: Preamble
myPreamble =
    MkPreamble
        { preambleTitle = "VRF Holder Validator"
        , preambleDescription = Just "Blueprint for a Plutus script managing VRF generation and verification in CoinFlip game"
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV2
        , preambleLicense = Just "MIT"
        }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
    MkValidatorBlueprint
        { validatorTitle = "VRF Holder Validator"
        , validatorDescription = Just "Plutus script validating VRF generation and submission"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Parameters"
                , parameterDescription = Just "Compile-time validator parameters"
                , parameterPurpose = Set.singleton Spend
                , parameterSchema = definitionRef @VRFHolderParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Redeemer"
                , argumentDescription = Just "Redeemer for the VRF holder validator"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @VRFHolderRedeemer
                }
        , validatorDatum =
            Just $ MkArgumentBlueprint
                { argumentTitle = Just "Datum"
                , argumentDescription = Just "Datum containing VRF information"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @VRFHolderDatum
                }
        , validatorCompiled = do
            let script = vrfHolderValidatorScript vrfHolderParams
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
