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

module Main where

import NFT3s
import Data.ByteString.Short qualified as Short
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Blueprint
import System.Environment (getArgs)

scriptIdentityParams :: ScriptIdentityParams
scriptIdentityParams =
  ScriptIdentityParams
    { sipHousePkh = error "Replace with house public key hash"
    }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "script-identity-policy"
    , contractPreamble = myPreamble
    , contractMintingPolicies = Set.singleton myMintingPolicy
    , contractDefinitions = deriveDefinitions @[ScriptIdentityParams]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Script Identity NFT Minting Policy"
    , preambleDescription = Just "Blueprint for a Plutus minting policy that creates identification NFTs for CoinFlip, VRFHolder, and HousePot scripts"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV2
    , preambleLicense = Just "Apache-2.0"
    }

myMintingPolicy :: MintingPolicyBlueprint referencedTypes
myMintingPolicy =
  MkMintingPolicyBlueprint
    { mintingPolicyTitle = "Script Identity NFT Policy"
    , mintingPolicyDescription = Just "Mints exactly three NFTs to identify official script instances"
    , mintingPolicyParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "House public key hash for authorization"
            , parameterPurpose = Set.singleton Mint
            , parameterSchema = definitionRef @ScriptIdentityParams
            }
        ]
    , mintingPolicyRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Unit redeemer (not used)"
          , argumentPurpose = Set.singleton Mint
          , argumentSchema = definitionRef @()
          }
    , mintingPolicyCompiled = do
        let script = scriptIdentityPolicyScript scriptIdentityParams
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledMintingPolicy PlutusV2 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)


