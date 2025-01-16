
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

module CoinFlipGame where
import VRFHolder (VRFHolderDatum(..))

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..), TokenName (..), TxInfo (..), TxOut (..), txOutAddress, txOutValue, PubKeyHash, TxInInfo(..), POSIXTime, txInfoValidRange)
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, txInfoOutputs, txSignedBy)
import PlutusLedgerApi.V1.Value (valueOf, lovelaceValueOf, Lovelace)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Eq qualified as PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusLedgerApi.V1.Credential (Credential(..))
import PlutusTx.Builtins (serialiseData)
import PlutusLedgerApi.V1.Time (POSIXTime, fromMilliSeconds)
import PlutusLedgerApi.V1.Interval (Extended(..), LowerBound(..), UpperBound(..), ivFrom)
import PlutusLedgerApi.V1.Value (Value)



-- BLOCK1
-- CoinFlipGame.hs
data CoinFlipGameParams = CoinFlipGameParams
  { cfgpPlayer :: PubKeyHash
  -- ^ Player's public key hash. The player who initiates the game.
  , cfgpBetAmount :: Lovelace
  -- ^ The amount of ADA bet by the player.
  , cfgpHousePotScriptHash :: PlutusTx.BuiltinByteString
  -- ^ The script hash of the HousePot validator
  , cfgpVRFHolderScriptHash :: PlutusTx.BuiltinByteString
  -- ^ The script hash of the VRFHolder validator
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''CoinFlipGameParams
PlutusTx.makeIsDataSchemaIndexed ''CoinFlipGameParams [('CoinFlipGameParams, 0)]

-- BLOCK2
-- CoinFlipGame.hs
data GameState = Initialized | Started
  -- ^ Represents the different states of the game.
  -- Initialized: The game has been initiated by player1.
  -- Started: Player2 has entered the game and placed their bet.
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''GameState
PlutusTx.makeIsDataSchemaIndexed ''GameState [('Initialized, 0), ('Started, 1)]

-- Add PlutusTx.Eq instance for GameState
instance PlutusTx.Eq GameState where
    {-# INLINEABLE (==) #-}
    Initialized == Initialized = True
    Started == Started = True
    _ == _ = False

-- BLOCK3
-- CoinFlipGame.hs
data CoinSide = Heads | Tails
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''CoinSide
PlutusTx.makeIsDataSchemaIndexed ''CoinSide [('Heads, 0), ('Tails, 1)]

instance PlutusTx.Eq CoinSide where
    {-# INLINEABLE (==) #-}
    Heads == Heads = True
    Tails == Tails = True
    _ == _ = False

data CoinFlipGameDatum = CoinFlipGameDatum 
    { cfgdGameState :: GameState
    , cfgdHousePotScriptHash :: PlutusTx.BuiltinByteString
    , cfgdVRFHolderScriptHash :: PlutusTx.BuiltinByteString
    , cfgdGameId :: PlutusTx.BuiltinByteString
    , cfgdPlayerGuess :: CoinSide
    -- ^ The player's guess (Heads or Tails)
    }
    deriving stock (Generic)

PlutusTx.makeIsDataIndexed ''CoinFlipGameDatum [('CoinFlipGameDatum, 0)]
PlutusTx.makeLift ''CoinFlipGameDatum

-- BLOCK4
-- CoinFlipGame.hs
data CoinFlipGameRedeemer = StartBet | ClaimPrize
  -- ^ Represents the possible actions that can be taken in the game.
  -- StartBet: Player2 enters the game and places their bet.
  -- ClaimPrize: The winner claims the prize (locked ADA).
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''CoinFlipGameRedeemer
PlutusTx.makeIsDataSchemaIndexed ''CoinFlipGameRedeemer [('StartBet, 1), ('ClaimPrize, 2)]

-- BLOCK5
-- CoinFlipGame.hs
{-# INLINEABLE coinFlipGameTypedValidator #-}
coinFlipGameTypedValidator ::
  CoinFlipGameParams ->
  CoinFlipGameDatum ->
  CoinFlipGameRedeemer ->
  ScriptContext ->
  Bool
coinFlipGameTypedValidator params datum@(CoinFlipGameDatum gameState housePotHash vrfHolderHash _ _) redeemer ctx@(ScriptContext txInfo _) =
  PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      StartBet ->
        [ -- The game must be in the Initialized state
          gameState PlutusTx.== Initialized
        , -- The transaction must be signed by the player
          txSignedBy txInfo (cfgpPlayer params)
        , -- The bet amount must be placed
          adaBetPlacedByPlayer
        , -- A correct new datum is produced with Started state
          correctOutput Started
        , -- The UTxO must have a valid game ID
          gameIdPresent
        , -- The player's guess must be valid
          validPlayerGuess
        , -- Input must contain a UTxO locked by the HousePot script
          correctHousePotInputHash housePotHash
        , -- Input must contain a UTxO locked by the VRFHolder script
          correctVRFHolderInputHash vrfHolderHash
        ]
      ClaimPrize ->
        [ -- The game must be in Started state
          gameState PlutusTx.== Started
        , -- The transaction must be signed by the original player
          txSignedBy txInfo (cfgpPlayer params)
        , -- Must have VRFHolder reference input with valid result
          validateVRFInput
        , -- VRF generation time must be at least 60 seconds after game start
          validateVRFTiming
        ]

    -- Helper functions
    adaBetPlacedByPlayer :: Bool
    adaBetPlacedByPlayer =
      case getContinuingOutputs ctx of
        [o] ->
          let v = txOutValue o
              requiredAmount = cfgpBetAmount params
          in lovelaceValueOf v PlutusTx.== requiredAmount
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    correctOutput :: GameState -> Bool
    correctOutput expectedState =
      case getContinuingOutputs ctx of
        [o] ->
          case txOutDatum o of
            OutputDatum (Datum datum) ->
              case PlutusTx.fromBuiltinData datum of
                Just (CoinFlipGameDatum state _ _ _ _) ->
                  state PlutusTx.== expectedState
                Nothing -> PlutusTx.traceError "Failed to decode datum"
            _ -> PlutusTx.traceError "Expected inline datum"
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    -- Helper function to check if a UTxO is locked by a specific script hash
    correctHousePotInputHash :: PlutusTx.BuiltinByteString -> Bool
    correctHousePotInputHash expectedHash =
      case PlutusTx.find
        ( \txInInfo ->
            case txInInfo of
              TxInInfo _ txOut ->
                case toScriptHash (txOutAddress txOut) of
                    Just actualScriptHash ->
                        PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData actualScriptHash) PlutusTx.== expectedHash
                    Nothing -> False
        )
        (txInfoInputs txInfo) of
        Just _ -> True
        Nothing -> PlutusTx.traceError "HousePot input not found"

    -- Helper function to check if the VRFHolder input has the correct script hash
    correctVRFHolderInputHash :: PlutusTx.BuiltinByteString -> Bool
    correctVRFHolderInputHash expectedHash =
      case PlutusTx.find
        ( \txInInfo ->
            case txInInfo of
              TxInInfo _ txOut ->
                case toScriptHash (txOutAddress txOut) of
                    Just actualScriptHash ->
                        PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData actualScriptHash) PlutusTx.== expectedHash
                    Nothing -> False
        )
        (txInfoInputs txInfo) of
        Just _ -> True
        Nothing -> PlutusTx.traceError "VRFHolder input not found"

    -- Helper function to validate game ID
    {-# INLINEABLE gameIdPresent #-}
    gameIdPresent :: Bool
    gameIdPresent =
      case getContinuingOutputs ctx of
        [o] ->
          case txOutDatum o of
            OutputDatum (Datum datum') ->
              case PlutusTx.fromBuiltinData datum' of
                Just (CoinFlipGameDatum _ _ _ gameId _) ->
                  let playerPkh = cfgpPlayer params
                      -- Get the transaction's validity start time
                      txTime = case txInfoValidRange txInfo of
                          interval -> case ivFrom interval of
                              LowerBound (Finite t) _ -> t
                              _ -> PlutusTx.traceError "Invalid time bound"
                      -- Create game ID from player's PKH and transaction time
                      expectedGameId = 
                          -- Take first 10 bytes of the hash
                          PlutusTx.sliceByteString 0 10 $ 
                            PlutusTx.sha2_256 $ 
                              -- Combine player PKH with transaction time
                              PlutusTx.appendByteString
                                (serialiseData $ PlutusTx.toBuiltinData playerPkh)
                                (serialiseData $ PlutusTx.toBuiltinData txTime)
                  in gameId PlutusTx.== expectedGameId
                Nothing -> PlutusTx.traceError "Failed to decode datum"
            _ -> PlutusTx.traceError "Expected inline datum"
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    -- Helper function to validate player's guess
    {-# INLINEABLE validPlayerGuess #-}
    validPlayerGuess :: Bool
    validPlayerGuess =
      case getContinuingOutputs ctx of
        [o] ->
          case txOutDatum o of
            OutputDatum (Datum datum') ->
              case PlutusTx.fromBuiltinData datum' of
                Just (CoinFlipGameDatum _ _ _ _ guess) ->
                  -- Verify that a valid guess (Heads or Tails) is present
                  guess PlutusTx.== Heads PlutusTx.|| guess PlutusTx.== Tails
                Nothing -> PlutusTx.traceError "Failed to decode datum"
            _ -> PlutusTx.traceError "Expected inline datum"
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    -- Helper function to validate VRF input
    {-# INLINEABLE validateVRFInput #-}
    validateVRFInput :: Bool
    validateVRFInput =
      case PlutusTx.find
        ( \txInInfo ->
            case txInInfo of
              TxInInfo _ txOut ->
                case toScriptHash (txOutAddress txOut) of
                    Just actualScriptHash ->
                        -- Check if reference input is from VRFHolder script
                        PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData actualScriptHash) 
                            PlutusTx.== cfgpVRFHolderScriptHash params
                    Nothing -> False
        )
        (txInfoReferenceInputs txInfo) of
        Just (TxInInfo _ txOut) ->
          case txOutDatum txOut of
            OutputDatum (Datum d) ->
              case PlutusTx.fromBuiltinData d of
                Just (VRFHolderDatum _ vrfResult vrfTime subId) ->
                  -- Verify subscription ID matches expected value
                  subId PlutusTx.== expectedSubscriptionId PlutusTx.&&
                  -- VRF result must be either 0 or 1
                  (vrfResult PlutusTx.== 0 PlutusTx.|| vrfResult PlutusTx.== 1)
                Nothing -> PlutusTx.traceError "Invalid VRF datum"
            _ -> PlutusTx.traceError "Expected inline VRF datum"
        Nothing -> PlutusTx.traceError "VRFHolder reference input not found"

    -- Helper function to validate VRF timing
    {-# INLINEABLE validateVRFTiming #-}
    validateVRFTiming :: Bool
    validateVRFTiming =
      case PlutusTx.find
        ( \txInInfo ->
            case txInInfo of
              TxInInfo _ txOut ->
                case toScriptHash (txOutAddress txOut) of
                    Just actualScriptHash ->
                        PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData actualScriptHash) 
                            PlutusTx.== cfgpVRFHolderScriptHash params
                    Nothing -> False
        )
        (txInfoReferenceInputs txInfo) of
        Just (TxInInfo _ txOut) ->
          case txOutDatum txOut of
            OutputDatum (Datum d) ->
              case PlutusTx.fromBuiltinData d of
                Just (VRFHolderDatum _ _ vrfTime _) ->
                  -- Get game creation time from game ID
                  let gameStartTime = case txInfoValidRange txInfo of
                          interval -> case ivFrom interval of
                              LowerBound (Finite t) _ -> t
                              _ -> PlutusTx.traceError "Invalid time bound"
                  -- Check if VRF was generated at least 60 seconds after game start
                  in vrfTime PlutusTx.>= (gameStartTime PlutusTx.+ 60000)  -- 60 seconds in milliseconds
                Nothing -> PlutusTx.traceError "Invalid VRF datum"
            _ -> PlutusTx.traceError "Expected inline VRF datum"
        Nothing -> PlutusTx.traceError "VRFHolder reference input not found"

    -- Expected Chainlink subscription ID (will be set in blueprint)
    {-# INLINEABLE expectedSubscriptionId #-}
    expectedSubscriptionId :: Integer
    expectedSubscriptionId = 1  -- This will be replaced with actual subscription ID in blueprint

-- BLOCK6
-- CoinFlipGame.hs
{-# INLINEABLE coinFlipGameUntypedValidator #-}
coinFlipGameUntypedValidator ::
  CoinFlipGameParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
coinFlipGameUntypedValidator params datum redeemer ctx =
  PlutusTx.check
    ( coinFlipGameTypedValidator
        params
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

coinFlipGameValidatorScript ::
  CoinFlipGameParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
coinFlipGameValidatorScript params =
  $$(PlutusTx.compile [||coinFlipGameUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params



