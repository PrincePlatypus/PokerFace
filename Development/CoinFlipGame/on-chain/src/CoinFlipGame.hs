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

module CoinFlipGame where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..), TokenName (..), TxInfo (..), TxOut (..), txOutAddress, txOutValue, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs, txInfoOutputs, txSignedBy)
import PlutusLedgerApi.V1.Value (valueOf, lovelaceValueOf, Lovelace)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Eq qualified as PlutusTx
import qualified PlutusTx.Builtins as Builtins
import PlutusLedgerApi.V1.Credential (Credential(..))

-- BLOCK1
-- CoinFlipGame.hs
data CoinFlipGameParams = CoinFlipGameParams
  { cfgpPlayer1 :: PubKeyHash
  -- ^ Player1's public key hash. The game is initiated by player1.
  , cfgpPlayer2 :: PubKeyHash
  -- ^ Player2's public key hash. The player who joins the game.
  , cfgpNFT     :: CurrencySymbol
  -- ^ The currency symbol of the NFT used in the game.
  , cfgpVRF     :: PubKeyHash
  -- ^ The public key hash of the VRF generator wallet.
  , cfgpBetAmount :: Lovelace
  -- ^ The amount of ADA bet by each player.
  , cfgpHousePotScriptHash :: PlutusTx.BuiltinByteString
  , cfgpVRFHolderScriptHash :: PlutusTx.BuiltinByteString
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
newtype CoinFlipGameDatum = CoinFlipGameDatum {cfgdGameState :: GameState}
  deriving stock (Generic)
  deriving newtype
    ( HasBlueprintDefinition
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

-- BLOCK4
-- CoinFlipGame.hs
data CoinFlipGameRedeemer =  StartBet | ForgePrize | ClaimPrize
  -- ^ Represents the possible actions that can be taken in the game.
  -- StartBet: Player2 enters the game and places their bet.
  -- ForgePrize: The VRF is generated, and the winner is determined.
  -- ClaimPrize: The winner claims the prize (locked ADA).
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''CoinFlipGameRedeemer
PlutusTx.makeIsDataSchemaIndexed ''CoinFlipGameRedeemer [('StartBet, 1), ('ForgePrize, 2), ('ClaimPrize, 3)]

-- BLOCK5
-- CoinFlipGame.hs
{-# INLINEABLE coinFlipGameTypedValidator #-}
coinFlipGameTypedValidator ::
  CoinFlipGameParams ->
  CoinFlipGameDatum ->
  CoinFlipGameRedeemer ->
  ScriptContext ->
  Bool
coinFlipGameTypedValidator params (CoinFlipGameDatum gameState) redeemer ctx@(ScriptContext txInfo _) =
  PlutusTx.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      StartBet ->
        [ -- The game must be in the Initialized state
          gameState PlutusTx.== Initialized
        , -- The transaction must be signed by player2
          txSignedBy txInfo (cfgpPlayer2 params)
        , -- Double the bet amount must be placed
          adaBetPlacedByPlayer2
        , -- A correct new datum is produced with Started state
          correctOutput Started
        , -- The NFT must remain in the contract
          nftStaysInContract
        , -- The UTxO must have a game ID
          gameIdPresent
        , -- Input must contain a UTxO locked by the HousePot script
          hasHousePotInput
        , -- Input must contain a UTxO locked by the VRFHolder script
          hasVRFHolderInput
        ]
      _ ->
        PlutusTx.traceError "Unimplemented redeemer"

    -- Helper functions
    adaBetPlacedByPlayer2 :: Bool
    adaBetPlacedByPlayer2 =
      case getContinuingOutputs ctx of
        [o] ->
          let v = txOutValue o
              requiredAmount = cfgpBetAmount params -- amount from first player
              totalRequired = requiredAmount PlutusTx.+ requiredAmount -- double the amount
          in lovelaceValueOf v PlutusTx.== totalRequired
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    correctOutput :: GameState -> Bool
    correctOutput expectedState =
      case getContinuingOutputs ctx of
        [o] ->
          case txOutDatum o of
            OutputDatum (Datum datum) ->
              case PlutusTx.fromBuiltinData datum of
                Just (CoinFlipGameDatum state) ->
                  state PlutusTx.== expectedState
                Nothing -> PlutusTx.traceError "Failed to decode datum"
            _ -> PlutusTx.traceError "Expected inline datum"
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    nftStaysInContract :: Bool
    nftStaysInContract =
      case getContinuingOutputs ctx of
        [o] ->
          let v = txOutValue o
          in valueOf v (cfgpNFT params) (TokenName Builtins.emptyByteString) PlutusTx.== 1
        _ -> PlutusTx.traceError "Expected exactly one continuing output"

    -- Helper function to check if a UTxO is locked by a specific script hash
    hasScriptInput :: PlutusTx.BuiltinByteString -> Bool
    hasScriptInput scriptHash =
      PlutusTx.any
        ( \txInInfo ->
            let address = txInInfoResolved txInInfo
            in case toScriptHash address of
                Just actualScriptHash ->
                    PlutusTx.traceIfFalse
                      "Input not locked by the required script"
                      (PlutusTx.sha2_256 (serialiseData $ PlutusTx.toBuiltinData actualScriptHash) PlutusTx.== scriptHash)
                Nothing -> PlutusTx.traceError "Input is not a script address"
        )
        (txInfoInputs txInfo)

    hasHousePotInput :: Bool
    hasHousePotInput = hasScriptInput (cfgpHousePotScriptHash params)

    hasVRFHolderInput :: Bool
    hasVRFHolderInput = hasScriptInput (cfgpVRFHolderScriptHash params)

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

