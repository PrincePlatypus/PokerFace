module HighLowGameTests where

import Test.Tasty
import Test.Tasty.HUnit

import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts
import PlutusTx.Prelude

import HighLowGame

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "HighLowGame tests"
  [ joinTests
  ]

joinTests :: TestTree
joinTests = testGroup "Join tests"
  [ testCase "Should validate join with correct hashed datum" $
      let hashedDatum = HashedDatum
            { hdRoomId = "room1"
            , hdPassword = "password1"
            }
          inlineDatum = InlineDatum
            { idBetAmount = 10
            , idMaxParticipants = 5
            , idParticipants = []
            , idStartTime = 1000
            , idEndTime = 2000
            , idWinner = Nothing
            }
          ctx = ScriptContext
            { scriptContextTxInfo = TxInfo
                { txInfoInputs = []
                , txInfoOutputs = []
                , txInfoFee = mempty
                , txInfoMint = mempty
                , txInfoDCert = []
                , txInfoWdrl = []
                , txInfoValidRange = interval 0 1000
                , txInfoSignatories = []
                , txInfoData = []
                , txInfoId = "tx1"
                }
            , scriptContextPurpose = Spending "utxo1"
            }
      in assertBool "Valid join not validated" $ validateJoin hashedDatum inlineDatum ctx
  , testCase "Should fail join with incorrect hashed datum" $
      let hashedDatum = HashedDatum
            { hdRoomId = "room1"
            , hdPassword = "wrong_password"
            }
          inlineDatum = InlineDatum
            { idBetAmount = 10
            , idMaxParticipants = 5
            , idParticipants = []
            , idStartTime = 1000
            , idEndTime = 2000
            , idWinner = Nothing
            }
          ctx = ScriptContext
            { scriptContextTxInfo = TxInfo
                { txInfoInputs = []
                , txInfoOutputs = []
                , txInfoFee = mempty
                , txInfoMint = mempty
                , txInfoDCert = []
                , txInfoWdrl = []
                , txInfoValidRange = interval 0 1000
                , txInfoSignatories = []
                , txInfoData = []
                , txInfoId = "tx1"
                }
            , scriptContextPurpose = Spending "utxo1"
            }
      in assertBool "Invalid join validated" $ not $ validateJoin hashedDatum inlineDatum ctx
  ]
