import Head from "next/head";
import { CardanoWallet, MeshBadge, useWallet } from "@meshsdk/react";
import { useState } from "react";
import { Transaction, Wallet } from "@meshsdk/core";

type Game = {
  gameId: string;
  createdBy: string;
  betAmount: number;
  maxParticipants: number;
  participants: string[];
  startTime: number;
  endTime: number;
  winner: string | null;
}

function generateRoomId(seed: string, password: string) {
  const timestamp = Date.now().toString(16);
  const randomHex = Math.floor(Math.random() * 0x10000000000000).toString(16);
  return (timestamp + randomHex + seed + password).slice(0, 20);
}

export default function Home() {
  const [betAmount, setBetAmount] = useState('');
  const [createChosenNumber, setCreateChosenNumber] = useState('');
  const [joinChosenNumber, setJoinChosenNumber] = useState('');
  const [maxParticipants, setMaxParticipants] = useState('');
  const [activeGames, setActiveGames] = useState<Game[]>([]);
  const [gameId, setGameId] = useState('');
  const { connected, wallet } = useWallet();
  const [seed, setSeed] = useState('');
  const [password, setPassword] = useState('');
  const [inviteMessage, setInviteMessage] = useState('');
  const [createGameError, setCreateGameError] = useState('');
  const [inviteError, setInviteError] = useState('');
  const [walletError, setWalletError] = useState('');

  const generateSeed = () => {
    const randomSeed = Math.random().toString(36).substring(2)+Math.random().toString(36).substring(4);
    setSeed(randomSeed);
  };

  const handleCreateGame = async () => {
    if (!connected || !wallet) {
      setWalletError('Please connect your wallet first');
      return;
    }
    setWalletError('');

    if (!seed) {
      setCreateGameError('Please generate a seed');
      return;
    } 
    if (!password) {
      setCreateGameError('Please enter a password');
      return;
    }
    if (!betAmount || Number(betAmount) <= 0) { 
      setCreateGameError('Please enter a valid bet amount');
      return;
    }
    if (!createChosenNumber || Number(createChosenNumber) < 0 || Number(createChosenNumber) > 100) {
      setCreateGameError('Please choose a valid number between 0 and 100');
      return;
    }
    if (!maxParticipants || Number(maxParticipants) <= 0) {
      setCreateGameError('Please enter a valid number of max participants');
      return;
    }
    setCreateGameError('');

    const gameId = generateRoomId(seed, password);
    const startTime = Date.now();
    const endTime = startTime + 5 * 60 * 1000; // 5 minutes

    const newGame: Game = {
      gameId,
      createdBy: (await wallet.getUsedAddresses())[0],
      betAmount: Number(betAmount),
      maxParticipants: Number(maxParticipants),
      participants: [],
      startTime,
      endTime,
      winner: null
    };

    const gameAddress = "addr_test1..."; // Replace with actual script address 

    const tx = new Transaction({ initiator: wallet })
      .sendLovelace(
        gameAddress,
        betAmount
      )
      .setMetadata(674, {
        gameId,
        action: 'create_game',
        chosenNumber: Number(createChosenNumber),
        maxParticipants: Number(maxParticipants)
      });

    const unsignedTx = await tx.build();
    const signedTx = await wallet.signTx(unsignedTx);
    const txHash = await wallet.submitTx(signedTx);
    console.log('Created game with tx', txHash);

    setActiveGames([...activeGames, newGame]);
  }

  const handleJoinGame = async () => {
    if (!connected || !wallet || !gameId) {
      setWalletError('Please connect your wallet first');
      return;
    }
    setWalletError('');

    const game = activeGames.find(g => g.gameId === gameId);
    if (!game || game.participants.length >= game.maxParticipants) return;

    const gameAddress = "addr_test1..."; // Replace with actual script address

    const tx = new Transaction({ initiator: wallet })
      .sendLovelace(
        gameAddress,
        game.betAmount.toString()
      ) 
      .setMetadata(674, {
        gameId,
        action: 'join_game',
        chosenNumber: joinChosenNumber
      });

    const unsignedTx = await tx.build();
    const signedTx = await wallet.signTx(unsignedTx);
    const txHash = await wallet.submitTx(signedTx);
    console.log('Joined game with tx', txHash);

    const userAddress = (await wallet.getUsedAddresses())[0];
    setActiveGames(activeGames.map(g => 
      g.gameId === gameId 
        ? {...g, participants: [...g.participants, userAddress]}
        : g
    ));
  }

  const generateInviteMessage = () => {
    if (!seed) {
      setInviteError('Please generate a seed first');
      return;
    }
    if (!password) { 
      setInviteError('Please enter a password');
      return;
    }
    if (!betAmount) {
      setInviteError('Please enter a bet amount');
      return;
    }
    setInviteError('');

    const gameId = generateRoomId(seed, password);
    const message = `Hi, enter my game of High Low!

Room ID: ${gameId}
Password: ${password}  
Bet Amount: ${betAmount} tokens

Let's see who's luckier!`;
    setInviteMessage(message);
  };

  return (
    <div className="bg-gray-900 w-full text-white text-center">
      <Head>
        <title>High-Low Game on Cardano</title>
        <meta name="description" content="A High-Low game powered by Mesh on Cardano" />
      </Head>
      <main className="flex min-h-screen flex-col items-center justify-center p-24">
        <h1 className="text-6xl font-thin mb-20">
          High-Low Game
        </h1>

        <div className="mb-20">
          <CardanoWallet />
          {walletError && (
            <p className="text-red-500 mt-4">{walletError}</p>
          )}
        </div>

        <div className="flex space-x-10">
          <div className="bg-gray-800 rounded-xl border border-white p-10 mb-10 w-1/2">
            <h2 className="text-3xl mb-5">Create New Game</h2>
            <div className="flex flex-col space-y-5">
              <label htmlFor="seed" className="text-left">Seed</label>
              <div className="flex space-x-2">
                <input
                  id="seed"
                  type="text"
                  readOnly
                  value={seed}
                  className="bg-gray-900 rounded p-4 flex-grow"
                />
                <button
                  className="bg-sky-600 rounded-xl py-4 px-6 font-bold hover:bg-sky-700"
                  onClick={generateSeed}
                >
                  Generate
                </button>
              </div>
              <label htmlFor="password" className="text-left">Password</label>
              <input
                id="password"
                type="password"
                placeholder="Enter a password"
                className="bg-gray-900 rounded p-4"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
              />
              <label htmlFor="betAmount" className="text-left">Bet Amount (in tokens)</label>
              <input 
                id="betAmount"
                type="number"
                placeholder="Enter bet amount"
                className="bg-gray-900 rounded p-4"
                value={betAmount}
                onChange={(e) => setBetAmount(e.target.value ? e.target.value.toString() : '')}
              />
              <label htmlFor="createChosenNumber" className="text-left">Your Number (0-100)</label>
              <input
                id="createChosenNumber"
                type="number" 
                placeholder="Enter your number"
                className="bg-gray-900 rounded p-4"
                value={createChosenNumber}
                onChange={(e) => setCreateChosenNumber(e.target.value ? e.target.value.toString() : '')}
              />
              <label htmlFor="maxParticipants" className="text-left">Max Participants</label>
              <input
                id="maxParticipants"
                type="number"
                placeholder="Enter max participants"
                className="bg-gray-900 rounded p-4" 
                value={maxParticipants}
                onChange={(e) => setMaxParticipants(e.target.value ? e.target.value.toString() : '')}
              />
              {createGameError && (
                <p className="text-red-500">{createGameError}</p>
              )}
              <button 
                className="bg-sky-600 rounded-xl py-4 px-6 font-bold hover:bg-sky-700"
                onClick={handleCreateGame}
              >
                Create Game
              </button>
              <button 
                className="bg-sky-600 rounded-xl py-4 px-6 font-bold hover:bg-sky-700 mt-5"
                onClick={generateInviteMessage}
              >
                Generate Invite Message  
              </button>
              {inviteError && (
                <p className="text-red-500">{inviteError}</p>
              )}
              {inviteMessage && (
                <div className="mt-5">
                  <label htmlFor="inviteMessage" className="text-left">Invite Message</label>
                  <textarea
                    id="inviteMessage"
                    readOnly
                    rows={4}
                    value={inviteMessage}
                    className="bg-gray-900 rounded p-4 w-full"
                  />
                </div>
              )}
            </div>
          </div>

          <div className="bg-gray-800 rounded-xl border border-white p-10 mb-10 w-1/2">
            <h2 className="text-3xl mb-5">Join Game</h2>
            <div className="flex flex-col space-y-5">
              <label htmlFor="gameId" className="text-left">Game ID</label>
              <input
                id="gameId"
                type="text"
                placeholder="Enter game ID"
                className="bg-gray-900 rounded p-4"
                value={gameId}
                onChange={(e) => setGameId(e.target.value)}
              />
              <label htmlFor="joinChosenNumber" className="text-left">Your Number (0-100)</label>
              <input
                id="joinChosenNumber"
                type="number"
                placeholder="Enter your number"
                className="bg-gray-900 rounded p-4"
                value={joinChosenNumber}
                onChange={(e) => setJoinChosenNumber(e.target.value ? e.target.value.toString() : '')}
              />
              <button
                className="bg-sky-600 rounded-xl py-4 px-6 font-bold hover:bg-sky-700"
                onClick={handleJoinGame}
              >
                Join Game
              </button>
            </div>
          </div>
        </div>

        <div className="mt-20">
          <h2 className="text-3xl mb-5">Active Games</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-10">
            {activeGames.map(game => (
              <div key={game.gameId} className="bg-gray-800 rounded-xl border border-white p-10">
                <p><strong>Game ID:</strong> {game.gameId}</p>
                <p><strong>Created By:</strong> {game.createdBy}</p>
                <p><strong>Bet Amount:</strong> {game.betAmount}</p>
                <p><strong>Participants:</strong> {game.participants.length} / {game.maxParticipants}</p>
                <p><strong>Ends In:</strong> {Math.round((game.endTime - Date.now()) / 1000)}s</p>
              </div>
            ))}
          </div>
        </div>
      </main>
      <footer className="p-8 border-t border-gray-300 flex justify-center">
        <MeshBadge isDark={true} />
      </footer>
    </div>
  );
}
