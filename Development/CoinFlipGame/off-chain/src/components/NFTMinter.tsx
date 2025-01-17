import { CardanoWallet, useWallet } from '@meshsdk/react';
import { useState, useEffect } from 'react';
import { 
  Transaction, 
  PlutusScript,
  AssetMetadata,
  Mint,
  DEFAULT_REDEEMER_BUDGET
} from '@meshsdk/core';

// Import the blueprint directly from on-chain directory
import scriptBlueprint from '../../../on-chain/compiled/nft-three-policy.plutus';

export default function NFTMinter() {
  const { connected, wallet } = useWallet();
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const [txHash, setTxHash] = useState<string | null>(null);

  useEffect(() => {
    // Log the entire blueprint to verify its contents
    console.log('Script Blueprint:', {
      id: scriptBlueprint.$id,
      title: scriptBlueprint.validators[0].title,
      hash: scriptBlueprint.validators[0].hash,
      fullCode: scriptBlueprint.validators[0].compiledCode
    });
  }, []);

  const handleMint = async () => {
    if (!connected) {
      setError("Please connect your wallet first");
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setTxHash(null);

      const usedAddress = await wallet.getUsedAddresses();
      const address = usedAddress[0];
      const collateral = await wallet.getCollateral();

      if (!collateral[0]) {
        setError("No collateral found. Please add some ADA as collateral in your wallet.");
        return;
      }

      // Define the three NFTs to mint with minimal metadata
      const coinFlipAsset: Mint = {
        assetName: "CoinFlip",
        assetQuantity: "1",
        metadata: { name: "CoinFlip" },
        label: "721",
        recipient: address
      };

      const vrfHolderAsset: Mint = {
        assetName: "VRFHolder",
        assetQuantity: "1",
        metadata: { name: "VRFHolder" },
        label: "721",
        recipient: address
      };

      const housePotAsset: Mint = {
        assetName: "HousePot",
        assetQuantity: "1",
        metadata: { name: "HousePot" },
        label: "721",
        recipient: address
      };

      // Define the plutus script from the blueprint WITHOUT parameters
      const script: PlutusScript = {
        code: scriptBlueprint.validators[0].compiledCode,
        version: "V2"
        // Remove params since they're already compiled into the script
      };

      // Create unit/void redeemer with budget
      const redeemer = {
        data: [],  // Empty array for unit/void value
        tag: "MINT",
        budget: DEFAULT_REDEEMER_BUDGET
      };

      // Create transaction with 10-second validity window
      const now = Date.now(); // Current time in milliseconds
      const tenSecondsFromNow = now + 10000; // Add 10 seconds

      const tx = new Transaction({ 
        initiator: wallet,
        validFrom: now,
        validTo: tenSecondsFromNow,
      })
        .mintAsset(script, coinFlipAsset, redeemer)
        .mintAsset(script, vrfHolderAsset, redeemer)
        .mintAsset(script, housePotAsset, redeemer)
        .setRequiredSigners([address])
        .setCollateral([collateral[0]]);

      const unsignedTx = await tx.build();
      const signedTx = await wallet.signTx(unsignedTx, true);
      const txHash = await wallet.submitTx(signedTx);

      setTxHash(txHash);
      console.log('Transaction submitted:', txHash);

    } catch (error) {
      console.error("Minting error:", error);
      setError(error instanceof Error ? error.message : "Unknown error occurred");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="flex flex-col items-center gap-6 p-8">
      <h1 className="text-3xl font-bold mb-4">CoinFlip Game NFT Minter</h1>
      
      <div className="flex flex-col sm:flex-row items-center gap-4">
        {/* Wallet Connection */}
        <CardanoWallet />

        {/* Minting Button - Always visible */}
        <button
          className={`
            px-6 py-3 rounded-lg font-semibold text-white
            ${loading 
              ? 'bg-gray-400 cursor-not-allowed' 
              : 'bg-blue-600 hover:bg-blue-700 active:bg-blue-800'}
            transition-colors duration-200
          `}
          onClick={handleMint}
          disabled={loading}
        >
          {loading ? (
            <span className="flex items-center gap-2">
              <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
              </svg>
              Minting...
            </span>
          ) : (
            'Mint Game NFTs'
          )}
        </button>
      </div>

      {/* Error Display */}
      {error && (
        <div className="text-red-500 text-sm mt-2">
          {error}
        </div>
      )}

      {/* Success Display */}
      {txHash && (
        <div className="text-green-500 text-sm mt-2">
          Transaction successful! Hash: {txHash}
        </div>
      )}
    </div>
  );
} 