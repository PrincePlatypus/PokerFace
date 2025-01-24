import { CardanoWallet, useWallet } from '@meshsdk/react';
import { useState } from 'react';
import { 
  Transaction, 
  PlutusScript,
  serializePlutusScript,
  stringToHex,
  resolveScriptHash
} from '@meshsdk/core';
import cbor from 'cbor';

// Import the blueprint directly from on-chain directory
import scriptBlueprint from '../../../on-chain/compiled/nft-three-policy.plutus';

export default function NFTMinter() {
  const { connected, wallet } = useWallet();
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const [txHash, setTxHash] = useState<string | null>(null);

  const handleMint = async () => {
    try {
      setLoading(true);
      setError(null);
      setTxHash(null);

      const walletAddress = (await wallet.getUsedAddresses())[0];
      const collateral = await wallet.getCollateral();

      if (!collateral[0]) {
        setError("No collateral found. Please add some ADA as collateral in your wallet.");
        return;
      }

      // Import and parse the script blueprint
      const plutusScript = {
        code: cbor
          .encode(
            Buffer.from(scriptBlueprint.validators[0].compiledCode, 'hex')
          )
          .toString('hex'),
        version: 'V2' as const
      };

      const scriptAddress = serializePlutusScript(plutusScript).address;
      const scriptHash = resolveScriptHash(plutusScript.code, plutusScript.version);

      // Define tokens with script hash
      const coinFlipToken = {
        assetName: 'CoinFlip',
        assetQuantity: '1',
        recipient: {
          address: scriptAddress,
          datum: { inline: true, value: [] }
        }
      };

      const vrfHolderToken = {
        assetName: 'VRFHolder',
        assetQuantity: '1',
        recipient: {
          address: scriptAddress,
          datum: { inline: true, value: [] }
        }
      };

      const housePotToken = {
        assetName: 'HousePot',
        assetQuantity: '1',
        recipient: {
          address: scriptAddress,
          datum: { inline: true, value: [] }
        }
      };

      const tx = new Transaction({ initiator: wallet })
        .mintAsset(plutusScript, coinFlipToken, { data: [] })
        .mintAsset(plutusScript, vrfHolderToken, { data: [] })
        .mintAsset(plutusScript, housePotToken, { data: [] })
        .setRequiredSigners([walletAddress])
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
        <CardanoWallet />
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

      {error && (
        <div className="text-red-500 text-sm mt-2">
          {error}
        </div>
      )}

      {txHash && (
        <div className="text-green-500 text-sm mt-2">
          Transaction successful! Hash: {txHash}
        </div>
      )}
    </div>
  );
} 