import type { NextPage } from 'next';
import Head from 'next/head';
import NFTMinter from '../components/NFTMinter';

const Home: NextPage = () => {
  return (
    <div>
      <Head>
        <title>CoinFlip Game NFT Minter</title>
        <meta name="description" content="Mint NFTs for CoinFlip Game" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className="min-h-screen flex flex-col items-center justify-center p-4">
        <NFTMinter />
      </main>
    </div>
  );
};

export default Home;
