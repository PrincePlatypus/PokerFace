import TwoColumnsScroll from "~/components/sections/two-columns-scroll";
import Codeblock from "~/components/text/codeblock";
import { SupportedSubmitters } from ".";

export default function SubmitterSubmitTransaction({
  blockchainProvider,
  provider,
}: {
  blockchainProvider: SupportedSubmitters;
  provider: string;
}) {
  return (
    <TwoColumnsScroll
      sidebarTo="submitTx"
      title="Submit Transaction"
      leftSection={Left()}
    />
  );
}

function Left() {
  return (
    <>
      <p>Submit a serialized transaction to the network.</p>
      <Codeblock data={`await blockchainProvider.submitTx(signedTx);`} />
    </>
  );
}
