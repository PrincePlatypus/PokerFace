declare module "*.plutus" {
  const content: {
    "$id": string;
    "validators": Array<{
      "title": string;
      "compiledCode": string;
      "hash": string;
    }>;
  };
  export default content;
} 