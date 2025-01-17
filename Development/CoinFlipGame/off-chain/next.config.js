/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  webpack: function (config, options) {
    config.experiments = {
      asyncWebAssembly: true,
      layers: true,
    };
    config.module.rules.push({
      test: /\.plutus$/,
      type: 'json'
    });
    // Add WebAssembly rules
    config.module.rules.push({
      test: /\.wasm$/,
      type: "webassembly/async"
    });
    return config;
  }
};

module.exports = nextConfig; 