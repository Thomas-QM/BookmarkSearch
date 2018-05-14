var webpack = require("webpack");
var common = require("./webpack.config.common");
var CopyWebpackPlugin = require('copy-webpack-plugin');

console.log("Bundling for production...");

module.exports = {
  entry: common.config.entry,
  output: {
    filename: '[name].js',
    path: common.config.buildDir,
  },
  module: {
    rules: common.getModuleRules()
  },
  plugins: common.getPlugins().concat([
    // new ExtractTextPlugin('style.css'),
    new webpack.optimize.UglifyJsPlugin({
      compress: { warnings: false }
    })
  ]),
  resolve: {
    modules: [common.config.nodeModulesDir]
  },
};
