var path = require("path");
var webpack = require("webpack");
var common = require("./webpack.config.common");

console.log("Bundling for development...");

module.exports = {
  devtool: "cheap-module-source-map",
  entry: common.config.entry,
  output: {
    sourceMapFilename: '[name].map.js',
    filename: '[name].js',
    path: common.config.buildDir,
    devtoolModuleFilenameTemplate: info =>
      path.resolve(info.absoluteResourcePath).replace(/\\/g, '/'),
  },
  module: {
    rules: common.getModuleRules()
  },
  plugins: common.getPlugins().concat([
      new webpack.HotModuleReplacementPlugin(),
      new webpack.NamedModulesPlugin()
  ]),
  resolve: {
    modules: [common.config.nodeModulesDir]
  },
};
