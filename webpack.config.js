'use strict';

const webpack = require("webpack");
// const HtmlWebpackPlugin = require('html-webpack-plugin');
// const CopyWebpackPlugin = require('copy-webpack-plugin');
// const CleanWebpackPlugin = require('clean-webpack-plugin');

module.exports = {
  context: __dirname + "/src",
  entry: {
    app: "./h2048.purs"
  },
  devtool: 'source-map',
  resolve: {
    extensions: ['.purs', '.js']
  },
  output: {
    path: __dirname + "/dist",
    filename: "[name].bundle.js",
  },
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [{
          loader: "purs-loader"
        }]
      }
    ],
  },
  plugins: []
};
