const path = require('path');
const webpack = require('webpack');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin')

module.exports = {
  entry: './src/index.bs.js',
  output: {
    path: path.join(__dirname, "dist"),
    filename: 'index.js',
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('production')
    }),
    new UglifyJsPlugin()
  ]
};
