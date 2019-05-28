var compressor = require('node-minify');

compressor.minify({
  compressor: 'gcc',
  input: './node-library/cssql.js',
  output: './node-library/cssql-lib.js'
});
