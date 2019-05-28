const {exec} = require('child_process');

// TODO add default to output mode
function cssql(inFile, outFile) {
  return new Promise((resolve, reject) => {
    exec(`node ${__dirname}/cssql-lib.js convert ${inFile} ${outFile}`, (err, stdout, stderr) => {
      console.log(stdout);
      if (err) {
        reject(err);
      } else {
        resolve({stdout, stderr});
      }
    });
  })
  .catch(e => {
    console.error(e);
  });
}


module.exports = {
  cssql
}
