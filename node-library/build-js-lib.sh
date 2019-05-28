stack build &&
cp ./.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/cssql-exe/cssql-exe.jsexe/all.js ./node-library/cssql.js
node ./node-library/prepare-library.js
# rm node-library/cssql.js
