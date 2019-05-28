# CSSQL
### Functional Programming Final Assignment

In this project we design and implement a programming language that transpiles into CSS.


### API

the haskell api has two commands

- runghc Main.hs convert <IN FILE.cssql> <OUT FILE.css>

  As the name would suggest that function converts a target cssql file into a css file

- runghc Main.hs runtests

  This executes our test suite

### FAQ

Q: I've heard that CSS often falls in the write-once-read-never mode of software, does this help to address that?
A: No, in fact it actually makes the problem worse by making the syntax more verbose and hence more difficult to read.

Q: Is this good for anything?
A: .... it could be ?????????

### Functions
`CREATE SELECTOR <SELECTOR>;`
Creates a selector table

`CREATE VARIABLE $KEY <VALUE>;`
Creates a css variable

`INSERT <SELECTOR> (<CSS_KEY>, <CSS_VALUE>);`
Insert an attribute into a table
#### TODO maybe add into after insert

`DELETE <SELECTOR> <CSS_KEY>;`
Remove attribute from a table

`DROP <SELECTOR>;`
Remove table from the database

`COPY <SELECTOR> AS <SELECTOR>;`
Copy a table as another table, old table still exists.

`RENAME <SELECTOR> AS <SELECTOR>;`
Rename a table

`MERGE <SELECTOR> AND <SELECTOR> AND ... AND <SELECTOR> AS <NEW SELECTOR>;`
Combine the attributes of a collection of selectors, order of input selectors determines resulting attribute table


### TODO
- Combine multiple files (probably need to be able to detect circular dependencies)
- Variable tables
- Consuming tables under another single name space (Consumption should pull that table out of the global name space)
- Media Queries and meta selectors as table views


### Developing

We use stack for our haskell deps and npm for our js deps. To prepare your environment cd into node-library and run npm install.
When that's done hop back up to the main level. Now run stack build, this will pull A LOT of deps in order to make ghcjs run. It'll take probably an hour to get the first compile to pass

To create a library build run

stack build

To run the tests run

stack test

To create a copy of the js library for distribution run

./node-library/build-js-lib.sh

To run the library in haskell without using stack do

runghc -i./src app/Main.hs convert FILE_IN FILE_OUT
