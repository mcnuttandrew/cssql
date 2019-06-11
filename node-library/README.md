# node-cssql

This library cuts through the javascript/css debate by providing a forbidden third option: css in sql!
Through the use of a SQL DDL-like syntax users are able to style files which are then transpiled in CSS. The transpiler is written in haskell, but is made more usefully available by being distribution in javascript through the use [ghcjs](https://github.com/ghcjs/ghcjs).

*Q*: I've heard that CSS often falls in the write-once-read-never mode of software, does this help to address that?<br/>
*A*: No, in fact it actually makes the problem worse by making the syntax more verbose and hence more difficult to read.

*Q*: Is this good for anything?<br/>
*A*: .... it could be ?????????

### Node API

You can install the library inside of a node project by running:

```sh
npm install node-cssql
```

You can then use it like:

```js
const {cssql} = require('node-cssql');

cssql('./test/tests/join.cssql', './test/tests/join.css');

```



### Functions
`CREATE SELECTOR <SELECTOR>;`<br/>
Creates a selector table

`CREATE VARIABLE $KEY <VALUE>;`<br/>
Creates a css variable

`INSERT <SELECTOR> (<CSS_KEY>, <CSS_VALUE>);`<br/>
Insert an attribute into a table

`DELETE <SELECTOR> <CSS_KEY>;`<br/>
Remove attribute from a table

`DROP <SELECTOR>;`<br/>
Remove table from the database

`COPY <SELECTOR> AS <SELECTOR>;`<br/>
Copy a table as another table, old table still exists.

`RENAME <SELECTOR> AS <SELECTOR>;`<br/>
`RENAME <SELECTOR> AS <INNERMOST SELECTOR> IN ... IN <OUTERMOST SELECTOR>;`<br/>
Rename a table

`MERGE <SELECTOR> AND <SELECTOR> AND ... AND <SELECTOR> AS <NEW SELECTOR>;`<br/>
Combine the attributes of a collection of selectors, order of input selectors determines resulting attribute table

`NEST <SELECTOR> INTO <SELECTOR>;`<br/>
`NEST <SELECTOR> INTO <INNERMOST SELECTOR> IN ... IN <OUTERMOST SELECTOR>;`<br/>
Destructively move a table into another table


### Syntax Example

What does this wacky language actually look like? Well,

```cssql

CREATE SELECTOR .margin-huge-top;
INSERT .margin-huge-top (margin-top, 50px);

CREATE SELECTOR .margin-huge-bottom;
INSERT .margin-huge-bottom (margin-bottom, 50px);

CREATE SELECTOR .margin-huge-left;
INSERT .margin-huge-left (margin-left, 50px);

CREATE SELECTOR .margin-huge-right;
INSERT .margin-huge-right (margin-right, 50px);

MERGE .margin-huge-top AND .margin-huge-bottom AND .margin-huge-left AND .margin-huge-right AS .margin-huge;

DROP .margin-huge-left;

```

Which will yield css like

```css
.margin-huge-top {
  margin-top: 50px;
}
.margin-huge-bottom {
  margin-bottom: 50px;
}
.margin-huge-right {
  margin-right: 50px;
}
.margin-huge {
  margin-bottom: 50px;
  margin-left: 50px;
  margin-right: 50px;
  margin-top: 50px;
}
```

You can check out more little examples in the [tests folder](https://github.com/mcnuttandrew/cssql/tree/master/test/tests).
