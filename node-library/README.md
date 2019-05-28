# node-cssql

This library cuts through the javascript/css debate by providing a third option: css in sql!
Through the use of a SQL-like syntax users are able to build up style sheets.
The feature list of this library is heavily inspired by scss.


### API

This library exports a single function that

```js
const {cssql} = require('node-cssql');

cssql('./test/tests/join.cssql', './test/tests/join.css');

```


### Functions

CREATE SELECTOR <SELECTOR>;
Creates a selector table

CREATE VARIABLES <NAMESPACE>;
Creates a variable table

INSERT <SELECTOR> (<CSS_KEY>, <CSS_VALUE>);
Insert an attribute into a table

DELETE <SELECTOR> <CSS_KEY>;
Remove attribute from a table

DROP <SELECTOR>;
Remove table from the database

COPY <SELECTOR> AS <SELECTOR>;
Copy a table as another table, old table still exists.

RENAME <SELECTOR> AS <SELECTOR>;
Rename a table

MERGE <SELECTOR> AND <SELECTOR> AND ... AND <SELECTOR> AS <NEW SELECTOR>;
Combine the attributes of a collection of selectors, order of input selectors determines resulting attribute table


### Syntax Example


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
