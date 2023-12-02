<!-- -*- Markdown -*- -->

## 0.7.2.1

- fix typo.

## 0.7.2.0

- apply new module names of relational-schema.

## 0.7.1.1

- fix build for old GHCs.

## 0.7.1.0

- add convertible instances of Word8 type and Word16 type.
- add foldlFetch and forFetch.

## 0.7.0.1

- update haddock about bracketed-prepare operations.

## 0.7.0.0

- support overwriting of type-map along with column-name.
- replace `execute` and `executeNoFetch`.

## 0.6.8.1

- apply renamed LiteralSQL class.

## 0.6.8.0

- apply enableWarning flag in Config type.

## 0.6.7.1

- fix. do safe convert for integral conversion from SQL value.
- add test suite of conversion from and to SQL value.

## 0.6.7.0

- add bulkInsert definitions, and deprecate chunksInsert.
- enable the mis-disabled warning message of executeNoFetch.
- make statement operations strict.

## 0.6.6.1

- update version constraint. ( along with re-versioned relational-query. )

## 0.6.6.0

- add a portable sequence number operation.
- defaultly use custom configuration in defineTableFromDB.

## 0.6.5.0

- apply relational-query-0.10.0

## 0.6.4.1

- apply integrated namespace with new exported symbols.

## 0.6.4.0

- add new function name definitions to execute bounded statement.

## 0.6.2.1

- add tested-with 8.2.1.

## 0.6.2.0

- Apply generic instances.

## 0.6.0.2

- Add tested-with.

## 0.6.0.1

- Update compatibility for GHC 8.
- Drop old tests of Oracle.

## 0.6.0.0

- Use updated template of persistable-record.
- Drop persistableSqlValue.

## 0.5.0.0

- Use updated template of relational-query.
- Drop old examples of Oracle.

## 0.4.0.0

- TH quotation of derive class names.

## 0.3.0.0

- Hide chunksInsertActions.
- Add withPrepareDelete.

## 0.2.0.0

- Add logging interface for schema driver.
