# Class OGRSQLResult (and methods)

OGRSQLResult objects are created by \[dbSendQuery()\] or
\[dbSendStatement()\], and encapsulate the result of an SQL statement.
They are a superclass of the \[DBIResult-class\] class. The "Usage"
section lists the class methods overridden by lazsf.

## Usage

``` r
# S4 method for class 'OGRSQLResult'
show(object)

# S4 method for class 'OGRSQLResult'
dbFetch(res, n = -1, ...)

# S4 method for class 'OGRSQLResult'
dbClearResult(res, ...)

# S4 method for class 'OGRSQLResult'
dbHasCompleted(res, ...)
```

## See also

The corresponding generic functions \[DBI::dbFetch()\],
\[DBI::dbClearResult()\], and \[DBI::dbHasCompleted()\].
