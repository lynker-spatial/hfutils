# Class OGRSQLConnection (and methods)

OGRSQLConnection objects are created by passing \[OGRSQL()\] as first
argument to \[DBI::dbConnect()\]. They are a superclass of the
\[DBIConnection-class\] class.

## Usage

``` r
# S4 method for class 'OGRSQLConnection'
show(object)

# S4 method for class 'OGRSQLConnection,ANY'
dbSendQuery(conn, statement, ...)

# S4 method for class 'OGRSQLConnection,character'
dbReadTable(conn, name, ...)

# S4 method for class 'OGRSQLConnection'
dbListTables(conn, ...)

# S4 method for class 'OGRSQLConnection,ANY'
dbExistsTable(conn, name, ...)

# S4 method for class 'OGRSQLConnection'
dbDisconnect(conn, ...)
```
