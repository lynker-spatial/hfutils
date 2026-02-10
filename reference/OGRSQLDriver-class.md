# Class OGRSQLDriver

OGRSQLDriver objects are created by \[OGRSQL()\] and used to select the
correct method in \[dbConnect()\]. They are a superclass of the
\[DBIDriver-class\] class, and used purely for dispatch.

## Usage

``` r
# S4 method for class 'OGRSQLDriver,ANY'
dbDataType(dbObj, obj, ...)

# S4 method for class 'OGRSQLDriver'
dbIsValid(dbObj, ...)

# S4 method for class 'OGRSQLDriver'
dbUnloadDriver(drv, ...)

# S4 method for class 'OGRSQLDriver'
dbGetInfo(dbObj, ...)
```
