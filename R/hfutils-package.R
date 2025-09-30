#' @keywords internal
#' @aliases hfutils-package
"_PACKAGE"

#' @importFrom utils packageVersion
#' @importFrom methods setMethod setClass setOldClass callNextMethod new show
#' @importFrom DBI  dbConnect  dbSendQuery dbFetch  dbDisconnect dbClearResult dbIsValid
#'  dbHasCompleted dbReadTable dbListTables dbExistsTable dbDataType dbGetInfo dbUnloadDriver
#' @importFrom dbplyr sql
#' @importFrom sf st_layers sf_extSoftVersion
#' @importFrom glue glue
#' @importFrom dplyr tbl select mutate rename if_any filter everything distinct collect any_of `%>%` pull
NULL
