#' @title Read an PostgreSQL data source into Spatial objects.
#' @param dsn PostgreSQL DSNs
#' @param sql a character vector of length 1 containing SQL
#' @param ... Other parameters passed on to methods.
#' @return Spatial DataFrame
#' @keywords readOgrSql
#' @export
#' @examples
#' \dontrun{ strSQL = "
#' SELECT gid, name,
#' ST_Transform(geom, 4326)::geometry(MultiPolygon, 4326) AS geom
#' FROM geo_states"
#' spdfStates = readOgrSql(dsn, strSQL, stringsAsFactors=FALSE)}
#'
#' @references \url{https://geospatial.commons.gc.cuny.edu/2013/12/31/subsetting-in-readogr/}
readOgrSql = function (dsn, sql, ...) {
  require(rgdal)
  require(RPostgreSQL)
  require(stringr)
  # check dsn starts "PG:" and strip
  if (str_sub(dsn, 1, 3) != "PG:") {
    stop("readOgrSql only works with PostgreSQL DSNs")
  }
  dsnParamList = str_trim(str_split(dsn, ":")[[1]][2])
  # Build dbConnect expression, quote DSN parameter values
  # if not already quoted
  if (str_count(dsnParamList, "=")
      == str_count(dsnParamList, "='[[:alnum:]]+'")) {
    strExpression = str_c(
      "dbConnect(dbDriver('PostgreSQL'), ",
      str_replace_all(dsnParamList, " ", ", "),
      ")"
    )
  }
  else {
    dsnArgs = word(str_split(dsnParamList, " ")[[1]], 1, sep="=")
    dsnVals = sapply(
      word(str_split(dsnParamList, " ")[[1]], 2, sep="="),
      function(x) str_c("'", str_replace_all(x, "'", ""), "'")
    )
    strExpression = str_c(
      "dbConnect(dbDriver('PostgreSQL'), ",
      str_c(dsnArgs, "=", dsnVals, collapse=", "),
      ")"
    )
  }
  # Connect, create spatial view, read spatial view, drop   spatial view
  conn = eval(parse(text=strExpression))
  dbSendQuery(conn, "DROP VIEW IF EXISTS vw_tmp_read_ogr;")
  strCreateView = paste("CREATE VIEW vw_tmp_read_ogr AS", sql)
  dbSendQuery(conn, strCreateView)
  temp = readOGR(dsn = dsn, layer = "vw_tmp_read_ogr", ...)
  dbSendQuery(conn, "DROP VIEW vw_tmp_read_ogr;")
  dbDisconnect(conn)
  return(temp)
}
