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
#' @references \url{https://geospatial.commons.gc.cuny.edu/2014/01/14/load-postgis-geometries-in-r-without-rgdal/}
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
  sql_encoding<-
    "SELECT pg_encoding_to_char(encoding) FROM pg_database WHERE datname = 'mydb';"
  db_encode<-dbGetQuery(conn, sql_encoding)
  if(as.character(get_os)=='windows'){
    ogr2ogr(src_datasource_name = dsn,
            layer = 'vw_tmp_read_ogr',verbose = T,f = 'ESRI Shapefile',
            dst_datasource_name = tempdir(),overwrite = T)
    spdfFinal = readOGR(dsn = file.path(tempdir(),'vw_tmp_read_ogr.shp'),
                        layer = "vw_tmp_read_ogr",stringsAsFactors = F, ...)
    # library(RPostgreSQL)
    # library(rgeos)
    # library(sp)
    # dfTemp = dbGetQuery(conn, 'select * from vw_tmp_read_ogr;')
    # row.names(dfTemp) = dfTemp$gid
    # # Create spatial polygons
    # # To set the PROJ4 string, enter the EPSG SRID and uncomment the
    # # following two lines:
    # # EPSG = make_EPSG()
    # # p4s = EPSG[which(EPSG$code == SRID), "prj4"]
    # for (i in seq(nrow(dfTemp))) {
    #   if (i == 1) {
    #     spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i])
    #     # If the PROJ4 string has been set, use the following instead
    #     # spTemp = readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    #   }
    #   else {
    #     spTemp = rbind(
    #       spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i])
    #       # If the PROJ4 string has been set, use the following instead
    #       # spTemp, readWKT(dfTemp$wkt_geometry[i], dfTemp$gid[i], p4s)
    #     )
    #   }
    # }
    # # Create SpatialPolygonsDataFrame, drop WKT field from attributes
    # spdfFinal = SpatialPolygonsDataFrame(spTemp, dfTemp[-2])
  }else{

    spdfFinal = readOGR(dsn = dsn, layer = "vw_tmp_read_ogr", ...)

  }
  dbSendQuery(conn, "DROP VIEW vw_tmp_read_ogr;")
  dbDisconnect(conn)
  return(spdfFinal)
}
.get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
