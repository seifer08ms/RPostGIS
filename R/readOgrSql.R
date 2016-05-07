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
readOgrSql = function (dsn, sql, gdal=T, ...) {
  get_os <- function(){
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
  require(rgdal)
  require(RPostgreSQL)
  require(stringr)
  # check dsn starts "PG:" and strip
  if (str_sub(dsn, 1, 3) != "PG:") {
    stop("readOgrSql only works with PostgreSQL DSNs")
  }
  dsn<-str_replace_all(dsn,'  ',' ')
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
  }  else {
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
  db_encode<-dbGetQuery(conn, sql_encoding)$pg_encoding_to_char
  if(as.character(get_os())=='windows'|gdal==F){
    sql_geom<-"select * from geometry_columns where f_table_name ='vw_tmp_read_ogr';"
    dfgeom<-dbGetQuery(conn,sql_geom)
    geom_name<-dfgeom$f_geometry_column
    srid_name<-dfgeom$srid
    geom_type<-dfgeom$type
    sql_proj4<-paste0('select proj4text from spatial_ref_sys where srid=',srid_name)
    proj4text<-dbGetQuery(conn,sql_proj4)$proj4text
    dfdata<-dbGetQuery(conn,'select * from vw_tmp_read_ogr limit 1')
    field.names.nogeom<-eval(parse(text=paste0('names(subset(dfdata,select = -c(',geom_name,')))')))
    fields.name<-paste(field.names.nogeom,collapse = ',')
    sql_export<-paste0("SELECT row_to_json(fc) ",
    " FROM ( SELECT 'FeatureCollection' As type, array_to_json(array_agg(f)) As features",
    " FROM (SELECT 'Feature' As type",
    ", ST_AsGeoJSON(lg.",geom_name,")::json As geometry",
    ", row_to_json(lp) As properties",
    " FROM (SELECT ROW_NUMBER() over (order by ",geom_name,") as gggid,* FROM vw_tmp_read_ogr) As lg ",
    " INNER JOIN (SELECT ROW_NUMBER() over (order by ",geom_name,") as gggid,",
    fields.name," FROM vw_tmp_read_ogr) As lp ",
    " ON lg.gggid = lp.gggid  ) As f )  As fc;"
    )
    # sql_export<-paste0('select st_asgeojson(',geom_name,') from vw_tmp_read_ogr')
    dfTemp<-dbGetQuery(conn,sql_export)[,1]
    tempdsn<-file.path(tempdir(),'temp.json')
    cat(dfTemp,file =(con<-file(tempdsn,'w',encoding = 'UTF-8')) )
    close(con)
    ## Get spatial data via geojson
    spdfFinal = suppressWarnings(rgdal::readOGR(
      dsn =tempdsn,p4s = proj4text,
      verbose = F,layer = ogrListLayers(tempdsn)[1],stringsAsFactors = F))
  }else{
    spdfFinal = readOGR(dsn = dsn, layer = "vw_tmp_read_ogr", ...)
  }
  dbSendQuery(conn, "DROP VIEW vw_tmp_read_ogr;")
  dbDisconnect(conn)
  return(spdfFinal)
}
