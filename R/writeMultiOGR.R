#' @title Write an MultiPolygon into PostgreSQL data source.
#' @param x a object
#' @param dsn PostgreSQL DSNs
#' @param nln a character indicating layer option
#' @param srid a integer variable indicating spatial reference id of OGR data source.The defalut value is 4326(WGS84)
#' @param dbname a character indicating database name
#' @param host If shp2pgsql is true,a charater is required to indicate hostname or ip address of PostgreSQL Server
#' @param username If shp2pgsql is true,a charater is required to indicate current user
#' @param verbose a boolean variable indicating whether to display verbose information
#' @param  encode a character indicating encode of vector data
#' @return Spatial DataFrame
#' @keywords writeMultiOGR
#' @export

writeMultiOGR<-function(x,dsn,nln=' ',verbose=F,srid='4326',dbname='mydb',host=' ',username=' ',
                        simplify = T,shp2pgsql=F,encode=NULL)
{
    require(gdalUtils)
    require(rgdal)
    tmpdsn<-file.path(tempdir(),'ttt_ogrm.shp')
    if (file.exists(tmpdsn)) {file.remove(tmpdsn)}
    if(!is.null(encode)){
      getCPLConfigOption("SHAPE_ENCODING")
      Sys.getenv("SHAPE_ENCODING")
      # setCPLConfigOption("SHAPE_ENCODING", "UTF-8")
      # Sys.setenv(SHAPE_ENCODING= "UTF-8" )
      setCPLConfigOption("SHAPE_ENCODING", encode)
      Sys.setenv(SHAPE_ENCODING= encode)
    }

    #   if (file.exists(tmpdsn)) {file.remove(tmpdsn)}
    #  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  #srid=4326
    #   system ( paste("psql -d ",dbname," -c 'drop table ",nln,"'"))
    if(shp2pgsql){
        require(rgdal)
        newproj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        x<-spTransform(x,CRS(newproj))
        writeOGR(x,dsn=tmpdsn,check_exists = T,
                 verbose =F,layer = "ttt_ogrm",driver='ESRI Shapefile',overwrite_layer = T)
        sqlcmd<-paste('echo "drop table ',nln,
                      '" | psql -d ',dbname,host,username)
        system(sqlcmd)
        print(sqlcmd)
        sqlcmd <-paste(' shp2pgsql -Ic -s  ',srid,
                       tmpdsn, nln,' | psql -d ',dbname,host,username
                       ,' >> /tmp/syscmd_res.log')
        print(sqlcmd)
        system(sqlcmd)
    }else{
        writeOGR(x,dsn=tmpdsn,check_exists = T,
                 verbose =F,layer = "ttt_ogrm",driver='ESRI Shapefile',overwrite_layer = T)
        ogr2ogr(src_datasource_name = tmpdsn,dst_datasource_name = dsn,simplify = simplify
                ,f = 'PostGreSQL',
                #                     lco =c('ENCODING=UTF-8'),
                a_srs = proj4string(x),
                nln =nln,layer = "ttt_ogrm",verbose=verbose,
                nlt='PROMOTE_TO_MULTI',overwrite = T)
    }
    setCPLConfigOption("SHAPE_ENCODING", '')
}
