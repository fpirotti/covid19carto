library(data.table)
library(readr)
library(rpostgis)
library(RPostgreSQL)  
# pgcon<-list(dbname = "wgis_integrato", host = "localhost", 
#             port = "5432",user = "postgres", password = "geolab02%postgres")
# canConnect = dbCanConnect(dbDriver("PostgreSQL"), dbname = pgcon$dbname, 
#                           host = pgcon$host, port = pgcon$port,
#                           user =pgcon$user, password = pgcon$password)
# if( !canConnect ){ 
#  stop("Problemi di connessione con Postgresql")
# }  
# pgconnection<-dbConnect(PostgreSQL(), dbname=pgcon$dbname,
#                         host=pgcon$host,
#                         port=pgcon$port,
#                         user=pgcon$user,
#                         password=pgcon$password  )



# provincie<- rpostgis::pgGetGeom(pgconnection, c("arch_geo","province_istat_2018"))
# object.size(provincie)/1000000
#class(dt$data)
