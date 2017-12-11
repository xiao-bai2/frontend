library(DBI)
library(RJDBC)
library(rJava)
#options(java.parameters = "-Xmx2048m")

#initialize Java VM; called before any rJava function can be used
.jinit()  
#Java class loader; add directory or JAR files to the class path
#.jaddClassPath("javalibs/ojdbc6.jar")

#create a driver object to start JDBC connections
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath = "javalibs/ojdbc7.jar")

#create a connection to the Oracle DB
getJdbcConn <- function() {
  connString = paste0("jdbc:oracle:thin:@",orclServerName,":",orclPortNo,":",sidName)
  dbConnect(jdbcDriver, connString, orclUserName, orclPassword)
}

# SPLIT database
orclServerName = "atmpwfmgdb.am.freescale.net"
orclUserName = "eng_readonly"
orclPassword = "eng_readonly"
orclPortNo = "1521"
sidName = "atmpwfmg"


getSplitData <- function(tablename) {
  
  out <- tryCatch(
    {
      sqlquery <- paste( "select * from ", tablename,sep='')
      
      jdbcConnection <- getJdbcConn()
      #Reading data from Oracle
      dbGetQuery(jdbcConnection,  sqlquery)
      
    },
    
    finally={
      dbDisconnect(jdbcConnection)
    }
  )
  return(out)
}

split_lot <- getSplitData("SPLIT_LOT")
split_waf <- getSplitData("SPLIT_WAF")



#PPDB DB
orclServerName = "atmppdbdb.am.freescale.net"
orclUserName = "m13eds"
orclPassword = "m13eds"
orclPortNo = "1521"
sidName = "atmppdb"




