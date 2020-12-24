install.packages("DBI")
install.packages("odbc")

library(odbc)
sort(unique(odbcListDrivers()[[1]]))

con <- dbConnect(odbc(), "DSN name")

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "datawarehouse", 
                 Trusted_Connection = "True")
