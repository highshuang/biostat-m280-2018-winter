# connect to spark
library(sparklyr)
library(dplyr)
library(ggplot2)

# specify where spark is
Sys.setenv(SPARK_HOME="/usr/lib/spark")
config <- spark_config()

# start spark connection 
sc <- spark_connect(master = "yarn-client", config = config)

# Cache flights Hive table into Spark
#tbl_cache(sc, 'flights')
flights_tbl <- tbl(sc, 'flights') #create pointer to the table
flights_tbl %>% print(width = Inf)

# Cache airlines Hive table into Spark
#tbl_cache(sc, 'airlines')
airlines_tbl <- tbl(sc, 'airlines')
airlines_tbl %>% print(width = Inf)

# Cache airports Hive table into Spark
#tbl_cache(sc, 'airports')
airports_tbl <- tbl(sc, 'airports')
airports_tbl %>% print(width = Inf)




# total data point by year
out <- flights_tbl %>%
  group_by(year) %>%
  count() %>%
  arrange(year) %>%
  collect()



