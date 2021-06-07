dbms <- "postgresql"
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = paste(
      Sys.getenv("shinydbServer"),
      Sys.getenv("shinydbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUser"),
    password = Sys.getenv("shinyDbPassword"),
    port = Sys.getenv("port"),
  )
connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

data <- DatabaseConnector::dbGetQuery(conn = connection,statement = "select * from concept_prevalence.UNIVERSE") 

write.csv(x = data,file = file.path(getwd(),"inst","csv","conceptPrevalenceUniverse.csv"),row.names = FALSE)

data <- DatabaseConnector::dbGetQuery(conn = connection,statement = "select * from concept_prevalence.CP_MASTER") 

write.csv(x = data,file = file.path(getwd(),"inst","csv","conceptPrevalenceCpMaster.csv"),row.names = FALSE)

data <- DatabaseConnector::dbGetQuery(conn = connection,statement = "select * from concept_prevalence.RECOMMENDED_BLACKLIST") 

write.csv(x = data,file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommendedBlacklist.csv"),row.names = FALSE)

data <- DatabaseConnector::dbGetQuery(conn = connection,statement = "select * from concept_prevalence.RECOMMENDER_SET") 

write.csv(x = data,file = file.path(getwd(),"inst","csv","conceptPrevalenceRecommenderSet.csv"),row.names = FALSE)
