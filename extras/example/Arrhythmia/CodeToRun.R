keyWords1 <- 'Arrhythmia'
keyWords2 <- 'Fibrillation'
keyWords3 <- 'Flutter'
keyWords4 <- 'Conduction Disorder'
keyWords5 <- 'Tachycardia'
keyWords6 <- 'Bradycardia'
keyWords7 <- 'Ectopic'
keyWords8 <- 'Block'
keyWords9 <- 'Cardiac Arrest'


folder <- stringr::str_replace_all(string = keyWords1,
                                   pattern = " ",
                                   replacement = "")
# Load the package
library(ConceptSetDiagnostics)

locationForResults <-
  file.path(rstudioapi::getActiveProject(), 'example', folder)
dir.create(path = locationForResults,
           recursive = TRUE,
           showWarnings = FALSE)

# Details for connecting to the server:
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    server = paste(
      Sys.getenv("phenotypeLibraryDbServer"),
      Sys.getenv("phenotypeLibraryDbDatabase"),
      sep = "/"
    ),
    user = Sys.getenv("shinyDbUserGowtham"),
    password = Sys.getenv("shinyDbPasswordGowtham"),
    port = Sys.getenv("phenotypeLibraryDbPort")
  )

connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

# get search results
conceptSetExpressionTable1 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords1)
conceptSetExpressionTable2 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords2)
conceptSetExpressionTable3 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords3)
conceptSetExpressionTable4 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords4)
conceptSetExpressionTable5 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords5)
conceptSetExpressionTable6 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords6)
conceptSetExpressionTable7 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords7)
conceptSetExpressionTable8 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords8)
conceptSetExpressionTable9 <-
  ConceptSetDiagnostics::getStringSearchConcepts(connection = connection,
                                                 searchString = keyWords9)
conceptSetExpressionTable <-
  dplyr::bind_rows(
    conceptSetExpressionTable1,
    conceptSetExpressionTable2,
    conceptSetExpressionTable3,
    conceptSetExpressionTable4,
    conceptSetExpressionTable5,
    conceptSetExpressionTable6,
    conceptSetExpressionTable7,
    conceptSetExpressionTable8,
    conceptSetExpressionTable9
  )

##  do automated processing based on search results
designDiagnostic <-
  performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionTable = conceptSetExpressionTable,
    exportResults = TRUE,
    locationForResults = locationForResults,
    iteration = 1
  )


############### review only standard for round 1
discoveredThruRecommender <- c(317302,
                               4091901,
                               4261842,
                               441872,
                               4115173,
                               314664,
                               313224,
                               4088351,
                               4171193,
                               4008580,
                               4038688,
                               4210313,
                               4099778,
                               4216773,
                               4303408,
                               4088985,
                               4256374,
                               4023336,
                               4088504,
                               40479232,
                               4088505,
                               4175473,
                               43021298,
                               4094188,
                               4089460,
                               4247537,
                               4108828,
                               4006208,
                               4166380,
                               4188347,
                               4078058,
                               4089461,
                               4161597,
                               40479264,
                               4091902,
                               4088506,
                               4091903,
                               4091904,
                               4224848,
                               42872924,
                               4049219,
                               4119603,
                               4088350,
                               4030583,
                               4028322,
                               4089464,
                               4121615,
                               4243143,
                               44783658,
                               4218242,
                               4271464,
                               4244893,
                               4088986,
                               4088210,
                               44784236,
                               4124704,
                               36714606,
                               36714539,
                               4088352,
                               4128968,
                               4102252,
                               4092011,
                               36715370,
                               4327066,
                               4088987,
                               37396235,
                               4148028,
                               4088507,
                               4029303,
                               37017187,
                               4262389,
                               4091446,
                               37312140,
                               321588,
                               36675005,
                               44784235,
                               46284985,
                               4089463,
                               36676642,
                               36715042,
                               44783199,
                               4110550,
                               4088502,
                               36674897,
                               36717434,
                               4092010,
                               44784368)

recommendedConceptSetExpression <- ConceptSetDiagnostics::getConceptIdDetails(connection = connection,
                                                                              conceptIds = discoveredThruRecommender)
recommendedConceptSetExpression <- recommendedConceptSetExpression %>%
  ConceptSetDiagnostics::getConceptSetExpressionFromConceptSetExpressionDataFrame(selectAllDescendants = TRUE) %>%
  ConceptSetDiagnostics::getConceptSetExpressionDataFrameFromConceptSetExpression() %>%
  dplyr::left_join(y = recommendedConceptSetExpression, by = 'conceptId')
conceptSetExpressionTable <- dplyr::union(conceptSetExpressionTable, recommendedConceptSetExpression)
blackList <- setdiff(designDiagnostic$recommendedStandard$conceptId, discoveredThruRecommender)
designDiagnostic <-
  performDesignDiagnosticsOnConceptTable(
    connection = connection,
    conceptSetExpressionTable = conceptSetExpressionTable,
    exportResults = TRUE,
    locationForResults = locationForResults,
    blackList = blackList,
    iteration = 2
  )
############## complete standard

## no source codes recommended
############## complete non standard


readr::write_excel_csv(
  x = designDiagnostic$conceptSetExpressionTableOptimized,
  file = file.path(locationForResults, "finalConceptSetExpression.csv"),
  append = FALSE,
  na = ""
) 

DatabaseConnector::disconnect(connection = connection)
