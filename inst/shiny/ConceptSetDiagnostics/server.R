shiny::shinyServer(function(input, output, session) {
  numberOfSearchPhrases <- reactiveVal(value = 1)
  col_names <-
    shiny::reactive(x = paste0("col",
                               seq_len(numberOfSearchPhrases())))
  
  shiny::observeEvent(eventExpr = input$addKeyword,
                      handlerExpr = {
                        numberOfSearchPhrases(numberOfSearchPhrases() + 1)
                      })
  
  shiny::observeEvent(eventExpr = input$removeKeyword,
                      handlerExpr = {
                        if (numberOfSearchPhrases() > 1) {
                          numberOfSearchPhrases(numberOfSearchPhrases() - 1)
                        }
                      })
  
  output$col <- renderUI({
    purrr::map(
      .x = col_names(),
      .f = ~ shiny::textInput(
        inputId = .x,
        label = NULL,
        value = isolate(expr = input[[.x]])
      )
    )
  })
  
  conceptSetSearchResults <- reactiveVal(value = NULL)
  conceptSetSearchResultsPassingtoConceptSetExpression <-
    reactiveVal(value = NULL)
  conceptSetResultsExpression <- reactiveVal(value = NULL)
  shiny::observeEvent(eventExpr = input$search,
                      handlerExpr = {
                        shiny::withProgress(expr = {
                          shinyjs::runjs(paste0('$("#col input").css("background-color","white")'))
                          keywords <- purrr::map_chr(.x = col_names(),
                                                     .f = ~ input[[.x]] %||% "")
                          if (length(keywords) > 0) {
                            conceptSetExpressionAllTerms <- list()
                            searchResultConceptIdsAllTerms <- list()
                            for (i in 1:length(keywords)) {
                              vocabularyIdToFilter <- input$vocabularyIdToFilter
                              domainIdToFilter <- input$domainIdToFilter
                              
                              # step perform string search
                              searchResultConceptIds <-
                                ConceptSetDiagnostics::performStringSearchForConcepts(
                                  connectionDetails = connectionDetailsLocalPostgres,
                                  vocabularyDatabaseSchema = vocabularyDatabaseSchema,
                                  searchString =  keywords[[i]]
                                )
                              if (length(vocabularyIdToFilter) > 0) {
                                searchResultConceptIds <- searchResultConceptIds %>%
                                  dplyr::filter(.data$vocabularyId %in% vocabularyIdToFilter)
                              }
                              if (length(domainIdToFilter) > 0) {
                                searchResultConceptIds <- searchResultConceptIds %>%
                                  dplyr::filter(.data$domainId %in% domainIdToFilter)
                              }
                              searchResultConceptIdsAllTerms[[i]] <-
                                searchResultConceptIds
                            }
                            searchResultConceptIdsAllTerms <-
                              dplyr::bind_rows(searchResultConceptIdsAllTerms) %>%
                              dplyr::distinct() %>%
                              dplyr::arrange(dplyr::desc(.data$drc))
                            conceptSetSearchResults(searchResultConceptIdsAllTerms) # set reactive value
                            conceptSetSearchResultsPassingtoConceptSetExpression(searchResultConceptIdsAllTerms) #set reactive value
                            conceptSetResultsExpression(NULL) #set to NULL
                          }
                        }, message = "Searching Results")
                      })
  
  output$isSearchResultFound <- shiny::reactive({
    return(is.null(conceptSetSearchResultsPassingtoConceptSetExpression()))
  })
  
  shiny::outputOptions(x = output,
                       name = 'isSearchResultFound',
                       suspendWhenHidden = FALSE)
  
  output$searchResultConceptIds <- DT::renderDT({
    if (is.null(conceptSetSearchResults())) {
      return(NULL)
    } else {
      standardDataTable(data = conceptSetSearchResults(),
                        selectionMode = "single") %>%
        DT::formatStyle(
          table = 'conceptName',
          columns = 'standardConcept',
          color = DT::styleEqual(
            levels = c('S', 'N', 'C'),
            values = c('blue', 'red', 'purple')
          )
        )
    }
  })
  
  output$numberOfRowSelectedInSearchResult <- shiny::reactive({
    return(length(input$searchResultConceptIds_rows_selected))
  })
  
  shiny::outputOptions(x = output,
                       name = 'numberOfRowSelectedInSearchResult',
                       suspendWhenHidden = FALSE)
  
  shiny::observeEvent(
    eventExpr = purrr::map_chr(.x = col_names(),
                               .f = ~ input[[.x]] %||% ""),
    handlerExpr = {
      shinyjs::runjs(paste0('$("#col input").css("background-color","white")'))
    }
  )
  
  shiny::observeEvent(eventExpr = input$deleteSearchResult,
                      handlerExpr = {
                        if (!is.null(input$searchResultConceptIds_rows_selected)) {
                          conceptSetSearchResults(conceptSetSearchResults()[-as.integer(input$searchResultConceptIds_rows_selected),])
                          conceptSetSearchResultsPassingtoConceptSetExpression(conceptSetSearchResultsPassingtoConceptSetExpression()[-as.integer(input$searchResultConceptIds_rows_selected),])
                          shinyjs::runjs(
                            paste0(
                              ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
                            )
                          )
                        }
                      })
  
  # observeEvent(eventExpr = input$searchResultConceptIds_rows_selected,handlerExpr = {
  #   idx <- input$searchResultConceptIds_rows_selected
  #   conceptName <- conceptSetSearchResults()[idx,]$conceptName
  #   shinyWidgets::updatePickerInput(session = session,inputId = "conceptId",choices = conceptName)
  #   ConceptSetDiagnostics::getConceptIdDetails(conceptIds = c(4028741),connection = connectionRemote)
  # })
  
  shiny::observeEvent(eventExpr = conceptSetSearchResultsPassingtoConceptSetExpression(),
                      handlerExpr = {
                        shiny::withProgress(message = "Building Concept Set Expression...", {
                          # develop a concept set expression based on string search
                          conceptSetExpressionDataFrame <-
                            ConceptSetDiagnostics::convertConceptSetDataFrameToExpression(
                              conceptSetExpressionDataFrame = conceptSetSearchResultsPassingtoConceptSetExpression(),
                              selectAllDescendants = TRUE
                            ) %>%
                            ConceptSetDiagnostics::getConceptSetSignatureExpression(connection = connectionRemote,
                                                                                    vocabularyDatabaseSchema = vocabularyDatabaseSchema) %>%
                            ConceptSetDiagnostics::convertConceptSetExpressionToDataFrame(
                              updateVocabularyFields = TRUE,
                              recordCount = TRUE,
                              connection = connectionRemote
                            ) %>%
                            dplyr::arrange(dplyr::desc(.data$drc))
                        })
                        conceptSetResultsExpression(conceptSetExpressionDataFrame)
                      })
  
  shiny::observeEvent(eventExpr = input$deleteConceptSetExpression,
                      handlerExpr = {
                        if (!is.null(input$conceptSetExpression_checkboxes_checked)) {
                          conceptSetResultsExpression(conceptSetResultsExpression()[-as.integer(input$conceptSetExpression_checkboxes_checked),])
                          conceptSetSearchResults(NULL)
                          shinyjs::runjs(
                            paste0(
                              ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
                            )
                          )
                        }
                      })
  
  output$numberOfRowSelectedInConceptSetExpression <-
    shiny::reactive({
      return(length(input$conceptSetExpression_checkboxes_checked))
    })
  
  shiny::outputOptions(x = output,
                       name = 'numberOfRowSelectedInConceptSetExpression',
                       suspendWhenHidden = FALSE)
  
  shiny::observeEvent(
    eventExpr = list(
      input$descendants_checkboxes_checked,
      input$mapped_checkboxes_checked,
      input$excluded_checkboxes_checked
    ),
    handlerExpr = {
      if (!is.null(input$descendants_checkboxes_checked) ||
          !is.null(input$mapped_checkboxes_checked) ||
          !is.null(input$excluded_checkboxes_checked)) {
        conceptSetSearchResults(NULL)
        shinyjs::runjs(
          paste0(
            ' setTimeout(function() {$("#col input").css("background-color","#D3D3D3")},500) '
          )
        )
        data <- conceptSetResultsExpression()
        if (!is.null(input$descendants_checkboxes_checked)) {
          for (i in min(as.integer(input$descendants_checkboxes_checked)):max(as.integer(input$descendants_checkboxes_checked))) {
            if (i %in% as.integer(input$descendants_checkboxes_checked)) {
              data$includeDescendants[i] <- TRUE
            } else {
              data$includeDescendants[i] <- FALSE
            }
          }
        }
        if (!is.null(input$mapped_checkboxes_checked)) {
          for (i in min(as.integer(input$mapped_checkboxes_checked)):max(as.integer(input$mapped_checkboxes_checked))) {
            if (i %in% as.integer(input$mapped_checkboxes_checked)) {
              data$includeMapped[i] <- TRUE
            } else {
              data$includeMapped[i] <- FALSE
            }
          }
        }
        if (!is.null(input$excluded_checkboxes_checked)) {
          for (i in min(as.integer(input$excluded_checkboxes_checked)):max(as.integer(input$excluded_checkboxes_checked))) {
            if (i %in% as.integer(input$excluded_checkboxes_checked)) {
              data$isExcluded[i] <- TRUE
            } else {
              data$isExcluded[i] <- FALSE
            }
          }
        }
        conceptSetResultsExpression(data)
      }
    }
  )
  
  # observeEvent(eventExpr = input$cohortDetails,
  #              handlerExpr = {
  #                if (input$cohortDetails == "conceptSetExpression" &&
  #                    !is.null(conceptSetSearchResults())) {
  #                  conceptSetResultsExpression(getConceptSetExpression())
  #                }
  #              })
  
  output$conceptSetExpression <- DT::renderDT({
    data <- conceptSetResultsExpression()
    if (is.null(data)) {
      return(NULL)
    } else {
      data$checkedDescendants <- ""
      data$checkedMapped <- ""
      data$checkedExcluded <- ""
      for (i in 1:nrow(data)) {
        if (data[i,]$includeDescendants) {
          data[i, ]$checkedDescendants <- 'checked=\"checked\"'
        }
        if (data[i,]$includeMapped) {
          data[i, ]$checkedMapped <- 'checked=\"checked\"'
        }
        if (data[i,]$isExcluded) {
          data[i, ]$checkedExcluded <- 'checked=\"checked\"'
        }
      }
      data <- data %>%
        dplyr::mutate(
          # use glue to create checked field in javascript
          select = glue::glue(
            '<input type="checkbox" class="selectConceptSetExpressionRow"  name="selectConceptSetExpressionRow"  value="{1:nrow(data)}"><br>'
          ),
          selectDescendants = glue::glue(
            '<input type="checkbox" class="selectDescendants"  name="selectDescendants" {data$checkedDescendants}  value="{1:nrow(data)}"><br>'
          ),
          selectMapped = glue::glue(
            '<input type="checkbox" class="selectMapped"  name="selectMapped" {data$checkedMapped}  value="{1:nrow(data)}"><br>'
          ),
          selectExcluded = glue::glue(
            '<input type="checkbox" class="selectExcluded"  name="selectExcluded" {data$checkedExcluded}  value="{1:nrow(data)}"><br>'
          )
        ) %>%
        dplyr::relocate(select)
      standardDataTable(
        data = data %>%
          dplyr::select(
            -.data$includeDescendants,
            -.data$includeMapped,
            -.data$isExcluded,
            -.data$checkedDescendants,
            -.data$checkedMapped,
            -.data$checkedExcluded
          ),
        selectionMode = "single"
      )
    }
  })
  
  getResolved <- shiny::reactive({
    shiny::withProgress(message = "Loading", {
      data <- conceptSetResultsExpression()
      data$includeDescendants <- FALSE
      data$includeMapped <- FALSE
      data$isExcluded <- FALSE
      if (is.null(input$descendants_checkboxes_checked)) {
        data$includeDescendants <-
          conceptSetResultsExpression()$includeDescendants
      } else {
        data$includeDescendants[as.integer(input$descendants_checkboxes_checked)] <-
          TRUE
      }
      if (is.null(input$mapped_checkboxes_checked)) {
        data$includeMapped <- conceptSetResultsExpression()$includeMapped
      } else {
        data$includeMapped[as.integer(input$mapped_checkboxes_checked)] <-
          TRUE
      }
      if (is.null(input$excluded_checkboxes_checked)) {
        data$isExcluded <- conceptSetResultsExpression()$isExcluded
      } else {
        data$isExcluded[as.integer(input$excluded_checkboxes_checked)] <-
          TRUE
      }
      # data <- data %>%
      #   dplyr::select(
      #     -.data$selectDescendants,-.data$selectMapped,-.data$selectExcluded,-.data$checkedDescendants,-.data$checkedMapped,-.data$checkedExcluded
      #   )
      
      conceptSetExpression <-
        ConceptSetDiagnostics::convertConceptSetDataFrameToExpression(conceptSetExpressionDataFrame = data)
      result <-
        ConceptSetDiagnostics::resolveConceptSetExpression(
          conceptSetExpression = conceptSetExpression,
          connection = connectionRemote,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        )
    })
    return(result)
  })
  
  output$numberOfRowSelectedInResolved <-
    shiny::reactive({
      return(length(input$resolved_checkboxes_checked))
    })
  
  shiny::outputOptions(output,
                       'numberOfRowSelectedInResolved',
                       suspendWhenHidden = FALSE)
  
  output$resolvedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      data <-  getResolved()$resolvedConcepts
      data <- data %>%
        dplyr::mutate(
          # use glue to create checked field in javascript
          select = glue::glue(
            '<input type="checkbox" class="selectResolvedRow"  name="selectResolvedRow"  value="{1:nrow(data)}"><br>'
          )
        ) %>%
        dplyr::relocate(select)
      standardDataTable(data)
    }
  })
  
  shiny::observeEvent(eventExpr = input$addResolved, handlerExpr = {
    if (length(input$resolved_checkboxes_checked) > 0) {
      idx <- input$resolved_checkboxes_checked
      conceptIds <- getResolved()$resolvedConcepts[idx,]$conceptId
      data <-
        ConceptSetDiagnostics::getConceptIdDetails(
          conceptIds = conceptIds,
          connection = connectionRemote,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        ) %>%
        dplyr::mutate(
          invalidReasonCaption = "",
          standardConceptCaption = "",
          includeDescendants = FALSE,
          includeMapped = FALSE,
          isExcluded = FALSE
        )
      
      if ("exclude" %in% input$resolvedParameters) {
        data$isExcluded <- TRUE
      }
      if ("descendent" %in% input$resolvedParameters) {
        data$includeDescendants <- TRUE
      }
      if ("mapped" %in% input$resolvedParameters) {
        data$includeMapped <- TRUE
      }
      conceptSetResultsExpression(rbind(conceptSetResultsExpression(), data))
      conceptSetSearchResults(NULL)
    }
  })
  
  output$numberOfRowSelectedInMapped <-
    shiny::reactive({
      return(length(input$mappedRow_checkboxes_checked))
    })
  
  shiny::outputOptions(output,
                       'numberOfRowSelectedInMapped',
                       suspendWhenHidden = FALSE)
  
  output$mappedConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      data <-  getResolved()$mappedConcepts
      data <- data %>%
        dplyr::mutate(
          # use glue to create checked field in javascript
          select = glue::glue(
            '<input type="checkbox" class="selectMappedRow"  name="selectMappedRow"  value="{1:nrow(data)}"><br>'
          )
        ) %>%
        dplyr::relocate(select)
      standardDataTable(data = data)
    }
  })
  
  shiny::observeEvent(eventExpr = input$addMapped, handlerExpr = {
    if (length(input$mappedRow_checkboxes_checked) > 0) {
      idx <- input$mappedRow_checkboxes_checked
      conceptIds <- getResolved()$mappedConcepts[idx,]$conceptId
      data <-
        ConceptSetDiagnostics::getConceptIdDetails(
          conceptIds = conceptIds,
          connection = connectionRemote,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        ) %>%
        dplyr::mutate(
          invalidReasonCaption = "",
          standardConceptCaption = "",
          includeDescendants = FALSE,
          includeMapped = FALSE,
          isExcluded = FALSE
        )
      
      if ("Exclude" %in% input$mappedParameters) {
        data$isExcluded <- TRUE
      }
      if ("Descendents" %in% input$mappedParameters) {
        data$includeDescendants <- TRUE
      }
      if ("Mapped" %in% input$mappedParameters) {
        data$includeMapped <- TRUE
      }
      conceptSetResultsExpression(rbind(conceptSetResultsExpression(), data))
      conceptSetSearchResults(NULL)
    }
  })
  
  getRecommendation <- shiny::reactive({
    shiny::withProgress(message = "Loading", {
      conceptSetExpression <-
        ConceptSetDiagnostics::convertConceptSetDataFrameToExpression(conceptSetExpressionDataFrame = conceptSetResultsExpression())
      data <-
        ConceptSetDiagnostics::getRecommendationForConceptSetExpression(
          conceptSetExpression = conceptSetExpression,
          connection = connectionRemote,
          vocabularyDatabaseSchema = vocabularyDatabaseSchema
        )
    })
    return(data)
  })
  
  output$recommendedStandardConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedStandard)
    }
  })
  
  output$recommendedSourceConceptSetExpression <- DT::renderDT({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      standardDataTable(data = getRecommendation()$recommendedSource)
    }
  })
  
  output$conceptSetExpressionJSON <- shiny::renderText({
    if (is.null(conceptSetResultsExpression())) {
      return(NULL)
    } else {
      data <-
        ConceptSetDiagnostics::convertConceptSetDataFrameToExpression(conceptSetExpressionDataFrame = conceptSetResultsExpression()) %>%
        RJSONIO::toJSON(digits = 23, pretty = TRUE)
    }
  })
  
  getConcetIdName <-
    shiny::eventReactive(eventExpr = selectedConceptId(), {
      if (!is.null(selectedConceptId()))
        return(
          ConceptSetDiagnostics::getConceptIdDetails(
            conceptIds = selectedConceptId(),
            connection = connectionRemote,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema
          )$conceptName
        )
    })
  
  output$conceptIdDetails <- shiny::renderUI({
    if (!is.null(selectedConceptId())) {
      tags$h5(paste("Concept : ", selectedConceptId(), "-", getConcetIdName()))
    }
  })
  
  selectedConceptId <- reactiveVal(NULL)
  
  shiny::observeEvent(
    eventExpr = input$searchResultConceptIds_rows_selected,
    handlerExpr = {
      idx <- input$searchResultConceptIds_rows_selected
      selectedConceptId(conceptSetSearchResults()$conceptId[idx])
    }
  )
  
  shiny::observeEvent(
    eventExpr = input$conceptSetExpression_rows_selected,
    handlerExpr = {
      idx <- input$conceptSetExpression_rows_selected
      selectedConceptId(conceptSetResultsExpression()$conceptId[idx])
    }
  )
  
  shiny::observeEvent(
    eventExpr = input$resolvedConceptSetExpression_rows_selected,
    handlerExpr = {
      idx <- input$resolvedConceptSetExpression_rows_selected
      selectedConceptId(getResolved()$resolvedConcepts$conceptId[idx])
    }
  )
  
  shiny::observeEvent(
    eventExpr = input$mappedConceptSetExpression_rows_selected,
    handlerExpr = {
      idx <- input$mappedConceptSetExpression_rows_selected
      selectedConceptId(getResolved()$mappedConcepts$conceptId[idx])
    }
  )
  
  output$isConceptIdSelected <-
    shiny::reactive({
      return(!is.null(selectedConceptId()))
    })
  
  shiny::outputOptions(x = output,
                       name = 'isConceptIdSelected',
                       suspendWhenHidden = FALSE)
  
  output$conceptSynonym <- DT::renderDT({
    if (!is.null(selectedConceptId())) {
      shiny::withProgress(message = "Building Concept Synonyms", {
        data <-
          ConceptSetDiagnostics::getConceptSynonym(
            conceptIds = selectedConceptId(),
            connection = connectionRemote,
            vocabularyDatabaseSchema = vocabularyDatabaseSchema
          )
        data <- data %>%
          dplyr::select(.data$conceptSynonymName) %>%
          dplyr::distinct() %>%
          dplyr::arrange(1)
        standardDataTable(data = data)
      })
    }
  })
  
  getDeepConceptRelationship <- shiny::reactive({
    shiny::withProgress(message = "Building Concept Relationship", {
      ConceptSetDiagnostics::getDeepConceptRelationship(
        conceptIds = selectedConceptId(),
        connection = connectionRemote,
        vocabularyDatabaseSchema = vocabularyDatabaseSchema
      )
    })
  })
  
  output$conceptRelationship <- DT::renderDT({
    if (is.null(selectedConceptId()) ||
        is.null(getDeepConceptRelationship())) {
      return(NULL)
    }
    data <- getDeepConceptRelationship()$conceptRelationship %>%
      dplyr::filter(is.na(.data$invalidReason)) %>%
      dplyr::select(
        -.data$searchedConceptId,
        -.data$maxLevelsOfSeparation,
        -.data$invalidReason,
        -.data$validStartDate,
        -.data$validEndDate
      ) %>%
      dplyr::distinct() %>%
      dplyr::inner_join(getDeepConceptRelationship()$concepts,
                        by = "conceptId")
    data <- data %>%
      dplyr::mutate(
        # use glue to create checked field in javascript
        select = glue::glue(
          '<input type="checkbox" class="selectRelationshipRow"  name="selectRelationshipRow"  value="{1:nrow(data)}"><br>'
        )
      ) %>%
      dplyr::relocate(select)
    standardDataTable(data = data, selectionMode = "none")
  })
  
  output$numberOfRowSelectedInRelationship <-
    shiny::reactive({
      return(length(input$relationship_checkboxes_checked))
    })
  
  shiny::outputOptions(x = output,
                       name = 'numberOfRowSelectedInRelationship',
                       suspendWhenHidden = FALSE)
  
  shiny::observeEvent(eventExpr = input$addRelationship,
                      handlerExpr = {
                        if (length(input$relationship_checkboxes_checked) > 0) {
                          idx <- input$relationship_checkboxes_checked
                          conceptIds <-
                            getDeepConceptRelationship()[idx,]$conceptId2
                          data <-
                            ConceptSetDiagnostics::getConceptIdDetails(
                              conceptIds = conceptIds,
                              connection = connectionRemote,
                              vocabularyDatabaseSchema = vocabularyDatabaseSchema
                            ) %>%
                            dplyr::mutate(
                              invalidReasonCaption = "",
                              standardConceptCaption = "",
                              includeDescendants = FALSE,
                              includeMapped = FALSE,
                              isExcluded = FALSE
                            )
                          
                          if ("exclude" %in% input$relationshipParameters) {
                            data$isExcluded <- TRUE
                          }
                          if ("descendent" %in% input$relationshipParameters) {
                            data$includeDescendants <- TRUE
                          }
                          if ("mapped" %in% input$relationshipParameters) {
                            data$includeMapped <- TRUE
                          }
                          conceptSetResultsExpression(rbind(conceptSetResultsExpression(), data))
                          conceptSetSearchResults(NULL)
                        }
                      })
})
