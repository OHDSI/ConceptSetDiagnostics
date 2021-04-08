library(magrittr)

shinydashboard::dashboardPage(sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
                              header = shinydashboard::dashboardHeader(title = "Concept Set Diagnostric"),
                              body = shinydashboard::dashboardBody(
                                shinydashboard::box(title = "Concept Set Diagnostric",width = NULL,status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                                    shiny::textInput(inputId = "keyword",label = "Keyword"),
                                                    shiny::actionButton(inputId = "search",label = "Search")
                                                    
                                )
                              ))
