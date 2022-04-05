





library(shiny)
library(magrittr)

bizdays::load_quantlib_calendars(c('UnitedStates/NYSE'),
                                 from='2016-01-01', to=lubridate::today(),
                                 financial = TRUE)
bizdays::create.calendar('UnitedStates/NYSE', financial = TRUE, weekdays = c("saturday", "sunday"))

sapply(as.character(fs::dir_ls(stringr::str_c(here::here(),"/scripts"))), source)

shinyUI(
    
    shinydashboard::dashboardPage(
        
        shinydashboard::dashboardHeader(title = "Financial Market Dashboard"),
        
        # sidebar ----------------------------------------------------------------
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(id="mysidebar",
                                        shiny::imageOutput("picture", height  = "auto"),
            
                shiny::dateInput(inputId = "val_date",
                                 label = "Select Valuation Date",
                                 value = lubridate::as_date(
                                     bizdays::offset(lubridate::today(), -1,'UnitedStates/NYSE')
                                 ),
                                 format = "dd.mm.yyyy"),
                # 1st Menu:
                shinydashboard::menuItem("Market Vola",
                                         tabName = "sector_volas",
                                         expandedName = "sector_vola",
                                         startExpanded = TRUE,
                                         icon = icon("chart-line")),
                # 2nd Menu:
                shinydashboard::menuItem(
                    strong("Index Entropy"),
                    expandedName = "index_entropy",
                    tabName = "index_entropys",
                    icon = icon("project-diagram")
                )
            )
        ),
        
        # Body --------------------------------------------------------------------
        shinydashboard::dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            
            shinydashboard::tabItem(tabName = "mr_overview",
                                    h2("Market Risk Summary"),
                                    
                                    shinyThings::radioSwitchButtons(inputId = "index_location", label = NULL, choices = tibble::tibble('EU Indices' = 1,
                                                                                                                                      'US Indices' = 2),
                                                                    not_selected_background = 'grey',
                                                                    selected_background = main_color_light),
                                    
                                    shiny::fluidRow(
                                        shiny::h3("Sector Volatility Overview"),
                                        
                                        shiny::div( # wrapping Output in div lets us align it to the right
                                            plotly::plotlyOutput("sector_vola",
                                                                 width = "100%",
                                                                 height = "600px") %>%
                                                shinycssloaders::withSpinner(type = 8),
                                            align = "right"
                                        ),
                                        br(),
                                        shiny::div(
                                            plotly::plotlyOutput("sector_line",
                                                                 width = "60%",
                                                                 height = "600px"),
                                            align = "center",
                                            style = "border-top:1px solid black;"
                                        )
                                    )
            )
        )
    )
)


