
library(shiny)
library(magrittr)

suppressMessages(bizdays::load_quantlib_calendars(c('UnitedStates/NYSE'),
                                 from='2016-01-01', to=lubridate::today(),
                                 financial = TRUE))
bizdays::create.calendar(
  'UnitedStates/NYSE',
  financial = TRUE,
  weekdays = c("saturday", "sunday")
)

options(spinner.color = main_color,
        spinner.type = 8)

if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
  sapply(as.character(fs::dir_ls(
    stringr::str_c(here::here(), "/app/scripts")
  )), source)
  } else { # For Linux Production
    sapply(as.character(fs::dir_ls(stringr::str_c(
      here::here(), "/scripts"
    ))), source)
    }


shinyUI(
    
    shinydashboard::dashboardPage(
        
        shinydashboard::dashboardHeader(title = "Financial Market Dashboard"),
        
        # sidebar ----------------------------------------------------------------
        shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            id = "mysidebar",
            shiny::imageOutput("picture",
                               height  = "auto"),
            shiny::dateInput(inputId = "val_date",
                             label = "Select Valuation Date",
                             value = lubridate::as_date(
                               bizdays::offset(lubridate::today(),
                                               -1,
                                               'UnitedStates/NYSE')
                               ),
                             format = "dd.mm.yyyy"),
                # 1st Menu:
                shinydashboard::menuItem(strong("Market Volatility"),
                                         tabName = "sector_volas",
                                         icon = icon("chart-line")),
                # 2nd Menu:
                shinydashboard::menuItem(strong("FED Funds Rate"),
                                         tabName = "fed_funds",
                                         icon = icon("project-diagram"))
            )
        ),
        
        # Body --------------------------------------------------------------------
        shinydashboard::dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", 
                          type = "text/css", 
                          href = "custom.css"),
                tags$style(HTML("
                      .shiny-output-error-validation {
                        color: #248A8A;
                        font-weight: bold;
                        font-size: 150%;
                      }")
                )
            ),
        
        # Website metadata for eg link preview
            metathis::meta() %>%
              metathis::meta_social(
                title = "Financial Market Dashboard",
                description = "analyze the weekly Index volatility change by sector for the US and EU",
                url = "https://aikia.org/dashboard/",
                image = "https://aikia.org/images/logo_aikia.png",
                image_alt = "aikia logo",
                twitter_card_type = "summary",
                twitter_site = "@aikia_lab",
                og_site_name = "https://aikia.org"
            ),


# Market Volatility Overview ----------------------------------------------

 shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "sector_volas",
                                    h2("Market Risk Summary"),
                                    
                                    shinyThings::radioSwitchButtons(
                                      inputId = "index_location", 
                                      label = NULL, 
                                      choices = tibble::tibble(
                                        'US Indices' = 1,
                                        'EU Indices' = 2),
                                      not_selected_background = 'grey',
                                      selected_background = main_color_light),
                                    # https://jnolis.com/blog/shiny_mobile/
                                    
                                    shiny::fluidRow(
                                      shiny::column(12,
                                                    class = "col-sm-12",
                                                    shiny::div(
                                                      class = "container-fluid",
                                                      shiny::div(
                                                        class = "text-center text-lg-start",
                                                        conditionalPanel(
                                                          condition = 'input.index_location == 2',
                                                          shiny::h3("Volatility Overview of 'Stoxx Europe 600' Index Family")
                                                        ),
                                                        conditionalPanel(
                                                          condition = 'input.index_location == 1',
                                                          shiny::h3("Volatility Overview of 'Dow Jones US' Index Family")
                                                        ),
                                                        shiny::h4("date to date relative change of volatility % -levels")
                                                      )
                                                    ), 
                                            
                                                    shiny::div(
                                                      # wrapping Output in div lets us align it to the right
                                                      plotly::plotlyOutput("sector_vola",
                                                                           width = "100%",
                                                                           height = "600px") %>%
                                                        shinycssloaders::withSpinner(),
                                                      align = "right"
                                                    ),
                                                    br(),
                                                    shiny::div(
                                                      br(),
                                                      br(),
                                                      plotly::plotlyOutput("sector_line",
                                                                           width = "90%",
                                                                           height = "600px") %>%
                                                        shinycssloaders::withSpinner(),
                                                      align = "center",
                                                      style = "border-top:1px solid black;"
                                                    )
                                        )
                                    )
            ),
        

# FED Funds Rate ----------------------------------------------------------


            shinydashboard::tabItem(tabName = "fed_funds",
                                    h2("Fed Funds Rates"),
                                  
                                    shiny::fluidRow(
                                      shiny::h3("Market Forward Curve Expectation"),
                                      
                                      shiny::column(width = 2,
                                      # Select 1st Date
                                      shiny::dateInput(inputId = "fed_date_1",
                                                       label = "Select Valuation Date for 1st Curve",
                                                       value = lubridate::as_date(
                                                         bizdays::offset(lubridate::today(),
                                                                         -1,
                                                                         'UnitedStates/NYSE')
                                                       ),
                                                       format = "dd.mm.yyyy",
                                                       daysofweekdisabled = c(0,6))
                                      ),
                                      
                                      shiny::column(width = 2,
                                      # Select 2nd Date
                                      shiny::dateInput(inputId = "fed_date_2",
                                                       label = "Select Valuation Date for 2nd Curve",
                                                       value = lubridate::as_date(
                                                         bizdays::offset(lubridate::today(), 
                                                                         -2,
                                                                         'UnitedStates/NYSE')
                                                       ),
                                                       format = "dd.mm.yyyy",
                                                       daysofweekdisabled = c(0,6))
                                      )
                                      ),
                                    shiny::fluidRow(
                                      
                                      plotly::plotlyOutput("fed_rates") %>%
                                        shinycssloaders::withSpinner(),
                                      "Note: 1 hike = 25bps rate step"
                                    ),
                                    br(), br(),
                                    shiny::fluidRow(
                                      h3("History of meeting date sensitivities")
                                      
                                    )
            )
        )
    )
)
)


