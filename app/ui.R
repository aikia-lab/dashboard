
library(shiny)
library(shinyBS)
library(magrittr)

suppressMessages(bizdays::load_quantlib_calendars(c('UnitedStates/NYSE'),
                                 from='2016-01-01', to=lubridate::today(),
                                 financial = TRUE))
bizdays::create.calendar(
  'UnitedStates/NYSE',
  financial = TRUE,
  weekdays = c("saturday", "sunday")
)


if(Sys.info()[[1]] == "Windows"){ #For testing on Windows

  sapply(as.character(fs::dir_ls(
    stringr::str_c(here::here(), "/app/scripts")
  )), source)
  } else { # For Linux Production
    sapply(as.character(fs::dir_ls(stringr::str_c(
      here::here(), "/scripts"
    ))), source)
    }


options(spinner.color = main_color,
        spinner.type = 8)


shinyUI(
   
    shinydashboardPlus::dashboardPage(     
      shinydashboardPlus::dashboardHeader(
        title = HTML(glue::glue(
            '<span class="logo-mini"><em>m</em><strong>d</strong></span>
           <span class="logo-lg"><em>market </em><strong>dashboard</strong></span>'
        )), 
        leftUi = tagList(
                  shiny::dateInput(inputId = "val_date",
                                   label = "Select Valuation Date",
                                   value = lubridate::as_date(
                                     bizdays::offset(lubridate::today(),
                                                     -1,
                                                     'UnitedStates/NYSE')
                                   ),
                                   format = "dd.mm.yyyy",
                                   daysofweekdisabled = c(0,6))
                  )
          ),
            
        
        # sidebar ----------------------------------------------------------------
        
        shinydashboardPlus::dashboardSidebar(
          collapsed = TRUE,
        # workaround for reactive sidebar collapsing        
          tags$script("$(document).on('click', '.sidebar-toggle', function () {
                      Shiny.onInputChange('SideBar_col_react', Math.random())});"),
      
          shinydashboard::sidebarMenu(
            id = "mysidebar",
            
            br(),
            shiny::imageOutput("picture", height  = "auto"),
            br(),

                # 1st Menu:
                shinydashboard::menuItem(strong("Market Volatility"),
                                         tabName = "sector_volas",
                                         icon = icon("bars-staggered")),
                # 2nd Menu:
                shinydashboard::menuItem(strong("FED Funds Rate"),
                                         tabName = "fed_funds",
                                         icon = icon("chart-line")),
            
                # 3rd Menu:
                shinydashboard::menuItem(strong("Index Entropy"),
                                         tabName = "idx_entro",
                                         icon = icon("project-diagram")),
                # 4th Menu:
                shinydashboard::menuItem(strong("Eco Forecasts"),
                                         tabName = "eco_fc",
                                         icon = icon("chart-simple"))
            )
        ),
        
        # Body --------------------------------------------------------------------
        shinydashboard::dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", 
                          type = "text/css", 
                          href = "custom.css"),
                tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);")
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
                                                       min = "2022-02-14",
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
                                                       min = "2022-02-14",
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
                                      h3("History of meeting date sensitivities"),
                                      
                                      shiny::column(width = 2,
                                        shiny::selectInput("meeting_d", "Select Meeting Date", 
                                                         choices = get_fed_meeting_dates())
                                      ),
                                      br(), br(),br(), br(),
                                      plotly::plotlyOutput("meeting_date",
                                                           width = "90%") %>%
                                        shinycssloaders::withSpinner(),
                                      br(), br(),
                                    )
            ),

# Index Entropy ----------------------------------------------------------


            shinydashboard::tabItem(tabName = "idx_entro",
                                    h2("Index Entropy via Network Graph"),
                                    
                                        shiny::fluidRow(
                                          shiny::h3("identify structural order via correlation limits"),
                                          
                                          shiny::column(width = 6,
                                                        shiny::selectInput("choose_idx", 
                                                                           label = "Select Market Index", 
                                                                           choices = c("S&P 500" = "^GSPC",
                                                                                       "Nasdaq 100" = "^NDX",
                                                                                       "Euro Stoxx 50" = "^STOXX50E",
                                                                                       "DAX 40" = "^GDAXI",
                                                                                       "ASX" = "^AXJO"),
                                                                           selected = "^NDX")
                                          ),
                                        
                                    
                                        shiny::column(width = 3,
                                                      shiny::selectInput("choose_grouping", "Colour Nodes by Level", 
                                                                         choices = c("Industry Level" = "Industry",
                                                                                     "Sector Level" = "Sector"),
                                                                         selected = "BIC_1")
                                        ),
                                        shiny::column(width = 3,
                                                      shiny::selectInput("choose_entropy_th", 
                                                                         label = tags$span("Choose Entropy Level", 
                                                                                           shinyBS::bsButton("entropy_info", 
                                                                                                    label = "", 
                                                                                                    icon = icon("info"), 
                                                                                                    style = "info", 
                                                                                                    size = "extra-small")), 
                                                                         choices = c("Correlation Limit 0.5" = 0.5,
                                                                                     "Correlation Limit 0.6" = 0.6,
                                                                                     "Correlation Limit 0.7" = 0.7,
                                                                                     "Correlation Limit 0.8" = 0.8),
                                                                         selected = 0.7,
                                                                         ),
                                                      shinyBS::bsPopover(
                                                        id = "entropy_info",
                                                        title = "More information",
                                                        content = paste0(
                                                          "On how Entropy works, please visit our website ",
                                                          a("aikia.org", href = "https://aikia.org", target="_blank")
                                                        ),
                                                        placement = "right",
                                                        trigger = "hover",
                                                        options = list(container = "body")
                                                      )
                                        )
                                          
                                        
                                      ),
                                        br(), br(),br(), br(),
                                          
                                        shiny::column(width = 6,
                                          plotly::plotlyOutput("idx_entrop", height = "600px") %>%
                                            shinycssloaders::withSpinner()
                                        ),
                                        shiny::column(width = 6,
                                          plotly::plotlyOutput("date_entropy") %>%
                                            shinycssloaders::withSpinner()
                                        )
                  ),

                                                                          
# Eco actual vs forecasts -------------------------------------------------                  
                  
                  shinydashboard::tabItem(tabName = "eco_fc",
                                          h2("Economic Actuals vs Forecasts"),
                                          
                                          shiny::fluidRow(
                                            shiny::h3("Check if actual figures match forecasted ones"),
                                            
                                            shiny::column(width = 3,
                                                shiny::selectInput("choose_eco_country", 
                                                                   label = "Select Country", 
                                                                   choices = c("United States" = "united-states",
                                                                               "China" = "china",
                                                                               "Germany" = "germany",
                                                                               "Japan" = "japan",
                                                                               "United Kingdom" = "united-kingdom",
                                                                               "France" ="france",
                                                                               "Italy" = "italy",
                                                                               "India" = "india"),
                                                                   selected = "united-states"),
                                            ),
                                            shiny::column(width = 3,
                                                shiny::selectInput("choose_eco_type", 
                                                                   label = "Select economic factor", 
                                                                   choices = c("Government Bond 10Y",
                                                                               "GDP Annual Growth Rate",
                                                                               "Current Account to GDP",
                                                                               "Balance of Trade",
                                                                               "Initial Jobless Claims",
                                                                               "Labour Costs",
                                                                               "Productivity",
                                                                               "Imports",
                                                                               "Exports"),
                                                                   selected = "Government Bond 10Y")
                                            )
                                            
                                          ),
                                          br(), br(),br(), br(),
                                          
                                          shiny::column(width = 10,
                                                        plotly::plotlyOutput("eco_forecasts", height = "600px") %>%
                                                          shinycssloaders::withSpinner()
                                          )
                  )
                        




            
            )
        )
    )
)



