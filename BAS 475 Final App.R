#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(fpp3)
library(readr)
library(shinyWidgets)
library(ggeasy)
#Loading and Cleaning Data
# Path where data is
getwd
file_path1 <- "C:/Users/matth/Documents/RStudio Directory/Memphis_Grizzlies.csv"
file_path2 <- "C:/Users/matth/Documents/RStudio Directory/Grizzlies Stats.csv"
# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path1, skip = 2)
grizz_trends <- read.csv(file_path2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
names(grizz_trends) <- c("year", "NBA","Team", "Wins", "Losses", "Win Percentage", "PlaceInDivision", "Simple Rating", "Pace", "Relative Pace", "Offensive Rating", "Relative Offensive Rating","Defensive Rating","Relative Defensive Rating", "Playoffs", "Coaches", "Best Player" )
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
grizz_trends$year<- 2022:1996
# Convert to tsibble
g_trends <- tsibble(g_trends)
grizz_trends <- tsibble(grizz_trends, index = "year")
# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$Interest <- as.numeric(
    ifelse(g_trends$Interest == "<1", 0, g_trends$Interest)
)

MODELS1 <- g_trends %>%
  model(
    NaiveModel = NAIVE(Interest),
    SeasonalNaiveModel = SNAIVE(Interest ~ lag("year")),
    Mean = MEAN(Interest),
    Drift = RW(Interest ~ drift())
  )

FORECAST1 <- MODELS1 %>%
  forecast(h = 12)

MODELS2 <- g_trends %>%
  model(
    HoltsModel = ETS(Interest ~ error("A")+ trend("A") + season("N")),
    HoltsWintersMultiplicativeModel = ETS(Interest ~ error("M") + trend ("A") + season("M"))
  )

FORECAST2 <- MODELS2 %>%
  forecast(h = 12)

MODELS3 <- g_trends %>%
  model(
    Arima210 = ARIMA(Interest ~ pdq(2,1,0)),
    Arima010 = ARIMA(Interest ~ pdq(0,1,0)),
    Stepwise = ARIMA(Interest),
    Search = ARIMA(Interest, stepwise = FALSE)
  )

FORECAST3 <- MODELS3 %>%
  forecast(h = 12)

#autoplot(g_trends)
#Define Sidebar
Side <- dashboardSidebar(
    sidebarMenu(
        menuItem("General Information", tabName = "GenInfo", icon = icon("comment")),
        menuItem("Time Series Plot",tabName = "TSP", icon = icon("th")), 
        menuItem("Secondary Plots",tabName = "SP", icon = icon("th")),
        menuItem("Grizzlies Franchise History",tabName = "GFH", icon = icon("th")),
        menuItem("Simple Models",tabName = "Simp", icon = icon("th")),
        menuItem("Holts-Winters Model",tabName = "HolW", icon = icon("th")),
        menuItem("ARIMA Models",tabName = "ARIMA", icon = icon("th"))
)
)
#Define Body
Body <- dashboardBody(
    tabItems(
        tabItem(tabName = "GenInfo", p("This app seeks to examine the changes in the Google interest for the Memphis Grizzlies over time, as well as look back at the history of the franchise. The Time Series Plot Tab displays a time series showing the interest in the Memphis Grizzlies on Google from 2004 to the present. The Secondary Graphs Tab allows users to select one of three graphs from a drop down menu to further examine different parts of the data such as seasonality, decomposition, and autocorrelation. Finally, the bottom tab allows users to choose one of several categories and examine how the Grizzlies have done over time as a franchise. Some of these categories are wins per season, win percentage, and offensive rating.")),
        tabItem(tabName = "TSP", fluidRow(box(title = "Time Series Plot", plotOutput("TimeSeries"), p("This time series graph displays the overall google search interest in the Memphis Grizzlies. The search interest tends to have a seasonal pattern, with a spike at the start, a dip during the middle of the season and a peak if the team makes the playoffs. The absolute peak on the graph was in May 2013 when the Grizzlies made the conference finals. It is currently on the rise due to the arrival of Ja Morant and much more media coverage.")))),
        tabItem(tabName = "SP", fluidRow(
                                    box(title = "Additional Plots", plotOutput("SelectedGraph")),
                                    box(pickerInput(
                                        inputId = "GraphType",
                                        label = "Desired Graph", 
                                        choices = c("Seasonality", "AutoCorrelation", "Decomposition"))),
                                    h3("Seasonality"), p("The Seasonality of this data is very much in sync with the ebbs and flows of the typical NBA season. It usually peaks during the playoffs, dips during the offseason, rises leading into the start of a new season, dips during the mid season and finally begins to rise again as it goes into the playoffs."),
                                    h3("AutoCorrelation"), p("The autocorrelation lags alo tend to support the argument made during the seasonality portion. The interest tends to build during the year, which explains the high correlation associated with the first lag. Since it is roughly cyclical every year, this explains why 11, 12 and 13 months also have high correlation values."),
                                    h3("Decomposition"), p("The decomposition graph shows that the trend is incredibly impactful, the random is important, and that the seasonal impact on data is the least impactful. This makes sense due to the general uptick in NBA popularity over the last decade, as well as the specific interest in Memphis due to some new, young superstars.")
                                    )),
        tabItem(tabName = "GFH", 
                fluidRow(
                    box(title = "Grizzlies Plot", plotOutput("GrizzPlot")),
                    box(
                        pickerInput(
                        inputId = "YVar",
                        label = "Category of Interest", 
                        choices = c("Wins", "Losses", "Win Percentage", "Simple Rating", "Pace", "Relative Pace", "Offensive Rating", "Relative Offensive Rating","Defensive Rating","Relative Defensive Rating")
                    )))
    ),
    tabItem(tabName = "Simp",fluidRow(box(title = "Simple Models", plotOutput("Forecast1")))),
    tabItem(tabName = "HolW",fluidRow(box(title = "Holts-Winters Model", plotOutput("Forecast2")))),
    tabItem(tabName = "ARIMA",fluidRow(box(title = "ARIMA Models", plotOutput("Forecast3"))))
)
)
# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "Memphis Grizzlies"),
    Side, 
    Body
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$TimeSeries <- renderPlot({
        autoplot(g_trends)})
    
    output$Forecast1 <- renderPlot({
      autoplot(g_trends)+autolayer(FORECAST1)
    })
    
    output$Forecast2 <- renderPlot({
      autoplot(g_trends)+autolayer(FORECAST2)
    })
    
    output$Forecast3 <- renderPlot({
      autoplot(g_trends)+autolayer(FORECAST3)
    })
    
    output$GrizzPlot <- renderPlot({
        History <- select(grizz_trends, input$YVar)
        autoplot(History) })
    
    ReactiveGraphs <- reactive({
        if (input$GraphType == "AutoCorrelation")
        {g_trends %>% ACF(g_trends$Interest, lag_max = 20) %>% autoplot()}
        else if (input$GraphType == "Decomposition"){
            g_trends %>% 
                model(
                    classical_decomposition(Interest, type = "multiplicative")
                ) %>% 
                components() %>% 
                autoplot()
        }
        else if (input$GraphType == "Seasonality"){
            g_trends %>%  gg_season(labels = "both")
        }
    })
    output$SelectedGraph <- renderPlot({ReactiveGraphs()})
}

# Run the application 
shinyApp(ui = ui, server = server)
