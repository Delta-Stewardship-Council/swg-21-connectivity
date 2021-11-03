#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(contentid)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

# Bring in Air Temperature ------------------------------------------------

cimis.bryte.id = contentid::store("../data/Bryte_CIMIS_daily.csv")

cimis.bryte.path <- contentid::resolve(cimis.bryte.id)

# fix the sample date format, and filter for species of interest
bryte <- read.csv(cimis.bryte.path) %>%
    mutate(Date = mdy(Date))

# make clean column names
library(janitor)
bryte <- janitor::clean_names(bryte)

# filter to cols of interest (drop qc cols)
bryte_filt <- select(bryte,
                     stn_id:date, precip_mm, sol_rad_w_sq_m,
                     ends_with("air_temp_c"))

cols <- names(bryte_filt)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring input data for the Connectivity Study"),

    p("Data for this application are from: "),
    tags$ul(
        tags$li("USGS: Bryte weather station, 1998-2018. change rest later",
                tags$a("doi:10.6073/pasta/b0b15aef7f3b52d2c5adc10004c05a6f", href="http://doi.org/10.6073/pasta/b0b15aef7f3b52d2c5adc10004c05a6f")
        )
    ),
    tags$br(),
    tags$hr(),

    verticalLayout(
        # Sidebar with a slider input for depth axis
        sidebarLayout(
            sidebarPanel(
                sliderInput("date",
                            "Date:",
                            min = as.Date("1998-01-01"),
                            max = as.Date("2020-12-31"),
                            value = c(as.Date("1998-01-01"), as.Date("2020-12-31")))
            ),
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot")
            )
        ),

        tags$hr(),

        sidebarLayout(
            sidebarPanel(
                selectInput("x_variable", "X Variable", cols, selected = "date"),
                selectInput("y_variable", "Y Variable", cols, selected = "sol_rad_w_sq_m"),
                selectInput("color_variable", "Color", cols, selected = "cimis_region")
            ),

            # Show a plot with configurable axes
            mainPanel(
                plotOutput("varPlot")
            )
        ),

        tags$hr()
    )
)

# Define server logic required to draw a time series
server <- function(input, output) {

    output$distPlot <- renderPlot({

        ggplot(bryte_filt, mapping = aes(date, max_air_temp_c)) +
            geom_point(colour = "blue", size = 1.5) +
            xlim(input$date[1], input$date[2]) +
            theme_light()
    })

    output$varPlot <- renderPlot({
        ggplot(bryte_filt, aes(x = .data[[input$x_variable]],
                               y = .data[[input$y_variable]],
                               color = .data[[input$color_variable]])) +
            geom_point(size = 1.5)+
            theme_light()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
