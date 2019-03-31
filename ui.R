#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("2018 Brazil's Public Expenses"),

    sidebarLayout(

        sidebarPanel(
            h5("Use the dropdown menus below to dive into each category and visualize its expenses"),
            uiOutput(outputId = "putselect")
        ),
        mainPanel(
            plotlyOutput(outputId = "plot")

        )
    )
))
