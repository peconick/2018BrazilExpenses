library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
#------------------------------------------------------------------------------
# UI
#------------------------------------------------------------------------------

ui = fluidPage(
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
)

#------------------------------------------------------------------------------
# SERVER
#------------------------------------------------------------------------------

server = function(input,output){
    budget<-read.csv("brazilianBudget2008.csv")
    levels<-names(budget)
    nLevels<-length(budget)-3


    output$putselect = renderUI({
        buildUI(budget,input)
    })
    output$plot = renderPlotly({
        buildPlot(budget,input)

    })

}
#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------

buildUI<-function(budget,input=NULL){
    levels<-names(budget)
    nLevels<-length(budget)-2

    i<-1
    stop=FALSE
    dropdown<-list()

    while (!stop){
        levelName<-levels[i]
        opt<-c("<<ALL>>",as.character(unique(budget[,levelName])))
        selectedValue<-input[[levelName]]
        if (is.null(selectedValue)) selectedValue<-"<<ALL>>"

        # fiter data finde nested subtypes
        if (selectedValue!="<<ALL>>"){
            filter<-budget[,levelName]==selectedValue
            budget<-budget[filter,]
        }

        # create dropdown
        dropdown[[i]]<-selectInput(inputId = levelName,
                                   label = levelName,
                                   choices   = opt,
                                   selected = selectedValue)
        # Stop criteria
        i<-i+1
        if (i==nLevels) stop=TRUE
        if (selectedValue=="<<ALL>>") stop=TRUE
    }
    dropdown
}

buildPlot<- function(budget,input=NULL){
    levels<-names(budget)
    stop = FALSE
    i=1
    while (!stop){
        levelName<-levels[i]
        selectedValue<-input[[levelName]]
        if (selectedValue!="<<ALL>>" && !is.null(selectedValue)){
            filter<-budget[,levelName]==selectedValue
            budget<-budget[filter,]
        }
        else {

            summary<-budget %>% group_by_(Group=levelName) %>%
                summarize(Expenses=sum(ORCAMENTO.REALIZADO))
            summary<-summary[order(summary$Expenses,decreasing = FALSE),]
            summary$Group<-factor(summary$Group,levels = summary$Group)
            plot<-ggplot(summary,aes(y=Expenses,x=Group))
            plot<-plot+geom_bar(stat = "identity",position = "dodge")
            plot<-plot+ylab("Expenses (BRL)")+xlab(levelName)+coord_flip()

            stop=TRUE

        }
        i<-i+1
        if (i==length(levels)-1) stop <-TRUE
    }
    plot
}


shinyApp(ui = ui , server = server)
