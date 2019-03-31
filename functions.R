library(shiny)
library(ggplot2)
library(plotly)
cleanData<-function(){
    budget<-read.csv("2018_OrcamentoDespesa.zip.csv",sep=";",dec=",")
    # remove reference code variables
    budget<-budget[,!grepl("CODIGO",names(budget))]
    budget<-budget[,-c(1,12,13)]
    write.csv(budget,"brazilianBudget2008.csv",row.names = FALSE)
    return(budget)

}
