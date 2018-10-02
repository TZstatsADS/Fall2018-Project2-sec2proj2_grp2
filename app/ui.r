packages.used=c("shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)
library(shinyBS)
library(plotly)
library(extrafont)
library(grDevices)
library(shinyjs)
library(shinythemes)








ui <- navbarPage(theme=shinytheme("lumen"),
  includeCSS("style.css"),
    tabPanel(
      title="Maps", icon=icon("map")),
    tabPanel(
      title="Comparison",icon=icon("balance-scale")),
    tabPanel(
      title="Recommendation System",icon=icon("thumbs-up"))
  ,
  fluidRow(
    column(width=12,div(style="height:200px;"))),
  fluidRow(
    column(width=12,div(style="height:60px;",HTML('<center><img src="sc4.png" width="70"></center>')
                        )
  )),
  fluidRow(
    column(width=4,offset=4,div(style="height:70px;"),h1("NY Schools Hunter",align="center",style="color:white;font-family:Montserrat;")
           )
  ),
  fluidRow(
    column(width=4,offset=4,div(style="height:100px",p("Our project takes all available data on colleges and universities in New York State and creates a useful 
                                                       shiny app that allows users to explore and compare schools based on user-specific filtering criteria. 
                                                       The purpose of our design is to provide users with a 
                                                       bird's eye view of New York colleges and universities; 
                                                       allow them to filter, search, and group schools by their preferred criteria; 
                                                       and further compare two schools on a more micro level.",align="center",style="color:#e6e6e6")))
  )
 
)



  
  
  
  

