packages.used=c("DT","shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(DT)
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


schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)





ui <- navbarPage(theme=shinytheme("lumen"),
  includeCSS("style.css"),
    tabPanel(
      title="Home",icon=icon("home"),
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
      
        ),
  
    
    tabPanel(
      title="Maps", icon=icon("map")),
    tabPanel(
      title="Comparison",icon=icon("balance-scale")),
    tabPanel(
      title="Recommendation System",icon=icon("thumbs-up"),
      
                 fluidRow(column(width=3,
              absolutePanel(width=550,
                wellPanel(
                  h3("SELECT FILTERS",style="color:black;font-family:Futura Md BT;"),
              sliderInput("satscore",label=tags$b("SAT Score",style="color:black;font-family:Futura Bk BT;"),min=400, max=1600, value=1000,step=10),
              
               selectInput("city",label=tags$b("Located in city/rural",style="color:black;font-family:Futura Bk BT;"),choices=c("City","Not City")),
              
              sliderInput("crime",label=tags$b("Crime Rate (Per 100000 people)",style="color:black;font-family:Futura Bk BT;"),min=20,max=1300,value=600),
              
                sliderInput("tuition", label=tags$b("Tuition (Per Year)",style="color:black;font-family:Futura Bk BT;"),min=10000,max=60000,value=30000),
              actionButton("getschool",label="WHAT'S YOUR SCHOOL"), dataTableOutput("uni")),style="opacity:0.8;font-family:Montserrat;"
              )),
               
                 column(width=8,div(style="height:50px"),
                 plotlyOutput("radarplot"))
      )))
      
  
  


  
  
  
  

