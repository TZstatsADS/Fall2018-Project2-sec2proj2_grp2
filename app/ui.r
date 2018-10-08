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
#Support data frames
major = c("Agriculture","Natural Resources And Conservation", "Architecture And Related Services",
          "Computer And Information Sciences And Support Services"," Education","Engineering"," Biological And Biomedical Sciences",
          "Mathematics And Statistics", "Psychology","Social Sciences","Business, Management, Marketing, And Related Support Services","History")
major.index =colnames(schooldata)[16:27]
major.frame = data.frame(major = major, index = major.index)
choicelist<-as.list(unique(as.data.frame(major.frame)[,2]))




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
      title="Maps", icon=icon("map"),
      div(class="outer",  
          # lealfet map
          leafletOutput("my_map", width="100%", height= "700px"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 100, left = 5, bottom = "auto",
                        width = "auto", height = "auto", cursor = "move",
                        wellPanel(style = "overflow-y:scroll; max-height: 600px",
                                  bsCollapse(id="collapse.filter",open="Filter", 
                                             
                                             bsCollapsePanel(tags$strong("Academic"),style="primary",
                                                             tags$style(type="text/css",
                                                                        ".shiny-output-error { visibility: hidden; }",
                                                                        ".shiny-output-error:before { visibility: hidden; }"),
                                                             fluidRow(column(12,checkboxGroupInput("filter","Major",choices=choicelist,selected=1))),
                                                             fluidRow(column(10,sliderInput('SAT',label=h3('SAT Range'),min=800,max=1600,value=c(1200,1600))))),
                                             
                                             
                                             
                                             
                                             bsCollapsePanel(tags$strong("Location Preference"),style = "primary",
                                                             bsCollapsePanel(tags$strong("Citytype"),style="info",
                                                                             fluidRow(column(10,selectInput("Citytype",tags$strong("Type prefer"),choices = c('Suburb','City','Town','Rural'),selected = "None"))
                                                                             )
                                                             ),
                                                             bsCollapsePanel(tags$strong("CrimeRate"),style="info",
                                                                             fluidRow(column(10,sliderInput("CrimeRate",label=h3('Crime Range'),min=0,max=1300,value=c(100,500))))
                                                                             
                                                             ),
                                                             
                                                             bsCollapsePanel(tags$strong("HappyScore"),style="info",  
                                                                             fluidRow(column(10,sliderInput("HappyScore",tags$strong("Happy Score"),min=30,max=80,value=c(50,60))))
                                                             )
                                             )
                                  ),                                                      
                                  actionButton("search", tags$strong("Searching!"))
                        )#WellPanel ends here
                        
          )
          
          
          
      )
      ,div(class="footer", "Applied Data Science Fall18 Group 2")
    ),
  #Map ends here
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
      
  
  


  
  
  
  

