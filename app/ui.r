#UI part

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

#Homepage
ui <- navbarPage(theme=shinytheme("flatly"),
  includeCSS("style.css"),
    tabPanel(
      title="Home",icon=icon("home"),
      fluidRow(
        column(width=12,div(style="height:100px;"))),

      fluidRow(
        column(width=4,offset=4,div(style="height:70px;"),h1("NY Schools Hunter",align="center",style="color:white;font-family:Montserrat;")
        )
      ),
      fluidRow(column(width=12,div(style="height:30px;"))),
      fluidRow(
        column(width=4,offset=4,div(style="height:100px",p("Our project takes all available data on colleges and universities in New York State and creates a useful 
                                                           shiny app that allows users to explore and compare schools based on user-specific filtering criteria. 
                                                           The purpose of our design is to provide users with a 
                                                           bird's eye view of New York colleges and universities; 
                                                           allow them to filter, search, and group schools by their preferred criteria; 
                                                           and further compare two schools on a more micro level.",align="center",style="color:#e6e6e6")))
        ),
      fluidRow(column(width=12,div(style="height:100px;")))
      
        ),
  
#Map 
    tabPanel(
      title="Maps", icon=icon("map"),
      div(class="outer",  
          # lealfet map
          leafletOutput("my_map", width="100%", height= "700px"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        style="opacity:0.8;font-family:Montserrat;",
                        draggable = TRUE, top = 100, left = 5, bottom = "auto",
                        width = "auto", height = "auto", cursor = "move",
                        wellPanel(style = "overflow-y:scroll; max-height: 600px",
                                  bsCollapse(id="collapse.filter",open="Filter", 
                                             
                                             bsCollapsePanel(tags$strong("Academic",style="font-family:Franklin Gothic Medium;"),style="primary",
                                                             tags$style(type="text/css",
                                                                        ".shiny-output-error { visibility: hidden; }",
                                                                        ".shiny-output-error:before { visibility: hidden; }"),
                                                             fluidRow(column(12,checkboxGroupInput("filter",tags$b("Major",style="font-family:Franklin Gothic Medium;"),choices=choicelist,selected=1))),
                                                             fluidRow(column(10,sliderInput('SAT',label=tags$b('SAT Range',style="font-family:Franklin Gothic Medium;"),min=800,max=1600,value=c(1200,1600))))),
                                             
                                             
                                             
                                             
                                             bsCollapsePanel(tags$strong("Location Preference",style="font-family:Franklin Gothic Medium;"),style = "primary",
                                                             bsCollapsePanel(tags$strong("City Type",style="font-family:Franklin Gothic Medium;"),style="info",
                                                                             fluidRow(column(10,selectInput("Citytype",tags$strong("Type prefer",style="font-family:Franklin Gothic Medium;"),choices = c('Suburb','City','Town','Rural'),selected = "None"))
                                                                             )
                                                             ),
                                                             bsCollapsePanel(tags$strong("Crime Rate",style="font-family:Franklin Gothic Medium;"),style="info",
                                                                             fluidRow(column(10,sliderInput("CrimeRate",label=tags$b('Crime Range',style="font-family:Franklin Gothic Medium;"),min=0,max=1300,value=c(100,500))))
                                                                             
                                                             ),
                                                             
                                                             bsCollapsePanel(tags$strong("Happy Score",style="font-family:Franklin Gothic Medium;"),style="info",  
                                                                             fluidRow(column(10,sliderInput("HappyScore",tags$strong("Happy Score",style="font-family:Franklin Gothic Medium;"),min=30,max=80,value=c(50,60))))
                                                             )
                                             )
                                  ),                                                      
                                  actionButton("search", tags$strong("Searching!"))
                        )#WellPanel ends here
                        
          )
          
          
          
      )
     
    ),

#Comparison
    tabPanel(
      title="Comparison",icon=icon("balance-scale"),
      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                tags$hr(style="border-color: #6088B1;"),
                h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times; 
                   font-size:50pt"),
                tags$hr(style="border-color: #6088B1;"),
                
                #### select schools ###
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                    strong(column(width=5,offset=1,"Select School A: "))
                                                    ,
                                                    strong(column(width=5,offset=1,"Select School B: "))
                )
                ),
                
                absolutePanel(id = "select schools", width="90%",#class = "City_Carrier_panel panel panel-default",
                              fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                                  selectInput("input1",label=NULL,choices=college$INSTNM),
                                                                  selectInput("input2",label=NULL,choices=college$INSTNM))
                              )),
                
                br(),br(),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    uiOutput("ui.1"),
                                                    uiOutput("ui.2")
                )
                ),
                br(),br(),
                # fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                #                                     imageOutput("logo1",height = "400", width = "400"),imageOutput("logo2",height = "400", width = "400")
                # )),
                # 
                # 
                # br(),
                # 
                
                
                # ==== Title in Orange
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Basic Information" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                
                # === Some text to explain the Figure:
                
                br(),
                # === display instnm
                # fluidRow(align = "center",splitLayout(cellWidths = c("50%","50%"),
                #                                       fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm1")),
                #                                       fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm2")))
                # ),br(),
                # === display city
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city1")),
                                                       fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city2")))
                ),br(),
                
                # === display clevel
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel1")),
                                                       fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel2")))
                ),br(),
                # === display control
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control2")))
                ),br(),
                # === display highest degree
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg1")),
                                                     fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg2")))
                ),br(),
                # === display crime rate
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=2,offset=1,"Crime rate: ")),textOutput("cr1")),
                                                       fluidRow(strong(column(width=2,offset=1,"Crime rate: ")),textOutput("cr2")))
                ),br(),
                # === display locale
                
                # fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                #                                      fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale1")),
                #                                      fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale2")))
                # ),br(),
                
                # ==== Title in Orange Adimission====
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Adimission" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                ####display admission rate
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate2")))
                ),br(),
                ### SAT & ACT
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("SAT & ACT Scores" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:10pt ; font-type:bold" )),
                                hr(style="color:#808080")
                         )),
                br(),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    textOutput("school1.2"),
                                                    textOutput("school2.2"))
                         
                ),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("sat1"),
                                                    tableOutput("sat2"))
                         
                ),br(),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("act1"),
                                                    tableOutput("act2"))
                         
                ),
                br(),
                br(),
                # === Bar with corresponding widget
                fluidRow(align="center",column(4,h2("Major Diversity",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("my_barplot1" , height = "500px"),
                                     plotlyOutput("my_barplot2" , height = "500px")
                         )
                         
                ),br(),br(),
                # ==== Title in Orange Cost & Aid====
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Cost & Aid" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                ### display in-state tuition
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                     fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in1")),
                                                     fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in2")))
                ),br(),
                ### display out-of-state tuition
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out2")))
                ),br(),
                ### display percentage of federal loans
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                             textOutput("pctfloan1")),
                                                    fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                             textOutput("pctfloan2")))
                ),br(),
                
                #### Median debt given income
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Calculate Median Debt by Family Income" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
                
                # === display sliderInput for family income
                fluidRow(align="center",sliderInput("fincome","Family Income: ",
                                                    min=0,max=200000,value=0,width = 600)
                         
                ),br(),
                # === display median debt based on family income input 
                
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                     fluidRow(column(width=7,offset=1,textOutput("school1")),
                                                              strong(column(width=7,offset = 1,"Median Debt based on Family Income : ")),br(),
                                                              textOutput("debt1")),
                                                     fluidRow(column(width=7,offset=1,textOutput("school2")),
                                                              strong(column(width=7,offset=1,"Median Debt based on Family Income: ")),br(),
                                                              textOutput("debt2")))
                ),
                br(),
                br(),
                
                # =======  EARNING =======
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Earning" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
                
                #### display pct over 25K
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=7,offset=1,"Share of students earning over 25K (6 yrs): \n")),
                                                              textOutput("gt25k_out1")),
                                                     fluidRow(strong(column(width=7,offset=1,"Share of students earning over 25K (6 yrs): \n")),
                                                              textOutput("gt25k_out2")))
                ),br(),
                
                ### earning 25-75 percentile
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Income" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:10pt ; font-type:bold" )),
                                hr(style="color:#808080")
                         )),
                br(),
                
                # fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                #                                     textOutput("school1.2"),
                #                                     textOutput("school2.2"))
                # 
                # ),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("earning1"),
                                                    tableOutput("earning2"))
                         
                ),br(),
                br(),
                
                
                #=======Demographics of Students=====
                
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),
                                hr(style="color:#808080"),
                                helpText( strong("Demographics of Students" , style="color: #6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
                #### display total Undergraduates Seeking Degrees
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                    fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                             textOutput("ugds1")),
                                                    fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                             textOutput("ugds2")))
                ),
                br(),
                br(),
                
                # === pie chart of ethnicity
                fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Ethnicity",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("demographics1",height="550"),
                                     plotlyOutput("demographics2",height="550"))
                         
                ),br(),
                fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Gender",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("female1",height="450"),
                                     plotlyOutput("female2",height="450")
                                     
                         ))
                
                )
    ),

#Recommendation
    tabPanel(
      title="Recommendation System",icon=icon("thumbs-up"),
     
       fluidRow(
                column(width=2,style="padding:0px;",
                       
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
                          
                      h3("Select Filters",style="color:black;font-family:Franklin Gothic Medium;"),
             
                      sliderInput("satscore",label=tags$b("SAT Score",style="color:black;font-family:Franklin Gothic Medium;"),min=400, max=1600, value=1000,step=10),
                      br(),
              
                      selectInput("city",label=tags$b("Located in city/rural",style="color:black;font-family:Franklin Gothic Medium;"),choices=c("City","Not City")),
                      br(),
              
                      sliderInput("crime",label=tags$b("Crime Rate (Per 100000 people)",style="color:black;font-family:Franklin Gothic Medium;"),min=20,max=1300,value=600),
                      br(),
                 
                      sliderInput("tuition", label=tags$b("Tuition (Per Year)",style="color:black;font-family:Franklin Gothic Medium;"),min=10000,max=60000,value=30000)),
      
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
                               
                      h3("Instruction",style="color:black;font-family:Franklin Gothic Medium;"),
                      
                      h6("This is a system designed for finding your perfect school, choose your SAT score, your preference of the location of university, the maximum crime rate(per 100000 people) and tuition(per year) you can accept, and your preference of major. Then hit the search SCHOOL button to get matched school in the summary table.", style="color:grey;font-family:Franklin Gothic Medium;"))),
              
               column(width=2, style="padding:0px;",
                     
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
             
                      checkboxGroupInput("Major",label=tags$b("Major",style="color:black;font-family:Franklin Gothic Medium;"),choices=choicelist,selected=1),
              
                      actionButton("getschool",label="Search SCHOOL")
              
    )),
               column(width=5,titlePanel(tags$b("Summary Table",style="color:white;font-family:Franklin Gothic Medium;")),
                      
                      wellPanel(dataTableOutput("uni"),style="opacity:0.8;font-family;Montserrat;")),
   
               column(width=3,div(style="height:50px"),
          
                      plotlyOutput("radarplot"))
                 
      )
    
    )
    
  ,div(class="footer", "Applied Data Science Fall18 Group 2")
    )
    
      
  
  


  
  
  
  

