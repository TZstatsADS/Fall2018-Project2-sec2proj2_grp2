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



schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)

#Convert City types into 1 and o (1 for city and 0 for not city)
school1 <- schooldata %>% mutate(city_nocity=ifelse(schooldata$Citytype=='City',1,0))%>%
  mutate(c_nc=ifelse(city_nocity==1,"City","Not City"))

#Prepare font list for radar plot
font1 <- list(family="Montserrat",size=16,color="white")

server <- function(input, output){
  
  #Recommendation System Plot
legendtitle_1 <- list(yref='paper',xref="paper",y=1.05,x=1, text="City Radar Chart",showarrow=F)
legendtitle_2 <- list(yref='paper',xref="paper",y=1.05,x=1, text="Not City Radar Chart",showarrow=F)

sat_score <- reactive({input$satscore/16})
tuition <- reactive({input$tuition/500})
crime <- reactive({10000/input$crime})
 
data_radar <- reactive({c(sat_score(),tuition(),crime())})
  
  observeEvent(input$city,{
    if(input$city=="City"){
  output$radarplot <- renderPlotly({
    plot_ly(
     type="scatterpolar",
     showlegend=T,
     mode='markers+lines+text',
     line=list(color="#8D8680",width=2),
     marker=list(color="#8D8680",size=15,opacity=0.8,symbol="star",line=list(color="#8D8680",width=1)),
     
      r=data_radar(),
      theta = c("SAT score","Tuition","Crime Rate"),
      fill='toself',fillcolor='rgba(255,0,0,0.5')%>%
      layout(
        polar = list(bgcolor='rgba(0,0,0,0.5)',
          radialaxis = list(
            linewidth=2,
            linecolor="white",
            tickwidth=2,
            tickcolor="white",
            showline = T,
            gridcolor="white",
            range = c(0,100)
          )
      ))%>%
        layout(plot_bgcolor='#00000000')%>%
        layout(paper_bgcolor='#00000000')%>%
        layout(font=font1,annotations=legendtitle_1)
     
    
  })}})
  
  observeEvent(input$city,{
    if(input$city=="Not City"){
      output$radarplot <- renderPlotly({
        plot_ly(
          type="scatterpolar",
          
          mode='markers+lines',
          line=list(color="#8D8680",width=2),
          marker=list(color="#8D8680",size=15,opacity=0.8,symbol="star",line=list(color="#8D8680",width=1)),
          r=data_radar(),
          theta = c("SAT score","Tuition","Crime Rate"),
          fill='toself',fillcolor='rgba(0,0,255,0.5')%>%
          layout(
            polar = list(bgcolor='rgba(0,0,0,0.5)',
                         radialaxis = list(
                           linewidth=2,
                           linecolor="white",
                           tickwidth=2,
                           tickcolor="white",
                           showline = T,
                           gridcolor="white",
                           range = c(0,100)
                         )
            ))%>%
          layout(plot_bgcolor='#00000000')%>%
          layout(paper_bgcolor='#00000000')%>%
          layout(font=font1,annotations=legendtitle_2)
        
        
      })}})
  
###Recommendation data table####
city1 <- reactive(if(input$city=="City"){city1 <- 1}
                    else {city1 <- 0})
satscore1 <- reactive({satscore <- input$satscore})
tuition1 <- reactive({tuition <- input$tuition})
crime1 <- reactive({crime <- input$crime})

d1 <- reactive({d1 <- filter(school1, as.numeric(SAT)<= satscore1())})
d2 <- reactive({d2 <- filter(d1(), as.numeric(CrimeRate) <= crime1())})
d3 <- reactive({d3 <- filter(d2(),as.numeric(gsub('\\$|,', '', Tuition.and.fees.y)) <= tuition1())})
d4 <- reactive({ if (city1() == 1){d4 <- filter(d3(),as.numeric(city_nocity)==1)}
   else{d4 <- filter(d3(),as.numeric(city_nocity)==0)} })

observeEvent(input$getschool,{
  output$uni <- DT::renderDataTable({
  
   school_dt <-
      subset(
        d4(),
        #d4(),
        select = c(
          "Name",
          "SAT",
          "c_nc",
          "Tuition.and.fees.y",
          "CrimeRate"
          
        )
      )
    
    colnames(school_dt) <-
      c(
        "School Name",
        "SAT Score",
        "City/Rural",
        "Tuition",
        "Crime Rate (Per 100000 people)"
        
      )
    
    datatable(
      school_dt,
      rownames = F,
      selection = "single",
      options = list(order = list(list(0, 'asc'), list(1, "asc")))
    )  %>%
      formatCurrency(("Tuition"), digits = 0)
  }, server = T)
}) 

}

