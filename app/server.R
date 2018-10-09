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

#Read in Files

college =  read.csv(file="college_top.csv")
college1 =  read.csv("college_top1.csv")
crime_rate=read.csv("crime_rate.csv")

college1$PCT25_EARN_WNE_P6=college1$PCT25_EARN_WNE_P6%>%as.character()
college1$PCT75_EARN_WNE_P6=college1$PCT75_EARN_WNE_P6%>%as.character()
college1$PCT25_EARN_WNE_P8=college1$PCT25_EARN_WNE_P8%>%as.character()
college1$PCT75_EARN_WNE_P8=college1$PCT75_EARN_WNE_P8%>%as.character()
college1$PCT25_EARN_WNE_P10=college1$PCT25_EARN_WNE_P10%>%as.character()
college1$PCT75_EARN_WNE_P10=college1$PCT75_EARN_WNE_P10%>%as.character()

#college.filtered = read.csv("data/school.select.csv")
#college =  read.csv("data/College2014_15_new.csv")

schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)

#Support data frames
major = c("Agriculture","Natural Resources And Conservation", "Architecture And Related Services",
          "Computer And Information Sciences And Support Services"," Education","Engineering"," Biological And Biomedical Sciences",
          "Mathematics And Statistics", "Psychology","Social Sciences","Business, Management, Marketing, And Related Support Services","History")
major.index =colnames(schooldata)[16:27]
major.frame = data.frame(major = major, index = major.index)


#Convert City types into 1 and o (1 for city and 0 for not city)
school1 <- schooldata %>% mutate(city_nocity=ifelse(schooldata$Citytype=='City',1,0))%>%
  mutate(c_nc=ifelse(city_nocity==1,"City","Not City"))

#Prepare font list for radar plot
font1 <- list(family="Montserrat",size=16,color="white")

server <- function(input, output,session){
##Map start here
  Major<-reactive({
    Major<-unlist(input$filter)
  })
  
  SAT<-reactive({
    SAT<-input$SAT
  })
  
  Citytype<-reactive({
    Citytype<-input$Citytype
  })
  
  CrimeRate<-reactive({
    CrimeRate<-input$CrimeRate
  })
  
  HappyScore<-reactive({
    HappyScore<-input$HappyScore
  })
  
  v1<-reactive({
    
    if (Major() == "") {
      v1<-schooldata
    } 
    else {
      v1 <- schooldata[(schooldata %>% select(Major()))>=1,]
    }
    
  })
  
  
  v2<- reactive({
    V2 <- filter(v1(),
                 as.numeric(SAT) >= SAT()[1] &
                   as.numeric(SAT) <= SAT()[2])
  })  
  
  
  v3<- reactive({
    if (Citytype() == "None") {
      v3<- v2()} 
    else {
      v3<- filter(v2(), v2()$Citytype==Citytype()) 
    }}) 
  
  
  v4<- reactive({
    v4 <- filter(v3(),
                 as.numeric(CrimeRate) >= CrimeRate()[1] &
                   as.numeric(CrimeRate) <= CrimeRate()[2]) 
  }) 
  
  v5<- reactive({
    v5 <- filter(v4(),
                 as.numeric(HappyScore) >= HappyScore()[1] &
                   as.numeric(HappyScore) <= HappyScore()[2]) 
  })
  
  
  
  
  
  
  
  output$my_map <- renderLeaflet({
    
    
    
    #load icon
    icon.ion <- makeAwesomeIcon(icon = "users", markerColor = "green",
                                library = "ion")
    
    #
    sub_data <- v5()
    
    
    
    if(nrow(sub_data) == 0){
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      
      
    }
    
    else{
      
      #data = head(arrange(airdata(),desc(frequency)), input$NumberofDestination)
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addAwesomeMarkers(lng = v5()$Longitude, lat = v5()$Latitude, icon=icon.ion, popup = paste(v5()$Name,"         ;Rank:",v5()$Rank,sep="\n"),label=v5()$URL)
      
      
      
      
      
    }
    
    #add marker for dest in the map
    # 
    
    
    
    
  })  
  
###Comparison starts here
  #### school names ####
  my_schools = reactive({output=c(input$input1,input$input2)})
  # output$instnm1 = renderText(my_schools()[1])
  # output$instnm2 = renderText(my_schools()[2])
  ###### school logo ######
  # output$logo1 = renderImage({
  #   my_schools_file <- input$input1
  #   regex_image <- ".png"
  #   filename <- normalizePath(file.path(
  #     paste("../output/www/",my_schools_file, regex_image, sep = "")))
  # 
  #   list(src=filename)
  # },deleteFile = FALSE)
  # 
  # output$logo2 = renderImage({
  #   my_schools_file <- input$input2
  #   regex_image <- ".png"
  #   filename <- normalizePath(file.path(
  #     paste("../output/www/",my_schools_file, regex_image, sep = "")))
  # 
  #   list(src=filename)
  # },deleteFile = FALSE)
  
  ############## get data used to display city
  
  MY_city_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    get.city.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get city
      city <- as.character(relevent_data$CITY)
      return(city)
    }
    output <- as.matrix(rbind(get.city.outputs(college,my_schools[1]),get.city.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$city1 = renderText(MY_city_display()[1,])
  output$city2 = renderText(MY_city_display()[2,])
  
  ############## get data used to display iclevel from college
  
  MY_iclevel_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    get.iclevel.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get city
      #get ICLEVEL 
      if(relevent_data$ICLEVEL == 1){
        iclevel <- "4-year Institution"
      }
      if(relevent_data$ICLEVEL == 2){
        iclevel <- "2-year Institution"
      }
      if(relevent_data$ICLEVEL == 3){
        iclevel <- "Less-that-2-year Institution"
      }
      return(iclevel)
    }
    output <- as.matrix(rbind(get.iclevel.outputs(college,my_schools[1]),get.iclevel.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$iclevel1= renderText(MY_iclevel_display()[1,])
  output$iclevel2= renderText(MY_iclevel_display()[2,])
  
  ############## get data used to display control
  MY_control_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.control.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get CONTROL 
      if(relevent_data$CONTROL == 1){
        control <- "Public"
      }
      if(relevent_data$CONTROL == 2){
        control <- "Private Nonprofit"
      }
      if(relevent_data$CONTROL == 3){
        control <- "Private For-Profit"
      }
      return(control)
    }
    output <- as.matrix(rbind(get.control.outputs(college,my_schools[1]),get.control.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$control1= renderText(MY_control_display()[1,])
  output$control2= renderText(MY_control_display()[2,])
  ############## get data used to display highest degree
  MY_highdeg_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.highdeg.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get HIGHDEG
      if(relevent_data$HIGHDEG == 0){
        highdeg <- "Non-Degree Granting"
      }
      if(relevent_data$HIGHDEG == 1){
        highdeg <- "Certificate Degree"
      }
      if(relevent_data$HIGHDEG == 2){
        highdeg <- "Associate Degree"
      }
      if(relevent_data$HIGHDEG == 3){
        highdeg <- "Bachelor's Degree"
      }
      if(relevent_data$HIGHDEG == 4){
        highdeg <- "Graduate Degree"
      }
      return(highdeg)
    }
    output <- as.matrix(rbind(get.highdeg.outputs(college,my_schools[1]),get.highdeg.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$highdeg1= renderText(MY_highdeg_display()[1,])
  output$highdeg2= renderText(MY_highdeg_display()[2,])
  
  ############## get data used to display crime rate
  
  MY_cr_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    get.cr.outputs <- function(data,school){
      relevent_data <- data[data$campus == school,]
      #get crime rate
      cr <- as.character(relevent_data$rate)
      return(cr)
    }
    output <- as.matrix(rbind(get.cr.outputs(crime_rate,my_schools[1]),get.cr.outputs(crime_rate,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$cr1 = renderText(paste(MY_cr_display()[1,],"%"))
  output$cr2 = renderText(paste(MY_cr_display()[2,],"%"))
  ############## get data used to display locale
  # MY_locale_display=reactive({
  #   # Get the needed reactive objects:
  #   my_schools=c(input$input1,input$input2)
  #   
  #   get.locale.outputs <- function(data,school){
  #     relevent_data <- data[data$INSTNM == school,]
  #     #get LOCALE
  #     if(relevent_data$LOCALE == 11){
  #       locale <- "City, large (population of 250,000 or more)"
  #     }
  #     if(relevent_data$LOCALE == 12){
  #       locale <- "City, midsize (population of at least 100,000 but less than 250,000)"
  #     }
  #     if(relevent_data$LOCALE == 13){
  #       locale <- "City, small (population of less than 100,000)"
  #     }
  #     if(relevent_data$LOCALE == 21){
  #       locale <- "Suburb, large (population of 250,000 or more)"
  #     }
  #     if(relevent_data$LOCALE == 22){
  #       locale <- "Suburb, midsize (population of at least 100,000 but less than 250,000)"
  #     }
  #     if(relevent_data$LOCALE == 23){
  #       locale <- "Suburb, midsize (population of less than 100,000)"
  #     }
  #     if(relevent_data$LOCALE == 31){
  #       locale <- "Town, fringe"
  #     }
  #     if(relevent_data$LOCALE == 32){
  #       locale <- "Town, distant"
  #     }
  #     if(relevent_data$LOCALE == 33){
  #       locale <- "Town, remote"
  #     }
  #     if(relevent_data$LOCALE == 41){
  #       locale <- "Rural, fringe"
  #     }
  #     if(relevent_data$LOCALE == 42){
  #       locale <- "Rural, distant"
  #     }
  #     if(relevent_data$LOCALE == 43){
  #       locale <- "Rural, remote"
  #     }
  #     return(locale)
  #   }
  #   output <- as.matrix(rbind(get.locale.outputs(college,my_schools[1]),get.locale.outputs(college,my_schools[2])))
  #   rownames(output) <- c(my_schools[1], my_schools[2])
  #   return(output)
  # })
  # 
  # output$locale1= renderText(MY_locale_display()[1,])
  # output$locale2= renderText(MY_locale_display()[2,])
  ############## get data used to display admission rate
  MY_adrate_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.adrate.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get adm_rate
      adm_rate <- relevent_data$ADM_RATE%>%as.character()%>%as.numeric()
      return(adm_rate)
    }
    output <- as.matrix(rbind(get.adrate.outputs(college,my_schools[1]),get.adrate.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$adm_rate1= renderText(MY_adrate_display()[1,])
  output$adm_rate2= renderText(MY_adrate_display()[2,])
  ############## get data used to display in-state tuition
  MY_tuitionfee_in_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.tuitionfee_in.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      tuitionfee_in <- relevent_data$TUITIONFEE_IN%>%as.character()%>%as.numeric()
      return(tuitionfee_in)
    }
    output <- as.matrix(rbind(get.tuitionfee_in.outputs(college,my_schools[1]),get.tuitionfee_in.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$tuitionfee_in1= renderText(paste0("$",MY_tuitionfee_in_display()[1,]))
  output$tuitionfee_in2= renderText(paste0("$",MY_tuitionfee_in_display()[2,]))
  # ############## get data used to display out-of-state tuition
  MY_tuitionfee_out_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.tuitionfee_out.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      tuitionfee_out <- relevent_data$TUITIONFEE_OUT%>%as.character()%>%as.numeric()
      return(tuitionfee_out)
    }
    output <- as.matrix(rbind(get.tuitionfee_out.outputs(college,my_schools[1]),get.tuitionfee_out.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$tuitionfee_out1= renderText(paste0("$",MY_tuitionfee_out_display()[1,]))
  output$tuitionfee_out2= renderText(paste0("$",MY_tuitionfee_out_display()[2,]))
  
  
  #### percent of loan 
  MY_pctfloan_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pctfloan.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      #get PCTFLOAN
      pctfloan <- relevent_data$PCTFLOAN %>% as.character()%>%as.numeric()
      pctfloan <- paste(pctfloan*100,"%",collapse="")
      return(pctfloan)
    }
    output <- as.matrix(rbind(get.pctfloan.outputs(college,my_schools[1]),get.pctfloan.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$pctfloan1= renderText(MY_pctfloan_display()[1,])
  output$pctfloan2= renderText(MY_pctfloan_display()[2,])
  
  ############## get data used to display ugds
  MY_ugds_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.ugds.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      ugds <- relevent_data$UGDS%>%as.character()%>%as.numeric()
      
      return(ugds)
    }
    output <- as.matrix(rbind(get.ugds.outputs(college,my_schools[1]),get.ugds.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$ugds1= renderText(MY_ugds_display()[1,])
  output$ugds2= renderText(MY_ugds_display()[2,])
  
  ############## get data used to display gt25k
  MY_gt25k_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.gt25k.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      gt25k <- relevent_data$GT_25K_P10%>%as.character()%>%as.numeric()
      gt25k <- paste(gt25k*100,"%",collapse="")
      return(gt25k)
    }
    output <- as.matrix(rbind(get.gt25k.outputs(college1,my_schools[1]),get.gt25k.outputs(college1,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$gt25k_out1= renderText(MY_gt25k_display()[1,])
  output$gt25k_out2= renderText(MY_gt25k_display()[2,])
  
  ####################### get data used to draw bar graph of different majors
  MY_summary_stat=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.bargraph.data <- function(data,school){
      my.text <- "PCIP[0-9][0-9]"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      if (is.na(my.data[1,1])){
        my.data <- rep(0,length(my.data))
      }
      return(my.data)
    }
    
    output <- as.matrix(rbind(get.bargraph.data(college,my_schools[1]),get.bargraph.data(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
    
  })
  
  major.names <- c("Agriculture", "Natural Resources and Conservation",
                   "Architecture", "Group (Gender, Ethnic, etc.) Studies",
                   "Communication & Journalism", "Communications Technologies",
                   "Computer & Information Sciences", "Personal & Culinary Services",
                   "Education", "Engineering","Engieering Technologies",
                   "Foreign Languages","Consumer/Human Sciences", "Legal Professions",
                   "English", "General Studies & Humanities", "Library Science", 
                   "Biological & Biomedical Sciences", "Mathematics & Statistics",
                   "Military Techologies", "Multi/Interdisciplinary Studies",
                   "Fitness Studies", "Philosophy & Religious Studies", "Theology",
                   "Physical Sciences","Science Technologies", "Psychology", 
                   "Homeland Security", "Public Admin. & Social Service", "Social Sciences",
                   "Construction Trades", "Mechanic and Repair Technologies", 
                   "Precision Production", "Transportation","Visual & Performing Arts",
                   "Health Professions","Business","History")
  
  output$my_barplot1=renderPlotly({ 
    plot_ly(
      x = major.names,
      y = MY_summary_stat()[1,],
      name = "school",
      type = "bar"
    ) %>%
      layout(title = paste("Major distribution of <br>", my_schools()[1]),
             xaxis = list(tickangle=-65),margin=list(b=230))
    
  })
  
  output$my_barplot2=renderPlotly({ 
    plot_ly(
      x = major.names,
      y = MY_summary_stat()[2,],
      name = "school",
      type = "bar"
    ) %>%
      layout(title = paste("Major distribution of <br>", my_schools()[2]),
             xaxis = list(tickangle=-65), margin=list(b=230))
    
  })
  
  ###############get data used to draw pie chart of ethnicity
  
  MY_ethnicity_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
        #colnames(my.df) <- "NA"
        return(output.df)
      } else if (!is.null(to.remove)){
        #my.df <- my.data[-to.remove]
        #colnames(my.df) <- demo.names[-to.remove]
        output.df <- my.df[-to.remove,]
        return(output.df)
      } else {return(my.df)}
      
    }
    
    #output <- as.matrix(get.pie.chart(college, my_schools[2]))
    #rownames(output) <- 2
    output <- get.pie.chart(college,my_schools[1])
    return(output)
    
  })
  #colors <- c('rgb(128,133,133)','rgb(211,94,96)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  output$demographics1 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data1(), labels = MY_ethnicity_data1()[,2],
              values = MY_ethnicity_data1()[,1], 
              type = 'pie',marker = list(colors = MY_ethnicity_data1()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 500, height = 500, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = paste("Ethnicity diversity of <br>", my_schools()[1],"<br>"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'), 
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))
      
      
    )
  
  MY_ethnicity_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
        #colnames(my.df) <- "NA"
        return(output.df)
      } else if (!is.null(to.remove)){
        #my.df <- my.data[-to.remove]
        #colnames(my.df) <- demo.names[-to.remove]
        output.df <- my.df[-to.remove,]
        return(output.df)
      } else {return(my.df)}
      
    }
    
    #output <- as.matrix(get.pie.chart(college, my_schools[2]))
    #rownames(output) <- 2
    output <- get.pie.chart(college,my_schools[2])
    return(output)
    
  })
  
  output$demographics2 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data2(), labels = MY_ethnicity_data2()[,2],
              values = MY_ethnicity_data2()[,1], 
              type = 'pie',marker = list(colors = MY_ethnicity_data2()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 500, height = 500, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = paste("Ethnicity diversity of <br>", my_schools()[2],"<br>"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'),
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))
      
    )
  
  #############get data used to draw pie chart of female students
  MY_female_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]%>%as.character()%>%as.numeric()
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"), color=c(1,2))
      }
      
      return(out.val)
    }
    output <- get.mf.data(college, my_schools[1])
    #colnames(output) <- c("1","mf")
    return(output)
    
  })
  output$female1 <- renderPlotly(
    plot_ly(MY_female_data1(), labels = ~MY_female_data1()[,2], values = ~MY_female_data1()[,1], type = 'pie',
            marker = list(colors = MY_female_data1()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F
    ) 
    %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[1]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))
  )
  MY_female_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]%>%as.character()%>%as.numeric()
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      
      #relevent_data <- data[data$INSTNM == school,]
      #female <- relevent_data$FEMALE
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"),color=c(1,2))
      }
      return(out.val)
      
    }
    output <- get.mf.data(college, my_schools[2])
    #colnames(output) <- c("2","mf")
    return(output)
    
  })
  output$female2 <- renderPlotly(
    plot_ly(MY_female_data2(), labels = MY_female_data2()[,2], values = MY_female_data2()[,1], type = 'pie',
            marker = list(colors = MY_female_data2()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F) 
    %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[2]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))
  )
  
  debt.data.1 <- reactive({
    family.income = input$fincome
    my_schools=c(input$input1,input$input2)
    
    get.debt.mdn <- function(data,school,family.income) {
      
      if (family.income <= 30000) {
        debt.mdn <- data$LO_INC_DEBT_MDN[data$INSTNM==school]%>% as.character()%>%as.numeric()
      } else if (family.income > 30000 & family.income <= 75000) {
        debt.mdn <- data$MD_INC_DEBT_MDN[data$INSTNM==school]%>% as.character()%>%as.numeric()
      } else if (family.income > 75000) {
        debt.mdn <- data$HI_INC_DEBT_MDN[data$INSTNM==school]%>% as.character()%>%as.numeric()
      }
      
      return(debt.mdn)
    }
    
    output1 = get.debt.mdn(college,my_schools()[1],family.income)
    return(output1)
  })
  
  debt.data.2 <- reactive({
    family.income = input$fincome
    my_schools=c(input$input1,input$input2)
    
    get.debt.mdn <- function(data,school,family.income) {
      
      if (family.income <= 30000) {
        debt.mdn <- data$LO_INC_DEBT_MDN[data$INSTNM==school]
      } else if (family.income > 30000 & family.income <= 75000) {
        debt.mdn <- data$MD_INC_DEBT_MDN[data$INSTNM==school]%>% as.character()%>%as.numeric()
      } else if (family.income > 75000) {
        debt.mdn <- data$HI_INC_DEBT_MDN[data$INSTNM==school]%>% as.character()%>%as.numeric()
      }
      
      return(debt.mdn)
    }
    
    output2 = get.debt.mdn(college,my_schools()[2],family.income)
    return(output2)
  })
  
  output$debt1 <- renderText(paste0("$",debt.data.1()))
  output$debt2 <- renderText(paste0("$",debt.data.2()))
  
  ### sat cat
  MY_score_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.testscore.data <- function(data,school){
      # "400-780" 
      #SAT Scores
      my.text <- "SAT[A-Z][A-Z][0-9]5"
      indices <- grepl(my.text,colnames(data))
      sat.data <- data[data$INSTNM==school,indices]
      
      #ACT Scores
      my.text <- "ACT[A-Z][A-Z][0-9]5"
      indices <- grepl(my.text,colnames(data))
      act.data <- data[data$INSTNM==school,indices]
      
      my.vector <- c(sat.data[1,1],sat.data[1,2],
                     sat.data[1,3],sat.data[1,4],
                     sat.data[1,5],sat.data[1,6],
                     act.data[1,1],act.data[1,2],
                     act.data[1,3],act.data[1,4],
                     act.data[1,5],act.data[1,6],
                     act.data[1,7],act.data[1,8])
      
      return(my.vector)
    }
    output <- get.testscore.data(college, my_schools[1])
    return(output)
    
  })
  
  MY_score_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.testscore.data <- function(data,school){
      # "400-780" 
      #SAT Scores
      my.text <- "SAT[A-Z][A-Z][0-9]5"
      indices <- grepl(my.text,colnames(data))
      sat.data <- data[data$INSTNM==school,indices]
      
      #ACT Scores
      my.text <- "ACT[A-Z][A-Z][0-9]5"
      indices <- grepl(my.text,colnames(data))
      act.data <- data[data$INSTNM==school,indices]
      
      my.vector <- c(sat.data[1,1],sat.data[1,2],
                     sat.data[1,3],sat.data[1,4],
                     sat.data[1,5],sat.data[1,6],
                     act.data[1,1],act.data[1,2],
                     act.data[1,3],act.data[1,4],
                     act.data[1,5],act.data[1,6],
                     act.data[1,7],act.data[1,8])
      
      return(my.vector)
    }
    output <- get.testscore.data(college, my_schools[2])
    return(output)
    
  })
  
  output$sat1 <- renderTable(
    # as.matrix(MY_score_data2())[1:6,],
    #rownames = c("SAT Verbal 25th","SAT Verbal 75th","SAT Math 25th","SAT Math 75th","SAT Writing 25th","SAT Writing 75th"),
    #colnames = "Score"
    
    cbind(SAT = c("Verbal" ,"Math", "Writing"),
          Score = c(paste(MY_score_data1()[1],"-",MY_score_data1()[2]),
                    paste(MY_score_data1()[2],"-", MY_score_data1()[4]),
                    paste(MY_score_data1()[5],"-",MY_score_data1()[6])))
  )
  output$sat2 <- renderTable(
    #as.matrix(MY_score_data2())[1:6,],
    #rownames = c("SAT Verbal 25th","SAT Verbal 75th","SAT Math 25th","SAT Math 75th","SAT Writing 25th","SAT Writing 75th"),
    #colnames = "Score"
    
    cbind(SAT = c("Verbal" ,"Math", "Writing"),
          Score = c(paste(MY_score_data2()[1],"-",MY_score_data2()[2]),
                    paste(MY_score_data2()[2],"-", MY_score_data2()[4]),
                    paste(MY_score_data2()[5],"-",MY_score_data2()[6])))
  )
  output$act1 <- renderTable(
    #as.matrix(MY_score_data1())[7:14,],
    #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
    #colnames = "Score"
    
    
    cbind(ACT = c("Cumulative Score" ,"English", "Math","Writing"),
          Score = c(paste(MY_score_data1()[7],"-",MY_score_data1()[8]),
                    paste(MY_score_data1()[9],"-", MY_score_data1()[10]),
                    paste(MY_score_data1()[11],"-",MY_score_data1()[12]),
                    paste(MY_score_data1()[13],"-",MY_score_data1()[14])))
  )
  output$act2 <- renderTable(
    #as.matrix(MY_score_data2())[7:14,],
    #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
    #colnames = "Score",
    cbind(ACT = c("Cumulative Score" ,"English", "Math","Writing"),
          Score = c(paste(MY_score_data2()[7],"-",MY_score_data2()[8]),
                    paste(MY_score_data2()[9],"-", MY_score_data2()[10]),
                    paste(MY_score_data2()[11],"-",MY_score_data2()[12]),
                    paste(MY_score_data2()[13],"-",MY_score_data2()[14])))
  )
  
  
  ####### Earning percentile ####### 
  
  MY_earning_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.earning.data <- function(data,school){
      
      
      relevant.data <- data[data$INSTNM==school,]
      
      my.vector <- c(relevant.data$PCT25_EARN_WNE_P6,relevant.data$PCT75_EARN_WNE_P6,
                     relevant.data$PCT25_EARN_WNE_P8,relevant.data$PCT75_EARN_WNE_P8,
                     relevant.data$PCT25_EARN_WNE_P10,relevant.data$PCT75_EARN_WNE_P10
      )
      my.vector=my.vector%>% as.numeric()
      return(my.vector)
    }
    output <- get.earning.data(college1, my_schools[1])
    return(output)
    
  })
  
  MY_earning_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.earning.data <- function(data,school){
      
      
      relevant.data <- data[data$INSTNM==school,]
      
      my.vector <- c(relevant.data$PCT25_EARN_WNE_P6,relevant.data$PCT75_EARN_WNE_P6,
                     relevant.data$PCT25_EARN_WNE_P8,relevant.data$PCT75_EARN_WNE_P8,
                     relevant.data$PCT25_EARN_WNE_P10,relevant.data$PCT75_EARN_WNE_P10
      )
      my.vector=my.vector %>% as.character() %>% as.numeric()
      return(my.vector)
    }
    output <- get.earning.data(college1,my_schools[2])
    return(output)
    
  })
  
  output$earning1 <- renderTable(
    
    
    cbind(After_Entry = c("6 yrs" ,"8 yrs", "10 yrs"),
          Income = c(paste("$",MY_earning_data1()[1],"-","$",MY_earning_data1()[2]),
                     paste("$",MY_earning_data1()[3],"-","$",MY_earning_data1()[4]),
                     paste("$",MY_earning_data1()[5],"-","$",MY_earning_data1()[6])))
  )
  output$earning2 <- renderTable(
    
    
    cbind(After_Entry = c("6 yrs" ,"8 yrs", "10 yrs"),
          Income = c(paste("$",MY_earning_data2()[1],"-","$",MY_earning_data2()[2]),
                     paste("$",MY_earning_data2()[3],"-","$",MY_earning_data2()[4]),
                     paste("$",MY_earning_data2()[5],"-","$",MY_earning_data2()[6])))
  )
  ####### end of earning percnetile ###### 
  
  
  output$school1 <- renderText(paste("(",my_schools()[1],")"))
  output$school2 <- renderText(paste("(",my_schools()[2],")"))
  output$school1.2 <- renderText(paste("(",my_schools()[1],")"))
  output$school2.2 <- renderText(paste("(",my_schools()[2],")"))
  
    
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
      theta = c("SAT","Tuition","Crime Rate"),
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
          theta = c("SAT","Tuition","Crime Rate"),
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
Major1 <- reactive({Major<-unlist(input$Major)})

d1<-reactive({if (Major1() == "") {d1<-school1} 
  else {d1 <- school1[(school1 %>% select(Major1()))>=1,]}})
d2 <- reactive({d2 <- filter(d1(), as.numeric(SAT)<= satscore1())})
d3 <- reactive({d3 <- filter(d2(), as.numeric(CrimeRate) <= crime1())})
d4 <- reactive({d4 <- filter(d3(),as.numeric(gsub('\\$|,', '', Tuition.and.fees.y)) <= tuition1())})
d5 <- reactive({ if (city1() == 1){d5 <- filter(d4(),as.numeric(city_nocity)==1)}
   else{d5 <- filter(d4(),as.numeric(city_nocity)==0)} })


observeEvent(input$getschool,{
  output$uni <- DT::renderDataTable({
  
   school_dt <-
      subset(
        d5(),
        #d5(),
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

