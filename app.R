#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

custom_name_repair <- function(nms){
  (sub("_Sec$", "", nms))
}

plotter <- function(data, patientID, var1){
  # assign temporary data frame to the current patient ID
  temp_df <- data[data$PatientID == patientID, c('Start_Date', var1)]
  names(temp_df) <- c('Start_Date', 'var1')
  # is.na(temp_df$var1) <- 0
  temp_df$var1 <- as.numeric(temp_df$var1)
  temp_df$Start_Date <- as.Date(temp_df$Start_Date, format = "%m/%d/%Y")
  #colnames(temp_df) <- sub("_Sec$", "", colnames(temp_df))
  if (var1 %in% c('SQI','Sleep_Efficiency','Sleep_Latency',
                  'Sleep_Duration','SAI','sAHI','Fragmentation_eLFCbb',
                  'Periodicity_eLFCnb','Stable_Duration','Stable_Percent',
                  'Unstable_Duration','Unstable_Percent',
                  'CVHR_Stable_Percent','CVHR_Unstable_Percent','TST',
                  'WASO','Mean_SpO2','sAHI_3%','sAHI_4%',
                  'sAHI_Obstructive_3%','sAHI_Obstructive_4%',  
                  'sAHI_Central_3%','sAHI_Central_4%','ODI_3%','ODI_4%',
                  'SpO2_Below90_Percent','SpO2_Below88_Percent',
                  'SpO2_Below80_Percent')){
    if (var1 == 'SQI' | var1 == 'Sleep_Efficiency' | var1 == 'SAI' |
        var1 == 'Fragmentation_eLFCbb' | var1 == 'Periodicity_eLFCnb' |
        var1 == 'Stable_Percent' | var1 == 'Unstable_Percent' |
        var1 == 'CVHR_Stable_Percent' | var1 == 'CVHR_Unstable_Percent' |
        var1 == 'Mean_SpO2' | var1 == 'SpO2_Below90_Percent'|
        var1 == 'SpO2_Below88_Percent' | var1 == 'SpO2_Below80_Percent'){
      if (var1 == 'Mean_SpO2'){
        if (min(var1) < 70){
          ymin <- min(temp_df$var1)
        } else {
          ymin <- 70
        }
      } else {
        ymin <- 0
      }
      ymax <- 100
    } else if (var1 == 'sAHI' | var1 == 'sAHI_3%' | var1 == 'sAHI_4%' |
               var1 == 'sAHI_Obstructive_3%' | var1 == 'sAHI_Obstructive_4%' |
               var1 == 'sAHI_Central_3%' | var1 == 'sAHI_Central_4%' |
               var1 == 'ODI_3%' | var1 == 'ODI_4%'){
      if (min(var1) < 10){
        ymin <- min(temp_df$var1)
      } else {
        ymin <- 10
      }
      ymax <- max(temp_df$var1)
    } else if (var1 == 'Sleep_Latency' | var1 == 'Sleep_Duration' |
               var1 == 'Stable_Duration' | var1 == 'Unstable_Duration' |
               var1 == 'TST' | var1 == 'WASO'){
      if (!is.na(temp_df$var1)){
        temp_df$var1 <- (temp_df$var1/3600)
      } else {
        temp_df$var1
      }
      ymin <- 0
      if (var1 == 'Sleep_Duration'){
        ymax <- 9
      } else {
        ymax <- max(temp_df$var1)
      }
    }
    g1 <- ggplot(data = temp_df, 
                 aes(x = Start_Date)) +
      
      geom_line(aes(y=var1), color = 'red') + 
      geom_point(aes(y=var1), color = 'black') +
      
      scale_x_date(limit=c((as.Date(Sys.Date(),format="%m/%d/%Y")-30),
                           (as.Date(Sys.Date(),format="%m/%d/%Y"))),
                   breaks = function(x) seq.Date(from = min(x), 
                                                 to = max(x), 
                                                 by = "3 days"),
                   minor_breaks = function(x) seq.Date(from = min(x), 
                                                       to = max(x), 
                                                       by = "1 days"),
                   date_labels = "%m/%d") +
                   # breaks = "3 days", minor_breaks = "1 day",  +
      
      ylim(ymin, ymax) +
      
      theme(axis.line = element_line(colour = "black"), # Set axis line as black
            # panel.grid.major = element_blank(), # Remove grid
            # panel.grid.minor = element_blank(), # Remove grid
            # panel.border = element_blank(), # Remove grid
            # panel.background = element_blank(), # Remove grid
            plot.subtitle = element_text(face = "italic"), # Make subtitle italic
            # axis.title.y = element_text(color = 'red', size=13),
            plot.title = element_text(face = 'bold'),
            axis.text.x=element_text(angle=90, hjust=1)) +
      labs(title = paste('Patient #',patientID,': ',var1,' over Time', sep = ''),
           x = 'Date', y = var1)
  } else {
    g1 <- ggplot(data = temp_df, 
                 aes(x = Start_Date)) +
      
      geom_line(aes(y=var1), color = 'red') + 
      geom_point(aes(y=var1), color = 'black') +
      
      scale_x_date(limit=c((as.Date(Sys.Date(),format="%m/%d/%Y")-30),
                           (as.Date(Sys.Date(),format="%m/%d/%Y"))),
                   breaks = function(x) seq.Date(from = min(x), 
                                                 to = max(x), 
                                                 by = "3 days"),
                   minor_breaks = function(x) seq.Date(from = min(x), 
                                                       to = max(x), 
                                                       by = "1 days"),
                   date_labels = "%m/%d") +
      # breaks = "3 days", minor_breaks = "1 day",  +
      
      ylim(ymin, ymax) +
      
      theme(axis.line = element_line(colour = "black"), # Set axis line as black
            # panel.grid.major = element_blank(), # Remove grid
            # panel.grid.minor = element_blank(), # Remove grid
            # panel.border = element_blank(), # Remove grid
            # panel.background = element_blank(), # Remove grid
            plot.subtitle = element_text(face = "italic"), # Make subtitle italic
            # axis.title.y = element_text(color = 'red', size=13),
            plot.title = element_text(face = 'bold'),
            axis.text.x=element_text(angle=90, hjust=1)) +
      labs(title = paste('Patient #',patientID,': ',var1,' over Time', sep = ''),
           x = 'Date', y = var1)
  }
  
  return(g1)
}
  
feature_list <- c("SQI","Sleep_Efficiency","Sleep_Latency",
                  "Sleep_Duration","SAI","sAHI","Fragmentation_eLFCbb",
                  "Periodicity_eLFCnb","Stable_Duration","Stable_Percent",
                  "Unstable_Duration","Unstable_Percent","REM_Duration",
                  "REM_Percent","CVHR_Stable_Percent","CVHR_Unstable_Percent",
                  "CVHR_REM_Percent","Apnea_Stable_Index",
                  "Apnea_Unstable_Index","Apnea_REM_Index","TST","WASO",
                  "Wake_Transitions","SpO2_Below90_Duration",
                  "SpO2_Below90_Percent","SpO2_Below88_Duration",
                  "SpO2_Below88_Percent","SpO2_Below80_Duration",
                  "SpO2_Below80_Percent","Min_SpO2","Max_SpO2","Mean_SpO2",
                  "sAHI_3%","sAHI_4%","sAHI_Obstructive_3%",
                  "sAHI_Obstructive_4%","sAHI_Central_3%","sAHI_Central_4%",
                  "ODI_3%","ODI_4%","Min_Apnea_Duration",
                  "Max_Apnea_Duration","Mean_Apnea_Duration",
                  "Min_Heart_Rate_BPM","Max_Heart_Rate_BPM",       
                  "Mean_Heart_Rate_BPM")

ui <- fluidPage(
  titlePanel('Sleep Patient Graph Tool Ver 1.3'),
  sidebarLayout(
    sidebarPanel(
      helpText("Visualizing Patient Sleep Data"),
      fileInput(inputId = 'file', buttonLabel = 'Select file',
                label = 'Upload File'),
      selectInput(inputId = 'patientID', label = h5('Select Patient'),
                  choices = 'No Choices Here Yet!'),
      selectInput(inputId = 'yvar1', label = h5('1st Response Variable'),
                  choices = feature_list, selected = 'SQI'),
      selectInput(inputId = 'yvar2', label = h5('2nd Response Variable'),
                  choices = feature_list, selected = 'sAHI'),
      fluidRow(
        column(5,
               actionButton('Submit', 'Submit')),
        column(6,
               downloadButton('DownloadAll','Download All'))
      )),
    mainPanel(
      plotOutput(outputId = 'plot1')
    )
  )
)

#report_path <- tempfile(fileext = '.pdf')
#file.copy("report.pdf", report_path, overwrite = TRUE)

server <- function(input, output, session){
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read_xlsx(inFile()$datapath, .name_repair = custom_name_repair)
    }
  })
  
  observe({
    opts <- colnames(myData()[10:58])
    updateSelectInput(
      session,
      "patientID",
      choices = (myData()$PatientID))
    updateSelectInput(
      session,
      "yvar1",
      choices = opts)
    updateSelectInput(
      session,
      "yvar2",
      choices = opts)
    })
  
  temp_df <- eventReactive(input$Submit,{
    filtered_df <- myData()[myData()$PatientID == input$patientID,
                            c('Start_Date',input$yvar1, input$yvar2)]
    names(filtered_df) <- c('Start_Date', 'var1', 'var2')
    filtered_df$Start_Date <- as.Date(filtered_df$Start_Date)
    #colnames(filtered_df) <- sub("_Sec$", "", colnames(filtered_df))
    if (input$yvar1 == 'Sleep_Latency' | input$yvar1 == 'Sleep_Duration' |
        input$yvar1 == 'Stable_Duration' | input$yvar1 == 'Unstable_Duration' |
        input$yvar1 == 'TST' | input$yvar1 == 'WASO' |
        input$yvar2 == 'Sleep_Latency' | input$yvar2 == 'Sleep_Duration' |
        input$yvar2 == 'Stable_Duration' | input$yvar2 == 'Unstable_Duration' |
        input$yvar2 == 'TST' | input$yvar2 == 'WASO'){
      if (!is.na(filtered_df$var1) | !is.na(filtered_df$var2)){
        filtered_df$var1 <- (filtered_df$var1/3600)
        filtered_df$var2 <- (filtered_df$var2/3600)
      } else {
        filtered_df$var1
        filtered_df$var2
      }}
    filtered_df
  })
  
  output$plot1 <- renderPlot({
    
    coeff <- reactive({
      max(temp_df()$var2, na.rm = TRUE)/max(temp_df()$var1, na.rm = TRUE)
      })
    
    viz1 <- ggplot(data = temp_df(), aes(x = temp_df()$Start_Date)) +
      
      geom_line(aes(y=temp_df()$var1), color = 'red') + 
      geom_line(aes(y=temp_df()$var2/coeff()), color = 'blue') +
      
      scale_x_date(breaks = "3 days") +
    
      # Divide by 10 to get the same range than the temperature
      
      scale_y_continuous(
        
        # Features of the first axis
        name = paste(input$yvar1),
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff(), name = paste(input$yvar2))) + 
      theme(axis.line = element_line(colour = "black"), # Set axis line as black
            panel.grid.major = element_blank(), # Remove grid
            panel.grid.minor = element_blank(), # Remove grid
            panel.border = element_blank(), # Remove grid
            panel.background = element_blank(), # Remove grid
            plot.subtitle = element_text(face = "italic"), #Make subtitle italic
            axis.title.y = element_text(color = 'red', size=13),
            axis.title.y.right = element_text(color = 'blue', size=13),
            plot.title = element_text(face = 'bold'),
            axis.text.x = element_text(face = 'bold'),
            axis.text.y = element_text(face = 'bold')) +
      labs(title = paste('Patient #',input$patientID,': ',
                         input$yvar1,' vs ',input$yvar2, sep = ''),
           x = 'Date')
    
    return(viz1)
    
    })
  
  output$DownloadAll <- downloadHandler(
    filename = function(){
      # dir.create(paste0('./Results/',
      #                    format(Sys.Date(), format="%m-%d-%Y")))
      tempdf2 <- myData()[myData()$PatientID == input$patientID,]
      # paste0('./Results/',format(Sys.Date(), format="%m-%d-%Y"),'/',
      #        tempdf2$Last_Name[1],', ',tempdf2$First_Name[1],'_',
      #        format(Sys.Date(), format="%m-%d-%Y"),".pdf")
      paste0(input$patientID,'_',
             format(Sys.Date(), format = "%m-%d-%Y"),'.pdf')
    },
    content = function(file){
      tempdf2 <- myData()[myData()$PatientID == input$patientID,]
      #colnames(tempdf2) <- sub("_Sec$", "", colnames(tempdf2))
      relevant_fields <- c('SQI','Sleep_Efficiency','Sleep_Latency',
                        'Sleep_Duration','SAI','sAHI','Fragmentation_eLFCbb',
                        'Periodicity_eLFCnb','Stable_Duration','Stable_Percent',
                        'Unstable_Duration','Unstable_Percent',
                        'CVHR_Stable_Percent','CVHR_Unstable_Percent','TST',
                        'WASO','Mean_SpO2','sAHI_3%','sAHI_4%',
                        'sAHI_Obstructive_3%','sAHI_Obstructive_4%',  
                        'sAHI_Central_3%','sAHI_Central_4%','ODI_3%','ODI_4%',
                        'SpO2_Below90_Percent','SpO2_Below88_Percent',
                        'SpO2_Below80_Percent')
      
      plot_list <- list()
      
      for (value in relevant_fields){
        p <- plotter(tempdf2, input$patientID, value)
        plot_list[[value]] <- p
      }
      
      pdf(paste0(# './Results/',format(Sys.Date(), format="%m-%d-%Y"),'/',
                 input$patientID,
                 '_', format(Sys.Date(), format="%m-%d-%Y"), '.pdf'))
      
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n',
           xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste(
        "Visualization Tool Ver 1.3"), 
           cex = 1.6, col = "black")
      
      par(mar = c(5, 4, 4, 2) + 0.1)
      
      for (value in relevant_fields){
        print(plot_list[[value]])
      }
      dev.off()
      
      file.copy(paste0(input$patientID,
                       '_', format(Sys.Date(), format="%m-%d-%Y"), '.pdf'),
                file)
    }
  )  
}

# Run the application 
shinyApp(ui = ui, server = server)

