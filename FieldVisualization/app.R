library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)
library(colormap)

options(shiny.maxRequestSize = 30*1024^2) #upload size = 30MB

vis_patterns <- c('series', '_MEAN', '_MAX', '_Q50')
colors <- c(colormap()[1], colormap()[36], colormap()[72]) #the lowest, middle and highest colors of the viridis colormap

plot_data_column <- function (data, column, midpoint=0.5, limits=c(0,1)){
  p <- ggplot(data=data, aes_string(x='X', y='Y', fill=column)) + geom_tile() + ggtitle(column) + scale_y_reverse() + scale_fill_gradient2(midpoint=midpoint, limits=limits, low=colors[1], mid=colors[2], high=colors[3], na.value='white')
  return(p)
}

ui <- shinyUI(fluidPage(
  titlePanel("Field visualization"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Input file',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 selectInput('selPar', 'Parameter:', ""),
                 selectInput('selTime', 'Timepoint:', "", selected = ""),
                 tags$br()

               ),
               mainPanel(
                 plotOutput("coolplot")
               )
             )
    )
  )

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, sep = '\t')

    labs <- names(df)[grep(paste(vis_patterns, collapse="|"), names(df))]
    updateSelectInput(session, inputId = 'selPar', label = 'Parameter',
                      choices = labs, selected = labs[1])
    tps <- levels(as.factor(df$time))
    updateSelectInput(session, inputId = 'selTime', label = 'Time',
                      choices = tps, selected = tps[1])
    
    return(df)
  })
  

   output$coolplot <- renderPlot({
     req(input$file1)
     ss <- subset(data(), as.character(time)==input$selTime)
     #ggplot(ss, aes_string(input$selPar)) + geom_histogram()
     plot_data_column(ss, input$selPar, midpoint=mean(data()[,input$selPar], na.rm=T),
                      limits = c(min(data()[,input$selPar], na.rm=T), 
                                 max(data()[,input$selPar], na.rm=T)))
     
    
  })
  
  
  
  
})

shinyApp(ui, server)