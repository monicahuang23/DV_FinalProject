# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {
        
      KPI_Low_Max_value <- reactive({input$KPI1})     
      KPI_Medium_Max_value <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
    
      df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            """select Average Reading Growth Grade || \\\' \\\' || \\\'Average Math Growth Grade\\\' as measure_names, avg(MATH_GROWTH_GRADE) || \\\' \\\' || \\\'avg(READ_GROWTH_GRADE)\\\' as measure_values, FINAL_PLANTYPE from FINAL_GRADES
            where EMH is not "A"
            group by FINAL_PLANTYPE
            union all
            select avg(ATTENDANCE_RATE) as AV_ATTENDANCE_RATE from TRUANCY
            order by 1;"""
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', 
            MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot3 <- renderPlot(height=1000, width=2000, {
            plot3 <- ggplot() + 
              coord_cartesian() + 
              scale_x_discrete() +
              scale_y_continuous() +
              #facet_wrap(~CLARITY, ncol=1) +
              labs(title='Blending 2 Data Sources') +
              labs(x=paste("Region Sales"), y=paste("Sum of Sales")) +
              layer(data=df3(), 
                    mapping=aes(x=EMH, y=FINAL_PLANTYPE), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="text",
                    geom_params=list(colour=KPI), 
                    position=position_identity()
              #) + coord_flip() +
              )+
              layer(data=df3(), 
                    mapping=aes(x=EMH, y=MEASURE_NAMES, label=(MEASURE_VALUES)), 
                    stat="identity", 
                    stat_params=list(), 
                    geom="text",
                    #geom_params=list(colour="", hjust=-0.5), 
                    position=position_identity()
              )
              plot3
      })
})

