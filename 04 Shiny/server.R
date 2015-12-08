# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {

# start of plot 2      
      KPI_bad_attendance <- reactive({input$KPI1})     
      KPI_average_attendance <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
      
      df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select SCHOOL_LEVEL, district_name, kpi,
            case                                                                                   when kpi < "p1" then \\\'Bad Attendance\\\'
            when kpi < "p2" then \\\'Average Attendance\\\'
            else \\\'Great Attendance\\\'
            end kpi                                                                                from (select school_level, district_name, avg(attendance_rate) as kpi
            from truancy 
            where DISTRICT_CODE <= 10
            group by SCHOOL_LEVEL, DISTRICT_NAME);"
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', 
                 MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_bad_attendance(), p2=KPI_average_attendance()), verbose = TRUE)))
      })
      
      output$distPlot2 <- renderPlot({             
            plot2 <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_discrete() +
                  scale_y_discrete() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("School Level"), y=paste("District Name")) +
                  layer(data=df2(), 
                        mapping=aes(x=SCHOOL_LEVEL, y=DISTRICT_NAME, label=round(KPI)), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="text",
                        geom_params=list(colour="black"), 
                        position=position_identity()
                  ) +
                  layer(data=df2(), 
                        mapping=aes(x=SCHOOL_LEVEL, y=DISTRICT_NAME, fill=KPI.1), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="tile",
                        geom_params=list(alpha=rv$alpha), 
                        position=position_identity()
                  )
           plot2
      }) 
      
      observeEvent(input$clicks, {
        print(as.numeric(input$clicks))
      })

})