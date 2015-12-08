# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {

# start of plot 1
      df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grades"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot1 <- renderPlot({             
            plot1 <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_continuous() +
                  scale_y_continuous() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("District Name"), y=paste("Free and Reduced Meals")) +
                  layer(data=df1(), 
                        mapping=aes(x=as.numeric(as.character(DISTRICTNAME)), y=as.numeric(as.character(X_FREE_AND_REDUCED)), color=FINAL_PLANTYPE),
                        stat="identity", 
                        stat_params=list(), 
                        geom="point",
                        #geom_params=list(colour="black"), 
                        position=position_jitter(width=0.3, height=0)
                  )
            plot1
      }) 

      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })
      
# start of plot 2     
      df2 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grades"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })
      
      output$distPlot2 <- renderPlot({             
        plot2 <- ggplot() + 
          coord_cartesian() + 
          scale_x_discrete() +
          scale_y_discrete() +
          labs(title='Title') +
          labs(x=paste("Initial Plantype"), y=paste("Overall Weighted Growth Grade")) +
          layer(data=df, 
                mapping=aes(x=INITIAL_PLANTYPE, y= OVERALL_WEIGHTED_GROWTH_GRADE), 
                stat="identity", 
                stat_params=list(), 
                geom="bar",
                geom_params=list(colour="blue"), 
                position=position_identity() 
          ) +        
          layer(data=df, 
                mapping=aes(x=INITIAL_PLANTYPE, y=MATH_GROWTH_GRADE, label= MATH_GROWTH_GRADE), 
                stat="identity", 
                stat_params=list(), 
                geom="point",
                geom_params=list(colour="orange"), 
                position=position_identity()
          ) 
          plot2
      }) 
      
      observeEvent(input$clicks, {
        print(as.numeric(input$clicks))
      })
      
#PLOT 3      
      
# start of plot 4      
      KPI_great <- reactive({input$KPI1})     
      KPI_average <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
      
      df4 <- eventReactive(input$clicks4, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select EMH, final_plantype, kpi,
             case                                                                                   when kpi < "p1" then \\\'Great\\\'
             when kpi < "p2" then \\\'Average\\\'
             else \\\'Ok\\\'
             end kpi                                                                                from (select EMH, final_plantype, sum(RANK_TOT) as kpi
             from FINAL_GRADE 
             where DISTRICTNAME = \\\'ADAMS 12 FIVE STAR SCHOOLS\\\'
             group by EMH, final_plantype)
             order by final_plantype;"
             ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', 
                MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_great(), p2=KPI_average()), verbose = TRUE)))
      })
      
      output$distPlot4 <- renderPlot({             
        plot4 <- ggplot() + 
          coord_cartesian() + 
          scale_x_discrete() +
          scale_y_discrete() +
          labs(title=isolate(input$title)) +
          labs(x=paste("Final Plantype"), y=paste("EMH")) +
          layer(data=df4(), 
                mapping=aes(x=EMH, y=FINAL_PLANTYPE, label=KPI), 
                stat="identity", 
                stat_params=list(), 
                geom="text",
                geom_params=list(colour="black"), 
                position=position_identity()
          ) +
          layer(data=df4(), 
                mapping=aes(x=EMH, y=FINAL_PLANTYPE, fill=KPI.1), 
                stat="identity", 
                stat_params=list(), 
                geom="tile",
                geom_params=list(alpha=rv$alpha), 
                position=position_identity()
          )
        plot4
      }) 
      
      observeEvent(input$clicks, {
        print(as.numeric(input$clicks))
      })
      
})