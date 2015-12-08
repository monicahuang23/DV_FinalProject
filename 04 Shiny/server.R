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
})