# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {

      observeEvent(input$clicks1, {
        if (input$scatter == "all_scatter") {
          df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select x_free_and_reduced, tot_days_possible, tot_days_attended, school_level, (tot_days_attended/tot_days_possible) as Act_Attendance_Rate from (select * from truancy
      LEFT JOIN k_12_frl
      ON truancy.SCHOOL_NAME=k_12_frl.SCHOOL_NAME
      LEFT JOIN enrl_working
      ON truancy.SCHOOL_NAME=enrl_working.SCHOOL_NAME
      LEFT JOIN final_grade
      ON truancy.SCHOOL_NAME=final_grade.SCHOOLNAME)
    where tot_days_possible > tot_days_attended
    and x_free_and_reduced is not null"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))) 
        }

       
       if (input$scatter == "elem") {
        df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select x_free_and_reduced, tot_days_possible, tot_days_attended, school_level, (tot_days_attended/tot_days_possible) as Act_Attendance_Rate from (select * from truancy LEFT JOIN k_12_frl ON truancy.SCHOOL_NAME=k_12_frl.SCHOOL_NAME LEFT JOIN enrl_working ON truancy.SCHOOL_NAME=enrl_working.SCHOOL_NAME LEFT JOIN final_grade ON truancy.SCHOOL_NAME=final_grade.SCHOOLNAME) where tot_days_possible > tot_days_attended and x_free_and_reduced is not null and school_level = \\\'Elementary\\\' "')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
       }
       if (input$scatter == "mid") {
        df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select x_free_and_reduced, tot_days_possible, tot_days_attended, school_level, (tot_days_attended/tot_days_possible) as Act_Attendance_Rate from (select * from truancy LEFT JOIN k_12_frl ON truancy.SCHOOL_NAME=k_12_frl.SCHOOL_NAME LEFT JOIN enrl_working ON truancy.SCHOOL_NAME=enrl_working.SCHOOL_NAME LEFT JOIN final_grade ON truancy.SCHOOL_NAME=final_grade.SCHOOLNAME) where tot_days_possible > tot_days_attended and x_free_and_reduced is not null and school_level = \\\'Middle/Jr. High\\\' "')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
       }
       if (input$scatter == "high") {
        df1 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select x_free_and_reduced, tot_days_possible, tot_days_attended, school_level, (tot_days_attended/tot_days_possible) as Act_Attendance_Rate from (select * from truancy LEFT JOIN k_12_frl ON truancy.SCHOOL_NAME=k_12_frl.SCHOOL_NAME LEFT JOIN enrl_working ON truancy.SCHOOL_NAME=enrl_working.SCHOOL_NAME LEFT JOIN final_grade ON truancy.SCHOOL_NAME=final_grade.SCHOOLNAME) where tot_days_possible > tot_days_attended and x_free_and_reduced is not null and school_level = \\\'Senior\\\' "')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))) 
       }
      output$distPlot1 <- renderPlot({             
        plot1 <- ggplot() + 
          coord_cartesian() + 
          scale_x_continuous() +
          scale_y_continuous() +
          labs(title=isolate(input$title)) +
          labs(x="Attendance Rate", y=paste("Free and Reduced Meals")) +
          layer(data=df1(), 
                mapping=aes(x=ACT_ATTENDANCE_RATE, y=X_FREE_AND_REDUCED, color = SCHOOL_LEVEL),
                stat="identity", 
                stat_params=list(), 
                geom="point",
                position=position_jitter(width=0.1, height=0)
          )  
        plot1
        })
      })
      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })
  
      # start of plot 3
      df3 <- eventReactive(input$clicks3, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="SELECT final_plantype, avg(overall_weighted_growth_grade) as avg_weighted_gg, avg(rank_tot) as avg_rank_tot FROM final_grade WHERE overall_weighted_growth_grade is not null AND rank_tot is not null and final_plantype is not null group by final_plantype order by final_plantype;"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })
      
      output$distPlot3 <- renderPlot({
        plot3 <- ggplot() + 
          coord_cartesian() + 
          scale_x_discrete() +
          scale_y_continuous() +
          labs(title='Overall Weighted Growth Average for Final Plantype') +
          labs(x="Final Plantype", y=paste("Avg Overall Weighted Growth Grade")) +
          layer(data=df3(), 
                mapping=aes(x=as.character(FINAL_PLANTYPE), y=AVG_WEIGHTED_GG, fill=AVG_RANK_TOT),
                stat="identity", 
                stat_params=list(), 
                geom="bar",
                geom_params=list(), 
                position=position_identity()
          ) +
          theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1))
        if (input$avg_read) {
          plot3 <- plot3 + geom_hline(aes(yintercept=6.62699, colour = "red")) +
            annotate("text", label = "Avg Reading Growth Grade", x = 2, 6.3, size = 4, colour = "red")
        }
        if (input$avg_math) {
          plot3 <- plot3 + geom_hline(aes(yintercept=6.71413, colour = "black")) +
            annotate("text", label = "Avg Math Growth Grade", x = 2, 7, size = 4, colour = "black")
        }
        if (input$avg_write) {
          plot3 <- plot3 + geom_hline(aes(yintercept=6.69606, colour = "blue")) +
            annotate("text", label = "Avg Writing Growth Grade", x = 5, 7, size = 4, colour = "blue")
        }
        plot3
      })
      
      observeEvent(input$clicks, {
        print(as.numeric(input$clicks))
      })
})