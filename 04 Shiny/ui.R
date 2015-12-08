#ui.R 

library(shiny)

navbarPage(
  title = "DV_FinalProject",
  tabPanel(title = "Scatterplot",
     sidebarPanel(
       textInput(inputId = "title", 
                 label = "Title",
                 value = "Attendance Rate vs FRL"),
       actionButton(inputId = "clicks1",  label = "Show the plot")
     ),
     
     mainPanel(plotOutput("distPlot1")
     )
  ),
  tabPanel(title = "Crosstab",
           sidebarPanel(
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark"),
             sliderInput("KPI1", "KPI_bad_attendance:", 
                         min = 1, max = 20,  value = 20),
             sliderInput("KPI2", "KPI_average_attendance:", 
                         min = 10, max = 32,  value = 32),
             textInput(inputId = "title", 
                       label = "CrossTab Title",
                       value = "CrossTab"),
             
             actionButton(inputId = "clicks2",  label = "Show the plot")
           ),
           
           mainPanel(plotOutput("distPlot2")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             checkboxInput(inputId = "avg_read",
                           label = strong("Show average reading growth grades"),
                           value = FALSE),
             
             checkboxInput(inputId = "avg_math",
                           label = strong("Show average math growth grades"),
                           value = FALSE),
             
             checkboxInput(inputId = "avg_write",
                           label = strong("Show average writing growth grades"),
                           value = FALSE),
             
             actionButton(inputId = "clicks3",  label = "Show the plot")
           ),
           
           mainPanel(plotOutput("distPlot3")
           )      
  )  
)