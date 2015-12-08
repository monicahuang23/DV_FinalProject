require("jsonlite")
require("RCurl")
require('dplyr')
require('ggplot2')

# Change the USER and PASS below to be your UTEid
final_grades <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select SCHOOLNUMBER, avg(MATH_GROWTH_GRADE) as AV_MATH_GG, avg(READ_GROWTH_GRADE) as AV_READ_GG, FINAL_PLANTYPE from final_grade where SCHOOLNUMBER is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

truancy <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select school_code,avg(attendance_rate) as avg_attendance_rate  from truancy where SCHOOL_CODE is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

# Changes column names to join by
names(final_grades)[names(final_grades)=="SCHOOLNUMBER"] <- "SCHOOL_CODE"

#Convert columns to the correct type
final_names <- c("SCHOOLNAME","DISTRICTNAME","EMH","EMH_COMBINED","INITIAL_PLANTYPE","FINAL_PLANTYPE","NOTES")
final_nums <- setdiff(names(final_grades),final_names)
final_grades[final_nums] <- lapply(final_grades[final_nums], function(x) as.numeric(as.character(x)))
final_grades[final_names] <- lapply(final_grades[final_names], function(x) as.character(x))

truancy <- c("ATTENDANCE_RATE", "SCHOOL_NAME")
#enrl_working[enrl_names] <- lapply(enrl_working[enrl_names], function(x) as.character(x))

# join by school code
x <- final_grades %>% inner_join(., truancy, by="SCHOOL_CODE", copy=TRUE) #%>% filter(EMH != NULL) 
x <- x %>% select(EMH, FINAL_PLANTYPE, AV_MATH_GG, AV_READ_GG, AVG_ATTENDANCE_RATE)

View (x)

KPI_bad_attendance =  10    
KPI_average_attendance = 25 

require(extrafont)
p <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Effect of Attendance on Growth Grade') +
  labs(x="EMH", y=paste("Final Plantype")) +
  layer(data= x, 
        mapping=aes(x=as.character(EMH), y=FINAL_PLANTYPE, label=AV_MATH_GG),
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(), 
        position=position_identity(),
  ) +
  layer(data=x,
        mapping=aes(as.character(EMH), y=FINAL_PLANTYPE, label= AV_READ_GG),
        stat="identity",
        stat_params=list(),
        geom="text",
        geom_params=list(),
        position=position_identity(),
  ) +
  layer(data=x,
        mapping=aes(as.character(EMH), y=FINAL_PLANTYPE, fill=KPI),
        stat="identity", 
        geom="tile",
        geom_params=list(alpha=0.50),
        position=position_identity(),
  )
  #theme(axis.text.x = element_text(size  = 8, angle = 20, hjust = 1, vjust = 1))

print(p) 
