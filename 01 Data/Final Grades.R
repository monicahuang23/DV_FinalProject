require("jsonlite")
require("RCurl")
# Change the USER and PASS below to be your UTEid
final_grades <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from FINAL_GRADE where SCHOOLNUMBER is not null"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
summary(final_grades)
#subset of final_grades df where the school ranking is 1
head(subset(final_grades, RANK_TOT == 1))

enrl_working <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from ENRL_WORKING where SCHOOL_CODE is not null"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(enrl_working)
#subset of enrl_working df where the % of asian students is < 10%
head(subset(enrl_working, PCT_ASIAN < 0.10))

k_12_frl <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from K_12_FRL where SCHOOL_CODE is not null"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

summary(k_12_frl)
#subset of k12 frl df where the % of students with free/reduced lunch is > 50%
head(subset(k_12_frl, X_FREE_AND_REDUCED > 0.50))
