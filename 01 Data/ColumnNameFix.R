require("jsonlite")
require("RCurl")
require(dplyr)


# Changes column names to join by
names(final_grades)[names(final_grades)=="SCHOOLNUMBER"] <- "SCHOOL_CODE"

#Convert columns to the correct type
final_names <- c("SCHOOLNAME","DISTRICTNAME","EMH","EMH_COMBINED","INITIAL_PLANTYPE","FINAL_PLANTYPE","NOTES")
final_nums <- setdiff(names(final_grades),final_names)
final_grades[final_nums] <- lapply(final_grades[final_nums], function(x) as.numeric(as.character(x)))
final_grades[final_names] <- lapply(final_grades[final_names], function(x) as.character(x))

enrl_names <- c("ORGANIZATION_NAME", "SCHOOL_NAME")
enrl_working[enrl_names] <- lapply(enrl_working[enrl_names], function(x) as.character(x))

k_names = c("DISTRICT_NAME", "SCHOOL_NAME")
k_nums = c("DISTRICT_CODE","SCHOOL_CODE")
k_12_frl[k_names] <- lapply(k_12_frl[k_names], function(x) as.character(x))
k_12_frl[k_nums] <- lapply(k_12_frl[k_nums], function(x) as.numeric(as.character(x)))


