library(data.table) # This is the old school library I use. It used to be much faster than tidy, now, I'm not so sure
library(ggplot2) # The best package in any programming language, ever, for all time
library(binom)
library(scales)
library(sandwich)
library(lmtest)
library(cregg)



get_marginal_means <- function(question_data,
                               by_var,
                               response_var="answer"){
  
  mm_by <- cj(question_data,formula(paste0(response_var,
                                   "~political_affiliation+age+upbringing+criminal_history",
                                   "+occupation+children+health_issues+race")), 
              id = ~responseid, estimate = "mm", by = formula(paste0("~",by_var)))
  mm_by <- data.table(mm_by)
  if("service" %in% names(mm_by)){
    mm_by <- merge(mm_by,question_to_cond, on = "service")
    mm_by[, cond := paste(context,gain_loss,give_fair)]
  }
  mm_by$level <- as.character(mm_by$level)
  mm_by[ , sig := (lower < 0.5 & upper < 0.5) | (lower > 0.5 & upper  > 0.5)]
  mm_by[,feature_name := mapvalues(feature,
                                   c("political_affiliation",
                                     "age",
                                     "upbringing",
                                     "criminal_history",
                                     "occupation",
                                     "children",
                                     "health_issues",
                                     "race"),
                                   c("Political\nAffiliation",
                                     "Age",
                                     "Upbringing",
                                     "Criminal\nHistory",
                                     "Occupation",
                                     "Children",
                                     "Health\nIssues",
                                     "Race"))] 
  return(mm_by)
}
