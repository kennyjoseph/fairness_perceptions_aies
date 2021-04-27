source("util.r")
library(plyr)
library(stringr)
library(ggpubr)
library(dominanceanalysis)
##############################################################
#### Load in data 
##############################################################


##############################################################
## Question data
question_data <- fread("data/question_info.csv") #you can replace fread with read_csv or whatever is commonly used to read in data
nrow(question_data)
head(question_data)#take a look

# ok, now we need to merge with the question conditions
question_to_cond <- fread("data/questionnum_to_condition_map.tsv")
question_data <- merge(question_data,question_to_cond, on = "condition_service_question")
nrow(question_data)

#ok finally let's rename the columns and order the factors so that they're a bit easier to work with and nicer to plot
ATTRIBUTE_COLUMNS = c("Upbringing","Age","Criminal History", "Occupation","Children","Health Issues", "Race", "Political Affiliation")
attribute_col_names <- sub(" ", "_",tolower(ATTRIBUTE_COLUMNS)) # change, e.g., "Criminal History" to "criminal_history"

# i don't know how you rename columns in tidy, but it should be something like this, just with a different function than setnames
setnames(question_data,ATTRIBUTE_COLUMNS, attribute_col_names)

# keep the non-attribute column names
nonattribute_names <- setdiff(names(question_data),attribute_col_names)

# erg, dunno what happened here ...
question_data <- question_data[!is.na(age)]
# ok! time to snoop.

# let's do some exploration of whether or not things seem to have worked first:
summary(question_data$slider) # erg. slider seems to be ordinal. Oh well. I think that's fine
table(question_data$condition_service_question) # looks like there was random-ish distribution of the service conditions. Good.
table(question_data$age)
table(question_data$upbringing)
table(question_data$criminal_history)
table(question_data$occupation)
question_to_cond$service <- factor(question_to_cond$condition_service_question)

question_data[, answer_gl := ifelse(gain_loss=="Gain", answer, 1-answer)]
question_data$age <- factor(question_data$age, levels=c("20","40","70"))
question_data$race <- relevel(factor(question_data$race), ref="White")
question_data$occupation <- relevel(factor(question_data$occupation), ref="unemployed")
question_data$political_affiliation <- relevel(factor(question_data$political_affiliation), ref="None specified")
question_data$health_issues <- relevel(factor(question_data$health_issues), ref="Generally Healthy")
question_data$upbringing <- relevel(factor(question_data$upbringing), ref="Grew up middle class")
question_data$criminal_history <- relevel(factor(question_data$criminal_history), ref="None")
question_data$children <- relevel(factor(question_data$children), ref="No kids")

question_data[, service := factor(condition_service_question)]

##############################################################
## User data
user_data <- fread("data/user_info.csv")

user_data[, party := ifelse(demog_political %in% c("Democrat","Republican"), demog_political, "Other/Independent")]
user_data[party == "Other/Independent"]$party <- with(user_data[party == "Other/Independent"], 
                                                      ifelse(demog_party_closeness == "Closer to the Democratic Party", "Democrat",
                                                        ifelse(demog_party_closeness == "Closer to the Republican Party","Republican", 
                                                               "Other/Independent")))
table(user_data$party)
user_data[, party := factor(party,c("Other/Independent","Republican","Democrat"))]

user_data[, education:= mapvalues(demog_education,
                                  c("Did not graduate from high school",
                                    "High school graduate",
                                    "Some college, but no degree",
                                    "2-year college degree",
                                    "4-year college degree",
                                    "Postgraduate degree (MA, MBA, MD, JD, PhD, etc.)"), 
                                  c(rep("HS or Less",3), "2-year college","4-year college", "Postgraduate"))]
user_data[, education := factor(education, c("HS or Less", "2-year college", "4-year college", "Postgraduate"))]

user_data[, choice_approach := mapvalues(decision_style,
              c("I compared the two persons attribute by attribute",
                "I examined all attributes associated with each person first to form my impression of him/her before comparing the two persons to make a decision"),
          c("Per attribute", "Whole Person"))]
user_data[!choice_approach %in% c("Per attribute", "Whole Person") ]$choice_approach <- "Other/Both"

write.table(user_data[!decision_style_text == ""]$decision_style_text)
table(user_data$decision_style)

setnames(user_data,"demog_age","age")

for(n in grep("importance",names(user_data),value = T)){
  user_data$x <- factor(user_data[,get(n)], levels=c("Not at all important",
                                              "Low importance",
                                              "Slightly important",
                                              "Neutral",
                                              "Moderately Important",
                                              "Very Important",
                                              "Extremely Important"))
  setnames(user_data, "x",sub("importance_","imp_",n))
}

user_data <- user_data[, c("responseid", "party", "age", "education", "choice_approach",
                           grep("imp_",names(user_data),value = T)),with=F]

user_data[, age_cat := ifelse(age < 35, "Under 35", ifelse(age < 50, "35-50", "50+"))]
user_data[, age_cat := factor(age_cat, levels=c("Under 35", "35-50","50+"))]
setnames(user_data, c("responseid", paste0("respondent_",names(user_data)[2:ncol(user_data)])))
table(user_data$respondent_age_cat)
##############################################################
## Merge user/question data
question_data <- merge(question_data, user_data, by="responseid")


question_data[, v := factor(paste(gsub(" ","_",context),severity))]
##############################################################
#### Analyses!
##############################################################
mm_by <- cj(question_data,formula(paste0("answer_gl",
                                         "~political_affiliation+age+upbringing+criminal_history",
                                         "+occupation+children+health_issues+race")), 
            id = ~responseid, estimate = "mm")
mm_by <- data.table(mm_by)
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
p <- ggplot(mm_by, 
            aes(level, estimate, ymin=lower,ymax=upper,
                alpha=ifelse(sig, 1, .3))) + 
  coord_flip() + 
  geom_hline(yintercept = .5, color='red') +
  scale_alpha(range=c(.3,1),guide='none') +
  geom_pointrange(position=position_dodge(.6),size=.8) + 
  facet_grid(feature_name~.,scales = "free_y",space="free") + 
  theme_bw(20)+
  scale_y_continuous("Marginal Prob. Individual with\nFeature is given Service",
                     labels=percent) +
  theme(panel.grid.major.y  = element_line(colour = "black",linetype='dashed',size=.2),
        panel.grid.major.x  = element_line(colour = "darkgrey",linetype='dashed',size=.2),
        strip.text.y = element_text(size = 12)) +
  xlab("Feature")
p




mm_by_service <- get_marginal_means(question_data,"v","answer_gl")
mm_by_service[, setting := mapvalues(v,
                                     c("COVID High",
                                     "COVID Low",
                                     "Social_Work High",
                                     "Social_Work Low"),
                                     c("Life-saving\ndevice\n(COVID High)",
                                       "Pain\nreduction\n(COVID Low)",
                                       "Housing\n(Social Work\nHigh)",
                                       "Tuition\nAssist\n(Social Work\nLow)")
                                     )]
p <- ggplot(mm_by_service, 
       aes(level, estimate, ymin=lower,ymax=upper,
           color=setting,
           shape=setting,
           alpha=ifelse(sig, 1, .3))) + 
  coord_flip() + theme_bw(10) + 
  scale_alpha(range=c(.2,1),guide='none') +
  geom_hline(yintercept = .5, color='black',linetype='dashed') + 
  scale_shape_discrete("Service\nSetting",
                       guide=guide_legend(keyheight = unit(2.5, "cm"))) +
  scale_color_discrete("Service\nSetting",
                       guide=guide_legend(keyheight = unit(2.5, "cm"))) +
  geom_pointrange(position=position_dodge(.6),size=.8) + 
  facet_grid(feature_name~.,scales = "free_y",space="free") + 

  theme_bw(20)+
  scale_y_continuous("Marginal Prob. Individual with\nFeature is given Service",
                     labels=percent) +
  theme(panel.grid.major.y  = element_line(colour = "black",linetype='dashed',size=.2),
        panel.grid.major.x  = element_line(colour = "darkgrey",linetype='dashed',size=.2),
        strip.text.y = element_text(size = 17)) +
  xlab("Feature")
p
ggsave("img/p1.pdf", p, h=22,w=10)


mm_by_party <- get_marginal_means(question_data,"respondent_party", "answer_gl")
mm_by_age <- get_marginal_means(question_data,"respondent_age_cat", "answer_gl")

#mm_by_resp <- rbind(mm_by_party,mm_by_age,fill=T)
#mm_by_resp[, BY := factor(BY, levels=c("Democrat","Other/Independent","Republican",
#                                       "Under 35", "35-50","50+"))]
p <- ggplot(mm_by_party[! feature_name %in% c("Age","Occupation")],
            aes(level, estimate, ymin=lower,ymax=upper,
                color=BY,
                shape=BY,
                alpha=ifelse(sig, 1, .3))) + 
  coord_flip() + theme_bw(10) + 
  scale_alpha(range=c(.3,1),guide='none') +
  geom_hline(yintercept = .5, color='black',linetype='dashed') + 
  scale_shape_discrete("Respondent\nPolitical\nAffiliation",
                       guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  scale_color_manual("Respondent\nPolitical\nAffiliation",
                     values=list("Other/Independent"="#39e355",
                                  "Republican"="red",
                                  "Democrat"="blue"),
                                # "Under 35"="purple",
                                # "35-50"="orange",
                                # "50+"="brown"),
                       guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  geom_pointrange(position=position_dodge(.6),size=.8) + 
  facet_grid(feature_name~.,scales = "free_y",space="free") + 
  theme_bw(20)+
  scale_y_continuous("Marginal Prob. Individual with\nFeature is given Service",
                     labels=percent) +
  theme(panel.grid.major.y  = element_line(colour = "black",linetype='dashed',size=.2),
        panel.grid.major.x  = element_line(colour = "darkgrey",linetype='dashed',size=.2),
        strip.text.y = element_text(size = 17)) +
  xlab("Feature")
p
ggsave("img/p2.pdf", p, h=12,w=11)

######################
### RQ 3/4
######################
res <- data.table()
p_res <- data.table()
question_data$gain_lossf <- factor(question_data$gain_loss,levels=c("Gain","Loss"))
question_data$give_fairf <- factor(question_data$give_fair,levels=c("fair","give"))
for(varm in c("gain_lossf","give_fairf")){
  for(l in c("COVID Low","COVID High","Social_Work Low", "Social_Work High")){
    k <- cj_anova(question_data[v == l], formula(paste0("answer_gl~",
                                                          "political_affiliation+age",
                                                          "+upbringing+criminal_history",
                                                          "+occupation+children+health_issues+race"))
                    , by=formula(paste0("~",varm)), id = ~ responseid)
    p_res <- rbind(p_res, data.table(varm=varm,l=l,Fval=k$F[2],
                                     df=k$Df[2],
                                     p=k$`Pr(>F)`[2]))
    if(k$`Pr(>F)`[2] < .05){
      tid <- mm_diffs(question_data[v == l], formula(paste0("answer_gl~",
                                                    "political_affiliation+age",
                                                    "+upbringing+criminal_history",
                                                    "+occupation+children+health_issues+race"))
                      , formula(paste0("~",varm)), id = ~ responseid)
      setnames(tid, varm, "diff_type")
      tid$l <- l
      res <- rbind(res,tid)
    } else{
      print(paste("NOT SIG: ", l, varm, k$`Pr(>F)`[2] ))
    }
    
  }

}

res[, setting := mapvalues(l,
                           c("COVID High",
                             "COVID Low",
                             "Social_Work High",
                             "Social_Work Low"),
                           c("Life-saving device",
                             "Pain reduction",
                             "Housing",
                             "Tuition Assist")
)]

plot_data <- res[setting == "Tuition Assist"]
plot_data[,feature_name := mapvalues(feature,
                                 c("political_affiliation",
                                   "age",
                                   "upbringing",
                                   "criminal_history",
                                   "occupation",
                                   "children",
                                   "health_issues",
                                   "race"),
                                 c("Political Affl.",
                                   "Age",
                                   "Upbringing",
                                   "Criminal History",
                                   "Occupation",
                                   "Children",
                                   "Health Issues",
                                   "Race"))] 

plot_data[, b := mapvalues(BY, c("Loss - Gain", "give - fair"), c("Reward vs\nPunishment","Fairness Cue"))]
p <- ggplot(plot_data[p < .05],
            aes(paste0(feature_name,": ", level),
                estimate*-1, ymin=lower*-1,ymax=upper*-1,
                color=b,
                shape=b)) + 
  coord_flip() + theme_bw(10) + 
  scale_alpha(range=c(.2,1),guide='none') +
  geom_pointrange(position=position_dodge(.6),size=.8) + 
  theme_bw(20)+
  scale_y_continuous("Estimated difference in margial mean",
                     labels=percent,breaks=c(-.2,0,.2)) +
  theme(panel.grid.major.y  = element_line(colour = "black",linetype='dashed',size=.1),
        panel.grid.major.x  = element_line(colour = "darkgrey",linetype='dashed',size=.2),
        strip.text.y = element_text(size = 17)) +
  scale_shape_discrete("Manipulation",
                       guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  scale_color_manual("Manipulation",
                     values=list("Reward vs\nPunishment"="orange",
                                 "Fairness Cue"="purple"),
                     guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  geom_pointrange(position=position_dodge(.6),size=.8) + 
  xlab("Attribute Level") +
  geom_hline(yintercept = 0, color='darkgrey')
p
ggsave("img/p4.pdf", p, h=4.5,w=12)

############### 
### RQ 5 ###
################
import <- melt(question_data, id=c("responseid","v","respondent_party","respondent_age_cat",
                                   "gain_loss","give_fair"), 
               measure=grep("_imp_",names(question_data)))
import[, value := factor(value, levels=c("Not at all important",
                                         "Low importance",
                                         "Slightly important",
                                         "Neutral",
                                         "Moderately Important",
                                         "Very Important",
                                         "Extremely Important"))]
import[, val_int := as.integer(value)]
import[, term := mapvalues(variable, 
                          c("respondent_imp_age",
                           "respondent_imp_race",
                           "respondent_imp_nchildren",
                           "respondent_imp_political",
                           "respondent_imp_criminal",
                           "respondent_imp_health",
                           "respondent_imp_occupation",
                           "respondent_imp_upbrinding"),
                          c("age",
                            "race",
                            "children",
                            "political_affiliation",
                            "criminal_history",
                            "health_issues",
                            "occupation",
                            "upbringing"))]

res <- data.table()
for(v_val in c("Social_Work High","Social_Work Low", "COVID High", "COVID Low")){
  for(rparty in c("Democrat","Republican")){
    print(rparty)
    dat <- question_data[respondent_party==rparty & v_val == v]
    mod <-glm(answer_gl~political_affiliation+age+upbringing+criminal_history+occupation+children+health_issues+race, 
              data=dat,
              family="binomial")
    dapres <- dominanceAnalysis(mod)
    avg_c <- averageContribution(dapres,fit.functions = "r2.m")
    anov <- data.table(term = names(avg_c$r2.m), avg_contr = avg_c$r2.m)
    anov$respondent_party <- rparty
    anov$v <- v_val
    res <- rbind(res,anov)
  }
}
mg <- merge(import[, mean(val_int), 
                   by = .(term,respondent_party,v)], 
            res, by=c("term","respondent_party","v"))
m2 <- mg[, list(V1=mean(V1),avg_cont=mean(avg_contr)), by = .(term,respondent_party)]


m2[, list(v=cor.test(V1,avg_cont,method = "spearman")$estimate,
          cor.test(V1,avg_cont,method = "spearman")$p.value), 
   by=respondent_party]


ml <- melt(m2, id=c("term","respondent_party"), measure=c("V1","avg_cont"))
ml$term <- factor(ml$term, levels=c("age","children","criminal_history","occupation","health_issues","upbringing","political_affiliation","race"))
ml[, rescl_val := (value-mean(value))/(2*sd(value)), by=.(variable)]
ml[,feature_name := mapvalues(term,
                              c("political_affiliation",
                                "age",
                                "upbringing",
                                "criminal_history",
                                "occupation",
                                "children",
                                "health_issues",
                                "race"),
                              c("Political Affl.",
                                "Age",
                                "Upbringing",
                                "Criminal History",
                                "Occupation",
                                "Children",
                                "Health Issues",
                                "Race"))] 
p <- ggplot(ml, aes(feature_name,rescl_val,color=variable,group=variable)) + 
  geom_point(size=5)  + 
  facet_wrap(~respondent_party,nrow=2) + theme_bw(20) + 
  geom_line()  + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  geom_hline(yintercept = (4-mean(ml[variable == "V1"]$value))/(2*sd(ml[variable == "V1"]$value)), 
             linetype="dashed",color='purple') +
  scale_shape_discrete("Importance Measure",labels=c("Likert","Conjoint\nAnalysis"),
                       guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  scale_color_manual("Importance Measure",
                     labels=c("Likert","Conjoint\nAnalysis"),
                     values=c("purple","orange"),
                     guide=guide_legend(keyheight = unit(1.5, "cm"))) +
  theme(axis.text.y = element_blank()) + ylab("") + xlab("")
ggsave("img/p5.pdf",p,h=7,w=10)
########################################################################





library(ggrepel)
ggplot(mg, aes(avg_contr,V1,label=term,color=respondent_party))  + 
  geom_point(size=4) +  
  scale_color_manual("Respondent\nPolitical\nAffiliation",
                     values=list("Other/Independent"="green",
                                 "Republican"="red",
                                 "Democrat"="blue"),
                     guide=guide_legend(keyheight = unit(2.5, "cm"))) + 
  geom_text_repel() + 
  geom_hline(yintercept=4)+theme_bw(15) + 
  facet_wrap(~v) + 
  stat_cor(label.x.npc = .7,label.y.npc=.2,method="spearman") + 
  xlab("Actual Impact on Responses (Sqrt(Deviance))") + 
  scale_y_continuous("Average Likert Response", limits=c(1,7),
                     breaks=c(1,4,7), 
                     labels=c("Not at all important","Neutral",
                              "Extremely important"))
p <- ggplot(m2, aes(avg_cont,V1,label=term,color=respondent_party))  + 
  geom_point(size=4) +  
  scale_color_manual("Respondent\nPolitical\nAffiliation",
                     values=list("Other/Independent"="green",
                                 "Republican"="red",
                                 "Democrat"="blue"),
                     guide=guide_legend(keyheight = unit(2.5, "cm"))) + 
  geom_text_repel() + 
  geom_vline(xintercept=mean(m2$avg_cont)) + 
  geom_hline(yintercept=4)+theme_bw(15) + 
  stat_cor(label.x.npc = .7,label.y.npc=.2,method="spearman") + 
  xlab("Average Contribution of Factor to Explaining\nConjoint Analysis Responses ") + 
  scale_y_continuous("Average Likert Response", limits=c(1,7),
                     breaks=c(1,4,7), 
                     labels=c("Not at all important","Neutral",
                              "Extremely important"))
