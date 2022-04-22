library(haven)
library(mediation)
library(tidyverse)
library(survey)

setwd("/Users/hartmabs/Documents/Masters/New")
data <- read_dta("/Users/hartmabs/Documents/Masters/New/masters_mediation.dta")

data$temp = 2016 - data$q10_18
data$age = ifelse(data$temp < 0, NA, data$temp)
controls = c("age", "female", "educ", "pid4", "discrim", "income", "region", "usborn", "sec_gen", "black_contact")
covar = c("race_important", "ethnic_important", "cat_effectall_aapi", "cat_effectall_ethnic", "group_con")

for(var in controls) {
  print(sum(is.na(data[[var]])))
}

for(var in c(covar, "asian_support", "latino_support")) {
  data[[var]] = as.numeric(data[[var]])
  print(sum(is.na(data[[var]])))
}


mediator = c("gov_act2","black_common", "int_currentevents")
for(var in mediator) {
  data[[var]] = as.numeric(data[[var]])
  print(table(data[[var]] ))
  print(sum(is.na(data[[var]])))
}

ma_mediation <- function(x) {
    data2 = data %>% dplyr::select(all_of(covar), all_of(controls), x, "race", "asian_support", "latino_support", "nweightnativity", "respid")
    data2 <- data2[complete.cases(data2[names(data2) %in% c(controls, covar, x)]), ]
    
    output = data.frame()
    
    data3 = data2 %>% dplyr::filter(race == 1)
    data4 = data2 %>% dplyr::filter(race == 6)
    
    ## For Asian
    svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data3)
    
    med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=gaussian))	
    
    out.fit <- suppressWarnings(svyglm(paste0("asian_support ~", x," + ",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=binomial(link='logit')))	
    
    for(var in covar) {
      if(x == "int_currentevents" | x == "black_common") {
      
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE, control.value = 0, treat.value = 1, sims = 100)
      
      } else if (x == "ineq_s2") {
        med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE, control.value = 0, treat.value = 1, sims = 100)
      } else {
        med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1, sims = 100)
      }
      
      med.out2 = summary(med.out)
      
      med = list(
        treat = x,
        mediator = var,
        group = "asian",
        n = med.out2[["nobs"]],
        direct_effect = med.out2[["z.avg"]],
        direct_effect_high = med.out2[["z.avg.ci"]][["97.5%"]],
        direct_effect_low = med.out2[["z.avg.ci"]][["2.5%"]],
        direct_effect_p = med.out2[["z.avg.p"]],
        indirect_effect = med.out2[["d.avg"]],
        indirect_effect_high = med.out2[["d.avg.ci"]][["97.5%"]],
        indirect_effect_low = med.out2[["d.avg.ci"]][["2.5%"]],
        indirect_effect_p = med.out2[["d.avg.p"]],
        total_effect = med.out2[["tau.coef"]],
        total_effect_high = med.out2[["tau.ci"]][["97.5%"]],
        total_effect_low = med.out2[["tau.ci"]][["2.5%"]],
        total_effect_p = med.out2[["tau.p"]],
        prop_med = med.out2[["n.avg"]]
      )
      med = data.frame(med)
      output = rbind(output, med)
    }
    
    svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data4)
    med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy))	
    
    out.fit <- suppressWarnings(svyglm(paste0("latino_support ~", x," + ",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=binomial(link='logit')))	
    ## For Latinos
    for(var in covar) {
      if(x == "int_currentevents" | x == "black_common") {
        med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1,sims = 100)
      } else if (x == "ineq_s2") {
        med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1,sims = 100)
      } else {
        med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = F,  control.value = 0, treat.value = 1,sims = 100)
      }
      
      med.out2 = summary(med.out)
      med = list(
        treat = x,
        mediator = var,
        group = "latino",
        n = med.out2[["nobs"]],
        direct_effect = med.out2[["z.avg"]],
        direct_effect_high = med.out2[["z.avg.ci"]][["97.5%"]],
        direct_effect_low = med.out2[["z.avg.ci"]][["2.5%"]],
        direct_effect_p = med.out2[["z.avg.p"]],
        indirect_effect = med.out2[["d.avg"]],
        indirect_effect_high = med.out2[["d.avg.ci"]][["97.5%"]],
        indirect_effect_low = med.out2[["d.avg.ci"]][["2.5%"]],
        indirect_effect_p = med.out2[["d.avg.p"]],
        total_effect = med.out2[["tau.coef"]],
        total_effect_high = med.out2[["tau.ci"]][["97.5%"]],
        total_effect_low = med.out2[["tau.ci"]][["2.5%"]],
        total_effect_p = med.out2[["tau.p"]],
        prop_med = med.out2[["n.avg"]]
      )
      med = data.frame(med)
      output = rbind(output, med)
    }
    return(output)
  }

black_common_df = ma_mediation("black_common")
events_df = ma_mediation("int_currentevents")
govact_df = ma_mediation("gov_act2")
ineq_df = ma_mediation("ineq_s2")

library(data.table)
all_data = as.data.table(rbind(black_common_df,govact_df,events_df,ineq_df))
write.csv(all_data, "mediation_analysis_new2.csv")
all_data[, sig_in := ifelse(indirect_effect_p  < 0.1, 'sig', 'insig')]
all_data[, sig_dir := ifelse(direct_effect_p  < 0.1, 'sig', 'insig')]
all_data[, sig_total := ifelse(total_effect_p  < 0.1, 'sig', 'insig')]

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
zp1 <- ggplot(all_data, aes(linetype=sig_in))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = direct_effect, ymin = direct_effect_low,
#                                ymax = direct_effect_high),
#                            lwd = 1, position = position_dodge(width = 1/2), shape = 1, fill = "WHITE") 
zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = indirect_effect, ymin = indirect_effect_low,
                                 ymax = indirect_effect_high), position = position_dodge(width = 1/2))
(zp1 <- zp1 + theme_bw() + facet_grid(group~treat, scales = "free")+ coord_flip()) 

zp1 <- ggplot(all_data, aes(linetype=sig_dir))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = direct_effect, ymin = direct_effect_low,
#                                ymax = direct_effect_high),
#                            lwd = 1, position = position_dodge(width = 1/2), shape = 1, fill = "WHITE") 
zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = direct_effect, ymin = direct_effect_low,
                                 ymax = direct_effect_high), position = position_dodge(width = 1/2))
(zp1 <- zp1 + theme_bw() + facet_grid(group~treat, scales = "free")+ coord_flip()) 


zp1 <- ggplot(all_data, aes(linetype=sig_total))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = direct_effect, ymin = direct_effect_low,
#                                ymax = direct_effect_high),
#                            lwd = 1, position = position_dodge(width = 1/2), shape = 1, fill = "WHITE") 
zp1 <- zp1 + geom_pointrange(aes(x = mediator, y = total_effect, ymin = total_effect_low,
                                 ymax = total_effect_high), position = position_dodge(width = 1/2))
(zp1 <- zp1 + theme_bw() + facet_grid(group~treat, scales = "free")+ coord_flip()) 



ma_mediation2 <- function(x) {
  data2 = data %>% dplyr::select(all_of(covar), all_of(controls), x, "race", "asian_support", "latino_support", "nweightnativity", "respid", "asian_opinion", "latino_opinion", )
  data2 <- data2[complete.cases(data2[names(data2) %in% c(controls, covar, x)]), ]
  
  output = data.frame()
  
  data3 = data2 %>% dplyr::filter(race == 1)
  data4 = data2 %>% dplyr::filter(race == 6)
  
  ## For Asian
  svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data3)
  
  med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                     design=svy, family=gaussian))	
  
  out.fit <- suppressWarnings(svyglm(paste0("asian_opinion ~", x," + ",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                     design=svy, family=binomial(link='logit')))	
  
  for(var in covar) {
    if(x == "int_currentevents" | x == "black_common") {
      
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE, control.value = 0, treat.value = 1, sims = 100)
      
    } else if (x == "ineq_s2") {
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE, control.value = 0, treat.value = 1, sims = 100)
    } else {
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1, sims = 100)
    }
    
    med.out2 = summary(med.out)
    
    med = list(
      treat = x,
      mediator = var,
      group = "asian",
      n = med.out2[["nobs"]],
      direct_effect = med.out2[["z.avg"]],
      direct_effect_high = med.out2[["z.avg.ci"]][["97.5%"]],
      direct_effect_low = med.out2[["z.avg.ci"]][["2.5%"]],
      direct_effect_p = med.out2[["z.avg.p"]],
      indirect_effect = med.out2[["d.avg"]],
      indirect_effect_high = med.out2[["d.avg.ci"]][["97.5%"]],
      indirect_effect_low = med.out2[["d.avg.ci"]][["2.5%"]],
      indirect_effect_p = med.out2[["d.avg.p"]],
      total_effect = med.out2[["tau.coef"]],
      total_effect_high = med.out2[["tau.ci"]][["97.5%"]],
      total_effect_low = med.out2[["tau.ci"]][["2.5%"]],
      total_effect_p = med.out2[["tau.p"]],
      prop_med = med.out2[["n.avg"]]
    )
    med = data.frame(med)
    output = rbind(output, med)
  }
  
  svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data4)
  med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                     design=svy))	
  
  out.fit <- suppressWarnings(svyglm(paste0("latino_opinion ~", x," + ",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                     design=svy, family=binomial(link='logit')))	
  ## For Latinos
  for(var in covar) {
    if(x == "int_currentevents" | x == "black_common") {
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1,sims = 100)
    } else if (x == "ineq_s2") {
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = FALSE,  control.value = 0, treat.value = 1,sims = 100)
    } else {
      med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = F,  control.value = 0, treat.value = 1,sims = 100)
    }
    
    med.out2 = summary(med.out)
    med = list(
      treat = x,
      mediator = var,
      group = "latino",
      n = med.out2[["nobs"]],
      direct_effect = med.out2[["z.avg"]],
      direct_effect_high = med.out2[["z.avg.ci"]][["97.5%"]],
      direct_effect_low = med.out2[["z.avg.ci"]][["2.5%"]],
      direct_effect_p = med.out2[["z.avg.p"]],
      indirect_effect = med.out2[["d.avg"]],
      indirect_effect_high = med.out2[["d.avg.ci"]][["97.5%"]],
      indirect_effect_low = med.out2[["d.avg.ci"]][["2.5%"]],
      indirect_effect_p = med.out2[["d.avg.p"]],
      total_effect = med.out2[["tau.coef"]],
      total_effect_high = med.out2[["tau.ci"]][["97.5%"]],
      total_effect_low = med.out2[["tau.ci"]][["2.5%"]],
      total_effect_p = med.out2[["tau.p"]],
      prop_med = med.out2[["n.avg"]]
    )
    med = data.frame(med)
    output = rbind(output, med)
  }
  return(output)
}

black_common_df = ma_mediation2("black_common")
govact_df = ma_mediation2("gov_act2")
events_df = ma_mediation2("int_currentevents")

all_data = rbind(black_common_df,govact_df,events_df)
all_data2 = as.data.table(all_data)

write.csv(all_data, "mediation_analysis2.csv")

all_data2[, sig_in := ifelse(indirect_effect_p  < 0.1, 'sig', 'insig')]
all_data2[, sig_dir := ifelse(direct_effect_p  < 0.1, 'sig', 'insig')]
write.csv(all_data2, "mediation_analysis2.csv")

all_data2[, pos := ifelse(indirect_effect > 0, 'pos', 'neg')]

indirect_asian = ggplot(all_data2[group == "asian"], aes(x=mediator,y=indirect_effect, ymin=indirect_effect_low,ymax=indirect_effect_high, alpha=sig, fill=pos, color=pos,linetype=sig)) + 
      geom_pointrange() + 
      scale_color_manual(name='', values=c('pos'='blue','neg'='red'))+
      scale_fill_manual(name='', values=c('pos'='blue','neg'='red'))+
      scale_alpha_manual(name='', values=c('sig'=1,'insig'=0.5))+
      scale_linetype_manual(name='', values=c('sig'='solid','insig'='dashed'))+
      coord_flip() + theme_pubr() + 
      labs(title = "Medation Analysis - Asian")

indirect_asian = indirect_asian + geom_hline(yintercept=0, color='gray')
indirect_asian = indirect_asian + facet_wrap(treat~.)
indirect_asian

indirect_latino = ggplot(all_data2[group == "latino"], aes(x=mediator,y=indirect_effect, ymin=indirect_effect_low,ymax=indirect_effect_high, alpha=sig, fill=pos, color=pos,linetype=sig)) + 
  geom_pointrange() + 
  scale_color_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_fill_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_alpha_manual(name='', values=c('sig'=1,'insig'=0.5))+
  scale_linetype_manual(name='', values=c('sig'='solid','insig'='dashed'))+
  coord_flip() + theme_pubr() + 
  labs(title = "Medation Analysis - Latino")

indirect_latino = indirect_latino + geom_hline(yintercept=0, color='gray')
indirect_latino = indirect_latino + facet_wrap(treat~.)
indirect_latino

all_data2[, sig := ifelse(direct_effect_p  < 0.05, 'sig', 'insig')]
all_data2[, pos := ifelse(direct_effect > 0, 'pos', 'neg')]

direct_asian = ggplot(all_data2[group == "asian"], aes(x=mediator,y=direct_effect, ymin=direct_effect_low,ymax=direct_effect_high, alpha=sig, fill=pos, color=pos,linetype=sig)) + 
  geom_pointrange() + 
  scale_color_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_fill_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_alpha_manual(name='', values=c('sig'=1,'insig'=0.5))+
  scale_linetype_manual(name='', values=c('sig'='solid','insig'='dashed'))+
  coord_flip() + theme_pubr() + 
  labs(title = "Medation Analysis - Asian")

direct_asian = direct_asian + geom_hline(yintercept=0, color='gray')
direct_asian = direct_asian + facet_wrap(treat~.)
direct_asian

direct_latino = ggplot(all_data2[group == "latino"], aes(x=mediator,y=direct_effect, ymin=direct_effect_low,ymax=direct_effect_high, alpha=sig, fill=pos, color=pos,linetype=sig)) + 
  geom_pointrange() + 
  scale_color_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_fill_manual(name='', values=c('pos'='blue','neg'='red'))+
  scale_alpha_manual(name='', values=c('sig'=1,'insig'=0.5))+
  scale_linetype_manual(name='', values=c('sig'='solid','insig'='dashed'))+
  coord_flip() + theme_pubr() + 
  labs(title = "Medation Analysis - Latino")

direct_latino = direct_latino + geom_hline(yintercept=0, color='gray')
direct_latino = direct_latino + facet_wrap(treat~.)
direct_latino


## For Asian
for (x in mediator) {
  data2 = data %>% dplyr::select(all_of(covar), all_of(controls), x, "race", "asian_support", "latino_support", "nweightnativity", "respid")
  data2 <- data2[complete.cases(data2[names(data2) %in% c(controls, covar, x)]), ]
  
  output = data.frame()
  
  data3 = data2 %>% dplyr::filter(race == 1)

    for(var in covar) {
    svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data3)
    med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=gaussian))	
    
    out.fit <- suppressWarnings(svyglm(paste0("asian_support ~", x,"*", var, "+",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=binomial(link='logit')))	
    print(x)
    print(var)
    med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = TRUE, sims = 100)
    print(test.TMint(med.out, conf.level = .95))
  }
}

## For Latinos
for (x in mediator) {
  data2 = data %>% dplyr::select(all_of(covar), all_of(controls), x, "race", "asian_support", "latino_support", "nweightnativity", "respid")
  data2 <- data2[complete.cases(data2[names(data2) %in% c(controls, covar, x)]), ]
  
  output = data.frame()
  
  data4 = data2 %>% dplyr::filter(race == 6)
  
  for(var in covar) {
    svy = svydesign(id = ~1, weights = as.formula(paste0('~', "nweightnativity")), data=data4)
    med.fit <- suppressWarnings(svyglm(paste0(x, " ~ ", paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=gaussian))	
    
    out.fit <- suppressWarnings(svyglm(paste0("latino_support ~", x,"*", var, "+",paste0(paste(controls, collapse = "+"),"+",paste(covar, collapse = "+"))), 
                                       design=svy, family=binomial(link='logit')))	
    print(x)
    print(var)
    med.out <- mediate(med.fit, out.fit, treat = var, mediator = x, robustSE = TRUE, sims = 100)
    print(test.TMint(med.out, conf.level = .95))
  }
}

