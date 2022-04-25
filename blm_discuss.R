## Packings loaded
library(psych)
library(effects)
library(ggpubr)
library(tidyverse)
library(data.table)
library(ggeffects)
## Loading in the Data
path <- "~/Dropbox/COVID-19"
folder <- "data/cleaned"
data <- "combined_core_network_variables.rds"
infile = file.path(path, folder, data)
protest_data <- read_fst(file.path(path,"data","additional_data","protest/protest-CCC/combined_protest_census_block.fst"))

dt <- as.data.table(readRDS(infile))
dt <- dt[partyid4!="Something else"]

dt[,talk_blm := ifelse(network_n_blm_discuss>0 & n_size_total>0,1,0)]
dt[,table(talk_blm)]
dt[,talk_blm_all := ifelse((network_n_blm_discuss/n_size_total)==1,1,0)]
dt[,table(talk_blm,talk_blm_all)]
dt[,talk_blm_some := ifelse(network_n_blm_discuss>0 & (network_n_blm_discuss/n_size_total)<1,1,0)]
dt[,table(talk_blm,talk_blm_some)]
dt[,table(talk_blm_all,talk_blm_some)]

dt[,talk_blm_none := ifelse(network_n_blm_discuss==0,1,0)]
dt[,table(talk_blm,talk_blm_none)]

dt[,talk_blm_multi := ifelse(talk_blm_none==1, "None", 
                      ifelse(talk_blm_all==1,"All","Some"))]
dt[,table(talk_blm_multi)]
dt$week <- as.factor(dt$week)
dt[date>as.Date("2020-06-29") & date<=as.Date("2020-10-29")] -> dt2                            

haven::write_dta(dt2,"blmdiscussion.dta")

dt2$week <- as.numeric(dt2$week)
dt2[,income:=ifelse(income=="Prefer not to tell",NA,income)]
ids <- unique(dt2[talk_blm_some==1]$rid)

# Models
library(survey)
design<-svydesign(id=~rid,weights=~weight_all, data=dt2[!is.na(weight_all)])

# Did they talk?
# Basic Controls
libary(lme4)
summary(m1a<-svyglm(talk_blm~race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))
summary(m1a2<-lmer(talk_blm~race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week+(1|state),
                    data=dt2,weight=weight_all))


# Add Network Variables
summary(m1b<-svyglm(talk_blm~n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))

summary(m1b2<-lmer(talk_blm~n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week+(1|state),
                    data=dt2,weight=weight_all))

# Add contextual variables
summary(m1c<-svyglm(talk_blm~metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))
summary(m1c2<-lmer(talk_blm~metro+protest_blm_all+scale(new_case_rate_state)+black_pop_2019+
 					county_vote16_democrat+n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week+(1|state),
                    data=dt2,weight=weight_all))

stargazer::stargazer(m1a,m1b,m1c, type="text")


m1 <- list()
i <- 1
for(r in unique(dt$race)) {

summary(m1[[i]]<-glm(talk_blm~metro+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+age_group+week,
                    data=dt2[race==r],family=binomial))
i <- i +1
}

stargazer::stargazer(m1[[1]],m1[[2]],m1[[3]],m1[[4]],m1[[5]], 
type="text",
column.labels = as.character(unique(dt$race)))

# Did they talk to everyone?
# Basic Controls
summary(m2a<-svyglm(talk_blm_all~race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))

# Add Network Variables
summary(m2b<-svyglm(talk_blm_all~n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))


# Add contextual variables
summary(m2c<-svyglm(talk_blm_all~metro+protest_blm_all+percblack+scale(new_case_rate_state)+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+age_group+week,
                    design=design,family=binomial))
stargazer::stargazer(m2a,m2b,m2c, type="text")

m1 <- list()
i <- 1
for(r in unique(dt$race)) {

summary(m1[[i]]<-glm(talk_blm_all~metro+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+age_group+week+I(week^2),
                    data=dt2[race==r],family=binomial))
i <- i +1
}

stargazer::stargazer(m1[[1]],m1[[2]],m1[[3]],m1[[4]],m1[[5]], type="text")


# Did they talk to some?
# Basic Controls
summary(m3a<-svyglm(talk_blm_some~race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))

# Add Network Variables
summary(m3b<-svyglm(talk_blm_some~n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))


# Add contextual variables
summary(m3c<-svyglm(talk_blm_some~metro+protest_blm_all+percblack+scale(new_case_rate_state)+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+
                    race+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))
stargazer::stargazer(m3a,m3b,m3c, type="text")
stargazer::stargazer(m2c, m3c, type="text")

m1 <- list()
i <- 1
for(r in unique(dt$race)) {

summary(m1[[i]]<-glm(talk_blm_some~metro+protest_blm_all+scale(new_case_rate_state)+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+age_group+week,
                    data=dt2[race==r],family=binomial))
i <- i +1
}

for(p in unique(dt$partyid4)) {

summary(m1[[i]]<-glm(talk_blm_some~metro+protest_blm_all+scale(new_case_rate_state)+
 					n_size_total+network_homophily_partyid+network_homophily_race+network_homophily_sex+partyid4+ideo+income+marital_status+sex+
                    as.factor(educ)+age_group+week,
                    data=dt2[race==r],family=binomial))
i <- i +1
}

stargazer::stargazer(m1[[1]],m1[[2]],m1[[3]],m1[[4]],m1[[5]],m1[[6]],m1[[7]], type="text")


summary(m3c<-svyglm(talk_blm_some~metro+protest_blm_all+percblack+scale(new_case_rate_state)+
 					n_size_total+as.numeric(ideo)+network_homophily_partyid+race+network_homophily_race+sex+network_homophily_sex+
                    partyid4+income+marital_status+sex+
                    as.factor(educ)+has_child+age_group+week,
                    design=design,family=binomial))

#If they didn't talk to everyone, who did they talk to?
infile = file.path(path, folder, "combined_core_network_dyad.rds")
dt <- as.data.table(readRDS(infile))
dt[,table(talkBLM)]
dt$week <- as.factor(dt$week)
dt[date>as.Date("2020-06-29") & date<=as.Date("2020-10-29")] -> dt2                            
dt2$week <- as.numeric(dt2$week)
dt2[,income:=ifelse(income=="Prefer not to tell",NA,income)]
dt3 <- dt2[rid %in% ids]

dt3[,a_college:=ifelse(a_educ == "College degree or higher",1,0)]
dt3[,college:=ifelse(educ %in% c("4 year degree","Doctorate","Professional/master degre",),1,0)]

dt3[,same_educ := ifelse(a_college == college,1,0)]
dt3[,same_party := ifelse(a_party == partyid4,1,0)]
dt3[,same_race := ifelse(a_race == as.character(race),1,0)]
dt3[,same_sex := ifelse(a_sex == sex,1,0)]
dt3[,same_age := ifelse(age %in%c('Age 17-24','Age 25-44'),1,0)==ifelse(a_age %in%c("Younger than 20", "20-39"),1,0)]

rid2 <- as.factor(dt3$rid)

jtools::summ(m1<-glmer(talkBLM=="Yes"~
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    race+partyid4+as.numeric(ideo)+income+marital_status+sex+
                    as.factor(educ)+age_group+week+(1|rid)+(1|state),
                    data=dt3),weights=weight_all,family=binomial)

jtools::summ(m1<-lmer(talkBLM=="Yes"~a_physical_contact+a_talkfreq+
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    race+partyid4+as.numeric(ideo)+income+marital_status+sex+
                    as.factor(educ)+age_group+week+(1|rid)+(1|state),
                    data=dt3))

jtools::summ(m1<-lmer(talkBLM=="Yes"~a_physical_contact+a_talkfreq+
					same_educ+same_party+same_race+same_sex+same_age+
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    race+partyid4+as.numeric(ideo)+income+marital_status+sex+
                    as.factor(educ)+age_group+week+(1|rid)+(1|state),
                    data=dt3))
jtools::summ(m1<-lmer(talkBLM~a_physical_contact+a_talkfreq+
					college*same_educ+as.numeric(ideo)*same_party+race*same_race+sex*same_sex+age_group*same_age+
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    income+week+(1|rid)+(1|state),
                    data=dt3))
jtools::effect_plot(m1,"same_race")

m1 <- list()
i <- 1
for(r in unique(dt3$race)) {

jtools::summ(m1[[i]]<-lmer(talkBLM=="Yes"~a_physical_contact+a_talkfreq+
					college*same_educ+as.numeric(ideo)*same_party+same_race+sex*same_sex+age_group*same_age+
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    income+week+(1|rid)+(1|state),
                    data=dt3[race==r]))

i <- i +1
}

for(p in unique(dt3$partyid4)) {

jtools::summ(m1[[i]]<-lmer(talkBLM=="Yes"~a_physical_contact+a_talkfreq+
					college*same_educ+as.numeric(ideo)*same_party+race*same_race+sex*same_sex+age_group*same_age+
					metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    income+week+(1|rid)+(1|state),
                    data=dt3[partyid4==p]))
i <- i +1
}

stargazer::stargazer(m1[[1]],m1[[2]],m1[[3]],m1[[4]],m1[[5]],m1[[6]],m1[[7]], type="text")

## Other stuff
dt3[,dontknow_party := as.factor(ifelse(a_party == "Don't know","Yes",0))]
dt3[,dontknow_race := as.factor(ifelse(a_race == "Don't know","Yes",0))]
dt3[,dontknow_educ := as.factor(ifelse(a_educ == "Don't know","Yes",0))]
dt3[,dontknow_age := as.factor(ifelse(a_age == "Don't know","Yes",0))]
dt3[,talkBLM:=ifelse(talkBLM=="Yes",1,0)]

dt3[,pa := as.factor(ifelse(dontknow_party=="Yes","Don't",same_party))]
dt3[,ra := as.factor(ifelse(dontknow_race=="Yes","Don't",same_race))]
dt3[,ea := as.factor(ifelse(dontknow_educ=="Yes","Don't",same_educ))]
dt3[,aa := as.factor(ifelse(dontknow_age=="Yes","Don't",same_age))]
dt3[,num := sum(ifelse(topic_politics=="Yes",1,0)) / .N,by="rid"]

jtools::summ(m1<-glmer(talkBLM~dontknow_party+dontknow_race+dontknow_educ+dontknow_age+
                               metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    			race+partyid4+as.numeric(ideo)+income+marital_status+sex+relationship+
                               (1|rid)+(1|state),
						data=dt3,weights=weight_all,family=binomial))
jtools::summ(m1<-glm(talkBLM~dontknow_party+dontknow_race+dontknow_educ+dontknow_age+
                               metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    			race+partyid4+as.numeric(ideo)+income+marital_status+sex+relationship
                               ,
						data=dt3,weights=weight_all,family=binomial))
jtools::summ(m1<-glm(talkBLM~pa+ra+ea+aa+a_talkfreq+a_physical_contact+
                               metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    			race+partyid4+as.numeric(ideo)+income+marital_status+sex+relationship
                               ,
						data=dt3,weights=weight_all,family=binomial))


jtools::summ(m2<-glm(topic_politics~pa+ra+ea+aa+a_talkfreq+a_physical_contact+
                               metro+scale(new_case_rate_state)+scale(black_pop_2019)+county_vote16_democrat+
                    		   race+partyid4+as.numeric(ideo)+income+marital_status+sex+relationship
                               ,
						data=dt3[num>0 & num <1],weights=weight_all,family=binomial))

stargazer::stargazer(m1,m2,type="text")
jtools::effect_plot(m1,pred = dontknow_party, interval = T,data=dt3)
plot(ggpredict(m1,c("pa","ra")))
plot(ggpredict(m1,c("pa")))


