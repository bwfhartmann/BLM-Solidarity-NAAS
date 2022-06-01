rm(list=ls()) 
library (dplyr)
library(ggplot2)
library(data.table)
library(caseMatch)


library(readxl)
df1 <- read_excel("C:/Users/bhartmann/Downloads/CFD 3.0 Norming Data and Codebook (2).xlsx", sheet =2)
df <- df1[9:nrow(df1),]
thing <- as.character(df1[7,])
names(df) <- ifelse(is.na(thing),as.character(df1[8,]),thing)

##############################################################################
#East Asian
df_ea <- df |> dplyr::filter(EthnicitySelf=="A" & GenderSelf == "M")

##############################################################################
#Indian
df2 <- read_excel("C:/Users/bhartmann/Downloads/CFD 3.0 Norming Data and Codebook (2).xlsx", sheet =4)
df_ <- df2[9:nrow(df2),]
thing <- as.character(df2[7,])
names(df_) <- ifelse(is.na(thing),as.character(df2[8,]),thing)

df_in <- df_ |> dplyr::filter(GenderSelf == "M")
df_in$EthnicitySelf <- "I"

##############################################################################
#White
df_wh <- df |> dplyr::filter(EthnicitySelf=="W" & GenderSelf == "M" & WhiteProb >.7)

col_ea <- names(df_ea)
col_in <- names(df_in)

vars <- intersect(col_ea,col_in)

#Combine data
combined_df <- rbind(df_ea[names(df_ea) %in% vars],
                     df_in[names(df_in) %in% vars],
                     df_wh[names(df_wh) %in% vars])

match.var <- c("match","AgeRated","Attractive","Trustworthy","Happy","Masculine")
dropvars <- names(combined_df)[!names(combined_df) %in% match.var]

char_var <- combined_df |> dplyr::select(Model,EthnicitySelf)
num_var <- combined_df[names(combined_df) %in% match.var] |> map_df(as.numeric)
match_df <- cbind(char_var,num_var)
match_in <- match_df[match_df$EthnicitySelf!="W",]
match_in$match <- ifelse(match_in$EthnicitySelf=="I",1,0)

out_in <- case.match(data=match_in[,-(2)], 
                   id.var="Model", 
                   distance="mahalanobis", 
                   case.N=2, 
                   greedy.match="pareto",
                   number.of.matches.to.return=20,
                   treatment.var="match", 
                   max.variance=TRUE,
                   match.case="AM-209")


match_wh <- match_df[match_df$EthnicitySelf!="I",]
match_wh$match <- ifelse(match_wh$EthnicitySelf=="W",1,0)

out_wh <- case.match(data=match_wh[,-(2)], 
                     id.var="Model", 
                     distance="mahalanobis", 
                     case.N=2, 
                     greedy.match="pareto",
                     number.of.matches.to.return=20,
                     treatment.var="match", 
                     max.variance=TRUE,
                     match.case="AM-209")


match_ea <- match_df[match_df$EthnicitySelf!="A",]
match_ea$match <- ifelse(match_ea$EthnicitySelf=="W",1,0)
out_wh <- case.match(data=match_ea[,-(2)], 
                     id.var="Model", 
                     distance="mahalanobis", 
                     case.N=2, 
                     greedy.match="pareto",
                     number.of.matches.to.return=20,
                     treatment.var="match", 
                     max.variance=TRUE,
                     match.case="WM-225")
