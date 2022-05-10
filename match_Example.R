install.packages("caseMatch")
library(caseMatch)
# NOT RUN {
data(EU)
mvars <- c("socialist","rgdpc","FHc","FHp","trade")
dropvars <- c("countryname","population")

## In this example, I subset to the first 40 obs. to cut run-time
out <- case.match(data=EU[1:40,], id.var="countryname", leaveout.vars=dropvars,
                  distance="mahalanobis", case.N=2, 
                  number.of.matches.to.return=10,
                  treatment.var="eu", max.variance=TRUE)
out$cases

# }
# NOT RUN {
## All cases:
## Find the best matches of EU to non-EU countries
out <- case.match(data=EU, id.var="countryname", leaveout.vars=dropvars,
                  distance="mahalanobis", case.N=2, 
                  number.of.matches.to.return=10,
                  treatment.var="eu", max.variance=TRUE)
out$cases

## Find the best matches while downweighting political variables
myvarweights <- c(1,1,.1,.1,.1)
names(myvarweights) <- c("rgdpc","trade","FHp","FHc","socialist")
myvarweights
(case.match(data=EU, id.var="countryname", leaveout.vars=dropvars,
            distance="mahalanobis", case.N=2, 
            number.of.matches.to.return=10, treatment.var="eu",
            max.variance=TRUE,varweights=myvarweights))$cases

## Find the best non-EU matches for Germany
EU <- as.data.table(EU)
tabGer <- case.match(data=rbind(EU,
                                EU[countryname!= "German Federal Republic"],
                                EU[countryname!= "German Federal Republic"]), match.case="German Federal Republic", 
                     id.var="countryname",leaveout.vars=dropvars,
                     distance="mahalanobis", case.N=2, 
                     number.of.matches.to.return=10,max.variance=TRUE,
                     treatment.var="eu")
# }
# NOT RUN {


# }