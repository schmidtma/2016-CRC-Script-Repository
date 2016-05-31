#McWeeney Lab
#Version 1.2 May 2016
#Modified May 2016, Wong Lab

library(OptimalCutpoints)
setwd("S:/Manuscripts in progress/CRC CD166/Data & Annotation/WD/")
crcdat<-read.csv(file="FullyAnnotatedData.csv", header=TRUE)

#Give user-readable names to columns, add negative %.
colnames(crcdat)[39] <- "CD166PosPct"
crcdat$CD166PosPct <- apply(crcdat[,c(37,40)], 1, function(x) x[2]/x[1]*100)
crcdat$CD166NegPct <- sapply(crcdat$CD166PosPct, function(x) 100-x)

#Redefine Stage 1 as "healthy" for comparison of 1 vs. later stages.
crcdat$Overall.Stage..clinical.for.rectum.[which(crcdat$Overall.Stage..clinical.for.rectum. == 1)] <- 0

#Determine optimal cut point for each marker 
#tag.healthy should match coding for status
#As specified gives equal weight to sensitivity and specificity
#Use Generalized Youden Index in Control.cutpoints if want different weights 

#ROC and optimal cut point analysis for patient data in CRC patients based on Overal staging and no covariates
#no covariates

#cd166
optimal.cutpoint.Youdencd166<-optimal.cutpoints(X = "CD166PosPct", status = "Overall.Stage..clinical.for.rectum.", tag.healthy = 0, methods = "Youden", data = crcdat, pop.prev = NULL, control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
#prints out Youden cutoff and AUC
print(optimal.cutpoint.Youdencd166)

#prints out Sensitivity, specificity, PPV, and NPV
summary(optimal.cutpoint.Youdencd166)

#note variables are re-used for subsequent analyses
#create new variables if you want to keep the results of prior analyses
rm(optimal.cutpoint.Youdencd166)


###########################################################
#ROC and optimal cut point analysis for patient data in CRC patients based on disease location and stage as covariates
#stage as covariate

#cd166
optimal.cutpoint.Youdencd166<-optimal.cutpoints(X = "CD166PosPct", status = "Overall.Stage..clinical.for.rectum.", tag.healthy = 0, methods = "Youden", data = crcdat, pop.prev = NULL, categorical.cov =
"Site.of.cancer..colon.or.rectum.",control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
print(optimal.cutpoint.Youdencd166)

summary(optimal.cutpoint.Youdencd166)

#note variables are re-used for subsequent analyses
#create new variables if you want to keep the results of prior analyses
rm(optimal.cutpoint.Youdencd166)

#################################################
#Determine optimal cut point for CD166(?) 
#tag.healthy should match coding for status
#smoke status as covariate

#double negative
optimal.cutpoint.YoudenDblNeg<-optimal.cutpoints(X = "median_dbl_neg_pct", status = "HPVStatus", tag.healthy = 0, methods = "Youden", data = eddyhnscc, pop.prev = NULL, categorical.cov =
"SmokeStatus", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
print(optimal.cutpoint.YoudenDblNeg)

#cd44
optimal.cutpoint.Youdencd44<-optimal.cutpoints(X = "median_cd44_pct", status = "HPVStatus", tag.healthy = 0, methods = "Youden", data = eddyhnscc, pop.prev = NULL, categorical.cov =
"SmokeStatus", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
print(optimal.cutpoint.Youdencd44)

#double positive
optimal.cutpoint.YoudenDblpos<-optimal.cutpoints(X = "median_dbl_pos_pct", status = "HPVStatus", tag.healthy = 0, methods = "Youden", data = eddyhnscc, pop.prev = NULL, categorical.cov =
"SmokeStatus", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
print(optimal.cutpoint.YoudenDblpos)

#cd166
optimal.cutpoint.Youdencd166<-optimal.cutpoints(X = "median_cd166_pct", status = "HPVStatus", tag.healthy = 0, methods = "Youden", data = eddyhnscc, pop.prev = NULL, categorical.cov =
"SmokeStatus", control = control.cutpoints(), ci.fit = TRUE, conf.level = 0.95, trace = FALSE)
print(optimal.cutpoint.Youdencd166)


#Review each print statement 
#Summary and plots based on inspection of results 

summary(optimal.cutpoint.Youdencd166)
summary(optimal.cutpoint.YoudenDblpos)
summary(optimal.cutpoint.Youdencd44)
summary(optimal.cutpoint.YoudenDblNeg)


boxplot(median_cd166_pct~SmokeStatus*HPVStatus, data=eddyhnscc, xlab="SmokingStatus by HPV Status (HPV+=1)", ylab="Cd166Pct")

plot(optimal.cutpoint.Youdencd166, legend = FALSE)