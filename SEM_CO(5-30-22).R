#Weekly data SEM using additional plot data (Soil Characteristics)
#Updated 4-8-22
#Written by: Jim Den Uyl

install.packages("semPlot", dependencies = TRUE)


library(Hmisc)
library(lavaan)
library(AICcmodavg)
library(lmerTest)
library(semPlot)
library(dplyr)
library(mice)


#Load Data####

setwd("/Users/jdenuyl/Downloads/RFiles")

comball<-read.csv("USSR2021(5-30-22).csv") # all data

comball$Warming<-ifelse(comball$Warming == "A", 0, 1)
comball$Elevation<-ifelse(comball$Elevation == "Low", 0, 1)
comball$Removal<-ifelse(comball$Removal == "C", 0, 1)




####Treatment Effects####

mod1 <- lmer(EFFLUX ~ Warming*Removal*Elevation + (1|Plot.ID), data=comball) 
anova(mod1)


####SEM Setup####

#Standardizing the data
comball$mean.VWC<-scale(comball$mean.VWC)
comball$EFFLUX<-scale(comball$EFFLUX)
comball$Tsch_C<-scale(comball$Tsch_C)
comball$Tair<-scale(comball$Tair)
comball$Tsoil<-scale(comball$Tsoil)
comball$mean.NDVI<-scale(comball$mean.NDVI)
comball$DOG<-scale(comball$DOG)
comball$Soil_Percent_C<-scale(comball$Soil_Percent_C)
comball$Soil_Percent_N<-scale(comball$Soil_Percent_N)
comball$Soil_CN<-scale(comball$Soil_CN)
comball$Bare_Soil<-scale(comball$Bare_Soil)
comball$Total_Plant_Cover_All<-scale(comball$Total_Plant_Cover_All)
comball$pH<-scale(comball$pH)
comball$mean_temp_date_air<-scale(comball$mean_temp_date_air)
comball$mean_temp_date_soil<-scale(comball$mean_temp_date_soil)
comball$max_temp_air<-scale(comball$max_temp_air)
comball$min_temp_air<-scale(comball$min_temp_air)
comball$max_temp_soil<-scale(comball$max_temp_soil)
comball$min_temp_soil<-scale(comball$min_temp_soil)
comball$Percent.Functional...Diversity<-scale(comball$Percent.Functional...Diversity)
comball$Phosphatase<-scale(comball$Phosphatase)
comball$PhosD<-scale(comball$PhosD)
comball$B.gluc<-scale(comball$B.gluc)
comball$NAG<-scale(comball$NAG)
comball$B.cell<-scale(comball$B.cell)
comball$B.xylo<-scale(comball$B.xylo)
comball$a.gluc<-scale(comball$a.gluc)
comball$LAP<-scale(comball$LAP)
comball$Phenol<-scale(comball$Phenol)
comball$Perox<-scale(comball$Perox)
comball$MicrobialBiomassC<-scale(comball$MicrobialBiomassC)
comball$MicrobialBiomassN<-scale(comball$MicrobialBiomassN)
comball$Graminoid<-scale(comball$Graminoid)
comball$Forb<-scale(comball$Forb)
comball$Woody<-scale(comball$Woody)
comball$Legume<-scale(comball$Legume)
comball$Cryptogram<-scale(comball$Cryptogram)
comball$Species_Richness<-scale(comball$Species_Richness)
comball$Total_Cover_Scaled<-scale(comball$Total_Cover_Scaled)
comball$ug_NH4_week_cm2<-scale(comball$ug_NH4_week_cm2)
comball$ug_NO3_week_cm2<-scale(comball$ug_NO3_week_cm2)
comball$Inorganic_N<-scale(comball$Inorganic_N)
comball$ug_TP_week_cm2<-scale(comball$ug_TP_week_cm2)
comball$NP<-scale(comball$NP)
comball$pH<-scale(comball$pH)
comball$Soil_Percent_C<-scale(comball$Soil_Percent_C)
comball$Soil_Percent_N<-scale(comball$Soil_Percent_N)
comball$Soil_CN<-scale(comball$Soil_CN)
comball$Bare_Soil<-scale(comball$Bare_Soil)
comball$Litter<-scale(comball$Litter)
comball$Rock<-scale(comball$Rock)
comball$Graminoid<-scale(comball$Graminoid)
comball$Forb<-scale(comball$Forb)
comball$Woody<-scale(comball$ug_NH4_week_cm2)
comball$ug_NH4_week_cm2<-scale(comball$Woody)
comball$Legume<-scale(comball$Legume)
comball$Cryptogram<-scale(comball$Cryptogram)
comball$Total_Plant_Cover_All<-scale(comball$Total_Plant_Cover_All)
comball$Total_Plant_Cover_Vascular_Only<-scale(comball$Total_Plant_Cover_Vascular_Only)
comball$Graminoid_Relative<-scale(comball$Graminoid_Relative)
comball$Forb_Relative<-scale(comball$Forb_Relative)
comball$Legume_Relative<-scale(comball$Legume_Relative)
comball$Woody_Relative<-scale(comball$Woody_Relative)
comball$Cryptogram_Relative<-scale(comball$Cryptogram_Relative)
comball$Species_Richness<-scale(comball$Species_Richness)
comball$Total_Cover_Scaled<-scale(comball$Total_Cover_Scaled)
comball$PCA_PlantComm_Loadings<-scale(comball$PCA_PlantComm_Loadings)
comball$Week<-scale(comball$Week)






#Select variables and elevation to work with (if desired)
dat.working<-subset(comball, select = c(Warming, DOG, Week, EFFLUX, pH,
                                   Hour, mean.VWC, mean.NDVI,
                                   Tsoil, mean_temp_date_air, max_temp_air, min_temp_air, min_temp_soil,
                                   Percent.Functional...Diversity, MicrobialBiomassN, MicrobialBiomassC,
                                   Perox, Phenol, PhosD, Phosphatase, LAP, a.gluc, B.cell, B.xylo,
                                   NAG, B.gluc, Graminoid, Forb, Woody, Legume, Species_Richness, 
                                   Total_Cover_Scaled, Total_Plant_Cover_All, Bare_Soil, Soil_Percent_C,
                                   Soil_Percent_N, Soil_CN, Warming, Elevation, Removal, mean_temp_date_soil) )

dat.working <- dat.working[complete.cases(dat.working$EFFLUX), ]

#dat.working <- mice(dat.working, remove_collinear = F)


#Exampine all variables (if desired)####

#Select variables
dat.temp<-subset(comball, select = c(DOG, Elevation, Warming, Week, EFFLUX, Tsch_C, Tair, Tsoil, mean_temp_date_air, mean_temp_date_soil, max_temp_air, max_temp_soil, min_temp_air, min_temp_soil) )
dat.temp.W<-subset(dat.temp, Warming=='W') # select  
dat.temp.A<-subset(dat.temp, Warming=='A') # select 

#Use Leaps to select most influential https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9
library(leaps)

lm1 <- lm(EFFLUX ~  Tair + Tsoil + mean_temp_date_air + mean_temp_date_soil + max_temp_air + max_temp_soil + min_temp_air + min_temp_soil, data=dat.temp)
summary(lm1)

Best_Subset <-
   regsubsets(EFFLUX ~ Tair + Tsoil + mean_temp_date_air + mean_temp_date_soil + max_temp_air + max_temp_soil + min_temp_air + min_temp_soil,
              data =dat.temp,
              nbest = 1,      # 1 best model for each number of predictors
              nvmax = NULL,    # NULL for no limit on number of variables
              force.in = NULL, force.out = NULL,
              method = "exhaustive")

summary_best_subset <- summary(Best_Subset)
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)

summary_best_subset$which[5,]

#Continue with Tsoil, mean_temp_date_air, max_temp_air, min_temp_air, min_temp_soil


#Exampine Temp variables####

#Select variables
dat.temp<-subset(comball, select = c(DOG, Elevation, Warming, Week, EFFLUX, Tsch_C, Tair, Tsoil, mean_temp_date_air, mean_temp_date_soil, max_temp_air, max_temp_soil, min_temp_air, min_temp_soil) )
dat.temp.W<-subset(dat.temp, Warming=='W') # select  
dat.temp.A<-subset(dat.temp, Warming=='A') # select 

#Use Leaps to select most influential https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9
library(leaps)

lm1 <- lm(EFFLUX ~  Tair + Tsoil + mean_temp_date_air + mean_temp_date_soil + max_temp_air + max_temp_soil + min_temp_air + min_temp_soil, data=dat.temp)
summary(lm1)

Best_Subset <-
   regsubsets(EFFLUX ~ Tair + Tsoil + mean_temp_date_air + mean_temp_date_soil + max_temp_air + max_temp_soil + min_temp_air + min_temp_soil,
              data =dat.temp,
              nbest = 1,      # 1 best model for each number of predictors
              nvmax = NULL,    # NULL for no limit on number of variables
              force.in = NULL, force.out = NULL,
              method = "exhaustive")

summary_best_subset <- summary(Best_Subset)
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)

summary_best_subset$which[5,]

#Continue with Tsoil, mean_temp_date_air, max_temp_air, min_temp_air, min_temp_soil

####Specify the model
#Youtube explaining indirect effects and categoricals: https://www.youtube.com/watch?v=nx7PYvczXWg&ab_channel=MikeCrowson

mod1<-"
   EFFLUX~ a*mean_temp_date_soil + c*Total_Cover_Scaled + b*Warming
   mean.VWC~ mean_temp_date_soil + pH + Tsoil
   mean.NDVI~ Bare_Soil  + DOG
   Soil_CN~Soil_Percent_C + Bare_Soil
   Tsoil~mean.NDVI + DOG 
   
   #indirect effect
   ac:=a*c
  
   #total effect
   total:=b+(a*c)
"

#Estimate the parameters for the model
model.est<-sem(mod1, data=dat.working, fixed.x=FALSE, estimator="MLF", missing="ML")

vartable(model.est)
lavInspect(model.est, "cov.lv")
semPaths(model.est)

#Prints the fit stats
model.est

#Extract information for the fitted object
summary(model.est)

parameterEstimates(model.est, standardized=T)

#The search for missing links
#Request modification indices greater than 3.0
#modification indices tell you the expected reduction in chi square if term is added to the model
mil<-modindices(model.est); print(mil[mil$mi>3.0,])


#Adding largest mi values first (starting with EFFLUX ~~ Week)
mod2<-"
  EFFLUX~ a*mean_temp_date_soil + c*Total_Cover_Scaled + b*Warming + DOG
   mean.VWC~ mean_temp_date_soil + pH + Tsoil
   mean.NDVI~ Bare_Soil  + DOG
   Soil_CN~Soil_Percent_C + Bare_Soil
   Tsoil~mean.NDVI + DOG 
   
   #indirect effect
   ac:=a*c
  
   #total effect
   total:=b+(a*c)
"

#Estimate the parameters for the model
model.est2<-sem(mod2, data=dat.working, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est, model.est2)

mi2<-modindices(model.est2); print(mi2[mi2$mi>3.0,])

#Adding
mod3<-"
  EFFLUX~ a*mean_temp_date_soil + c*Total_Cover_Scaled + b*Warming + DOG + Soil_Percent_C
   mean.VWC~ mean_temp_date_soil + pH + Tsoil
   mean.NDVI~ Bare_Soil  + DOG
   Soil_CN~Soil_Percent_C + Bare_Soil
   Tsoil~mean.NDVI + DOG 
   
   #indirect effect
   ac:=a*c
  
   #total effect
   total:=b+(a*c)
"


#Estimate the parameters for the model
model.est3<-sem(mod3, data=dat.working, fixed.x=FALSE, estimator="MLF", missing="ML")

summary(model.est3)

#Comparing fit
anova(model.est2, model.est3)

mi3<-modindices(model.est3); print(mi3[mi3$mi>3.0,])



#
mod4<-"
   EFFLUX~mean.NDVI  + Soil_CN + Soil_Percent_C + Soil_Percent_N + Tsoil + 
      max_temp_air + min_temp_air + min_temp_soil +
      MicrobialBiomassN + Legume + 
      Bare_Soil + Soil_Percent_C + Soil_Percent_N + Soil_CN
   mean.VWC~ DOG + Soil_CN
   mean.NDVI~Bare_Soil  + DOG

   Tsoil~~mean_temp_date_air 
   max_temp_air~~mean_temp_date_air 
   min_temp_air~~mean_temp_date_air 
   min_temp_air~~min_temp_soil
   mean.VWC~~Tsoil+max_temp_air
"

model.est4<-sem(mod4, data=dat.working, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est3, model.est4)

#
summary(model.est4)
mi4<-modindices(model.est4); print(mi4[mi4$mi>3.0,])


#
mod5<-"
   EFFLUX~mean.NDVI  + Soil_CN + Soil_Percent_C + Soil_Percent_N + Tsoil + 
      max_temp_air + min_temp_air + min_temp_soil +
      MicrobialBiomassN + Legume + 
      Bare_Soil + Soil_Percent_C + Soil_Percent_N + Soil_CN
   mean.VWC~ DOG + Soil_CN
   mean.NDVI~Bare_Soil  + DOG

   Tsoil~~mean_temp_date_air 
   max_temp_air~~mean_temp_date_air 
   min_temp_air~~mean_temp_date_air 
   min_temp_air~~min_temp_soil
"

model.est5<-sem(mod5, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est5, model.est4)

summary(model.est5)
mi5<-modindices(model.est5); print(mi5[mi5$mi>3.0,])


#
mod6<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair
   Tair~DOG
   Dominant~Bareground
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est6<-sem(mod6, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est5, model.est6)

summary(model.est6)
mi6<-modindices(model.est6); print(mi6[mi6$mi>3.0,])


#
mod7<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est7<-sem(mod7, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est7, model.est6)

summary(model.est7)
mi7<-modindices(model.est7); print(mi7[mi7$mi>3.0,])


#
mod8<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est8<-sem(mod8, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est7, model.est8)

summary(model.est8)
mi8<-modindices(model.est8); print(mi8[mi8$mi>3.0,])


#
mod9<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est9<-sem(mod9, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est9, model.est8)

summary(model.est9)
mi9<-modindices(model.est9); print(mi9[mi9$mi>3.0,])


#
mod10<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est10<-sem(mod10, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est9, model.est10)

summary(model.est10)
mi10<-modindices(model.est10); print(mi10[mi10$mi>3.0,])


#
mod11<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est11<-sem(mod11, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est11, model.est10)

summary(model.est11)
mi11<-modindices(model.est11); print(mi11[mi11$mi>3.0,])


#
mod12<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~Bareground + Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
"

model.est12<-sem(mod12, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est11, model.est12)

summary(model.est12)
mi12<-modindices(model.est12); print(mi12[mi12$mi>3.0,])



#
mod13<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen
   pH~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est13<-sem(mod13, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est13, model.est12)

summary(model.est13)
mi13<-modindices(model.est13); print(mi13[mi13$mi>3.0,])

#
mod14<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen
   pH~C.N + Carbon + Nitrogen

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est14<-sem(mod14, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est13, model.est14)

summary(model.est14)
mi14<-modindices(model.est14); print(mi14[mi14$mi>3.0,])

#
mod15<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est15<-sem(mod15, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est15, model.est14)

summary(model.est15)

mi15<-modindices(model.est15); print(mi15[mi15$mi>3.0,])


#
mod16<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen
   Bareground~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est16<-sem(mod16, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est15, model.est16)

mi16<-modindices(model.est16); print(mi16[mi16$mi>3.0,])


#
mod17<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen + mean.VWC
   Bareground~C.N

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est17<-sem(mod17, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est17, model.est16)

mi17<-modindices(model.est17); print(mi17[mi17$mi>3.0,])




#
mod18<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
"

model.est18<-sem(mod18, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est17, model.est18)

mi18<-modindices(model.est18); print(mi18[mi18$mi>3.0,])



#
mod19<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est19<-sem(mod19, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est19, model.est18)

mi19<-modindices(model.est19); print(mi19[mi19$mi>3.0,])




#
mod20<-"
   mean.NDVI~mean.VWC + DOG + Bareground + Dominant + C.N
   EFFLUX~mean.NDVI + Tair + DOG  + mean.VWC + DOG + pH + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~C.N + Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est20<-sem(mod20, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est19, model.est20)

mi20<-modindices(model.est20); print(mi20[mi20$mi>3.0,])

summary(model.est20)




####STOP - Removing Pieces###

mod16<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~mean.NDVI + Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen + Bareground
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground 
   Tsoil~mean.NDVI + DOG
   Tair~DOG

   Tsoil~~Tair
"

model.est16<-sem(mod16, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est20, model.est16)

summary(model.est16)

#

mod17<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~ Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen + Bareground
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground 
   Tsoil~mean.NDVI + DOG
   Tair~DOG

   Tsoil~~Tair
"

model.est17<-sem(mod17, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est17, model.est16)

summary(model.est17)


#
mod18<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~ Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen + Bareground
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground 
   Tsoil~mean.NDVI 
   Tair~DOG

   Tsoil~~Tair
"

model.est18<-sem(mod18, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est18, model.est17)

summary(model.est18)


#
mod19<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~ Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen + Bareground
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground 
   Tsoil~mean.NDVI 
   Tair~DOG

   Tsoil~~Tair
"

model.est19<-sem(mod19, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est19, model.est18)

summary(model.est19)


#
mod20<-"
   mean.NDVI~ DOG + Bareground + Dominant + C.N
   EFFLUX~ Tair + DOG  + DOG + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~ Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est20<-sem(mod20, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est19, model.est20)

summary(model.est20)

#
mod21<-"
   mean.NDVI~ DOG + Bareground + C.N
   EFFLUX~ Tair + DOG  + DOG + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~Tsoil + Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~ Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est21<-sem(mod21, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est21, model.est20)

summary(model.est21)


#
mod22<-"
   mean.NDVI~ DOG + Bareground + C.N
   EFFLUX~ Tair + DOG  + DOG + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~ Tair + DOG + C.N + Carbon + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~ Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est22<-sem(mod22, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est21, model.est22)

summary(model.est22)


#
mod23<-"
   mean.NDVI~ DOG + Bareground + C.N
   EFFLUX~ Tair + DOG  + DOG + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~ Tair + DOG + C.N + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~ Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est23<-sem(mod23, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est23, model.est22)

summary(model.est23)


#
mod24<-"
   mean.NDVI~ DOG + Bareground + C.N
   EFFLUX~ Tair + DOG  + DOG + C.N + Carbon + Nitrogen + Tsoil + Bareground
   mean.VWC~ Tair + DOG + C.N + Nitrogen + Bareground
   Tair~DOG
   Dominant~ Carbon + Nitrogen + C.N
   pH~ Carbon + Nitrogen + mean.VWC
   Bareground~C.N
   Carbon~Bareground
   Nitrogen~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   Nitrogen~~C.N
   Carbon~~Nitrogen
   pH~~Bareground
   Bareground~~Dominant
   mean.NDVI~~Tsoil
"

model.est24<-sem(mod24, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est23, model.est24)

summary(model.est24)



#
mod25<-"
   mean.NDVI~mean.VWC + DOG + Bareground
   EFFLUX~ Tair  + mean.VWC  + Carbon + Nitrogen
   mean.VWC~Tsoil + Tair + DOG + Nitrogen
   Tair~DOG
   Dominant~Bareground + pH
   Carbon~Nitrogen + Dominant
   Nitrogen~Carbon + Dominant
   pH~Carbon + Bareground
   Tsoil~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   pH~~C.N
   mean.VWC~~pH
"

model.est25<-sem(mod25, data=dat2, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est25, model.est24)

summary(model.est25)
mi14<-modindices(model.est14); print(mi14[mi14$mi>3.0,])




#
mod26<-"
   mean.NDVI~mean.VWC + DOG + Bareground
   EFFLUX~ Tair  + mean.VWC  + Carbon + Nitrogen
   mean.VWC~Tsoil + Tair + DOG + Nitrogen
   Tair~DOG
   Dominant~Bareground + pH
   Carbon~Nitrogen + Dominant
   Nitrogen~Carbon + Dominant
   pH~Carbon + Bareground
   Tsoil~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   pH~~C.N
   mean.VWC~~pH
"

model.est26<-sem(mod26, data=dat2, fixed.x=FALSE, estimator="MLF", missing="ML")

#Comparing fit
anova(model.est25, model.est26)

summary(model.est26)
mi26<-modindices(model.est26); print(mi26[mi26$mi>3.0,])







####Final low elevation####

modlow<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~mean.NDVI + Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground
   Tsoil~mean.NDVI + DOG

   Tsoil~~Tair
"

model.estlow<-sem(modlow, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

summary(model.estlow, standardized = TRUE)
summary(model.estlow, fit.measures=TRUE)



#New final low with additional data (11-14-21)

mod24<-"
  mean.NDVI~C.N  + Bareground  + DOG
   EFFLUX~ Tair + DOG  + Tsoil + C.N + Carbon + Nitrogen + Bareground
   mean.VWC~Tair + DOG + C.N + pH
   Dominant~Bareground + pH + Carbon + Nitrogen + C.N
   C.N~Carbon + Nitrogen + Bareground 
   Tsoil~mean.NDVI 
   Tair~DOG

   Tsoil~~Tair
"

model.est24<-sem(mod24, data=dat3, fixed.x=FALSE, estimator="MLF", missing="ML")

summary(model.est24)


####Final high elevation####


modhigh<-"
   mean.NDVI~mean.VWC + DOG + Bareground
   EFFLUX~ Tair  + mean.VWC  + Carbon + Nitrogen + DOG
   mean.VWC~Tsoil + Tair + DOG + Nitrogen
   Tair~DOG
   Dominant~Bareground + pH
   Carbon~Nitrogen + Dominant
   Nitrogen~Carbon + Dominant
   pH~Carbon + Bareground
   Tsoil~Bareground

   Tsoil~~Tair
   Carbon~~C.N
   pH~~C.N
   mean.VWC~~pH
"

model.esthigh<-sem(modhigh, data=dat4, fixed.x=FALSE, estimator="MLF", missing="ML")


summary(model.esthigh, standardized = TRUE)



####Comparing Models####

library(nonnest2)
vuongtest(model.esthigh, model.estlow, nested = FALSE, adj = "none")
if p> 0.05 --> models are indistiguishable
if p < 0.05 --> run the second analysis:
   icci(fit1, fit, conf.level = 0.95)


install.packages("nonnest2", dep=T)
library(nonnest2)
vuongtest(fit1, fit2, nested = FALSE, adj = "none")

