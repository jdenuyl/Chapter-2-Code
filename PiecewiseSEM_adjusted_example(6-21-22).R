####### SUPPLEMENTARY ANALYSES FOR PIECEWISE SEM #######

# Original author: Jon Lefcheck, jslefche@vims.edu 
# Last updated: 16 November 2015

####### 

install.packages('ape')
install.packages('caper')
install.packages('lavaan')
install.packages("piecewiseSEM")

# Load required libraries
library(ape) #Version 3.3
library(caper) # Vresion 0.5.2
library(nlme) # Version 3.1.122
library(lavaan) # Version 0.5.19
library(piecewiseSEM) # Version 1.0.0
library(sem.fit)

####### EXAMPLE 1: KELP FOREST (BYRNES ET AL. 2011) ####### 

setwd("C:/Users/locdenuylji/Desktop/Chapter-2-Code")
setwd("/Users/jdenuyl/Desktop/Chapter-2-Code")


# Read in data
kelp = read.csv("kelp.csv.csv")

# Convert -Inf to NA
kelp$max_Max.OV[which(kelp$max_Max.OV == -Inf)] = NA

# Log response variables to mirror original analysis of Byrnes et al 2011
kelp$kelp = log(kelp$kelp+1)
kelp$prev.kelp = log(kelp$prev.kelp + 1)
kelp[, 18:23] = log(kelp[, 18:23] + 1)

# Remove rows where response or predictors are NA
vars = c("SITE", "TRANSECT", "YEAR", "max_Max.OV", "prev.kelp", "habitat", "spring_canopy_150", "kelp", 
         "algae_richness", "sessile_invert_richness", "mobile_richness", "richness", "consumer_richness", "linkdensity")
kelp = kelp[, vars]
kelp = na.omit(kelp)

# Create interaction term
kelp$wave_kelp_int = kelp$max_Max.OV * kelp$prev.kelp

# Replicate original analysis

# List structured equations for lavaan
kelp_model = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat

  kelp ~ max_Max.OV + prev.kelp  + habitat + spring_canopy_150

  richness ~ kelp   + prev.kelp  + habitat + spring_canopy_150

  linkdensity ~ richness  + kelp  + prev.kelp  + habitat + spring_canopy_150
'

# Fit vcov SEM
kelp_model.sem = sem(kelp_model, kelp, estimator = "MLM")

# Summary output with standardized coefficients
summary(kelp_model.sem, standardize = TRUE)

# Get R2 for models
inspect(kelp_model.sem, "rsquare")

####### 

# Now fit piecewise model with random effect

# Create component models and store in list
kelp_pSEM_randomList = list(
  
  # Predicting spring kelp canopy
  spring_canopy_150 = lme(spring_canopy_150 ~ max_Max.OV * prev.kelp + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting summer kelp density
  kelp = lme(kelp ~ max_Max.OV + prev.kelp  + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp),
  
  # Predicting total richness
  richness = lme(richness ~ kelp + prev.kelp + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp),
  
  # Predict linkage density
  linkdensity = lme(linkdensity ~ richness + kelp + prev.kelp + habitat + spring_canopy_150, random = ~ 1 | SITE, data = kelp)
  
)

# I had to edit the following as it had been changed ... https://cran.r-project.org/web/packages/piecewiseSEM/vignettes/piecewiseSEM.html
kelp_psem <- as.psem(kelp_pSEM_randomList)
summary.kelp<-summary(kelp_psem)
#Summary contains R2 value
summary.kelp
AIC(kelp_psem)

#See d-sep tests
summary.kelp$dTable

#Find Fisher’s C statistic, and the results from the χ2 test
summary.kelp$Cstat

#Path coefficients and the standardized estimates
summary.kelp$coefficients

####### 

# Fit piecewise SEM with random AND autocorrelation structures
kelp_pSEM_CAR1List = list(
  
  # Predicting spring kelp canopy
  spring_canopy_150 = lme(spring_canopy_150 ~ max_Max.OV * prev.kelp + habitat,
                          random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predicting summer kelp density
  kelp = lme(kelp ~ max_Max.OV * prev.kelp  + habitat + spring_canopy_150,
             random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predicting total richness
  richness = lme(richness ~ kelp + prev.kelp + habitat + spring_canopy_150,
                 random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp),
  
  # Predict linkage density
  linkdensity = lme(linkdensity ~ richness + kelp + prev.kelp + habitat + spring_canopy_150,
                    random = ~ 1 | SITE/TRANSECT, correlation = corCAR1(form = ~ YEAR), data = kelp)
  
)

# I had to edit, same as above
kelp_psem.car <- as.psem(kelp_pSEM_CAR1List)
summary.kelp.car<-summary(kelp_psem.car)
#Summary contains R2 value
summary.kelp.car
AIC(kelp_psem.car)

#See d-sep tests
summary.kelp.car$dTable

#Find Fisher’s C statistic, and the results from the χ2 test
summary.kelp.car$Cstat

#Path coefficients and the standardized estimates
summary.kelp.car$coefficients

####### 

# Break apart total richness into resource (algal & sessile inverts) and consumer components

# List structured equations for lavaan
kelp_model_richness = '
  spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat
  
  kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150 + habitat
  
  algae_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  sessile_invert_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  mobile_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat 
  
  mobile_richness ~~ algae_richness
  mobile_richness ~~ sessile_invert_richness
  sessile_invert_richness ~~ algae_richness
'
# Fit vcov SEM
kelp_model_richness.sem = sem(kelp_model_richness, kelp, estimator = "MLM")

# Return coefficients
summary(kelp_model_richness.sem, standardize = TRUE)

# Fit with piecewise

# Create component models and store in list
kelp_richness_pSEM_randomList = list(
  
  # Predicting spring kelp canopy
  spring_canopy_150 = lme(spring_canopy_150 ~ max_Max.OV + prev.kelp + wave_kelp_int + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting summer kelp density
  kelp = lme(kelp ~ max_Max.OV + prev.kelp  + spring_canopy_150 + habitat, random = ~ 1 | SITE, data = kelp),
  
  # Predicting richness
  algae_richness = lme(algae_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat , random = ~ 1 | SITE, data = kelp),
  
  sessile_invert_richness = lme(sessile_invert_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat , random = ~ 1 | SITE, data = kelp),
  
  mobile_richness = lme(mobile_richness ~ max_Max.OV + kelp + prev.kelp + spring_canopy_150 + habitat, random = ~ 1 | SITE, data = kelp)
  
)

# I had to edit, same as above
kelp_psem.rich <- as.psem(kelp_richness_pSEM_randomList)
summary.kelp.rich<-summary(kelp_psem.rich)
#Summary contains R2 value
summary.kelp.rich
AIC(kelp_psem.rich)

#See d-sep tests
summary.kelp.rich$dTable

#Find Fisher’s C statistic, and the results from the χ2 test
summary.kelp.rich$Cstat

#Path coefficients and the standardized estimates
summary.kelp.rich$coefficients


