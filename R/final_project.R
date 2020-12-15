#required packages
library(dplyr)
library(tidyr)
library(survey)

#download data, permanent copy can be found in github repo also 
cbecs <- read.csv("https://www.eia.gov/consumption/commercial/data/2012/xls/2012_public_use_data_aug2016.csv")

#filter down to relevant variables
cbecs <- cbecs %>% select(PUBID, CENDIV, FREESTN, SQFT, YRCON, ELCNS, FINALWT:FINALWT197)

#rename variables to be more readable 
cbecs <- dplyr::rename(cbecs, Division = CENDIV, Freestanding = FREESTN,
                       NWEIGHT = FINALWT)

#map division numbers to names 
cbecs$Division <- plyr::mapvalues(cbecs$Division, from = c(1,2,3,4,5,6,7,8,9),
                                      to = c("New England","Middle Atlantic","East North Central",
                                             "West North Central", "South Atlantic","East South Central","West South Central",
                                             "Mountain","Pacific"))

#the freestanding variable stores 0s as NAs by default, change for modelling
cbecs <- cbecs %>% mutate(Freestanding = replace_na(Freestanding, 0))

#we only want to focus on 1946 and after
cbecs <- cbecs %>% filter(YRCON != 995)

#filter out 0s 
cbecs <- cbecs %>% filter(ELCNS != 0)

#drop missing values
cbecs <- cbecs %>% drop_na()

#survey weighting with jackknife technique
mod <- svrepdesign(weights=cbecs$NWEIGHT, repweights=(cbecs %>% select(matches("FINALWT"))), type="JK1", mse=TRUE,data=cbecs)

#our model 
gl <- svyglm(ELCNS ~ YRCON + bs(SQFT, 3) + Division + Freestanding, design=mod)