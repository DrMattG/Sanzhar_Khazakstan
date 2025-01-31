#############################################################################################
## ---- Sanzhar Occupancy Models ------------------------------------------------------
#############################################################################################

## ---- load packages -------------------------------------------
# install.packages("unmarked") # first time only
library(unmarked)
library(tidyverse)
library(readxl)
library(AICcmodavg)

#############################################################################################
## ---- Single Species Model ------------------------------------------------------
#############################################################################################

# Load detection history - Bears
detection_history_bear <- read_excel(here::here("data/Data_for_model.xlsx"), sheet = "Bear")

# Examine data
head(detection_history_bear)

View(detection_history_bear)

detection_history_bear<-detection_history_bear |> 
  mutate(across(10:68, ~ na_if(., "N/A"))) |> 
  mutate(across(10:68, as.numeric))

# Create unmarkedFrameOccu that holds the data
sample.unmarkedFrame_simple <- unmarkedFrameOccu( # y is a matrix with observed detection history 
  # (0's and 1's, one row per site, one column per survey)
  y = as.matrix(detection_history_bear[10:68])) 

# S4 class for occupancy model data
summary(sample.unmarkedFrame_simple)



## ----buildbasicoccu------------------------------------------------------
# Build basic single-season occupancy model with intercepts only (one estimate for detection, one for occupancy)
occu.m1 <- occu(formula = ~1 # detection formula first
                ~1, # occupancy formula second, 
                data = sample.unmarkedFrame_simple)

summary(occu.m1) # Show AIC, estimates (on logit scale), SE, z-scores

# To get real estimate of occupancy (with 95% CI)
predict(occu.m1, 
        newdata = data.frame(site = 1),
        type = "state")

# To get real estimate of detection (with 95% CI)
predict(occu.m1, 
        newdata = data.frame(site = 1),
        type = "det")

# Equivalent to inverse logit
boot::inv.logit(coef(occu.m1)[1]) # Real estimate of occupancy
boot::inv.logit(coef(occu.m1)[2]) # Real estimate of detection


## ----covariatesload------------------------------------------------------
# Load covariate data
# 
site_cov <- detection_history_bear[5:9]
# Convert to numeric
site_cov$Elevation<- as.numeric(site_cov$Elevation)

site_cov <- site_cov %>%
  rename(
    Dist_road = 'Distance to the nearest road km',
    Dist_village = 'Distance to nearest town/village km') %>%
    select(-Notes)

# scale the site covariates

site_cov <- site_cov %>%
  mutate(across(everything(), ~ scale(.x, center = TRUE, scale = TRUE)))

# Build a new unmarkedFramOccu
sample.unmarkedFrame_cov <- unmarkedFrameOccu( # y is a matrix with observed detection history 
  # (0's and 1's, one row per site, one column per survey)
  y = as.matrix(detection_history_bear[10:68]),
  # obsCovs = observation covariates in a list, 
  # each variable has site rows x survey columns
  #obsCovs = list(effort = effort,
   #              observers = observers),
  # siteCovs = dataframe with site rows x column variables
  siteCovs = site_cov) 

# S4 class for occupancy model data
summary(sample.unmarkedFrame_cov)

## ----buildoccucov--------------------------------------------------------

## Tried some different types of models here:


# # make some starting values (I DONT KNOW WHAT I AM DOING HERE, JUST PLAYING AROUND)
# starting_values <- c(
#   intercept_detection = 0.1,
#   elevation_effect = 0.01,
#   year_effect = 0.01,
#   additional_param = 0.01
# )

# probably not enough variation in the covariates to run the model

summary(sample.unmarkedFrame_cov@siteCovs)
 occu.m1 <- occu(formula = ~ 1 # detection formula first
                 ~ Elevation + Year, # occupancy formula second,
                 data = sample.unmarkedFrame_cov)

# occu.m2 <- occu(formula = ~ 1 # detection formula first
#                 ~ Elevation , # occupancy formula second,
#                 data = sample.unmarkedFrame_cov)


occu.m2 <- occu(formula = ~ 1 # detection formula first
                 ~ Dist_road , # occupancy formula second,
                 data = sample.unmarkedFrame_cov)


occu.m3 <- occu(formula = ~ 1 # detection formula first
                ~ Dist_village , # occupancy formula second,
                data = sample.unmarkedFrame_cov)

occu.m4 <- occu(formula = ~ 1 # detection formula first
                ~  Elevation + Year+Dist_village , # occupancy formula second,
                data = sample.unmarkedFrame_cov)


# Summarize
summary(occu.m1)
summary(occu.m2)# does not converge
summary(occu.m3)
summary(occu.m4)
## ----covpredict----------
# Predict effect on new data set to see how occupancy changes with `Dist_village`
predict_m3_Dist <- cbind(predict(occu.m2,
                                 newdata = data.frame(Dist_village = seq(min(site_cov$Dist_village, 
                                                                             na.rm = TRUE),
                                                                         max(site_cov$Dist_village, 
                                                                             na.rm = TRUE), 
                                                                         by = 0.01)),
                                 type = "state"),
                         data.frame(Dist_village = seq(min(site_cov$Dist_village, 
                                                           na.rm = TRUE),
                                                       max(site_cov$Dist_village, 
                                                           na.rm = TRUE), 
                                                       by = 0.01)))

## ----plotrelationships----
# Plot relationship with Dist_village
ggplot(data = predict_m2_Dist, aes(x = Dist_village, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Distance to village (scaled)", y = "Predicted Occupancy Probability") +
  theme_classic()


## ----mbgof---------------------------------------------------------------

# Do Mackenzie-Bailey goodness of fit test for single-season occupancy model
m2_mb.gof.boot <- mb.gof.test(occu.m2,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m2_mb.gof.boot


#############################################################################################
## ---- Multispecies Model ------------------------------------------------------
#############################################################################################

# bring in data
detection_history_moose <- read_excel(here::here("data/Data_for_model.xlsx"), sheet = "Moose")
detection_history_reddeer <- read_excel(here::here("data/Data_for_model.xlsx"),sheet = "Red Deer")
detection_history_roedeer <- read_excel(here::here("data/Data_for_model.xlsx"), sheet = "Roe Deer")



## moose ------------------------------------------------------------------------------
detection_history_moose<-detection_history_moose |> 
  mutate(across(10:68, ~ na_if(., "N/A"))) |> 
  mutate(across(10:68, as.numeric))

# Create unmarkedFrameOccu that holds the data
sample.unmarkedFrame_simple <- unmarkedFrameOccu( # y is a matrix with observed detection history 
  # (0's and 1's, one row per site, one column per survey)
  y = as.matrix(detection_history_moose[10:68])) 

# S4 class for occupancy model data
summary(sample.unmarkedFrame_simple)


## reddeer ------------------------------------------------------------------------------
detection_history_reddeer<-detection_history_reddeer |> 
  mutate(across(10:68, ~ na_if(., "N/A"))) |> 
  mutate(across(10:68, as.numeric))

# Create unmarkedFrameOccu that holds the data
sample.unmarkedFrame_simple <- unmarkedFrameOccu( # y is a matrix with observed detection history 
  # (0's and 1's, one row per site, one column per survey)
  y = as.matrix(detection_history_reddeer[10:68])) 

# S4 class for occupancy model data
summary(sample.unmarkedFrame_simple)



## roedeer ------------------------------------------------------------------------------
detection_history_roedeer<-detection_history_roedeer |> 
  mutate(across(10:68, ~ na_if(., "N/A"))) |> 
  mutate(across(10:68, as.numeric))

# Create unmarkedFrameOccu that holds the data
sample.unmarkedFrame_simple <- unmarkedFrameOccu( # y is a matrix with observed detection history 
  # (0's and 1's, one row per site, one column per survey)
  y = as.matrix(detection_history_roedeer[10:68])) 

# S4 class for occupancy model data
summary(sample.unmarkedFrame_simple)


