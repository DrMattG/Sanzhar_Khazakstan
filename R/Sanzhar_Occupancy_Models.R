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
detection_history_bear <- read_excel(here::here("data/Data_for_model_new.xlsx"), sheet = "Bear")

# Examine data
#head(detection_history_bear)

#View(detection_history_bear)

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
# occu.m1 <- occu(formula = ~1 # detection formula first
#                 ~1, # occupancy formula second, 
#                 data = sample.unmarkedFrame_simple)
# 
# summary(occu.m1) # Show AIC, estimates (on logit scale), SE, z-scores
# 
# # To get real estimate of occupancy (with 95% CI)
# predict(occu.m1, 
#         newdata = data.frame(site = 1),
#         type = "state")
# 
# # To get real estimate of detection (with 95% CI)
# predict(occu.m1, 
#         newdata = data.frame(site = 1),
#         type = "det")
# 
# # Equivalent to inverse logit
# boot::inv.logit(coef(occu.m1)[1]) # Real estimate of occupancy
# boot::inv.logit(coef(occu.m1)[2]) # Real estimate of detection
# 

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

dist_village_mean <- mean(site_cov$Dist_village, na.rm = TRUE)
dist_village_sd <- sd(site_cov$Dist_village, na.rm = TRUE)
dist_road_mean <- mean(site_cov$Dist_road, na.rm = TRUE)
dist_road_sd <- sd(site_cov$Dist_road, na.rm = TRUE)
elevation_mean <- mean(site_cov$Elevation, na.rm = TRUE)
elevation_sd <- sd(site_cov$Elevation, na.rm = TRUE)
year_mean <- mean(site_cov$Year, na.rm = TRUE)
year_sd <- sd(site_cov$Year, na.rm = TRUE)


# scale the site covariates

site_cov <- site_cov %>%
  mutate(across(everything(), ~ scale(.x, center = TRUE, scale = TRUE)))

site_cov <- as.data.frame(scale(site_cov))

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

 occu.m2 <- occu(formula = ~ 1 # detection formula first
                 ~ Elevation , # occupancy formula second,
                 data = sample.unmarkedFrame_cov)


occu.m3 <- occu(formula = ~ 1 # detection formula first
                 ~ Dist_road , # occupancy formula second,
                 data = sample.unmarkedFrame_cov)


occu.m4 <- occu(formula = ~ 1 # detection formula first
                ~ Dist_village , # occupancy formula second,
                data = sample.unmarkedFrame_cov)

occu.m5 <- occu(formula = ~ 1 # detection formula first
                ~  Elevation + Year+Dist_village , # occupancy formula second,
                data = sample.unmarkedFrame_cov)


# Summarize
summary(occu.m1)
summary(occu.m2)
summary(occu.m3)
summary(occu.m4)
summary(occu.m5)

## ----covpredict----------
# Predict effect on new data set to see how occupancy changes with `Dist_village`
predict_m4_Dist <- cbind(predict(occu.m4,
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





predict_m4_Dist <- predict_m4_Dist %>%
  mutate(Dist_village_original = (Dist_village * dist_village_sd) + dist_village_mean)


## ----plotrelationships----
# Plot relationship with Dist_village
ggplot(data = predict_m4_Dist, aes(x = Dist_village, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Distance to village (scaled)", y = "Predicted Occupancy Probability") +
  theme_classic()

ggplot(data = predict_m4_Dist, aes(x = Dist_village_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  stat_smooth(method = "loess", col = "black", se = FALSE) +
  labs(x = "Distance to village (original scale)", y = "Predicted Occupancy Probability") +
  theme_classic()


predict_m1 <- cbind(
  predict(occu.m1, 
          newdata = data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01),
                               Year = mean(site_cov$Year)), 
          type = "state"),
  data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01))
)

predict_m1 <- predict_m1 %>%
  mutate(Elevation_original = (Elevation * elevation_sd) + elevation_mean)


predict_m2 <- cbind(
  predict(occu.m2, 
          newdata = data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01)), 
          type = "state"),
  data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01))
)

predict_m2 <- predict_m2 %>%
  mutate(Elevation_original = (Elevation * elevation_sd) + elevation_mean)

ggplot(data = predict_m2, aes(x = Elevation_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Elevation (m)", y = "Predicted Occupancy Probability") +
  theme_classic()






predict_m3 <- cbind(
  predict(occu.m3, 
          newdata = data.frame(Dist_road = seq(min(site_cov$Dist_road), max(site_cov$Dist_road), by = 0.01)), 
          type = "state"),
  data.frame(Dist_road = seq(min(site_cov$Dist_road), max(site_cov$Dist_road), by = 0.01))
)

predict_m3 <- predict_m3 %>%
  mutate(Dist_road_original = (Dist_road * dist_road_sd) + dist_road_mean)


ggplot(data = predict_m3, aes(x = Dist_road_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Distance to Road (m)", y = "Predicted Occupancy Probability") +
  theme_classic()

ggplot(data = predict_m1, aes(x = Elevation_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Elevation (m)", y = "Predicted Occupancy Probability") +
  theme_classic()



#(A) Vary Elevation, Keep Year & Distance to Village Constant
predict_m5_Elev <- cbind(
  predict(occu.m5, 
          newdata = data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01),
                               Year = mean(site_cov$Year, na.rm = TRUE),
                               Dist_village = mean(site_cov$Dist_village, na.rm = TRUE)), 
          type = "state"),
  data.frame(Elevation = seq(min(site_cov$Elevation), max(site_cov$Elevation), by = 0.01))
)

predict_m5_Elev <- predict_m5_Elev %>%
  mutate(Elevation_original = (Elevation * elevation_sd) + elevation_mean)
#(B) Vary Year, Keep Elevation & Distance to Village Constant
predict_m5_Year <- cbind(
  predict(occu.m5, 
          newdata = data.frame(Elevation = mean(site_cov$Elevation, na.rm = TRUE),
                               Year = seq(min(site_cov$Year), max(site_cov$Year), by = 0.01),
                               Dist_village = mean(site_cov$Dist_village, na.rm = TRUE)), 
          type = "state"),
  data.frame(Year = seq(min(site_cov$Year), max(site_cov$Year), by = 0.01))
)

predict_m5_Year <- predict_m5_Year %>%
  mutate(Year_original = (Year * year_sd) + year_mean)
#(C) Vary Distance to Village, Keep Elevation & Year Constant
predict_m5_Dist <- cbind(
  predict(occu.m5, 
          newdata = data.frame(Elevation = mean(site_cov$Elevation, na.rm = TRUE),
                               Year = mean(site_cov$Year, na.rm = TRUE),
                               Dist_village = seq(min(site_cov$Dist_village), max(site_cov$Dist_village), by = 0.01)), 
          type = "state"),
  data.frame(Dist_village = seq(min(site_cov$Dist_village), max(site_cov$Dist_village), by = 0.01))
)

predict_m5_Dist <- predict_m5_Dist %>%
  mutate(Dist_village_original = (Dist_village * dist_village_sd) + dist_village_mean)
#2. Plot the Effects
#Now, we create separate plots for each covariate.

#Plot (A): Effect of Elevation
ggplot(data = predict_m5_Elev, aes(x = Elevation_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Elevation (m)", y = "Predicted Occupancy Probability") +
  theme_classic()
#Plot (B): Effect of Year
ggplot(data = predict_m5_Year, aes(x = Year_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Year", y = "Predicted Occupancy Probability") +
  theme_classic()
#Plot (C): Effect of Distance to Village
ggplot(data = predict_m5_Dist, aes(x = Dist_village_original, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray") +
  geom_line(color = "black") +
  labs(x = "Distance to Village (m)", y = "Predicted Occupancy Probability") +
  theme_classic()












## ----mbgof---------------------------------------------------------------

# Do Mackenzie-Bailey goodness of fit test for single-season occupancy model
m2_mb.gof.boot <- mb.gof.test(occu.m2,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m2_mb.gof.boot


m3_mb.gof.boot <- mb.gof.test(occu.m3,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m3_mb.gof.boot



m4_mb.gof.boot <- mb.gof.test(occu.m4,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m4_mb.gof.boot





m5_mb.gof.boot <- mb.gof.test(occu.m5,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m5_mb.gof.boot





m1_mb.gof.boot <- mb.gof.test(occu.m1,
                              # Demonstrate with small number of sims (10), 
                              # but then change to large number (e.g. 1000)
                              nsim = 1000)

# View Results
m1_mb.gof.boot




# Load detection history for each species
detection_history_moose <- read_excel(here::here("data/Data_for_model_new.xlsx"), sheet = "Moose")
detection_history_reddeer <- read_excel(here::here("data/Data_for_model_new.xlsx"), sheet = "Red Deer")
detection_history_roedeer <- read_excel(here::here("data/Data_for_model_new.xlsx"), sheet = "Roe Deer")
detection_history_bear <- read_excel(here::here("data/Data_for_model_new.xlsx"), sheet = "Bear")

# Function to clean and standardize missing data
process_detection_data <- function(df) {
  df |> mutate(across(10:68, ~ na_if(., "N/A"))) |> 
    mutate(across(10:68, as.numeric))
}

# Apply function to each dataset
detection_history_moose <- process_detection_data(detection_history_moose)
detection_history_reddeer <- process_detection_data(detection_history_reddeer)
detection_history_roedeer <- process_detection_data(detection_history_roedeer)
detection_history_bear <- process_detection_data(detection_history_bear)

# Convert to matrices
y_moose <- as.matrix(detection_history_moose[10:68])
y_red_deer <- as.matrix(detection_history_reddeer[10:68])
y_roe_deer <- as.matrix(detection_history_roedeer[10:68])
y_bear <- as.matrix(detection_history_bear[10:68])

# Ensure all species have the same number of sites and surveys
min_sites <- min(nrow(y_bear), nrow(y_red_deer), nrow(y_moose), nrow(y_roe_deer))
min_surveys <- min(ncol(y_bear), ncol(y_red_deer), ncol(y_moose), ncol(y_roe_deer))

y_bear <- y_bear[1:min_sites, 1:min_surveys]
y_red_deer <- y_red_deer[1:min_sites, 1:min_surveys]
y_moose <- y_moose[1:min_sites, 1:min_surveys]
y_roe_deer <- y_roe_deer[1:min_sites, 1:min_surveys]

# # Standardize missing values across all species
# na_mask <- is.na(y_bear) | is.na(y_red_deer) | is.na(y_moose) | is.na(y_roe_deer)
# y_bear[na_mask] <- NA
# y_red_deer[na_mask] <- NA
# y_moose[na_mask] <- NA
# y_roe_deer[na_mask] <- NA
# 
# # # Create detection array
# # y_array <- array(
# #   c(y_bear, y_red_deer, y_moose, y_roe_deer), 
# #   dim = c(nrow(y_bear), ncol(y_bear), 4), 
# #   dimnames = list(NULL, NULL, c("bear", "red_deer", "moose", "roe_deer"))
# # )
# 
# # Create detection array
# y_array <- array(
#   c(as.numeric(y_bear), as.numeric(y_moose)),
#   dim = c(nrow(y_bear), ncol(y_bear), 2),
#   dimnames = list(NULL, NULL, c("bear", "moose")))


# Load and clean site covariates
site_covs <- detection_history_bear[5:9]  # Use a dataset with full site-level data
site_covs <- site_covs[1:min_sites, ]  # Ensure it matches the number of sites

# Convert to numeric where needed
site_covs <- site_covs %>%
  rename(Dist_road = 'Distance to the nearest road km',
         Dist_village = 'Distance to nearest town/village km') %>%
  mutate(Elevation = as.numeric(Elevation))

# Ensure site covariates have the same number of rows as y_array
stopifnot(nrow(site_covs) == dim(y_array)[1])

site_covs <- site_covs %>% select(-Notes)
site_covs <- site_covs %>% select(-Year)

str(site_covs)
nrow(site_covs) == dim(y_array)[1]

str(y_array)

# Create multispecies occupancy frame
ms_occ_data <- unmarkedFrameOccuMulti(y = y_array, siteCovs = site_covs)

# Fit multispecies occupancy model
ms_occ_model <- occuMulti(
  detformulas = list(~1, ~1, ~1, ~1), # Detection models for each species
  stateformulas = list(~Elevation + Year, ~Elevation, ~Dist_road, ~Dist_village), # Occupancy models
  data = ms_occ_data
)

# View summary
summary(ms_occ_model)
