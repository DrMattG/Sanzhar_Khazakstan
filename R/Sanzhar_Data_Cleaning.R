## Load Packages
library(tidyverse)
library(lubridate)

# load data
raw_camera_data <- read.csv("data/Raw_Data_USED_AT.csv")
camera_data <- read.csv("data/camera_data_AT.csv")

######## CLEANING DATA ################

camera_data<-camera_data |> 
  janitor::clean_names()

camera_data <- camera_data |>
  rename(Camera.Number = x_camera_id)

# clean data for time
raw_camera_data_2 <- raw_camera_data |>
  mutate(time = parse_time(Time)) |>
  mutate(date = as.POSIXct(mdy(Date))) |>
  mutate(datetime = as.POSIXct(paste(date, time), tz = "UTC")) |>
  mutate(datetime_label = format(datetime, "%Y-%m-%d %H:%M")) |>
  mutate(across(where(is.character), ~ str_replace_all(., "\\s+", "")))


# clean data for detections for YEARLY ACTIVITY (subset by 6 hour)
yearly_activity <- raw_camera_data_2 |>
  arrange(Camera.Number, Species, datetime) |>                 # sort data
  group_by(Camera.Number, Species) |>                          # group by BOTH
  mutate(dt = as.numeric(difftime(datetime, lag(datetime), units = "hours"))) |>
  filter(is.na(dt) | dt > 6)                           # keep within 12 hours


# clean data for detections for DAILY ACTIVITY (subset by 1 hour)
daily_activity <- raw_camera_data_2 |>
  arrange(Camera.Number, Species, datetime) |>                 # sort data
  group_by(Camera.Number, Species) |>                          # group by BOTH
  mutate(dt = as.numeric(difftime(datetime, lag(datetime), units = "hours"))) |>
  filter(is.na(dt) | dt > 1)                        # keep within 1 hour



######### DATA FOR PLOTTING ################

# table for plotting daily activity
df_summary <- daily_activity |>
  mutate(hour_interval = cut(hour(datetime), 
                             breaks = 0:24, 
                             labels = paste0(sprintf("%02d:00–%02d:00", 0:23, 1:24)[1:24]),
                             include.lowest = TRUE,
                             right = FALSE)) |>
  group_by(hour_interval, Species) |>
  summarize(count = n(), .groups = "drop")

daily_activity_table <- df_summary |>
  pivot_wider(names_from = Species, values_from = count, values_fill = 0)
#View(daily_activity_table)


# table for plotting bear detections by elevation and distance to humans
camera_bear_summary <- yearly_activity |>
  filter(Species == "bear") |>           # keep only bears
  group_by(Camera.Number) |>             # group by camera
  summarize(
    detections = n(),                     # count total rows per camera
    .groups = "drop"                      # ungroup after summarizing
  ) |>
  arrange(desc(detections))               # sort by number of detections
#View(camera_bear_summary)

camera_bear_data <- camera_bear_summary |>
  right_join(camera_data, by = "Camera.Number") |>
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))
#View(camera_bear_data)

######## RESULTS ################

# total pictures taken
nrow(raw_camera_data_2)


# total detections
nrow(yearly_activity)


# species summary (based on 6 hour cut off)
species_summary <- yearly_activity |>
  group_by(Species) |>                  # group by species
  summarize(
    detections = n()                     # count total rows per species
  ) |>
  arrange(desc(detections))   
#View(species_summary)


# species detections by camera
species_summary_by_camera <- yearly_activity |>
  group_by(Camera.Number, Species) |>   # group by camera AND species
  summarize(
    detections = n(),                    # count total rows per group
    .groups = "drop"                     # ungroup after summarizing
  ) |>
  arrange(Camera.Number, desc(detections)) |> # sort by camera and most detections
  pivot_wider(
    names_from = Camera.Number,   # each camera becomes a column
    values_from = detections,     # fill values with detection counts
    values_fill = 0               # replace NA with 0 if a species was not detected at a camera
  )
#View(species_summary_by_camera)


# what was earliest and latest detection date
bear_summary_calendar <- yearly_activity |>
  filter(Species == "bear") |>                        # keep only bears
  mutate(month_day = format(datetime, "%m-%d")) |>    # extract month and day as "MM-DD"
  summarize(
    earliest_date = min(month_day),                    # earliest detection (ignoring year)
    latest_date   = max(month_day),                    # latest detection (ignoring year)
    total_detections = n()                             # total bear detections
  )
#View(bear_summary_calendar)

######## SOME QUICK PLOTS ################

## Daily Activity

# Subset for selected species
selected_species <- c("bear", "moose", "reddeer", "roedeer")


df_plot <- df_summary |>
  filter(Species %in% selected_species)

# Plot daily activity as line plot
ggplot(df_plot, aes(x = hour_interval, y = count, color = Species, group = Species)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_discrete(drop = FALSE) +           # keep all hour intervals
  labs(
    title = "Daily Activity Patterns",
    x = "Hour of Day",
    y = "Number of Detections",
    color = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x labels
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



## Detection Rate

# Subset for selected species (optional)
selected_species <- c("bear", "moose", "reddeer", "roedeer")

df_julian <- yearly_activity |>
  filter(Species %in% selected_species) |>
  mutate(julian_day = yday(datetime))  # extract Julian date (1-365/366)

# Count detections per Julian day per species
julian_summary <- df_julian |>
  group_by(Species, julian_day) |>
  summarize(detections = n(), .groups = "drop")

# Plot detections by Julian day
ggplot(julian_summary, aes(x = julian_day, y = detections, color = Species)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 1.2) +  # smooth activity curve
  labs(
    title = "Seasonal Activity Patterns by Julian Date",
    x = "Julian Day",
    y = "Number of Detections",
    color = "Species"
  ) +
  scale_x_continuous(breaks = seq(0, 365, 30)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
