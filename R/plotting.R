# get silhouettes

library(rphylopic)
library(tidyverse)
library(ggimage)
library(lubridate)
library(patchwork)
# 1. Get UUIDs
uid_bear     <- get_uuid("Ursus arctos")
uid_moose    <- get_uuid("Alces alces")
uid_red_deer <- get_uuid("Cervus elaphus")
uid_roe_deer <- get_uuid("Capreolus capreolus")

# 2. Fetch silhouettes (vector format by default)
img_bear     <- get_phylopic(uid_bear)
img_moose    <- get_phylopic("1a20a65d-1342-4833-a9dd-1611b9fb383c") # change the moose
img_red_deer <- get_phylopic(uid_red_deer)
img_roe_deer <- get_phylopic(uid_roe_deer)

# 3. Save as PNGs
dir.create("images", showWarnings = FALSE)

save_phylopic(img_bear,     path = "images/bear.png",
              width = 500, height = 500, bg = "transparent")
save_phylopic(img_moose,    path = "images/moose.png",
              width = 500, height = 500, bg = "transparent")
save_phylopic(img_red_deer, path = "images/red_deer.png",
              width = 500, height = 500, bg = "transparent")
save_phylopic(img_roe_deer, path = "images/roe_deer.png",
              width = 500, height = 500, bg = "transparent")

grob_bear     <- rasterGrob(image_read("images/bear.png"), interpolate = TRUE)
grob_moose    <- rasterGrob(image_read("images/moose.png"), interpolate = TRUE)
grob_red_deer <- rasterGrob(image_read("images/red_deer.png"), interpolate = TRUE)
grob_roe_deer <- rasterGrob(image_read("images/roe_deer.png"), interpolate = TRUE)


## get images for the other species

species_all <- sort(unique(yearly_activity$Species))

species_missing <- setdiff(
  species_all,
  c("bear", "reddeer", "roedeer", "moose")
)

get_uuid_resolve <- function(latin_name) {
  
  # --- 1. Try species-level match ---
  uid <- try(get_uuid(latin_name), silent = TRUE)
  if(!inherits(uid, "try-error") && length(uid) > 0) {
    return(uid[1])
  }
  
  # --- 2. Try genus-level match ---
  genus <- strsplit(latin_name, " ")[[1]][1]
  uid <- try(get_uuid(genus), silent = TRUE)
  if(!inherits(uid, "try-error") && length(uid) > 0) {
    message("Using genus-level silhouette for: ", latin_name)
    return(uid[1])
  }
  
  # --- 3. Try order/family fallback (fixed, non-recursive list) ---
  fallback_terms <- c(
    "Mustelidae",   # wolverine, badger, sable
    "Felidae",      # lynx
    "Canidae",      # fox, wolf
    "Leporidae",    # hare
    "Galliformes",  # capercaillie
    "Suidae",       # wild boar
    "Moschidae"     # musk deer
  )
  
  for(term in fallback_terms) {
    uid <- try(get_uuid(term), silent = TRUE)
    if(!inherits(uid, "try-error") && length(uid) > 0) {
      message("Using fallback silhouette (", term, ") for: ", latin_name)
      return(uid[1])
    }
  }
  
  # --- 4. If nothing found ---
  warning("No silhouette found for: ", latin_name)
  return(NA)
}



latin_lookup <- list(
  badger       = "Meles meles",
  capercaillie = "Tetrao urogallus",
  fox          = "Vulpes vulpes",
  hare         = "Lepus timidus",
  lynx         = "Lynx lynx",
  sable        = "Martes zibellina",
  wildboar     = "Sus scrofa",
  wolf         = "Canis lupus",
  muskdeer     = "Moschus moschiferus",
  wolverine    = "Gulo gulo"
)

uuid_table <- tibble(
  species = names(latin_lookup),
  scientific = unlist(latin_lookup),
  uuid = sapply(latin_lookup, get_uuid_resolve)
)


uuid_table

save_all_silhouettes <- function(uuid_table, folder = "images") {
  dir.create(folder, showWarnings = FALSE)
  
  for (i in seq_len(nrow(uuid_table))) {
    sp  <- uuid_table$species[i]
    id  <- uuid_table$uuid[i]
    
    if (is.na(id) || id == "") {
      message("Skipping ", sp, " (no UUID)")
      next
    }
    
    message("Downloading silhouette for: ", sp)
    
    # fetch silhouette (rphylopic 1.6.0)
    img <- get_phylopic(id)
    
    # save as transparent PNG
    save_phylopic(
      img,
      path  = file.path(folder, paste0(sp, ".png")),
      width = 800,
      height = 800,
      bg = "transparent"
    )
  }
  
  message("All available silhouettes saved.")
}

save_all_silhouettes(uuid_table)



#############################################################
# 1. Filter species with >= 5 detections
#############################################################

species_keep <- yearly_activity |> 
  group_by(Species) |> 
  summarise(n = n()) |> 
  filter(n >= 100) |> 
  pull(Species)

#############################################################
# 2. Build daily detection summary
#############################################################

weekly_activity <- yearly_activity |> 
  filter(Species %in% species_keep) |> 
  mutate(week = floor_date(date, "week")) |> 
  count(Species, week, name = "detections") |> 
  group_by(Species) |> 
  filter(n() >= 5)

#############################################################
# 3. Map species -> silhouette filenames
#############################################################

sil_dir <- "images"

file_map <- tibble(
  Species = species_keep,
  filename = case_when(
    Species == "reddeer" ~ "red_deer.png",
    Species == "roedeer" ~ "roe_deer.png",
    TRUE ~ paste0(Species, ".png")
  ),
  image = file.path(sil_dir, filename)
) |> 
  filter(file.exists(image))

#############################################################
# 4. Compute ymax per species (peak daily detections)
#############################################################

ymax_df <- weekly_activity |> 
  group_by(Species) |> 
  summarise(ymax = max(detections), .groups = "drop")

#############################################################
# 5. Build silhouette positions
#############################################################

sil_pos <- file_map |> 
  left_join(ymax_df, by = "Species") |> 
  mutate(
    x = as.Date(max(weekly_activity$week))-200,  # right edge of timeline
    y = ymax * 2                       # slightly above tallest bar
  )

#############################################################
# 6. Final plot - daily detections with silhouettes
#############################################################

p<-ggplot(weekly_activity, aes(week, detections)) +
  geom_col()+
  geom_image(
    data = sil_pos,
    aes(x = x, y = y, image = image),
    size = 0.55,     # adjust globally (0.45-0.65 works well)
    asp = 1
  ) +
  facet_wrap(~ Species, scales = "free_y") +
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "3 months"
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Date",
    y = "Detections"
  )+
  theme(
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  )

facet_labels <- c(
  "bear"     = "a ",
  "moose"    = "b ",
  "reddeer"  = "c",
  "roedeer"  = "d"
)

p <- p +
  facet_wrap(~ Species, labeller = labeller(Species = facet_labels))+
  theme(strip.text = element_text(hjust = 0))


#############################################################
# 1. Prepare annual data and filter rare species
#############################################################

annual <- yearly_activity |> 
  mutate(
    time_of_day =
      hour(datetime) +
      minute(datetime)/60 +
      second(datetime)/3600
  ) |> 
  group_by(Species) |> 
  filter(n() >= 100) |> 
  ungroup()

species_filtered <- unique(annual$Species)

#############################################################
# 2. Correct the silhouette file names (mapping table)
#############################################################

file_map <- tibble(
  Species = species_filtered,
  filename = case_when(
    Species == "reddeer" ~ "red_deer.png",
    Species == "roedeer" ~ "roe_deer.png",
    TRUE ~ paste0(Species, ".png")   # default rule
  ),
  image = file.path("images", filename)
) |> 
  filter(file.exists(image))

#############################################################
# 3. Compute ymax for silhouette placement
#############################################################

ymax_df <- annual |> 
  mutate(bin = floor(time_of_day)) |>         # 0-23 hour bins
  count(Species, bin) |>                      # counts per bin
  group_by(Species) |> 
  summarise(ymax = max(n), .groups = "drop")  # tallest bar per species

#############################################################
# 4. Join image paths with silhouette positions
#############################################################

sil_pos <- file_map |> 
  left_join(ymax_df, by = "Species") |> 
  mutate(
    x = 23,          # right side of the 24-hour axis
    y = ymax *1.4 # near top of the panel
  )

#############################################################
# 5. Final plot with properly sized silhouettes
#############################################################

facet_labels <- setNames(
  paste0(letters[1:length(species_filtered)]),
  species_filtered
)

p<-ggplot(annual, aes(time_of_day)) +
  geom_histogram(binwidth = 1, fill = "grey40", colour = "white") +
  geom_image(
    data = sil_pos,
    aes(x = x, y = y, image = image),
    size = 0.55,
    asp = 1
  ) +
  facet_wrap(~ Species,
             scales = "free_y",
             labeller = labeller(Species = facet_labels)) +
  
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  # expand y-limit upward to create headroom for silhouettes
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(
      hjust = 0,         # <<<<<< LEFT ALIGN the a) b) c) labels
      face = "bold",
      margin = margin(l = 4)   # small left padding (looks better)
    ),
    strip.background = element_rect(fill = NA, colour = NA)
  ) +
  labs(
    x = "Hour of Day",
    y = "Detections",
    title = ""
  )
p


#############################################################
# 1. Filter species with >=5 detections
#############################################################

species_keep <- yearly_activity |> 
  group_by(Species) |> 
  summarise(n = n()) |> 
  filter(n >= 5) |> 
  pull(Species)

#############################################################
# 2. Build monthly detection summary
#############################################################

monthly_activity <- yearly_activity |> 
  filter(Species %in% species_keep) |> 
  mutate(
    month = month(datetime, label = TRUE, abbr = TRUE),
    month_num = month(datetime)
  ) |> 
  group_by(Species, month, month_num) |> 
  summarise(detections = n(), .groups = "drop") |> 
  arrange(Species, month_num)

#############################################################
# 3. Map species -> corrected silhouette file names
#############################################################

sil_dir <- "images"

file_map <- tibble(
  Species = species_keep,
  filename = case_when(
    Species == "reddeer" ~ "red_deer.png",
    Species == "roedeer" ~ "roe_deer.png",
    TRUE ~ paste0(Species, ".png")
  ),
  image = file.path(sil_dir, filename)
) |> 
  filter(file.exists(image))   # only species with silhouettes

#############################################################
# 4. Compute ymax per species (max monthly bar height)
#############################################################

ymax_df <- monthly_activity |> 
  group_by(Species) |> 
  summarise(ymax = max(detections), .groups = "drop")

#############################################################
# 5. Join silhouette positions
#############################################################

sil_pos <- file_map |> 
  left_join(ymax_df, by = "Species") |> 
  mutate(
    x = "Nov",           # place near the right side
    y = ymax * 1.1     # slightly above tallest bar
  )

#############################################################
# 6. Final monthly plot with silhouettes
#############################################################

ggplot(monthly_activity, aes(month, detections)) +
  geom_col(fill = "grey40") +
  geom_image(
    data = sil_pos,
    aes(x = x, y = y, image = image),
    size = 0.55,   # adjust globally
    asp = 1
  ) +
  facet_wrap(~ Species, scales = "free_y") +
  # add headroom for silhouettes
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Month",
    y = "Detections"
  )

## bear distance to...


elv<-lm(detections~elevation_m, camera_bear_data)
summary(elv)

road<-lm(detections~dist_road_km, camera_bear_data)
summary(road)

vill<-lm(detections~dist_village_km, camera_bear_data)
summary(vill)



#-------------------------
# 2. Path to bear silhouette
#-------------------------
bear_img <- "images/bear.png"   # adjust if needed

#-------------------------
# 3. Function to make a nice regression plot
#-------------------------
plot_bear_reg <- function(df, xvar, xlabel) {
  
  xvar <- rlang::ensym(xvar)
  xname <- rlang::as_string(xvar)
  
  xmax <- max(df[[xname]], na.rm = TRUE)
  xmin <- min(df[[xname]], na.rm = TRUE)
  ymax <- max(df$detections, na.rm = TRUE)
  
  ggplot(df, aes(x = !!xvar, y = detections)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
    
    # Put silhouette in TOP-LEFT corner
    geom_image(
      aes(
        x = xmin + (xmax - xmin) * 0.10,   # move right 5% of plot width
        y = ymax * 1.12                    # move slightly higher
      ),
      image = bear_img,
      size = 0.12,
      hjust = 0,
      vjust = 1,
      inherit.aes = FALSE
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35)))+
    labs(
      x = xlabel,
      y = "Bear detections"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 10, 5, 10)
    )
}
#-------------------------
# 4. Generate the three plots
#-------------------------

p1 <- plot_bear_reg(camera_bear_data, elevation_m, "Elevation (m)")
p2 <- plot_bear_reg(camera_bear_data, dist_road_km, "Distance to road (km)")
p3 <- plot_bear_reg(camera_bear_data, dist_village_km, "Distance to village (km)")

# Combine


final_plot <- (p2 | p3 | p1) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.margin = margin(t = 5.5, r = 15, b = 15, l = 5.5)  # add space on right + bottom
  )

final_plot

