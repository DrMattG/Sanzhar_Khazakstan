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



