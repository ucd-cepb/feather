# California Fire Hazard Severity Zones - Static Map for Word Document
# This script generates a static map of California fire hazard zones

# Load required libraries
library(sf)          # For spatial data handling
library(ggplot2)     # For static plotting
library(dplyr)       # For data manipulation
library(viridis)     # For color palettes



# California Fire Hazard Severity Zones - Static Map for Word Document
# Updated with current 2025 data sources

# Load required libraries
library(sf)          # For spatial data handling
library(ggplot2)     # For static plotting
library(dplyr)       # For data manipulation
library(viridis)     # For color palettes

# Function to generate static fire hazard map
generate_static_fire_map <- function(width = 12, height = 10, dpi = 300, county_filter = NULL) {
  
  # Updated URLs for 2025 FHSZ data - try multiple sources
  # Option 1: California State Geoportal (most reliable)
  geoportal_url <- "https://services1.arcgis.com/jUJYIo9tSA7EHvfZ/arcgis/rest/services/California_Fire_Hazard_Severity_Zones_FHSZ/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
  
  # Option 2: Alternative CAL FIRE service
  calfire_url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Fire_Hazard_Severity_Zones/FeatureServer/0/query?where=STATE_NAME%3D%27California%27&outFields=*&outSR=4326&f=geojson"
  test <- read_sf(calfire_url)
  
  # Option 3: Try the California Open Data portal
  opendata_url <- "https://data.ca.gov/api/3/action/datastore_search_sql?sql=SELECT%20*%20from%20%22fire-hazard-severity-zones%22"
  
  cat("Loading fire hazard data from updated sources...\n")
  
  fire_zones <- NULL
  
  # Try each data source in order
  for(i in 1:3) {
    url <- switch(i,
                  geoportal_url,
                  calfire_url, 
                  opendata_url)
    
    cat("Trying data source", i, "...\n")
    
    tryCatch({
      # For the first two sources (GeoJSON)
      if(i <= 2) {
        fire_zones <- st_read(url, quiet = TRUE)
        if(nrow(fire_zones) > 0) {
          cat("Successfully loaded data from source", i, "\n")
          break
        }
      } else {
        # For CSV/API sources, you'd need different handling
        cat("Alternative data loading not implemented for source", i, "\n")
      }
    }, error = function(e) {
      cat("Source", i, "failed:", e$message, "\n")
    })
  }
  
  # If online sources fail, provide instructions for manual download
  if(is.null(fire_zones) || nrow(fire_zones) == 0) {
    cat("\n=== ALTERNATIVE APPROACH ===\n")
    cat("Online data sources are currently unavailable. Here are manual options:\n\n")
    
    cat("1. Download from California State Geoportal:\n")
    cat("   https://gis.data.ca.gov/maps/CALFIRE-Forestry::california-fire-hazard-severity-zones-fhsz/explore\n")
    cat("   - Click 'Download' and select GeoJSON format\n")
    cat("   - Save as 'ca_fire_zones.geojson' in your working directory\n\n")
    
    cat("2. Download from CAL FIRE OSFM:\n") 
    cat("   https://osfm.fire.ca.gov/what-we-do/community-wildfire-preparedness-and-mitigation/fire-hazard-severity-zones/fire-hazard-severity-zones-maps\n")
    cat("   - Download county-specific shapefiles\n\n")
    
    cat("3. Use the Fire Hazard Severity Zone Viewer:\n")
    cat("   https://gis.data.ca.gov/datasets/CALFIRE-Forestry::fire-hazard-severity-zone-viewer/about\n\n")
    
    cat("After downloading, modify this script to read your local file:\n")
    cat("   fire_zones <- st_read('path/to/your/downloaded/file.geojson')\n\n")
    
    return(NULL)
  }
  
  # Process the data (adapt column names as needed)
  cat("Processing fire hazard data...\n")
  
  # Standardize column names - adapt based on actual data structure
  if("HAZ_CLASS" %in% names(fire_zones)) {
    hazard_col <- "HAZ_CLASS"
  } else if("FHSZ" %in% names(fire_zones)) {
    hazard_col <- "FHSZ" 
  } else if("hazard_level" %in% names(fire_zones)) {
    hazard_col <- "hazard_level"
  } else {
    # Print available columns to help debug
    cat("Available columns:", names(fire_zones), "\n")
    hazard_col <- names(fire_zones)[2] # Use second column as fallback
    cat("Using column:", hazard_col, "\n")
  }
  
  # Filter by county/area if specified
  if(!is.null(county_filter)) {
    cat("Filtering for:", county_filter, "\n")
    # This may need adjustment based on actual column names
    county_cols <- names(fire_zones)[grepl("county|area|name", names(fire_zones), ignore.case = TRUE)]
    if(length(county_cols) > 0) {
      filter_col <- county_cols[1]
      fire_zones <- fire_zones %>%
        filter(grepl(county_filter, get(filter_col), ignore.case = TRUE))
    }
  }
  
  cat("Creating static map...\n")
  
  # Define colors for hazard classes (adapt as needed)
  hazard_colors <- c(
    "Non-Wildland/Non-Urban" = "#E8F4FD",
    "Moderate" = "#FFE066", 
    "High" = "#FF9933",
    "Very High" = "#CC3333",
    "Low" = "#E8F4FD",
    "Medium" = "#FFE066"
  )
  
  # Create the static map
  map_title <- if(!is.null(county_filter)) {
    paste("Fire Hazard Severity Zones -", county_filter, "Area")
  } else {
    "California Fire Hazard Severity Zones"
  }
  
  fire_map <- ggplot(fire_zones) +
    geom_sf(aes_string(fill = hazard_col), color = "white", size = 0.1) +
    scale_fill_manual(
      values = hazard_colors,
      name = "Fire Hazard\nSeverity Zone",
      na.value = "lightgray"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.8, "cm")
    ) +
    labs(
      title = map_title,
      subtitle = "Updated 2025 Fire Hazard Severity Zones"
    ) +
    coord_sf(crs = st_crs(4326), expand = FALSE)
  
  # Save the map with appropriate filename
  filename <- if(!is.null(county_filter)) {
    paste0(gsub("[^A-Za-z0-9]", "_", county_filter), "_fire_hazard_map.png")
  } else {
    "california_fire_hazard_map_2025.png"
  }
  
  ggsave(
    filename = filename,
    plot = fire_map,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  
  cat("Map saved as:", filename, "\n")
  cat("Dimensions:", width, "x", height, "inches at", dpi, "DPI\n")
  
  # Display the map
  print(fire_map)
  
  return(fire_map)
}

# Alternative function to read local files if downloads are needed
load_local_fire_data <- function(file_path) {
  cat("Loading local fire hazard data from:", file_path, "\n")
  
  tryCatch({
    fire_zones <- st_read(file_path, quiet = TRUE)
    cat("Successfully loaded", nrow(fire_zones), "features\n")
    return(fire_zones)
  }, error = function(e) {
    cat("Error loading local file:", e$message, "\n")
    return(NULL)
  })
}

# Generate and save the static map for full California
fire_map <- generate_static_fire_map(width = 12, height = 10, dpi = 300)

# Optional: Create maps for specific counties/areas
# Examples - uncomment and modify as needed:

# Los Angeles County
# la_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "Los Angeles")

# Orange County  
# oc_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "Orange")

# San Diego County
# sd_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "San Diego")

# Riverside County
# riverside_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "Riverside")

# Ventura County
# ventura_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "Ventura")

# Santa Barbara County
# sb_map <- generate_static_fire_map(width = 8, height = 6, dpi = 300, county_filter = "Santa Barbara")

# The maps will be saved with county-specific filenames in your working directory
# e.g., "Los_Angeles_fire_hazard_map.png"