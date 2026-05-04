#!/usr/bin/env Rscript
# Convert Dallas shapefiles to GeoJSON for neighborhood watch app

suppressPackageStartupMessages({
  library(sf)
})

# Paths
boundary_dir <- "data/boundaries"
output_neighborhoods <- file.path(boundary_dir, "neighborhoods.geojson")
output_zip_codes <- file.path(boundary_dir, "zip_codes.geojson")

# Expected input files
beats_shp <- "data/raw/Beats/Beats.shp"
zcta_shp <- "data/raw/DallasCoZCTA/DallasCoZCTA.shp"

# Convert Beats to neighborhoods GeoJSON
if (file.exists(beats_shp)) {
  cat("Converting Beats shapefile...\n")
  beats <- st_read(beats_shp, quiet = TRUE)
  st_write(beats, output_neighborhoods, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  cat("✓ Created", output_neighborhoods, "\n")
} else {
  cat("Error: Beats shapefile not found at", beats_shp, "\n")
}

# Convert Dallas County ZCTA to zip_codes GeoJSON
if (file.exists(zcta_shp)) {
  cat("Converting ZCTA shapefile...\n")
  zcta <- st_read(zcta_shp, quiet = TRUE)
  st_write(zcta, output_zip_codes, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  cat("✓ Created", output_zip_codes, "\n")
} else {
  cat("Error: ZCTA shapefile not found at", zcta_shp, "\n")
}
