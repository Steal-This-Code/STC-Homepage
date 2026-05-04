# Boundary Files

This directory contains GeoJSON boundary files used by the neighborhood-watch app.

## Files

### neighborhoods.geojson
**Source:** Dallas Police Department Beats (2011)  
**Features:** 72 police beat boundaries  
**Key Properties:**
- `BEAT` — Beat number (e.g., "111")
- `SECTOR` — Sector number
- `DIVISION` — Police division name (CENTRAL, NORTH, SOUTH, NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST)

### zip_codes.geojson
**Source:** Dallas County ZIP Code Tabulation Areas (ZCTA)  
**Features:** ZIP code boundaries within Dallas County  
**Key Properties:**
- ZCTA code and geographic identifiers

## Data Age

- **Beats:** 2011 (latest available; may want to check for updates)
- **ZCTA:** 2017 (latest available; may want to check for updates)

## Conversion Process

These files were converted from shapefiles using [convert_shapefiles_to_geojson.R](../scripts/convert_shapefiles_to_geojson.R):

```bash
Rscript scripts/convert_shapefiles_to_geojson.R
```

## Notes for Future Updates

When fresher boundary files become available:
1. Place shapefiles in `data/raw/Beats/` and `data/raw/DallasCoZCTA/`
2. Run the conversion script to regenerate the GeoJSON files
3. The build process will automatically use the updated boundaries
