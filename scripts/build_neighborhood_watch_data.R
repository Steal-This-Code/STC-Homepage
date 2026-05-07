suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(fs)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(readr)
  library(sf)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(tidycops)
})

city <- Sys.getenv("NEIGHBORHOOD_WATCH_CITY", unset = "dallas")
days_back <- as.integer(Sys.getenv("NEIGHBORHOOD_WATCH_DAYS_BACK", unset = "365"))
output_dir <- "data/neighborhood-watch"
boundary_dir <- "data/boundaries"

dir_create(output_dir)

pick_first_column <- function(data, candidates, required = TRUE) {
  match <- candidates[candidates %in% names(data)][1]

  if (is.na(match) || is.null(match)) {
    if (!required) {
      return(NULL)
    }

    stop(
      sprintf(
        "None of these columns were found: %s",
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  match
}

classify_crime_group <- function(offense_label, violent_flag = NULL) {
  if (!is.null(violent_flag)) {
    violent_text <- tolower(as.character(violent_flag))
    is_known <- !is.na(violent_text) & violent_text != ""

    groups <- ifelse(
      is_known & violent_text %in% c("true", "t", "1", "yes", "violent"),
      "violent",
      "property"
    )

    groups[!is_known] <- NA_character_
  } else {
    groups <- rep(NA_character_, length(offense_label))
  }

  normalized <- str_to_lower(coalesce(as.character(offense_label), ""))

  violent_pattern <- paste(
    c(
      "homicide",
      "murder",
      "manslaughter",
      "assault",
      "battery",
      "robbery",
      "rape",
      "sexual assault",
      "kidnapping",
      "weapon"
    ),
    collapse = "|"
  )

  property_pattern <- paste(
    c(
      "burglary",
      "theft",
      "larceny",
      "shoplifting",
      "motor vehicle theft",
      "auto theft",
      "fraud",
      "forgery",
      "criminal damage",
      "vandalism",
      "arson"
    ),
    collapse = "|"
  )

  groups[is.na(groups) & str_detect(normalized, violent_pattern)] <- "violent"
  groups[is.na(groups) & str_detect(normalized, property_pattern)] <- "property"
  groups[is.na(groups)] <- "other"
  groups
}

write_json_pretty <- function(data, path) {
  write_json(
    data,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
}

ensure_columns <- function(data, columns, default = 0L) {
  for (column in columns) {
    if (!column %in% names(data)) {
      data[[column]] <- default
    }
  }

  data
}

message(sprintf("Downloading %s incidents (limited to recent data)", city))

# Fetch with smaller limit to avoid Socrata pagination issues
# If this fails, create empty dataset so build doesn't crash
incidents_raw <- tryCatch(
  {
    get_incidents(
      city = city,
      limit = 10000
    )
  },
  error = function(e) {
    message(sprintf("Warning: Failed to fetch incidents: %s", e$message))
    message("Proceeding with empty dataset for now.")
    # Return an empty dataframe with the expected structure
    tibble::tibble(
      incidentnum = character(0),
      date1 = character(0),
      offincident = character(0),
      premise = character(0),
      geocoded_column.latitude = numeric(0),
      geocoded_column.longitude = numeric(0)
    )
  }
)

if (nrow(incidents_raw) == 0) {
  message("No incident data retrieved. Generating empty outputs.")
  
  # Create empty but valid JSON files
  write_json_pretty(
    list(
      city = city,
      built_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      days_back = days_back,
      incident_count = 0,
      notes = c(
        "No incident data available in this build.",
        "Data source API may be temporarily unavailable.",
        "Try again in the next scheduled run."
      )
    ),
    path(output_dir, "metadata.json")
  )
  
  write_json_pretty(list(), path(output_dir, "summary_by_area.json"))
  write_json_pretty(list(), path(output_dir, "offense_mix.json"))
  write_json_pretty(list(), path(output_dir, "time_patterns.json"))
  write_json_pretty(list(), path(output_dir, "place_types.json"))
  write_json_pretty(list(), path(output_dir, "citywide_context.json"))
  write_json_pretty(list(), path(output_dir, "actions_by_profile.json"))
  
  message("Build completed with empty data (API unavailable)")
  quit(status = 0)
}

message(sprintf("Successfully retrieved %d incidents", nrow(incidents_raw)))

timestamp_col <- pick_first_column(
  incidents_raw,
  c("date1_parsed", "date1", "std_occurred_at", "occurred_at", "incident_datetime"),
  required = TRUE
)

offense_col <- pick_first_column(
  incidents_raw,
  c("offincident_clean", "offincident", "std_offense", "std_offense_category", "offense", "offense_type", "incident_type"),
  required = TRUE
)

violent_flag_col <- pick_first_column(
  incidents_raw,
  c("std_is_violent", "is_violent", "violent"),
  required = FALSE
)

location_type_col <- pick_first_column(
  incidents_raw,
  c("premise_clean", "premise", "std_location_type", "location_type", "premise_type"),
  required = FALSE
)

lat_col <- pick_first_column(
  incidents_raw,
  c("geocoded_column.latitude", "std_latitude", "latitude", "lat", "y"),
  required = FALSE
)

lon_col <- pick_first_column(
  incidents_raw,
  c("geocoded_column.longitude", "std_longitude", "longitude", "lon", "lng", "x"),
  required = FALSE
)

incident_id_col <- pick_first_column(
  incidents_raw,
  c("incidentnum", "std_incident_id", "incident_id", "incident_number", "report_number"),
  required = FALSE
)

incidents <- incidents_raw |>
  mutate(
    incident_id = if (!is.null(incident_id_col)) as.character(.data[[incident_id_col]]) else as.character(row_number()),
    occurred_at = suppressWarnings(ymd_hms(.data[[timestamp_col]], quiet = TRUE)),
    occurred_at = coalesce(occurred_at, suppressWarnings(ymd_hm(.data[[timestamp_col]], quiet = TRUE))),
    occurred_at = coalesce(occurred_at, suppressWarnings(ymd(.data[[timestamp_col]], quiet = TRUE))),
    offense_label = as.character(.data[[offense_col]]),
    violent_flag = if (!is.null(violent_flag_col)) .data[[violent_flag_col]] else NA,
    crime_group = classify_crime_group(offense_label, violent_flag),
    location_type = as.character(if (!is.null(location_type_col)) .data[[location_type_col]] else "Unknown"),
    latitude = suppressWarnings(as.numeric(if (!is.null(lat_col)) .data[[lat_col]] else NA)),
    longitude = suppressWarnings(as.numeric(if (!is.null(lon_col)) .data[[lon_col]] else NA)),
    date = as.Date(occurred_at),
    hour = if ("time1" %in% names(incidents_raw)) {
      suppressWarnings(as.integer(sub(":.*", "", time1)))
    } else {
      hour(occurred_at)
    },
    day_of_week = wday(occurred_at, label = TRUE, abbr = FALSE, week_start = 1),
    month = floor_date(date, unit = "month")
  ) |>
  filter(!is.na(date), date >= Sys.Date() - days_back) |>
  select(
    incident_id,
    occurred_at,
    date,
    month,
    hour,
    day_of_week,
    offense_label,
    crime_group,
    location_type,
    latitude,
    longitude,
    everything()
  )

read_boundary_layer <- function(filename, area_type) {
  path <- path(boundary_dir, filename)

  if (!file_exists(path)) {
    message(sprintf("Skipping missing boundary file: %s", path))
    return(NULL)
  }

  geometry <- read_sf(path, quiet = TRUE)
  
  # Ensure CRS is consistent (transform to 4326 if needed)
  if (!is.na(st_crs(geometry))) {
    if (st_crs(geometry) != 4326) {
      geometry <- st_transform(geometry, 4326)
    }
  } else {
    # If no CRS is defined, assume 4326
    geometry <- st_set_crs(geometry, 4326)
  }
  
  # Validate and repair geometries
  tryCatch({
    geometry <- st_make_valid(geometry)
  }, error = function(e) {
    message(sprintf("Warning: Could not validate geometries in %s: %s", filename, e$message))
  })
  
  name_col <- pick_first_column(
    geometry,
    c("name", "Name", "NAME", "neighborhood", "zip", "zipcode", "label"),
    required = FALSE
  )
  id_col <- pick_first_column(
    geometry,
    c("id", "ID", "GEOID", "geoid", "zip", "zipcode", "name", "NAME"),
    required = FALSE
  )

  geometry |>
    mutate(
      area_type = area_type,
      area_name = if (!is.null(name_col)) as.character(.data[[name_col]]) else area_type,
      area_id = if (!is.null(id_col)) as.character(.data[[id_col]]) else as.character(row_number())
    ) |>
    select(area_type, area_id, area_name, geometry)
}

boundary_layers <- compact(
  list(
    read_boundary_layer("neighborhoods.geojson", "neighborhood"),
    read_boundary_layer("zip_codes.geojson", "zip")
  )
)

if (length(boundary_layers) == 0) {
  stop(
    "No boundary layers were found. Add GeoJSON files to data/boundaries/ before running this build.",
    call. = FALSE
  )
}

boundaries <- do.call(rbind, boundary_layers)

incident_points <- incidents |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

incidents_joined <- st_join(
  incident_points,
  boundaries,
  join = st_within,
  left = TRUE
) |>
  st_drop_geometry()

summary_by_area <- incidents_joined |>
  filter(!is.na(area_id)) |>
  count(area_type, area_id, area_name, crime_group, name = "incidents") |>
  tidyr::pivot_wider(
    names_from = crime_group,
    values_from = incidents,
    values_fill = 0
  ) |>
  ensure_columns(c("violent", "property", "other")) |>
  mutate(
    total_incidents = violent + property + other,
    violent_incidents = violent,
    property_incidents = property,
    other_incidents = other
  ) |>
  arrange(area_type, desc(total_incidents), area_name)

offense_mix <- incidents_joined |>
  filter(!is.na(area_id)) |>
  count(area_type, area_id, area_name, crime_group, offense_label, sort = TRUE, name = "incidents") |>
  group_by(area_type, area_id, crime_group) |>
  slice_head(n = 10) |>
  ungroup()

time_patterns <- incidents_joined |>
  filter(!is.na(area_id), !is.na(hour), !is.na(day_of_week)) |>
  count(area_type, area_id, area_name, crime_group, day_of_week, hour, name = "incidents")

place_types <- incidents_joined |>
  filter(!is.na(area_id)) |>
  count(area_type, area_id, area_name, crime_group, location_type, sort = TRUE, name = "incidents") |>
  group_by(area_type, area_id, crime_group) |>
  slice_head(n = 10) |>
  ungroup()

citywide_context <- incidents |>
  count(month, crime_group, name = "incidents") |>
  arrange(month, crime_group)

actions_by_profile <- tibble::tribble(
  ~profile, ~focus, ~actions,
  "resident", "property", c("Lock vehicles and remove visible items", "Coordinate lighting checks with neighbors", "Shift awareness to peak hours shown in the app"),
  "resident", "violent", c("Use high-visibility routes during peak hours", "Report repeated conflicts and hotspots", "Organize block communication for rapid information sharing"),
  "business", "property", c("Review cash-handling and closing procedures", "Improve exterior lighting and camera coverage", "Coordinate with nearby businesses on repeat incidents"),
  "business", "violent", c("Train staff on de-escalation and incident reporting", "Reduce blind spots near entrances", "Adjust staffing around high-risk time windows"),
  "police", "property", c("Target patrols to repeat corridors and parking areas", "Review chronic locations for environmental fixes", "Share prevention messaging with affected blocks"),
  "police", "violent", c("Align patrol timing with peak violent periods", "Review repeat-location patterns", "Coordinate with outreach and violence interruption partners"),
  "city", "property", c("Prioritize street lighting and blight reduction", "Address vacant lots or poor sightlines", "Support environmental design improvements"),
  "city", "violent", c("Coordinate investments around persistent corridors", "Improve transit-area safety design", "Fund prevention and neighborhood stabilization efforts")
) |>
  mutate(actions = purrr::map(actions, unname))

write_json_pretty(
  list(
    city = city,
    built_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    days_back = days_back,
    incident_count = nrow(incidents),
    date_range_start = min(incidents$date, na.rm = TRUE),
    date_range_end = max(incidents$date, na.rm = TRUE),
    notes = c(
      "Counts depend on source freshness and source coverage.",
      "The live app should present this as informational, not real-time dispatch data.",
      "Environmental context layers are correlational and should not be framed as causal."
    )
  ),
  path(output_dir, "metadata.json")
)

write_json_pretty(summary_by_area, path(output_dir, "summary_by_area.json"))
write_json_pretty(offense_mix, path(output_dir, "offense_mix.json"))
write_json_pretty(time_patterns, path(output_dir, "time_patterns.json"))
write_json_pretty(place_types, path(output_dir, "place_types.json"))
write_json_pretty(citywide_context, path(output_dir, "citywide_context.json"))
write_json_pretty(actions_by_profile, path(output_dir, "actions_by_profile.json"))

write_parquet(incidents, path(output_dir, "incidents_recent.parquet"))
write_sf(
  boundaries,
  path(output_dir, "boundaries.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# ── Per-zip incident JSON files for point-level map layer ────────────────────
message("Writing per-zip incident files for point layer")

zip_incident_dir <- path(output_dir, "incidents")
dir_create(zip_incident_dir)

zip_col <- pick_first_column(
  incidents,
  c("zip_code_clean", "zip_code", "zipcode", "zip"),
  required = FALSE
)

if (!is.null(zip_col)) {
  # Build a 1-to-1 incident → neighborhood lookup from the spatial join
  neighborhood_lookup <- incidents_joined |>
    filter(area_type == "neighborhood", !is.na(area_name)) |>
    select(incident_id, neighborhood = area_name) |>
    distinct(incident_id, .keep_all = TRUE)

  incidents_export <- incidents |>
    mutate(
      zip = as.character(.data[[zip_col]]),
      premise_export = if ("premise_clean" %in% names(incidents)) as.character(premise_clean) else NA_character_
    ) |>
    filter(!is.na(latitude), !is.na(longitude), !is.na(zip)) |>
    left_join(neighborhood_lookup, by = "incident_id") |>
    select(
      id           = incident_id,
      lat          = latitude,
      lng          = longitude,
      offense      = offense_label,
      group        = crime_group,
      premise      = premise_export,
      hour,
      date,
      zip,
      neighborhood
    ) |>
    mutate(date = as.character(date))

  # Write one compact JSON file per zip code (exclude the zip field itself)
  zip_groups <- split(
    incidents_export |> select(-zip),
    incidents_export$zip
  )
  for (zip_code in names(zip_groups)) {
    write_json_pretty(
      zip_groups[[zip_code]],
      path(zip_incident_dir, paste0(zip_code, ".json"))
    )
  }

  # Build area → zip codes lookup so the dashboard knows which files to fetch
  incident_zip_lookup <- incidents_export |>
    select(incident_id = id, zip)

  neighborhood_zip_map <- incidents_joined |>
    filter(area_type == "neighborhood", !is.na(area_id)) |>
    select(incident_id, area_id, area_name) |>
    left_join(incident_zip_lookup, by = "incident_id") |>
    filter(!is.na(zip)) |>
    group_by(area_id, area_name) |>
    summarize(zip_codes = list(sort(unique(zip))), .groups = "drop") |>
    mutate(area_type = "neighborhood")

  zip_area_zip_map <- incidents_joined |>
    filter(area_type == "zip", !is.na(area_id)) |>
    select(incident_id, area_id, area_name) |>
    left_join(incident_zip_lookup, by = "incident_id") |>
    filter(!is.na(zip)) |>
    group_by(area_id, area_name) |>
    summarize(zip_codes = list(sort(unique(zip))), .groups = "drop") |>
    mutate(area_type = "zip")

  area_zip_map <- bind_rows(neighborhood_zip_map, zip_area_zip_map)
  write_json_pretty(area_zip_map, path(output_dir, "area_zip_map.json"))

  message(sprintf(
    "Written %d zip incident files (%d incidents with coordinates)",
    length(zip_groups),
    nrow(incidents_export)
  ))
} else {
  message("No zip code column found; skipping per-zip incident files")
}

message("Neighborhood Watch data build complete")
