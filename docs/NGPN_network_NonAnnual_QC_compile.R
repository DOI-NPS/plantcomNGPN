# Params for troubleshooting ----
## These parameters come from NGPM_network_NonAnnual_QC_Checks.Rmd

# library(plantcomNGPN)
# library(tidyverse) # dplyr, purrr, tidyr
# library(knitr) # for kable and include_graphic()
# library(kableExtra) # for custom kable features
# library(sf) # for checking plot coords against park bounding boxes
# library(data.table) # faster summarizing than dplyr for big datasets
# library(DT) # much faster at making tables for large dfs than kable
#
# all_years <- params$all_years
# year_curr <- params$year_curr
# year_range <- if(all_years == TRUE){2011:year_curr} else {year_curr}
# year_hist <- 2011:(year_curr - 1)
#
# # If SQL setup:
# # importData(type = 'local',
# #            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
# #                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
# #                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
# #            keep_tables = T)
#
# # If SQL not set up, use import below instead, and update the import_path:
# importData(type = 'csv',
#            import_path = "./data/NGPN_FFI_table_export_20260616.zip",
#            keep_tables = T)

# Start of source code ----
## Loading data ----

# Species list
# or UPDATE PATH

try(tab4_spp <- read.csv("./data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"), 
    silent = T)
try(tab4_spp <- read.csv("../data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"),
    silent = T)

# PCM Panel sampling schedule
# Paths will need to be updated

## All parks ----
try(panel_sch1 <- read.csv("./data/panel_schedule.csv", na.strings = ""), 
    silent = T)
try(panel_sch1 <- read.csv("../data/panel_schedule.csv", na.strings = ""), 
    silent = T)

# pivot to longer
panel_sch <- panel_sch1 |>
  tidyr::pivot_longer(-Year,
                      names_to = "Panel") |>
  dplyr::filter(!is.na(value)) |>
  # filtering to current date
  dplyr::filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  dplyr::select(Year,
                Panel)

## THRO Panel sch ----
### Upland
try(thro_up_panel_sch1 <- read.csv("./data/panel_schedule_thro_up.csv", 
                                   na.strings = ""), silent = T)
try(thro_up_panel_sch1 <- read.csv("../data/panel_schedule_thro_up.csv", 
                                   na.strings = ""), silent = T)

# pivot to longer
thro_up_panel_sch <- thro_up_panel_sch1 |>
  tidyr::pivot_longer(-Year,
                      names_to = "Panel") |>
  dplyr::filter(!is.na(value)) |>
  # filtering to current date
  dplyr::filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  dplyr::select(Year,
                Panel)

### Riparian
### Upland
try(thro_rip_panel_sch1 <- read.csv("./data/panel_schedule_thro_rip.csv", 
                                    na.strings = ""), silent = T)
try(thro_rip_panel_sch1 <- read.csv("../data/panel_schedule_thro_rip.csv", 
                                    na.strings = ""), silent = T)

# pivot to longer
thro_rip_panel_sch <- thro_rip_panel_sch1 |>
  tidyr::pivot_longer(-Year,
                      names_to = "Panel") |>
  dplyr::filter(!is.na(value)) |>
  # filtering to current date
  dplyr::filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  dplyr::select(Year,
                Panel)

## BADL Panel sch ----
try(badl_panel_sch1 <- read.csv("./data/panel_schedule_badl.csv", 
                                na.strings = ""), silent = T)
try(badl_panel_sch1 <- read.csv("../data/panel_schedule_badl.csv", 
                                na.strings = ""), silent = T)

# pivot to longer
badl_panel_sch <- badl_panel_sch1 |>
  tidyr::pivot_longer(-Year,
                      names_to = "Panel") |>
  dplyr::filter(!is.na(value)) |>
  # filtering to current date
  dplyr::filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
  dplyr::select(Year,
                Panel)

# Forest panel sampling schedule (not running yet) ----
# try(forest_sch1 <- read.csv("./data/panel_forest_schedule.csv",
#                             na.strings = ""), silent = T)
# try(forest_sch1 <- read.csv("../data/panel_forest_schedule.csv",
#                             na.strings = ""), silent = T)
#
# # pivot to linger
# forest_sch <- forest_sch1 |>
# tidyr::pivot_longer(-Year,
#                     names_to = "Panel") |>
#   dplyr::filter(!is.na(value)) |>
#   # filtering to current date
#   dplyr::filter(Year <= as.integer(format(Sys.Date(), "%Y"))) |>
#   dplyr::select(Year,
#                 Panel)

## Functions ----
# Summarize results of QC check
QC_check <- function(df, meas_type, tab, check, chk_type = "error"){
  result <- data.frame("Type" = meas_type,
                       "Data" = tab,
                       "Description" = check,
                       "Num_Records" = nrow(df),
                       "check_type" = chk_type)
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
      kable(df,
            format = 'html',
            align = 'c',
            caption = cap)  |>
        kable_styling(fixed_thead = TRUE,
                      bootstrap_options = c('condensed'),
                      full_width = TRUE,
                      position = 'left',
                      font_size = 12) |>
        row_spec(0,
                 extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        collapse_rows(1,
                      valign = 'top') |>
        row_spec(nrow(df),
                 extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df,
            format = 'html',
            align = 'c',
            caption = cap)  |>
        kable_styling(fixed_thead = TRUE,
                      bootstrap_options = c('condensed'),
                      full_width = TRUE,
                      position = 'left',
                      font_size = 12) |>
        row_spec(0,
                 extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df),
                 extra_css = 'border-bottom: 1px solid #000000;')
    }
  } else NULL
}

make_dt <- function(df, cap){
  datatable(df,
            class = 'cell-border stripe',
            rownames = FALSE,
            caption = cap,
            extensions = c("FixedColumns", "Buttons"),
            options = list(
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$('body').css({'font-size': '11px'});",
                "$('body').css({'font-family': 'Arial'});",
                "$(this.api().table().header()).css({'font-size': '11px'});",
                "$(this.api().table().header()).css({'font-family': 'Arial'});",
                "}"),
              pageLength = nrow(df),
              autoWidth = FALSE,
              scrollX = '850px',
              scrollY = '600px',
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 1),
              dom = "Blfrtip",
              buttons = c('copy', 'csv', 'print')
            ),
            filter = list(position = c('top'),
                          clear = FALSE))
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level),
                                 collapse = ""),
                          " ",
                          tab_title,
                          " {.tabset} ", "\n\n")}
  check_null(table)
}

## Macroplot data ----
### Merging tables ----

#### Plot Matrix List
macro <- NGPN_tables$MacroPlot # list of plots and purpose
samp <- NGPN_tables$SampleEvent # list of sample events

#### formatting date
samp$SampleEvent_Date <-
  as.Date(substr(samp$SampleEvent_Date, 1, 11), format = "%Y-%m-%d")

#### selecting only pcm type plots
plots <- macro$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_", macro$MacroPlot_Name)]

### Macro plot df ----
macro_plots <- macro |>
  # filtering pcm plots
  filter(MacroPlot_Name %in% plots)|>
  # cleaning up columns
  select(MacroPlot_Name,
         MacroPlot_Purpose,
         MacroPlot_Type,
         MacroPlot_RegistrationUnit_GUID,
         MacroPlot_UTM_X,
         MacroPlot_UTM_Y,
         MacroPlot_UTMzone,
         MacroPlot_Datum,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long,
         MacroPlot_Elevation,
         MacroPlot_Aspect,
         MacroPlot_Azimuth,
         MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect,
         MacroPlot_UV1,
         MacroPlot_UV2,
         MacroPlot_UV3,
         MacroPlot_UV4,
         MacroPlot_GUID) |>
  # naming parks
  mutate(park = substr(MacroPlot_Name, 1, 4)) |>
  # dropping duplicate plots
  distinct()

### Macro plot/sample events ----
# joining macro plots with sample events
macro_samp <- left_join(macro_plots, samp, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID")) |>
  # cleaning up columns
  select(park,
         MacroPlot_Name,
         MacroPlot_GUID,
         MacroPlot_Purpose,
         MacroPlot_Type,
         SampleEvent_GUID,
         SampleEvent_Date,
         MacroPlot_UTM_X,
         MacroPlot_UTM_Y,
         MacroPlot_UTMzone,
         MacroPlot_Datum,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long,
         MacroPlot_Elevation,
         MacroPlot_Aspect,
         MacroPlot_Azimuth,
         MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect,
         MacroPlot_UV1,
         MacroPlot_UV2,
         MacroPlot_UV3,
         MacroPlot_UV4) |>
  # removing duplicates
  distinct()

# adding year
macro_samp$year <- as.numeric(format(as.Date(macro_samp$SampleEvent_Date,
                                             format = "%Y-%m-%d"), "%Y"))

### Add monitoringstatus_base ----
monstat <- NGPN_tables$MonitoringStatus # monitoringstatus_base
mm_monstat_se <- NGPN_tables$MM_MonitoringStatus_SampleEvent # needed for GUID matching

# merge for correct GUID
macro_samp_ms1 <- left_join(macro_samp,
                            mm_monstat_se,
                            by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"))

# merge for monitoringstatus_base
macro_samp_ms2 <- left_join(macro_samp_ms1,
                            monstat,
                            by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID",
                                   "datasource"))


macro_samp_ms <- macro_samp_ms2 |>
  # removing anything before 2011
  filter(year >= 2011) |>
  select(park,
         MacroPlot_Name,
         MonitoringStatus_Name,
         MacroPlot_Purpose,
         MacroPlot_Type,
         SampleEvent_Date,
         year,
         MacroPlot_UTM_X,
         MacroPlot_UTM_Y,
         MacroPlot_UTMzone,
         MacroPlot_Datum,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long,
         MacroPlot_Elevation,
         MacroPlot_Aspect,
         MacroPlot_Azimuth,
         MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect,
         MacroPlot_UV1,
         MacroPlot_UV2,
         MacroPlot_UV3,
         MacroPlot_UV4)

## Panel Filter ----
# removing sample events from macro_samp_ms that don't have a panel_yr match
# (keeping all observations for panel_sch)

# standard plots
all_parks <- macro_samp_ms |>
  dplyr::filter(!park %in% c("BADL",
                             "THRO")) |>
  dplyr::semi_join(panel_sch,
                   by = c("MacroPlot_Purpose" = "Panel",
                          "year" = "Year"))

# badl
badl <- macro_samp_ms |>
  dplyr::filter(park == "BADL") |>
  dplyr::semi_join(badl_panel_sch,
                   by = c("MacroPlot_Purpose" = "Panel",
                          "year" = "Year"))

# thro upland
thro_up <- macro_samp_ms |> 
  dplyr::filter(park == "THRO" &
                  MacroPlot_Type == "Upland") |>
  dplyr::semi_join(thro_up_panel_sch,
                   by = c("MacroPlot_Purpose" = "Panel",
                          "year" = "Year"))

# thro riparian
thro_rip <- macro_samp_ms |> 
  dplyr::filter(park == "THRO" &
                  MacroPlot_Type == "Riparian") |>
  dplyr::semi_join(thro_up_panel_sch,
                   by = c("MacroPlot_Purpose" = "Panel",
                          "year" = "Year"))

macro_samp_ms_all <- rbind(all_parks,
                           badl,
                           thro_up,
                           thro_rip)

# filtering anything that isn't PlantCommunity samples 
macro_samp_ms_all1 <- macro_samp_ms_all |> 
  # filtering anything that isn't plant community samples
  mutate(keep = case_when(!grepl("Fire",
                                 MonitoringStatus_Name) &
                            grepl("PlantCommunity",
                                  MonitoringStatus_Name) ~ 1,
                          grepl("Dual",
                                MonitoringStatus_Name) ~ 1,
                          TRUE ~ 0)) |>
  filter(keep == 1) |> select(-keep) |>
  arrange(MacroPlot_Name, year)
  
## Off Sched Panels ----
samp_events_off <- anti_join(macro_samp_ms,
                             macro_samp_ms_all1) |> 
  select(park,
         MacroPlot_Name,
         MonitoringStatus_Name,
         MacroPlot_Purpose,
         SampleEvent_Date)

### Creating Park List ----
park_list_samp_off <- sort(unique(samp_events_off$park))

## Macroplot Checks ----
macro_plots <- macro_samp_ms_all1 |>
  distinct()

park_list <- sort(unique(macro_plots$park))

### Coordinates Checks ----
# names(macro_plots)

### Missing Coordinates ----
macro_miss_utm <- macro_plots |>
  filter(is.na(MacroPlot_UTM_X) | is.na(MacroPlot_UTM_Y) | is.na(MacroPlot_UTMzone)) |>
  select(MacroPlot_Name,
         MacroPlot_UTM_X,
         MacroPlot_UTM_Y,
         MacroPlot_UTMzone,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long) |>
  arrange(MacroPlot_Name)

# Adding to initial table
QC_table <- QC_check(df = macro_miss_utm, meas_type = "MacroPlot", tab = "Plot Info",
                     check = "NGPN PCM plots missing UTM X, Y, and/or UTM Zone data.",
                     chk_type = 'error')

# Individual table
kbl_macro_miss_utm <- make_kable(macro_miss_utm,
                                 "NGPN PCM plots missing UTM X, Y, and/or Zone data. ") |>
  column_spec(2, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTM_X),
                                     "#F2F2A0", "white")) |>
  column_spec(3, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTM_Y),
                                     "#F2F2A0", "white")) |>
  column_spec(4, background = ifelse(is.na(macro_miss_utm$MacroPlot_UTMzone),
                                     "#F2F2A0", "white"))

### Plots outside NPS Bounds ----
# Set bounding box for each park and check UTMs and/or lat/long against them:
# First downloaded NPS Administrative Boundaries from:
# https://irma.nps.gov/DataStore/Reference/Profile/2309935
try(nps_bounds <- read_sf("./www/Administrative Boundaries of National Park System Units.shp"), silent = T)

try(nps_bounds <- read_sf("./docs/www/Administrative Boundaries of National Park System Units.shp"), silent = T)

if(!exists("nps_bounds"))stop("Administrative Boundaries of National Park System Units.shp \nnot found. Please download it from \nhttps://irma.nps.gov/DataStore/Reference/Profile/2309935 \nand add it to the ./docs/www/ folder")

# st_crs(nps_bounds) # EPSG 4269

# lookup for each park
crs_lookup <- tibble::tribble(~UNIT_CODE, ~crs,
                              "AGFO", 26913,
                              "BADL", 26913,
                              "DETO", 26913,
                              "FOLA", 26913,
                              "FOUS", 26913,
                              "JECA", 26913,
                              "KNRI", 26914,
                              "MORU", 26913,
                              "SCBL", 26913,
                              "THRO", 26913,
                              "WICA", 26913)

# Creating polygons for NGPN
ngpn_poly <- nps_bounds |>
  dplyr::filter(UNIT_CODE %in% park_list) |>
  left_join(crs_lookup,
            by ="UNIT_CODE")

# splitting into parks
poly_list <- ngpn_poly |>
  split(ngpn_poly$UNIT_CODE) |>
  map(~ st_as_sf(.x))

# Getting Plot locations
macro_plots_gps <- macro_plots |>
  select(park,
         MacroPlot_Name,
         MacroPlot_UTM_X, 
         MacroPlot_UTM_Y) |>
  rename(UNIT_CODE = park) |>
  filter(!is.na(MacroPlot_UTM_X)) |>
  left_join(crs_lookup, by = "UNIT_CODE")

# convert each park to sf object
pts_list <- macro_plots_gps |>
  split(macro_plots_gps$UNIT_CODE) |>
  map(~ st_as_sf(.x, coords = c("MacroPlot_UTM_X", "MacroPlot_UTM_Y"), crs = .x$crs[1]))

# Identify points outside polygons
out_pts_list <- map(names(pts_list), function(p) {
  pts <- pts_list[[p]]
  poly <- poly_list[[p]]

  # Matching CRS
  pts <- st_transform(pts, st_crs(poly))

  # getting points that don't intersect polys
  pts[!st_intersects(pts, poly, sparse = FALSE), ]
})

out_pts <- bind_rows(out_pts_list)

out_pts_utm <- out_pts |>
  mutate(UTM_X = st_coordinates(geometry)[,1],
         UTM_Y = st_coordinates(geometry)[,2]) |>
  st_drop_geometry() |>
  select(MacroPlot_Name, UTM_X, UTM_Y) |>
  arrange(MacroPlot_Name)

# Adding values to QC_table
QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_utm,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.",
                           chk_type = 'error'))

kbl_out_pts_utm <- make_kable(out_pts_utm,
                              cap = "NGPN PCM MacroPlot UTM coordinates that are not within the park boundary.")

### NPGN plots with non-standard datum ----
datum <- macro_plots |>
  select(MacroPlot_Name, MacroPlot_UTM_X, MacroPlot_UTM_Y,
         MacroPlot_UTMzone, MacroPlot_Datum) |>
  filter(!MacroPlot_Datum %in% "NAD83") |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = datum,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM plot coordinates with datum not matching 'NAD83'.",
                           chk_type = 'error'))

kbl_datum <- make_kable(datum,
                        cap = "NGPN PCM plot coordinates with datum not matching 'NAD83'")

# For plots with lat/long only, check if they're within the park bounds.
# This helps if we need to use the lat/longs


# Getting Plot locations
macro_plots_DD <- macro_plots |>
  select(park,
         MacroPlot_Name,
         MacroPlot_DD_Lat,
         MacroPlot_DD_Long) |>
  rename(UNIT_CODE = park) |>
  filter(!is.na(MacroPlot_DD_Lat))

# convert each park to sf object
ptsdd_list <- macro_plots_DD |>
  split(macro_plots_DD$UNIT_CODE) |>
  map(~ st_as_sf(.x, coords = c("MacroPlot_DD_Long", "MacroPlot_DD_Lat"), crs = 4269))

# Identify points outside polygons
out_ptsdd_list <- map(names(ptsdd_list), function(p) {
  pts <- pts_list[[p]]
  poly <- poly_list[[p]]

  # Matching CRS
  pts <- st_transform(pts, st_crs(poly))

  # getting points that don't intersect polys
  pts[!st_intersects(pts, poly, sparse = FALSE), ]
})

out_ptsdd <- bind_rows(out_ptsdd_list)

out_pts_dd <- out_ptsdd |>
  mutate(DD_Long = st_coordinates(geometry)[,1],
         DD_Lat = st_coordinates(geometry)[,2]) |>
  st_drop_geometry() |>
  select(MacroPlot_Name,
         DD_Long,
         DD_Lat) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = out_pts_dd,
                           meas_type = "MacroPlot",
                           tab = "Plot Info",
                           check = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.",
                           chk_type = 'error'))

kbl_out_pts_dd <- make_kable(out_pts_dd,
                             cap = "NGPN PCM MacroPlot DD coordinates that are not within the park boundary for plots missing UTM X,Y.")

### Check for blank macro data ----
macro_blanks <- macro_plots |>
  select(MacroPlot_Name,
         MacroPlot_Elevation,
         MacroPlot_Aspect,
         MacroPlot_Azimuth,
         MacroPlot_SlopeHill) |>
  filter(is.na(MacroPlot_Elevation) | is.na(MacroPlot_Azimuth) | 
           is.na(MacroPlot_Aspect) | is.na(MacroPlot_SlopeHill)) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_blanks,
                           meas_type = "MacroPlot",
                           tab = "Blank Loc. Values",
                           check = "Macroplots missing location information",
                           chk_type = 'error'))

kbl_macro_blanks <- make_kable(macro_blanks,
                               cap = "Macroplots missing location information")

### Check for impossible macro data ----
macro_imp <- macro_plots |>
  select(MacroPlot_Name,
         MacroPlot_Elevation,
         MacroPlot_Azimuth,
         MacroPlot_Aspect,
         MacroPlot_SlopeHill,
         MacroPlot_SlopeTransect) |>  # slopes should be <= 100%
  filter(MacroPlot_SlopeTransect != 9999,
         MacroPlot_Azimuth > 360 | MacroPlot_Azimuth < 0 | 
           MacroPlot_Aspect > 360 | MacroPlot_Aspect < 0 |
           MacroPlot_SlopeHill > 100 | MacroPlot_SlopeTransect > 100) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_imp,
                           meas_type = "MacroPlot",
                           tab = "Impossible Values",
                           check = "Macroplot location data with impossible values",
                           chk_type = 'error'))

kbl_macro_imp <- make_kable(macro_imp,
                            cap = "Macroplot location data with impossible values")

### Check on UV values ----
# UV1 = Topographic position; UV2 = Surface water; UV3 = Hydrologic Regime; UV4 = Vegetation Type
macro_uv <- macro_plots |>
  select(MacroPlot_Name,
         MacroPlot_UV1,
         MacroPlot_UV2,
         MacroPlot_UV3,
         MacroPlot_UV4)

#### UV 1 ----
# check topo positions that aren't CR, DR, LV, LS, MS, RO, SB, US
topo_codes <- c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US')

pattern <- paste0("^(", 
                  paste(topo_codes, collapse="|"), 
                  ")(-(", 
                  paste(topo_codes, collapse="|"), 
                  "))?$")

macro_topo <- macro_uv |>
  filter(!grepl(pattern, MacroPlot_UV1)) |> 
  # filter(!MacroPlot_UV1 %in%
  #          c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US')) |>
  select(MacroPlot_Name,
         MacroPlot_UV1) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_topo,
                           meas_type = "MacroPlot",
                           tab = "UV1 Topo Positions",
                           check = "Macroplot topographic positions (UV1) that don't identically match c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US')",
                           chk_type = "check"))

kbl_macro_topo <- make_kable(macro_topo,
                             cap = "Macroplot topographic positions (UV1) that don't identically match c('CR', 'DR', 'LV', 'LS', 'MS', 'RO', 'SB', 'US').")

#### UV 4 ----
# UV4 = Veg type; check vegetation type
veg_codes <- c("BS", "HR", "PP", "RW", "SH", "UG", "WD")

veg_pattern <- paste0("^(", 
                      paste(veg_codes, collapse="|"), 
                      ")(-(", 
                      paste(veg_codes, collapse="|"), 
                      "))?$")

macro_veg <- macro_uv |>
  filter(!grepl(veg_pattern, MacroPlot_UV4)) |> 
  # filter(!MacroPlot_UV4 %in% c("BS", "HR", "PP", "RW", "SH", "UG", "WD")) |>
  select(MacroPlot_Name,
         MacroPlot_UV4) |>
  arrange(MacroPlot_Name)

QC_table <- rbind(QC_table,
                  QC_check(df = macro_veg,
                           meas_type = "MacroPlot",
                           tab = "UV4 Veg. Type",
                           check = "Macroplot Vegetation Type (UV4) that doesn't identically match c('BS', 'HR', 'PP', 'RW', 'SH', 'UG', 'WD')",
                           chk_type = "error"))

kbl_macro_veg <- make_kable(macro_veg,
                            cap = "Macroplot Vegetation Type (UV4) that doesn't identically match c('BS', 'HR', 'PP', 'RW', 'SH', 'UG', 'WD')")


# check if MacroPlot checks returned at least 1 record to determine whether to include that tab in report
macro_check <- QC_table |> filter(Type %in% "MacroPlot" & Num_Records > 0)
macro_include <- tab_include(macro_check)

# Compile final QC Table ----
# revise for different color combos for checks (99 vs 90)? Drop for checks vs. errors?
QC_cap <- "The table below documents Quality Control checks performed on NGPN
Plant Community Monitoring data that are stored in the FFI database. This report
primarily checks data that are entered when a new plot is added to the sampling
schedule, which once fixed, are unlikely to produce errors again. If records
are returned for a given check, the row is  highlighted yellow for errors and
blue for records that aren't necessarily errors, but need further review. A
separate tab corresponding to each check that returned results by protocol
module is printed to the right of this tab. Only MacroPlots with samples that
fall on the panel schedule sampling scheme are included in this section of the
report."

QC_check_table <- kable(QC_table,
                        format = 'html',
                        align = 'c',
                        caption = QC_cap,
                        col.names = c("Type",
                                      "Data Tab",
                                      "Check Description",
                                      "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE,
                bootstrap_options = c('condensed'),
                full_width = TRUE,
                position = 'left',
                font_size = 12) |>
  row_spec(0,
           extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3,
              width = "300px") |>
  column_spec(2:ncol(QC_table),
              background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error",
                       "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check",
                              "#b7d8ef",
                              "#ffffff"))) |>
  collapse_rows(1,
                valign = 'top') |>
  row_spec(nrow(QC_table),
           extra_css = 'border-bottom: 1px solid #000000;')

