# Params for troubleshooting ----
## These parameters come from FireEffects_network_MacroPlot_Qc_Checks.Rmd

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
# importData(type = 'local',
#            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#            keep_tables = T)

# # If SQL not set up, use import below instead, and update the import_path:
# 
# importData(type = 'csv', 
#            import_path = "../data/NGPN_FFI_table_export_20261019.zip",
#            keep_tables = T)

# Start of source code ----
## Loading data ----

# Species list
# or UPDATE PATH

try(tab4_spp <- read.csv("./data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"), 
    silent = T)
try(tab4_spp <- read.csv("../data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"),
    silent = T)

## Macroplot Checks ----
### Merging tables ----

#### Plot Matrix List
macro <- NGPN_tables$MacroPlot # list of plots and purpose
samp <- NGPN_tables$SampleEvent # list of sample events

#### formatting date
samp$SampleEvent_Date <-
  as.Date(substr(samp$SampleEvent_Date, 1, 11), format = "%Y-%m-%d")

#### selecting only pcm type plots
plots <- macro$MacroPlot_Name[grepl("_PCM_|_FPCM_|_RCM_", macro$MacroPlot_Name)]

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
         SampleEvent_Date) |>
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
         year)

## Sample Events from FireEffects 
samp_events_fire <- macro_samp_ms |>
  # filtering fire MonitoringStatus_Names
  mutate(keep = ifelse(grepl("Pre|Burn|Post|yr|Yr",
                             MonitoringStatus_Name), 1, 0)) |>
  filter(keep == 1) |>
  select(-keep) |>
  arrange(MacroPlot_Name, year)

# getting MonStat_Name plot
samp_events_count_fire <- samp_events_fire |>
  summarize(count = n(),
            .by = c(park,
                    MonitoringStatus_Name,
                    year)) |>
  arrange(year) |>
  pivot_wider(names_from = year,
              values_from = count) |>
  arrange(park, MonitoringStatus_Name)

# current year list of plot names
current_plots <- samp_events_fire |> 
  filter(year == params$year_curr) |> 
  select(park,
         MacroPlot_Name,
         MonitoringStatus_Name,
         SampleEvent_Date) |> 
  arrange(MacroPlot_Name)

# checking duplicate rows
samp_events_dup <- samp_events_fire[duplicated(samp_events_fire) | duplicated(samp_events_fire, fromLast = TRUE), ]

## Creating Park List ----
park_list_samp_fire <- sort(unique(samp_events_count_fire$park))
park_list_plot_names <- sort(unique(current_plots$park))
park_list_dup <- sort(unique(samp_events_dup$park))
