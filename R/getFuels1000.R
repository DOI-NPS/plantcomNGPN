#' @include getSampleEvent.R
#'
#' @title getFuels1000
#'
#' @description This function filters and joins FFI Surface Fuels 1000Hr data by
#' park, plot name, purpose, project, sample year, and other parameters.
#'
#' @importFrom dplyr distinct
#'
#' @param park Filter on park code (aka Unit_Name). Can select more than one.
#' Valid inputs:
#' \itemize{
#' \item{"all":} {Include all NGPN parks with FFI data}
#' \item{"AGFO":} {Agate Fossil Beds National Monument}
#' \item{"BADL":} {Badlands National Park}
#' \item{"DETO":} {Devils Tower National Monument}
#' \item{"FOLA":} {Fort Laramie National Historic Site}
#' \item{"FOUS":} {Fort Union Trading Post National Historic Site}
#' \item{"JECA":} {Jewel Cave National Monument}
#' \item{"KNRI":} {Knife River Indian Villages National Historic Sites}
#' \item{"MORU":} {Mount Rushmore National Monument}
#' \item{"SCBL":} {Scotts Bluff National Monument}
#' \item{"THRO":} {Theodore Roosevelt National Park}
#' \item{"WICA":} {Wind Cave National Park}
#'}
#'
#' @param plot_name Quoted string to return a particular plot based on
#' MacroPlot_Name. Default is "all", which if purpose is set to "NGPN_PCM"
#' (default), and project is set to "Park" (default), then only NGPN Plant
#' Community Monitoring plots (e.g.,macroplots with "_PCM_" in their names) will
#' be included. Can select multiple plots. If a plot name is specified that does
#' not occur in the imported data, function will error out with a list of
#' unmatched plot names.
#'
#' @param project Quoted string to return plots of a particular project, based
#' on ProjectUnit_Name. In NGPN, this typically is the strata a given plot
#' belongs to. By default, selects NGPN_PCM plots, which are plots with "_PCM_"
#' in their name and the "Park" stratum for those plots. Note that some plots
#' fall in multiple stratum, such as Park and Native Prairie in AGFO. In those
#' cases, the "Park" strata is selected by default. If a user wants a different
#' strata than "Park", that can be specified using the codes below. Only one
#' project can be specified at a time.
#' Current valid inputs:
#' \itemize{
#' \item{"Park":} {Default. *NGPN_PCM* stratum covering whole park (same.}
#' \item{"ABAM":} {*NGPN_PCM* stratum in WICA.}
#' \item{"Bodmer":} {*NGPN_PCM* stratum in FOUS.}
#' \item{"Fort":} {*NGPN_PCM* stratum in FOUS.}
#' \item{"Native Prairie":} {*NGPN_PCM* stratum in AGFO.}
#' \item{"North Riparian":} {*NGPN_PCM* stratum in THRO.}
#' \item{"North Upland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"North Unit":} {*NGPN_PCM* stratum in BADL.}
#' \item{"Pine Forest":} {*NGPN_PCM* stratum in DETO, JECA, MORU, and WICA.}
#' \item{"Prairie":} {*NGPN_PCM* stratum in BADL, DETO, FOUS, KNRI, SCBL, THRO, and WICA.}
#' \item{"Riparian":} {*NGPN_PCM* stratum in AGFO, DETO, and FOLA.}
#' \item{"Shrubland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"South Riparian":} {*NGPN_PCM* stratum in THRO.}
#' \item{"South Upland":} {*NGPN_PCM* stratum in THRO.}
#' \item{"Upland":} {*NGPN_PCM* stratum in DETO and FOLA.}
#' }
#' Other options include c("ABAM Supplemental", "AnnualBrome_Research",
#'                         "American Elk Invasive Research", "Archaeology JFSP",
#'                         "Belle Fourche Invasive Research", "CBI",
#'                         "Cedar Removal Study", "Centennial Invasive Research",
#'                         "Control Invasive Research", "Deciduous Woodland",#'
#'                         "FFI TESTING", "Highland Creek TH Herbicide Trial",
#'                         "INACTIVE", "Juniper Woodland", "Lithograph Invasive Research",
#'                         "Monitoring", "Pringle Dog Town Herbicide Trial", "Woodland")
#'
#' @param purpose Quoted string to return plots with a particular purpose, which
#' typically refers to a characteristic of the plot's sample design in NGPN
#' (e.g., Panel1). Note that purpose is not standard across parks. This function
#' standardizes some purposes (eg "FX" and "Fire Effects" are both called "FX
#' monitoring"). The following purposes that can be specified are below. By
#' default, "NGPN_PCM" plots are selected, which includes all plots with "_PCM_"
#' in their name. If new purposes are added in the future, they will need to be
#' added to the bug handling code in the function.
#' Valid inputs:
#' \itemize{
#' \item{"all":} {All plots in imported FFI database}
#' \item{"NGPN_PCM":} {Default. NGPN Plant Community Monitoring Plots with c("_PCM_", "_FPCM_", "_LPCM_", and, "_RCM_") in their name, and all of the purposes defined below.}
#' \item{"ForestStructure":}{NGPN Forest Structure plot. Found in KNRI and WICA.}
#' \item{"IM_Intensive"}{NGPN intensive monitoring plot. Found in AGFO, FOUS, and THRO.}
#' \item{"Panel1":} {NGPN PCM Panel 1}
#' \item{"Panel2":} {NGPN PCM Panel 2}
#' \item{"Panel3":} {NGPN PCM Panel 3}
#' \item{"Panel4":} {NGPN PCM Panel 4}
#' \item{"Panel5":} {NGPN PCM Panel 5}
#' \item{"Panel6":} {NGPN PCM Panel 6}
#' \item{"Panel7":} {NGPN PCM Panel 7}
#' \item{"Panel8":} {NGPN PCM Panel 8}
#' \item{"Panel9":} {NGPN PCM Panel 9}
#' \item{"Panel10":} {NGPN PCM Panel 10}
#' \item{"PanelE":} {NGPN PCM Extensive. Found in DETO, FOLA, JECA, MORU, SCBL, and THRO.}
#' \item(""){A number of PCM plots have a blank purpose. Until this is resolved, or we know these can be dropped, they are included in the NGPN_PCM purpose}
#' }
#' Other non NPGN_PCM options include:
#'    c("AnnualBromeResearch" (BADL, SCBL)
#'      "ABAM Supplemental" (BADL, FOLA, WICA),
#'      "CBI plot monitoring" (WICA),
#'      "Control" (MNRR),
#'      "Daubenmire Plot" (KNRI),
#'      "Early Detection" (DETO),
#'      "FIRE" (JECA, KNRI),
#'      "Fire/I&M Veg Monitoring Plot" (DETO, KNRI, SCBL, and THRO),
#'      "Fire/IM Pilot Study Plot" (DETO),
#'      "FIRE_Dual" (WICA),
#'      "FMH Forest Plot" (BADL, DETO, JECA, KNRI, MORU, SCBL, THRO, WICA),
#'      "FMH Grass Plot" (AGFO, BADL, DETO, KNRI, SCBL, THRO, WICA),
#'      "FMH Shrub Plot" (BADL, SCBL, and THRO),
#'      "Forest and Fuels" (MORU, SCBL, and WICA.),
#'      "Forest Fuels and Vegetation" (WICA),
#'      "Forest Plot" (WICA),
#'      "FPCM Grassland plot" (DETO),
#'      "FX Dual" (DETO and WICA),
#'      "FX Extensive" (WICA),
#'      "FX Intensive" (BADL, KNRI, THRO),
#'      "FX Monitoring" (AGFO, BADL, DETO, FOUS, KNRI, MORU, SCBL, THRO, and WICA),
#'      "HTLN Legacy" (AGFO and SCBL),
#'      "I&M_tower_vegetation (Found in DETO),
#'      "IM_FX_Dual" (DETO),
#'      "IM_veg" (THRO),
#'      "Invasives Research" (DETO, JECA, and WICA),
#'      "Lafferty Plot" (MORU),
#'      "LTEM/FMH" (AGFO),
#'      "Modified Forest Plot" (THRO),
#'      "Modified Shrub Plot" (THRO),
#'      "NGP Fire Forest Fuel Veg Protcol" (DETO),
#'      "NGP Grassland Plot - Interior Burn Unit" (BADL),
#'      "Pre- and Post-treatment of fuels" (JECA),
#'      "Research" (WICA),
#'      "Treatment" (MNRR))
#'
#' @param mon_status Quoted string. Allows you to select different
#' MonitoringStatus$MonitoringStatus_Name types. Default is "NGPN_PCM", which
#' will pull in sample events coded a NGPN Plant Community Monitoring (see
#' description for NGPN_PCM below).
#' Current valid inputs:
#' \itemize{
#' \item{"NGPN_PCM":} {Default. Pulls in records with monitoring status base of "PlantCommunity".}
#' \item{"PlantCommunity":} {PlantCommunity only records}
#' \item{"FirePlantCommunity":} {FirePlantCommunity only records}
#' \item{"ForestStructure":} {ForestStructure only records}
#' }
#'
#' Other options include c("00Pre", "00Pre2", "01Burn", "01Post", "01Pre",
#'                         "01yr01", "01yr02", "01yr10", "FireOther_1",
#'                         "Dual", "FPCM_Other_01", "FPCM_Other_02", "Other",
#'                         "PCM_Other", "FireOther_2", "FPCM_Other",
#'                         "FireOther", "FireOther_FuelReduction",
#'                         "FPCM_Other_03", "FireOther_3", "Fire_Other",
#'                         "PCM_Fire", "Fire", "Ext", "00Pre02")
#'
#' @param years Numeric. Filter on years. Accepted values start at 2011. Default
#' is 2011 to current year,  which represents the time NGPN plant community
#' monitoring began using latest protocol and sample design.
#'
#' @param complete_events Logical. If TRUE (Default) only returns sample events
#' with associated sample data (eg Cover Point Data). If FALSE, returns all
#' sample events with a record in the SampleEvent table.
#' +++++ NOT YET ENABLED +++++
#'
#' @param output Quoted string. Options are "short" (default), which only
#' returns most important columns; "verbose" returns all columns in the
#' DensityBelts database tables.
#'
#' @examples
#' \dontrun{
#'
#' library(plantcomNGPN)
#' importViews(import_path = "./data/NGPN_FFI_views_20250708.zip")
#'
#' # get all surface fuels 1000hr for all parks, all years, for NGPN_PCM plots
#' sf1000 <- getFuels1000()
#' head(sf1000)
#'
#' # return THRO North Upland data
#' sf1000_thro <- getFuels1000(park = "THRO", project = "North Upland")
#' table(sf1000_thro$Unit_Name, sf1000_thro$ProjectUnit_Name, useNA = 'always')
#' head(sf1000_thro)
#'
#' # get surface fuels ForestStructure monitoring status
#' sf1000_for <- getFuels1000(mon_status = "ForestStructure")
#' table(sf1000_for$Unit_Name, sf1000_for$MonitoringStatus_Base, useNA = 'always')
#'
#' }
#'
#' @return Returns a data frame of 1000hr surface fuels data
#'
#' @export

getFuels1000 <- function(park = 'all', plot_name = "all", project = "Park",
                         purpose = "NGPN_PCM", mon_status = "NGPN_PCM",
                         years = 2011:format(Sys.Date(), "%Y"),
                         complete_events = TRUE, output = "short"){
  #---- Bug handling ----

  ## park
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "AGFO", "BADL", "DETO", "FOLA", "FOUS",
                      "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA"))

  if(any(park == "all")){
    park = c("AGFO", "BADL", "DETO", "FOLA", "FOUS",
             "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA")
  } else {park}

  ## years
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2011)

  ## output
  output <- match.arg(output, c("short", "verbose"))

  #---- Compile data ----
  env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}

  sampev <- getSampleEvent(park = park,
                           plot_name = plot_name,
                           project = project,
                           purpose = purpose,
                           mon_status = mon_status,
                           years = years,
                           complete_events = complete_events)

  ## get surface fuels
  sf1000 <- tryCatch(get("Surface_Fuels_1000Hr", envir = env),
                     error = function(e){
                       stop(paste0("Specified database does not contain Surface ",
                                   "Fuels 1000Hr data."))})

  sf1000$ProjectUnit_Name <- project

  # Filter on sample events
  sampev_guids <- unique(sampev$SampleEvent_GUID)
  sf1000_samp <- sf1000[sf1000$SampleEvent_GUID %in% sampev_guids,]
  # Drop records where Visited is NA or F
  sf1000_samp2 <- sf1000_samp[sf1000_samp$Visited == TRUE,]

  keep_cols <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose",
                 "ProjectUnit_Name", "UTM_X", "UTM_Y", "UTMzone", "Elevation",
                 "Aspect", "Azimuth", "SlopeHill", "SlopeTransect",
                 "SampleEvent_Date", "year", "month", "doy",
                 "DefaultMonitoringStatus", #"MonitoringStatus_Base",
                 "Visited",
                 "NumTran", "TranLen", "Index", "Transect", "Slope", "LogNum",
                 "Dia", "DecayCl", "CWDFuConSt", "Comment", #"UV1Desc", "UV2Desc",
                 "SaComment", "MacroPlot_GUID", "SampleEvent_GUID",
                 "RegistrationUnit_GUID")

  final_names <- if(output == "short"){
    keep_cols
  } else {c(keep_cols, sort(setdiff(names(sf1000_samp2), keep_cols)))}

  sf1000_final <- distinct(sf1000_samp2[order(sf1000_samp2$MacroPlot_Name,
                                              sf1000_samp2$SampleEvent_Date,
                                  sf1000_samp2$Transect,
                                  sf1000_samp2$LogNum),
                                final_names])

  if(nrow(sf1000_final) == 0){
    warning("Specified arguments returned an empty dataframe.")
  }

  return(sf1000_final)
}
