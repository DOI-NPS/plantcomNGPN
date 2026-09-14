#' @title getMacroPlot
#'
#' @description This function filters FFI macroplot data by park, plot name,
#' project, and purpose. This function was primarily developed to pull out NGPN
#' plant community monitoring plots. Using combinations of plot names, projects
#' or purposes that are outside NGPN PCM plots hasn't been tested as thoroughly,
#' and may not return intended results in every case. Note that this is more of
#' an internal function that other data-related getter functions source to
#' efficiently filter on records.
#'
#' @importFrom dplyr distinct left_join
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
#' Community Monitoring plots (e.g.,macroplots with "_PCM_") will be included.
#' Can select multiple plots. If a plot name is specified that does not occur
#' in the imported data, function will error out with a list of unmatched plot
#' names.
#'
#' @param project Quoted string to return plots of a particular project, based
#' on ProjectUnit_Name. In NGPN, this typically is the strata a given plot
#' belongs to. By default, selects NGPN_PCM plots, which are plots with "_PCM_"
#' in their name and the "Park" stratum for those plots. Note that some plots
#' fall in multiple stratum, such as Park and Native Prairie in AGFO. In those
#' cases, the "Park" strata is selected by default. If a user wants a different
#' strata than "Park", that can be specified using the codes below.
#' Only one project can be specified at a time. Current valid inputs:
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
#'                         "INACTIVE", "Juniper Woodland",
#'                         "Lithograph Invasive Research", "Monitoring",
#'                         "Pringle Dog Town Herbicide Trial", "Woodland")
#'
#' @param purpose Quoted string to return plots with a particular purpose, which
#' typically refers to a characteristic of the plot's sample design in NGPN
#' (e.g. Panel1). Note that purpose is not standard across parks. This function
#' standardizes some purposes (eg "FX" and "Fire Effects" are both called "FX
#' monitoring"). The following purposes that can be specified are below. By
#' default, "NGPN_PCM" plots are selected, which includes all plots with "_PCM_"
#' in their name. If new purposes are added in the future, they will need to be
#' added to the bug handling code in the function.
#' Valid inputs:
#' \itemize{
#' \item{"all":} {All plots in imported FFI database}
#' \item{"NGPN_PCM":} {Default. NGPN Plant Community Monitoring Plots with "_PCM_" in their name, and Panel 1 - 10 defined below.}
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
#'       "Forest Plot" (WICA),
#'      "FPCM Grassland plot" (DETO),
#'      "FX Dual" (DETO and WICA),
#'      "FX Extensive" (WICA),
#'      "FX Intensive" (BADL, KNRI, THRO),
#'      "FX Monitoring" (AGFO, BADL, DETO, FOUS, KNRI, MORU, SCBL, THRO, and WICA),
#'      "HTLN Legacy" (AGFO and SCBL),
#'       "I&M_tower_vegetation (Found in DETO),
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
#' @param output Quoted string. Options are "short" (default), which only
#' returns most important columns; "verbose" returns all columns in the
#' MacroPlot database table.
#'
#' @examples
#' \dontrun{
#'
#' library(plantcomNGPN)
#' importViews(import_path = "NGPN_FFI_views_20250708.zip")
#'
#' # return all NGPN Plant Community Monitoring plots (ie vital signs plots),
#' # for the Park stratum and all purposes used by NGPN
#' macro_vs <- getMacroPlot()
#' table(macro_vs$Unit_Name, macro_vs$MacroPlot_Purpose)
#'
#' # return Prairie stratum for SCBL for NGPN_PCM plots
#' macro_pr_vs <- getMacroPlot(park = "SCBL", project = "Prairie")
#' table(macro_pr_vs$Unit_Name)
#'
#' # query NGPN only plots from North Dakota
#' macro_nd <- getMacroPlot(park = c("FOUS", "KNRI", "THRO"))
#' table(macro_nd$Unit_Name, macro_nd$MacroPlot_Purpose)
#'
#' # query and combine North and South Upland for THRO
#' thro_upn <- getMacroPlot(park = "THRO", project = "North Upland") |>
#' dplyr::select(-ProjectUnit_North_Upland)
#' thro_upn$ProjectUnit <- "North_Upland"
#'
#' thro_ups <- getMacroPlot(park = "THRO", project = "South Upland") |>
#' dplyr::select(-ProjectUnit_South_Upland)
#' thro_ups$ProjectUnit <- "South_Upland"
#'
#' thro_up <- rbind(thro_upn, thro_ups)
#' table(thro_up$Unit_Name, thro_up$ProjectUnit)
#' }
#'
#' @return Returns a data frame of macroplots
#'
#' @export

getMacroPlot <- function(park = 'all', plot_name = "all", project = "Park",
                         purpose = "NGPN_PCM", output = "short"){

  #---- Bug handling ----

  ## park
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "AGFO", "BADL", "DETO", "FOLA", "FOUS",
                      "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA"))

  if(any(park == "all")){
    park = c("AGFO", "BADL", "DETO", "FOLA", "FOUS",
             "JECA", "KNRI", "MORU", "SCBL", "THRO", "WICA")
    } else {park}

  ## project
  if(length(project) > 1){
    stop("Can only specify one project.")
  }

  ## output
  output <- match.arg(output, c("short", "verbose"))

  #---- Compile data ----
  env <- if(exists("VIEWS_NGPN")){VIEWS_NGPN} else {.GlobalEnv}

  ## get marcoplots
  tryCatch(
    macro <- get("MacroPlots", envir = env),
    error = function(e){
      stop("MacroPlots view not found. Please import NGPN FFI data.")
      }
    )

  # Check that plot_name matches at least one record in macroplot table
  plot_names <- sort(unique(macro$MacroPlot_Name))

  typo_names <- plot_name[!plot_name %in% c(plot_names, "all")]

  if(length(typo_names) > 0){
      stop(paste0("The following plot names do not have matching records in the",
                  " MacroPlot table: "),
           paste0(typo_names, collapse = ", "))
    }

  # Create plot list to filter on
  plot_list1 <- if(any(plot_name == "all")){
    plot_names
    } else {plot_name}

  plot_list <- if(all(purpose == "NGPN_PCM") &
                  any(plot_name == "all")){
    plot_names
  } else if(!any(plot_name %in% "all") &
             any(purpose == "NGPN_PCM")){
    intersect(plot_list1, plot_names)
  } else if(any(plot_name %in% "all") &
            any(purpose %in% "all") &
            any(project %in% "all")){
    plot_list1
  } else {plot_name}

  # Set purpose list if all is selected
  mac_purpose <- sort(unique(macro$MacroPlot_Purpose))

  purpose_list <- if(any(purpose %in% c("all"))){
    mac_purpose
  } else if(any(purpose %in% "NGPN_PCM")){
    mac_purpose[
      mac_purpose %in% c("Panel1", "Panel2", "Panel3", "Panel4", "Panel5", "Panel6",
                          "Panel7", "Panel8", "Panel9", "Panel10"#, "PanelE"
                         )]
  # } else if(any(purpose %in% "IM_Intensive")){
  #   mac_purpose[
  #     mac_purpose %in% c("FX Dual", "FX Intensive", "FX Monitoring", "IM_FX_Dual",
  #                        "IM_Intesive", "FX Extensive"
  #     )]
    } else {distinct(purpose)}

  # check that the specified purpose is found in the column names
  bad_purpose <- setdiff(purpose_list, mac_purpose)

  if(length(bad_purpose) > 0){
    stop("Specified purpose(s) not found in data: ",
         paste0(bad_purpose, sep = ", "))
    }

  # filter macro table on purpose
  macro2 <- macro[macro$MacroPlot_Purpose %in% purpose_list, ]

  # filter on park
  macro3 <- macro2[macro2$Unit_Name %in% park,]

  # filter on plot name
  macro4 <- macro3[macro3$MacroPlot_Name %in% plot_list,]

  # Set project list if all or NGPN_PCM selected
  mac_project1 <- names(macro4[grepl("ProjectUnit_", names(macro4))])
  mac_project2 <- gsub("ProjectUnit_", "", mac_project1)
  mac_project <- gsub("_", " ", mac_project2)

  # Check that project exists in the dataset
  bad_project <- setdiff(project, mac_project)

  if(length(bad_project) > 0){
    stop("Specified project not found in data: ",
         paste0(bad_project, sep = ", "))
    }

  project_col <- paste0("ProjectUnit_", gsub(" ", "_", project))
  macro5 <- macro4[macro4[,project_col] == 1,]

  # Compile final dataset
  keep_cols <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose",
                 "MacroPlot_Type", project_col, "Agency", "UTM_X", "UTM_Y",
                 "UTMzone", "Datum", "DD_Lat", "DD_Long", "Elevation",
                 "ElevationUnits", "Azimuth", "Aspect", "SlopeHill",
                 "SlopeTransect", "StartPoint", "Directions",
                 "MacroPlot_Comment", "MacroPlot_UV1", "MacroPlot_UV2",
                 "MacroPlot_UV3", "MacroPlot_UV4", "MacroPlot_UV5",
                 "MacroPlot_UV6", "MacroPlot_UV7", "MacroPlot_UV8", "Metadata",
                 "MacroPlot_GUID", "RegistrationUnit_GUID")

  macro6 <- if(output == "short"){
    macro5[,keep_cols]
  } else {macro5}

  if(nrow(macro6) == 0){stop(paste0("Specified function arguments returned an ",
                                    "empty data frame. Check that the ",
                                    "combination of plot_name, purpose, project,",
                                    "etc. arguments will return records."))}

  return(macro6)
  }
