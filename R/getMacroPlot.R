#' @title getMacroPlot
#'
#' @description This function filters FFI macroplot data by park, plot name,
#' and project. This function was primarily developed to pull out NGPN
#' plant community monitoring plots. Using combinations of plot names or projects
#' that are outside NGPN PCM plots hasn't been tested as thoroughly, and may not 
#' return intended results in every case. Note that this is more of an internal 
#' function that other data-related getter functions source to efficiently 
#' filter on records.
#'
#' @importFrom dplyr distinct left_join
#'
#' @param park Filter on park code (aka Unit_Name). Can select more than one.
#' Valid inputs:
#' \itemize{
#' \item{"all":} {Default. Include all NGPN parks with FFI data}
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
#' MacroPlot_Name. Default is "all", but user can select multiple plots. If a 
#' plot name is specified that does not occur in the imported data, function 
#' will error out with a list of unmatched plot names.
#'
#' @param project Quoted string to return plots of a particular project, based
#' on ProjectUnit_Name. In NGPN, this typically is the strata a given plot
#' belongs to. By default, selects NGPN_PCM plots, which are plots with "Park" 
#' stratum. Note that some plots fall in multiple stratum, such as Park and 
#' Native Prairie in AGFO. In those cases, the "Park" strata is selected by 
#' default. If a user wants a different strata than "Park", that can be 
#' specified using the codes below. Only one project can be specified at a time. 
#' Current valid inputs:
#' \itemize{
#' \item{"Park":} {Default. *NGPN_PCM* stratum covering all macroplot names from NGPN sampling.}
#' \item{"ABAM":} {a stratum in WICA.}
#' \item{"Bodmer":} {a stratum in FOUS}
#' \item{"Fort":} {a stratum in FOUS.}
#' \item{"Native Prairie":} {a stratum in AGFO.}
#' \item{"North Riparian":} {a stratum in THRO.}
#' \item{"North Upland":} {a stratum in THRO.}
#' \item{"North Unit":} {a stratum in BADL.}
#' \item{"Pine Forest":} {a stratum in DETO, JECA, MORU, and WICA.}
#' \item{"Prairie":} {a stratum in BADL, DETO, FOUS, KNRI, SCBL, THRO, and WICA.}
#' \item{"Riparian":} {a stratum in AGFO, DETO, and FOLA.}
#' \item{"Shrubland":} {a stratum in THRO.}
#' \item{"South Riparian":} {a stratum in THRO.}
#' \item{"South Upland":} {a stratum in THRO.}
#' \item{"Upland":} {a stratum in DETO and FOLA.}
#' }
#' Other options include c("Deciduous Woodland", "INACTIVE")
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
#' # for the Park stratum and all purposes used by NGPN park project
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
                         # purpose = "NGPN_PCM", 
                         output = "short"){

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
  
  # Filtering plot names 
  # Create plot list to filter on
  if(identical(plot_name, "all")){
    
    # plot_name = "all"
    plot_list <- plot_names
    
  } else {
    plot_list <- plot_names[plot_names %in% plot_name]
  }

  # filter on park
  macro1 <- macro[macro$Unit_Name %in% park,]

  # filter on plot name
  macro2 <- macro1[macro1$MacroPlot_Name %in% plot_list,]

  # Set project list if all or NGPN_PCM selected
  mac_project1 <- names(macro2[grepl("ProjectUnit_", names(macro2))])
  mac_project2 <- gsub("ProjectUnit_", "", mac_project1)
  mac_project <- gsub("_", " ", mac_project2)

  # Check that project exists in the dataset
  bad_project <- setdiff(project, mac_project)

  if(length(bad_project) > 0){
    stop("Specified project not found in data: ",
         paste0(bad_project, sep = ", "))
    }

  project_col <- paste0("ProjectUnit_", gsub(" ", "_", project))
  macro3 <- macro2[macro2[,project_col] == 1,]

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

  macro4 <- if(output == "short"){
    macro3[,keep_cols]
  } else {macro5}

  if(nrow(macro4) == 0){stop(paste0("Specified function arguments returned an ",
                                    "empty data frame. Check that the ",
                                    "combination of plot_name, purpose, project,",
                                    "etc. arguments will return records."))}

  return(macro4)
  }
