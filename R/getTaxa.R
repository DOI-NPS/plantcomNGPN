#' @title getTaxa
#'
#' @description Combines the local species and master species plant lists for
#' easier joins with other tables. Note that scientific names are combined from
#' the MasterSpecies and AuxSpecies tables such that, if a scientific name is
#' missing from the MasterSpecies table, the the scientific name from the
#' AuxSpecies table is used. The same goes for ITIS_TSN, family, genus, and
#' symbol columns.
#'
#' @importFrom dplyr left_join select
#'
#' @param output Quoted string. Options are "short" (default), which only
#' returns most important columns; "verbose" returns all columns in the
#' SampleEvent-related tables.
#'
#' @examples
#' \dontrun{
#'
#' library(plantcomNGPN)
#' importData(type = 'local',
#'   dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'              "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'              "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#'   export = F)
#'
#' # Compile only important columns of taxa table (default)
#' spp <- getTaxa()
#'
#' # Compile all columns of taxa table, with most important columns first
#' spp_all <- getTaxa(output = 'verbose')
#'
#' }
#'
#' @return Returns a data frame of plant species data
#'
#' @export

getTaxa <- function(output = "short"){

  #---- Bug handling ----

  ## output
  output <- match.arg(output, c("short", "verbose"))

  #---- Load species tables ----
  env <- if(exists("NGPN_tables")){NGPN_tables} else {.GlobalEnv}

  tryCatch(localspp <- get("LocalSpecies", envir = env),
    error = function(e){
      stop("LocalSpecies table not found. Please import NGPN FFI data tables.")
      })

  tryCatch(mastspp <- get("MasterSpecies", envir = env),
    error = function(e){
      stop("MasterSpecies table not found. Please import NGPN FFI data tables. ")
      })

  tryCatch(auxspp <- get("AuxSpecies", envir = env),
    error = function(e){
      stop("AuxSpecies table not found. Please import NGPN FFI data tables.")
      })

  tryCatch(regunit <- get("RegistrationUnit", envir = env),
    error = function(e){
      stop("RegistrationUnit table not found. Please import NGPN FFI data tables.")
      })

  tryCatch(lifeform <- get("LU_LifeForm", envir = env),
    error = function(e){
      stop("LU_LifeForm table not found. Please import NGPN FFI data tables.")
      })

  # NGPN does not appear to use the SpeciesPickList, so not including here.
  # lifecycle doesn't appear to be used much by NGPN, so not including it here.

  #---- Table joins ----
  locspp_reg <- left_join(localspp, regunit,
                    by = c("LocalSpecies_RegistrationUnitGUID" = "RegistrationUnit_GUID",
                           "datasource"))

  spp1 <- left_join(locspp_reg, auxspp,
                    by = c("LocalSpecies_AuxSpeciesGUID" = "AuxSpecies_GUID",
                           "LocalSpecies_RegistrationUnitGUID" = "AuxSpecies_RegistrationUnitGUID",
                           "datasource"))

  spp2 <- left_join(spp1, mastspp,
                    by = c("LocalSpecies_MasterSpeciesGUID" = "MasterSpecies_GUID",
                           'datasource'))

  spp3 <- left_join(spp2, lifeform,
                    by = c("LocalSpecies_PreferedLifeForm_GUID" = "LU_LifeForm_GUID",
                           "datasource"))

  # Merge Master and Aux species list to return complete species list.
  spp3$ScientificName <- ifelse(is.na(spp3$MasterSpecies_ScientificName),
                                spp3$AuxSpecies_ScientificName,
                                spp3$MasterSpecies_ScientificName)

  spp3$ITIS_TSN <- ifelse(is.na(spp3$MasterSpecies_ITIS_TSN),
                          spp3$AuxSpecies_ITIS_TSN,
                          spp3$MasterSpecies_ITIS_TSN)

  spp3$Family <- ifelse(is.na(spp3$MasterSpecies_Family),
                        spp3$AuxSpecies_Family,
                        spp3$MasterSpecies_Family)

  spp3$Genus <- ifelse(is.na(spp3$MasterSpecies_Genus),
                       spp3$AuxSpecies_Genus,
                       spp3$MasterSpecies_Genus)

  spp3$Symbol <- ifelse(is.na(spp3$MasterSpecies_Symbol),
                        spp3$AuxSpecies_Symbol,
                        spp3$MasterSpecies_Symbol)

  spp3$NotBiological <- ifelse(is.na(spp3$MasterSpecies_NotBiological),
                               spp3$AuxSpecies_NotBiological,
                               spp3$MasterSpecies_NotBiological)

  spp3$Nativity <- ifelse(is.na(spp3$LocalSpecies_Nativity),
                          spp3$MasterSpecies_Nativity,
                          spp3$LocalSpecies_Nativity)

  spp3$CommonName <- ifelse(is.na(spp3$LocalSpecies_CommonName),
                            spp3$MasterSpecies_CommonName,
                            spp3$LocalSpecies_CommonName)

  spp4 <- spp3 |> select(-MasterSpecies_ScientificName,
                         -AuxSpecies_ScientificName,
                         -MasterSpecies_ITIS_TSN,
                         -AuxSpecies_ITIS_TSN,
                         -MasterSpecies_Family,
                         -AuxSpecies_Family,
                         -MasterSpecies_Genus,
                         -AuxSpecies_Genus,
                         -MasterSpecies_Symbol,
                         -LocalSpecies_Symbol,
                         -AuxSpecies_Symbol,
                         -MasterSpecies_NotBiological,
                         -AuxSpecies_NotBiological,
                         -LocalSpecies_Nativity,
                         -MasterSpecies_Nativity,
                         -LocalSpecies_CommonName,
                         -MasterSpecies_CommonName)

  # hacky way to keep LocalSpecies_UV1 as is
  names(spp4)[names(spp4) == "LocalSpecies_GUID"] <- "Spp_GUID"
  names(spp4)[names(spp4) == "LocalSpecies_UV1"] <- "Species_UV1"

  # Drop table names from column names for easier coding
  names(spp4) <-
    gsub("^MacroPlot_|^LocalSpecies_|^MasterSpecies_|^LU_|^AuxSpecies_|^Registration",
         "", names(spp4))


  keep_cols <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName", "Family",
                 "Genus", "Nativity", "Invasive", "Cultural", "Concern",
                 "LifeCycle", "LifeForm_Name", "NotBiological", "UserAdded",
                 "Species_UV1", "IsUnknown", "IsUnlisted", "Description",
                 "Comment", "Unit_Name", "SymbolKey", "Synonym_SymbolKey",
                 "Spp_GUID", "RegistrationUnitGUID")

  final_names <- if(output == "short"){
    keep_cols
    } else {c(keep_cols, sort(setdiff(names(spp4), keep_cols)))}

  spp_final <- spp4[,final_names]

  return(spp_final)
  }
