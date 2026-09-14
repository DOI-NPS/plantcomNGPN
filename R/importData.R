#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI
#' database tables using the schema designed for the Northern Great Plains
#' Network plant community monitoring protocol. Currently can only import from
#' a local installation of a park FFI database in SQL Server Management Studio
#' (SSMS), but the goal is to add an option for importing directly from the SQL
#' Server where the production FFI databases are housed. If multiple parks
#' worth of data are imported, tables are row binded, so that all NPGN parks
#' can be queried, summarized, etc. at once. After raw FFI tables are imported,
#' they are then joined into flattened views for each protocol. The views
#' currently only include plots that are part of NGPN PCM monitoring, and only
#' include sample events from 2011 and on, but this can be revised as needed.
#' The raw tables with all data and views of only NGPN PCM plots >= 2011 can
#' both be  exported via the export arguments. Function is slow if running for
#' all parks and exporting tables or views. Once the views are created/exported,
#' they can be imported using importViews() for faster importing. Note that
#' view column names dropped the original table name (eg 'MacroPlot_', 'SampleEvent')
#' where unnecessary for easier coding.
#'
#' @importFrom dplyr bind_rows collect distinct inner_join left_join mutate rename right_join select tbl
#' @importFrom tidyr pivot_wider
#' @importFrom purrr flatten map set_names
#'
#' @param type Indicate how to import the database tables
#' \describe{
#' \item{"local"}{Import tables in 'dbo' schema from the local installation of an FFI database in SQL Server Management Studio (SSMS).}
#' \item{"server"}{Import tables in 'dbo' schema from the FFI database on the production SQL Server (not enabled).}
#' \item{"csv"}{ Import zip file containing csvs that were exported from the FFI database. Option doesn't require SSMS to be installed (not yet enabled).}
#' }
#'
#' @param server If type = 'server', requires quoted FFI server address (not currently enabled).
#'
#' @param dbname If type = "server" or "local", quoted name of database matching
#' the name of the database (eg. "FFI_RA_AGFO"). If multiple database names are
#' specified, views will be row bound for tables in common for all of the
#' databases with a column indicating the dbname in each table. Note that the
#' tables being row binded must be identical for this to work, and there aren't
#' thorough checks built in the function to ensure that's true (i.e.,
#' it's likely to fail, but the error message may be weird).
#'
#' @param new_env Logical. If TRUE (default), will import tables to VIEWS_NGPN
#' environment. If FALSE, will import tables to global environment.
#'
#' @param keep_tables Logical. If TRUE, will return the raw FFI database tables
#' in a NGPN_tables environment. If FALSE (default), only returns the flattened
#' views. This feature is primarily for internal use to QC the original data.
#'
#' @param export_views Logical. If TRUE, will export a zip file of csvs to
#' specified export_path for the flattened views of the data. Views are the
#' analysis-ready files that have all associated MacroPlot, Sample Event, and
#' sample data for a given FFI protocol (e.g. Point Intercept).
#'
#' @param export_tables Logical. If TRUE, will export a zip file of csvs to
#' specified export_path for the raw data tables from the FFI database. The raw
#' tables are what can be imported the same as importing from a local instance
#' of the FFI database.
#'
#' @param export_path Quoted string to export zipped csvs to if export = TRUE.
#' If not specified, will export to the working directory.
#'
#' @param import_path Quoted string to import a zipped file of csvs if type =
#' 'csv'. The name of the zipped file should be included in the path. Can
#' specify multiple paths to import multiple parks/projects.
#'
#' @examples
#' \dontrun{
#' library(plantcomNGPN)
#' #--- From Local install of FFI SQL databases
#' # Import data for AGFO and export tables to zip file
#' importData(type = 'local', dbname = "FFI_RA_AGFO", export_tables = T, export_views = T)
#'
#' # Import data for all NGPN parks (takes a few seconds) from local copy on SSMS
#' # and export both the analysis-ready views and the raw tables.
#' importData(type = 'local',
#'            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", #"FFI_RA_MNRR",
#'                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#'            export_views = T, export_tables = T,
#'            export_path = "./data")
#'
#' # Check that the multiple-park import worked
#' table(NGPN_tables$MacroPlot$datasource)
#'
#' #--- From zipped csvs of FFI data
#' # Import THRO from zip file
#' importData(type = 'csv', import_path = "./data/FFI_RA_THRO.zip")
#'
#' # Import zipped files of NGPN parks and export as 1 zip file with all parks included
#' zips <- c("NGPN_FFI_tables_AGFO_20250508.zip", "NGPN_FFI_tables_BADL_20250508.zip",
#'           "NGPN_FFI_tables_DETO_20250508.zip", "NGPN_FFI_tables_FOLA_20250508.zip",
#'           "NGPN_FFI_tables_FOUS_20250508.zip", "NGPN_FFI_tables_JECA_20250508.zip",
#'           "NGPN_FFI_tables_KNRI_20250508.zip", "NGPN_FFI_tables_MORU_20250508.zip",
#'           "NGPN_FFI_tables_SCBL_20250508.zip", "NGPN_FFI_tables_THRO_20250508.zip",
#'           "NGPN_FFI_tables_WICA_20250508.zip")
#'
#' filepath = "C:PATH/TO/ZIP/FFI_zips/"
#' zips_full <- paste0(filepath, zips)
#' importData(type = 'csv', import_path = zips_full, export_tables = T)
#'
#' }
#'
#' @returns Either an environment of NGPN_PCM views as data frames and possibly
#' an environment of the original FFI database tables.
#'
#' @export
#'

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO",
                       new_env = T, export_views = F, export_tables = F,
                       export_path = NA, import_path = NA, keep_tables = F){

  #---- Bug Handling ----
  # Check that suggested package required for this function are installed
  # Need to make this conditional on type.
  if(type %in% c("local", "server") & !requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(type %in% c("local", "server") & !requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(type %in% c("local", "server") & !requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace("zip", quietly = TRUE) & (export_tables == T | export_views == TRUE)){
    stop("Package 'zip' needed when export_views = TRUE or export_tables = TRUE. Please install it.",
         call. = FALSE)
  }

  ## type
  type <- match.arg(type, c("local", "server", 'csv'))

  ## environment
  stopifnot(is.logical(new_env))

  ## export views
  stopifnot(is.logical(export_views))

  ## export tables
  stopifnot(is.logical(export_tables))

  ## type error
  if(any(type %in% c("local", "server")) & any(is.na(dbname))){
    stop("Must specify a dbname if type is 'local' or 'server'")
  }

  if(type == "server" & is.na(server)){
    stop("Must specify a server address if type = 'server'")
  }

  ## keep tables
  stopifnot(is.logical(keep_tables))

  #++++++ Update as more features are added ++++++
  if(type %in% c("server")){
    stop(paste0("Sorry, type = ",  type, " is not yet enabled."))
  }

  # Error handling for paths

  ## export path
  if(export_views == TRUE | export_tables == TRUE){
    if(is.na(export_path)){
      export_path <- getwd()
      print(paste0("No export_path specified. Output saved to working directory: ",
                   getwd()), quote = FALSE)
    }

    ## correct path
    if(!grepl("/$", export_path)){
      export_path <- paste0(export_path, "/")
    } # add / to end of path if doesn't exist

    ## path doesn't exist
    if(!dir.exists(export_path)){
      stop("Specified export_path directory does not exist.")
    }

    # Normalize filepath for zip
    export_pathn <- normalizePath(export_path)
  }

  ## CSV path
  if(type == 'csv'){
    if(!any(file.exists(import_path))){
      stop(paste0("Specified import_path does not exist. ",
                  ifelse(any(grepl("sharepoint", import_path)),
                         " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))
    }

    ## specify import path
    if(all(is.na(import_path))){
      stop("Must specify an import_path for type = 'csv'.")
    }

    ## name of file
    if(any(!grepl(".zip$", import_path))){
      stop("Must include the name of the zip file in import_path.")
    } # add / to end of path if doesn't exist

    ## direcotyr doesn't exist
    if(any(!file.exists(import_path))){
      stop("Specified import_path directory does not exist.")
    }

    # Normalize filepath for zip
    import_pathn <- normalizePath(import_path)
  }

  #---- Data Import ----
  cat(noquote("Importing data tables."), "\n\n")

  # check environment
  env <- if(keep_tables == TRUE){
    NGPN_tables <<- new.env()
  } else {
    environment()
    }

  # local env
  if(type == "local"){
  error_mess <- paste0("Unable to connect to specified SQL database. ",
                       "Make sure you have a local installation of the database in SSMS, ",
                       "and check that the database name is correct.")

  if(length(dbname) == 1){
    tryCatch(
      con <- odbc::dbConnect(odbc::odbc(),
                             Driver = "ODBC Driver 17 for SQL Server",
                             Server = "localhost\\SQLEXPRESS",
                             Database = dbname,
                             Trusted_Connection = "Yes"),
     error = function(e){
       stop(error_mess)
       },
     warning = function(w){
       stop(error_mess)
       })

  # list data tables
  tbls <- DBI::dbListTables(con, schema = "dbo")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(tbls), style = 3)

  # Import views using their names and show progress bar
  tbl_import <- lapply(seq_along(tbls), function(x){

    # progress bar
    setTxtProgressBar(pb, x)

    # tables
    tbl <- tbls[x]

    tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |>
      # put data into tibble
      dplyr::collect() |>
      # make data frame
      as.data.frame() |>
      # add dbname
      dplyr::mutate(datasource = dbname)

    return(tab)
    })

  # naming tables
  tbl_import <- setNames(tbl_import, tbls)

  tbl_import2 <- tbl_import[sort(names(tbl_import))]

  # remove empty tables
  tbl_import3 <- tbl_import2[sapply(tbl_import2, nrow) > 0]

  list2env(tbl_import3, envir = env)

  # disconnect from dbase
  DBI::dbDisconnect(con)

  # Keeping tables
  if(keep_tables == TRUE){
    NGPN_tables <<- new.env()
    list2env(tbl_import2[sapply(tbl_import2, nrow) > 0], envir = NGPN_tables)
  }

  # exporting tables
  if(export_tables == TRUE){
    dir.create(tmp <- tempfile())
    dbtbls <- names(tbl_import3)

    invisible(lapply(seq_along(dbtbls), function(x){
      temp_tbl = get(dbtbls[x], envir = env)
      write.csv(temp_tbl,
                paste0(tmp, "\\", dbtbls[x], ".csv"),
                row.names = FALSE)
    }))

    file_list <- list.files(tmp)

    # park list
    park <- substr(dbname, nchar(dbname)-3, nchar(dbname))

    # naming with park
    zip_name = paste0("NGPN_FFI_table_export_", park, "_",
                      format(Sys.Date(), "%Y%m%d"), ".zip")

    # saving .zip files
    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
              root = tmp,
              files = file_list)

  }

  # Add check that all csvs were imported
    } else if(length(dbname) > 1){
    dbimport <-
      purrr::map(seq_along(dbname), function(db){
        error_mess = paste0("Unable to connect to specified SQL database named ",
                            dbname[db], ". Make sure you have a local ",
                            "installation of the database in SSMS, ",
                            "and check that the database name is correct.")

        tryCatch(
          con <- odbc::dbConnect(odbc::odbc(),
                                 Driver = "ODBC Driver 17 for SQL Server",
                                 Server = "localhost\\SQLEXPRESS",
                                 Database = dbname[db],
                                 Trusted_Connection = "Yes"),
          error = function(e){
            stop(error_mess)
            },
          warning = function(w){
            stop(error_mess)
            })

        tbls <- DBI::dbListTables(con, schema = "dbo")

      # Import views using their names and show progress bar
      tbl_import <- lapply(seq_along(tbls), function(x){
       # setTxtProgressBar(pb, x)
        tbl <- tbls[x]

        tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |>
          dplyr::collect() |>
          as.data.frame() |>
          dplyr::mutate(datasource = dbname[db])
        return(tab)
        })

      DBI::dbDisconnect(con)

      tbl_import <- setNames(tbl_import, tbls)

      #list2env(tbl_import)

      }, .progress = T) |>
      purrr::set_names(dbname)

    # flatten list to bind like tables together
    dbflat1 <- purrr::flatten(dbimport)

    dbflat2 <- tapply(dbflat1, names(dbflat1), dplyr::bind_rows)

    # remove empty tables
    dbflat3 <- dbflat2[sapply(dbflat2, nrow) > 0]

    # sort tables alphabetically
    dbflat4 <- dbflat3[sort(names(dbflat3))]

    list2env(dbflat4, envir = env)

    if(keep_tables == TRUE){
      NGPN_tables <<- new.env()
      list2env(dbflat3[sort(names(dbflat3))], envir = NGPN_tables)
    }

    if(export_tables == TRUE){
      dir.create(tmp <- tempfile())
      dbtbls <- names(dbflat4)

      invisible(lapply(seq_along(dbtbls), function(x){
        temp_tbl = get(dbtbls[x], envir = env)
        write.csv(temp_tbl,
                  paste0(tmp, "\\", dbtbls[x], ".csv"),
                  row.names = FALSE)
      }))

      file_list <- list.files(tmp)

      zip_name = paste0("NGPN_FFI_table_export_",
                        format(Sys.Date(), "%Y%m%d"), ".zip")

      zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                root = tmp,
                files = file_list)
    }

  } # end of dbname>1
  } # end of type = local

  if(type == 'csv'){

    # Pulling in only tables commonly used across NGPN parks
    csv_list1 <- c('AuxSpecies', 'Cover_Frequency_metric_Attribute',
                   'Cover_Frequency_metric_Sample','Cover_Points_metric_Attribute',
                   'Cover_Points_metric_Sample', 'Cover_SpeciesComposition_metric_Attribute',
                   'Cover_SpeciesComposition_metric_Sample', 'DataGridViewSettings',
                   'Density_Belts_metric_Attribute','Density_Belts_metric_Sample',
                   'Density_Quadrats_metric_Attribute', 'Density_Quadrats_metric_Sample',
                   'DisturbanceHistory_Attribute', 'DisturbanceHistory_Sample',
                   'FuelConstants_CWD', 'FuelConstants_DL','FuelConstants_ExpDL',
                   'FuelConstants_FWD', 'FuelConstants_Veg', 'LU_Contact', 'LU_DataLevel',
                   'LU_DataType','LU_LifeCycle', 'LU_LifeForm', 'LU_MacroPlot_Type',
                   'LU_Shape', 'LU_Unit', 'Last_Modified_Date', 'LocalSpecies',
                   'MM_LocalSpecies_SpeciesPickList', 'MM_Method_Reference',
                   'MM_MonitoringStatus_SampleEvent', 'MM_Organization_Method',
                   'MM_ProjectUnit_MacroPlot','MM_Project_Protocol', 'MM_Protocol_Method',
                   'MM_SampleEvent_Protocol', 'MSchange_tracking_history','MacroPlot',
                   'MasterSpecies', 'MasterSpecies_LastModified', 'Method', 'MethodAttribute',
                   'MethodAttributeCode', 'MethodVersion', 'MonitoringStatus',
                   'Organization', 'OrganizationGroup', 'PostBurnSeverity_metric_Attribute',
                   'PostBurnSeverity_metric_Sample', 'Program', 'Project', 'ProjectUnit',
                   'Protocol', 'ProtocolVersion', 'RegistrationUnit','SampleAttribute',
                   'SampleAttributeCode', 'SampleEvent', 'SchemaVersions', 'Schema_Version',
                   'Settings','SpeciesPickList', 'SurfaceFuels_1000Hr_Attribute',
                   'SurfaceFuels_1000Hr_Sample', 'SurfaceFuels_Duff_Litter_Attribute',
                   'SurfaceFuels_Duff_Litter_Sample', 'SurfaceFuels_Fine_Attribute',
                   'SurfaceFuels_Fine_Sample','Trees_Individuals_metric_Attribute',
                   'Trees_Individuals_metric_Sample')


    if(length(import_path) == 1){

      file_name1 <- sort(sub(".*/", "", import_path, perl = T))
      file_name2 <- gsub("[[:digit:]]+|.zip", "", file_name1)
      file_name <- gsub("_$","", file_name2)

      # Check if can read files within the zip file
      tryCatch(
        {zfiles = utils::unzip(import_path, list = T)$Name},
         error = function(e){
           stop(paste0("Unable to import specified zip file."))
           })

      z_list = sort(zfiles[grepl(paste0(csv_list1, collapse = "|"), zfiles)])

      # Drop csv from file names
      z_list_names <- gsub("*.csv", "", z_list)

      # Drop csvs from csv_list not in z_list
      csv_list <- csv_list1[csv_list1 %in% z_list_names]

      miss_tbls <- setdiff(z_list_names, csv_list) # currently circular. Once I know the tables that should always be included,
      # I'll update csv_list1 above and use the same tables for each park.

      # Check for missing views
      if(length(miss_tbls) > 0){
        warning("The following tables are not included in specified database: ",
                paste0(miss_tbls, collapse = ", "))}

      # Since the missing test passed, clean up files so only includes names in
      # view_list, but maintain order in files

      # Import views now that all tests passed
      pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

      tbls1 <- unzip(import_path, junkpaths = TRUE, exdir = tempdir())

      tbls2 <- sort(tbls1[grepl(".csv", tbls1)])

      tbls <- sort(tbls2[grepl(paste0(csv_list, collapse = "|"), tbls2)])

      tbl_import <-
        lapply(seq_along(tbls), function(x){
          setTxtProgressBar(pb,x)
          tbl <- tbls[x]
          tab <- read.csv(tbls[x], na.string = c("NA", "NULL"),
                          check.names = FALSE) |>
            dplyr::mutate(datasource = file_name)
          return(tab)})

      tbl_import <- setNames(tbl_import, z_list_names)
      tbl_import2 <- tbl_import
      list2env(tbl_import, envir = env)

      if(keep_tables == TRUE){
        NGPN_tables <<- new.env()
        list2env(tbl_import2, envir = NGPN_tables)
      }

      # Close progress bar
      close(pb)

      if(export_tables == TRUE){
        dir.create(tmp <- tempfile())
        csvtbls <- names(tbl_import)

        invisible(lapply(seq_along(csvtbls), function(x){
          temp_tbl = get(csvtbls[x], envir = env)
          write.csv(temp_tbl,
                    paste0(tmp, "\\", csvtbls[x], ".csv"),
                    row.names = FALSE)
        }))

        file_list <- list.files(tmp)
        park <- substr(dbname, nchar(dbname)-3, nchar(dbname))
        zip_name = paste0(file_name, "_table_export_",
                          format(Sys.Date(), "%Y%m%d"), ".zip")

        zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                  root = tmp,
                  files = file_list)
        }

    } else if(length(import_path > 1)){

      file_names1 <- sort(sub(".*/", "", import_path, perl = T))
      file_names2 <- gsub("[[:digit:]]+|.zip", "", file_names1)
      file_names <- gsub("_$","", file_names2)

      zip_import <-
      purrr::map(seq_along(import_path), function(ip){

        file_name = file_names[ip]

        tryCatch(
          {zfiles = utils::unzip(import_path[ip], list = T)$Name},
           error = function(e){
             stop(paste0("Unable to import specified zip file: ", ip))
             })

        z_list = sort(zfiles[grepl(paste0(csv_list1, ".csv", collapse = "|"),
                                   zfiles)])

        # Drop date stamp (if it exists) from file name if exists in 2 steps
        # Drop csv from file names
        z_list_names <- gsub("*.csv", "", z_list)

        # Drop csvs from csv_list not in z_list
        csv_list <- csv_list1[csv_list1 %in% z_list_names]

        miss_tbls <- setdiff(z_list_names, csv_list) # currently circular. Once I know the tables that should always be included,
        # I'll update csv_list1 above and use the same tables for each park.

        # Check for missing views
        if(length(miss_tbls) > 0){
          warning("Missing the following tables from ",
                  "the specified import_path: ",
                  import_path[ip], "\n",
                  paste0(miss_tbls, collapse = ", "))
          }

        # Since the missing test passed, clean up files so only includes names in view_list, but
        # maintain order in files
        tbls1 <- unzip(import_path[ip], junkpaths = TRUE, exdir = tempdir())
        tbls2 <- sort(tbls1[grepl(".csv", tbls1)])
        tbls <- sort(tbls2[grepl(paste0(csv_list, ".csv", collapse = "|"), tbls2)])

        tbl_import <-
          lapply(seq_along(tbls), function(x){
          tbl_temp <- read.csv(tbls[x], na.strings = c("NA", "NULL", ""),
                               check.names = FALSE, as.is = T) |>
            dplyr::mutate(datasource = file_name)

          # When fields from one park are all NAs vs other parks have data, read.csv doesn't always assign
          # them as consistent field types. Converting these to character by default. If importData returns
          # error that Can't Combine two columns, it needs to be added here. I couldn't figure out how
          # to generalize this better, given that parks have different tables.
          cols_to_char <-
              c("UV1", "UV2", "UV3", "AttributeData_DataRow_GUID",
                "AttributeData_SampleRow_GUID", "Spp_GUID", "Status",
                "AgeCl", "Comment", "AttributeData_Original_GUID",
                "AttributeData_CreatedBy", "AttributeData_CreatedDate",
                "AttributeData_ModifiedBy", "AttributeData_ModifiedDate",
                "SampleData_SampleRow_GUID", "SampleData_SampleEvent_GUID",
                "FieldTeam", "EntryTeam", "SaComment", "SampleData_Original_GUID",
                "SampleData_CreatedBy", "SampleData_CreatedDate",
                "SampleData_ModifiedBy", "SampleData_ModifiedDate",
                "MethodAtt_Value_Default", "ProtocolVersion_GUID",
                "ProtocolVersion_Family_GUID", "ProtocolVersion_Protocol_GUID",
                "ProtocolVersion_TimeStamp", "UV1Desc", "UV2Desc", "UV3Desc",
                "TypeCov", "NFRatio", "NFNum", "SizeCl", "MacroPlot_UV5",
                "MacroPlot_UV7", "MM_SpeciesPickList_GUID", "MM_LocalSpecies_GUID",
                "MonitoringStatus_Suffix", "SampleAttributeCode_GUID",
                "SampleAttributeCode_SampleAttribute_GUID",
                "SampleAttributeCode_Code", "SampleAttributeCode_Text",
                "SampleAttributeCode_Description", "SampleEvent_GUID", "val1",
                "key1", "SpeciesPickList_GUID",
                "SpeciesPickList_RegistrationUnitGUID", "SpeciesPickList_Name",
                "SpeciesPickList_Describe", "DamCd3", "DamCd4", "DamCd5")

          if(any(cols_to_char %in% names(tbl_temp))){
            cols_to_char2 <- cols_to_char[cols_to_char %in% names(tbl_temp)]
            tbl_temp[,cols_to_char2] <- sapply(tbl_temp[,cols_to_char2], as.character)
            #tbl_temp[,cols_to_char2] <- as.character(tbl_temp[,cols_to_char2])
            }

          return(tbl_temp)
          })

        tbl_import <- setNames(tbl_import, z_list_names)},
        .progress = TRUE) |> purrr::set_names(file_names)

      # flatten list to bind like tables together
      zipflat1 <- purrr::flatten(zip_import)
      zipflat2 <- tapply(zipflat1, names(zipflat1), dplyr::bind_rows)
      # remove empty tables
      zipflat3 <- zipflat2[sapply(zipflat2, nrow) > 0]
      # sort tables alphabetically
      zipflat4 <- zipflat3[sort(names(zipflat3))]

      list2env(zipflat4, envir = env)

      if(keep_tables == TRUE){
        NGPN_tables <<- new.env()
        list2env(zipflat3[sort(names(zipflat3))], envir = NGPN_tables)
      }

      if(export_tables == TRUE){
        dir.create(tmp <- tempfile())
        ziptbls <- names(zipflat4)

        invisible(lapply(seq_along(ziptbls), function(x){
          temp_tbl = get(ziptbls[x], envir = env)
          write.csv(temp_tbl,
                    paste0(tmp, "\\", ziptbls[x], ".csv"),
                    row.names = FALSE)
        }))

        file_list <- list.files(tmp)

        zip_name = paste0("NGPN_FFI_table_export_",
                          format(Sys.Date(), "%Y%m%d"), ".zip")

        zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                  root = tmp,
                  files = file_list)}
      } # end of fp 2
      } # type = csv

  if(export_tables == TRUE){
    cat(noquote(paste0('Export of raw data tables complete and saved to: ',
                       export_pathn, "\\", zip_name)),
        "\n\n")
    }

  #---- Make Views ----
  cat(noquote("Joining tables into views."), "\n\n")

  pb <- txtProgressBar(min = 0, max = 11, style = 3)

  setTxtProgressBar(pb,1)

  #---- MacroPlot View -----
  #### Compile MacroPlot data ####
  #env <- if(exists("NGPN_tables")){NGPN_tables} else {.GlobalEnv}
  tryCatch(
    macro_orig <- get("MacroPlot", envir = env),
    error = function(e){
      stop("MacroPlot table not found. Please import NGPN FFI data.")
      })
  tryCatch(
    mm_projunit <- get("MM_ProjectUnit_MacroPlot", envir = env),
    error = function(e){
      stop("MM_ProjectUnit_MacroPlot table not found. Please import NGPN FFI data tables.")
      })
  tryCatch(
    projunit <- get("ProjectUnit", envir = env),
    error = function(e){
      stop("ProjectUnit table not found. Please import NGPN FFI data tables.")
      })
  tryCatch(
    regunit <- get("RegistrationUnit", envir = env),
    error = function(e){
      stop("RegistrationUnit table not found. Please import NGPN FFI data tables.")
      })

  # Clean up purpose
  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose == "Panel 9"] <- "Panel9"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in%
                                 c("FX", "Fx", "FX Monitoring", "FX monitoring",
                                   "Fire Effects Monitoring")] <- "FX Monitoring"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FX Dual",
                                                                   "FX_Dual")] <- "FX Dual"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Research",
                                                                   "research")] <- "Research"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Determine Strategies for efficient early detection",
                                                                   "Determine strategies for efficient early detection",
                                                                   "Early Invasives Detection")] <- "Early Detection"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FMH Grass Plot",
                                                                   "FMH Grass Plot ")] <- "FMH Grass Plot"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Intensive",
                                                                   "FIRE_intensive",
                                                                   "FX_Intensive",
                                                                   "FX_ Intensive",
                                                                   "FIRE_intesive")] <- "FX Intensive"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FIRE_Extensive")] <- "FX Extensive"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Lafferty Plot",
                                                                   "Lafferty Plot ")] <- "Lafferty Plot"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("invasive research",
                                                                   "Invasives Research",
                                                                   "Invasvies Research")] <- "Invasives Research"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Modified Shrub Plot",
                                                                   "Modified Shrub Plot ")] <- "Modified Shrub Plot"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("Pre- and Post- treatment of fuels",
                                                                   "pre- and post-treatment forest and fuels")] <- "Pre and post fuels treatment"

  macro_orig$MacroPlot_Purpose[macro_orig$MacroPlot_Purpose %in% c("FS")] <- "ForestStructure" # KNRI_PCM_038

  projunit$ProjectUnit_Name[projunit$ProjectUnit_Name %in% c("IN-ACTIVE",
                                                             "In-Active",
                                                             "Inactive")] <- "INACTIVE"

  # cleanup project and projectunit data
  projunit$ProjectUnit_Agency <- "NPS"
  NGPN_plots <- macro_orig$MacroPlot_Name[grepl("_PCM_|_LPCM_|_FPCM_|_RCM_",
                                                macro_orig$MacroPlot_Name)]

  macro <- macro_orig[macro_orig$MacroPlot_Name %in% NGPN_plots,]

  # Joining macroplot-relevant tables
  macro1 <- left_join(macro, mm_projunit,
                      by = c("MacroPlot_GUID" = "MM_MacroPlot_GUID",
                             "datasource"))

  macro2 <- left_join(macro1, regunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "RegistrationUnit_GUID",
                             "datasource"))

  macro3 <- left_join(macro2, projunit,
                      by = c("MacroPlot_RegistrationUnit_GUID" = "ProjectUnit_RegistrationUnitGUID",
                             "MM_ProjectUnit_GUID" = "ProjectUnit_GUID",
                             "datasource"))

  # hacky way to keep tblname_UV1 as is
  names(macro3)[names(macro3) == "MacroPlot_UV1"] <- "MacroPlotUV1"
  names(macro3)[names(macro3) == "MacroPlot_UV2"] <- "MacroPlotUV2"
  names(macro3)[names(macro3) == "MacroPlot_UV3"] <- "MacroPlotUV3"
  names(macro3)[names(macro3) == "MacroPlot_UV4"] <- "MacroPlotUV4"
  names(macro3)[names(macro3) == "MacroPlot_UV5"] <- "MacroPlotUV5"
  names(macro3)[names(macro3) == "MacroPlot_UV6"] <- "MacroPlotUV6"
  names(macro3)[names(macro3) == "MacroPlot_UV7"] <- "MacroPlotUV7"
  names(macro3)[names(macro3) == "MacroPlot_UV8"] <- "MacroPlotUV8"
  names(macro3)[names(macro3) == "MacroPlot_GUID"] <- "MacroPlotGUID"
  names(macro3)[names(macro3) == "MacroPlot_Comment"] <- "MacroPlotComment"
  names(macro3)[names(macro3) == "MacroPlot_Name"] <- "MacroPlotName"
  names(macro3)[names(macro3) == "MacroPlot_Purpose"] <- "MacroPlotPurpose"
  names(macro3)[names(macro3) == "MacroPlot_Type"] <- "MacroPlotType"
  names(macro3)[names(macro3) == "ProjectUnit_Name"] <- "ProjectUnitName"
  names(macro3)[names(macro3) == "MM_ProjectUnit_GUID"] <- "MM_ProjectUnitGUID"
  names(macro3)[names(macro3) == "RegistrationUnit_GUID"] <- "RegUnitGUID"

  # Drop table names from most column names for easier coding
  names(macro3) <-
    gsub("^MacroPlot_|^ProjectUnit_|^Registration", "", names(macro3))

  # Add the _ back
  names(macro3) <- gsub("MacroPlot", "MacroPlot_", names(macro3))
  names(macro3) <- gsub("ProjectUnit", "ProjectUnit_", names(macro3))
  names(macro3) <- gsub("RegUnit", "RegistrationUnit_", names(macro3))

  # Compile final dataset
  keep_cols_macro <-
    c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
      "ProjectUnit_Name", "Agency", "UTM_X", "UTM_Y", "UTMzone", "Datum",
      "DD_Lat", "DD_Long", "Elevation", "ElevationUnits", "Azimuth", "Aspect",
      "SlopeHill", "SlopeTransect", "MacroPlot_UV1", "MacroPlot_UV2",
      "MacroPlot_UV3", "MacroPlot_UV4", "MacroPlot_UV5", "MacroPlot_UV6",
      "MacroPlot_UV7", "MacroPlot_UV8", "Metadata", "StartPoint", "Directions",
      "MacroPlot_Comment","Unit_Comment", #"Description",
      "MacroPlot_GUID", "RegistrationUnit_GUID", #"MM_ProjectUnit_GUID",
      "datasource")

  # Had to drop description and MM_ProjectUnit_GUID to make macroplot rows unique
  macro4 <- macro3[,keep_cols_macro]
  macro4$sampled <- 1

  # Replace spaces and . with "_", so easier to query
  macro4$ProjectUnit_Name <- gsub(" ", "_", macro4$ProjectUnit_Name)

  # make project column wide, so more efficient shape
  macro5 <- macro4 |>
    pivot_wider(names_from = "ProjectUnit_Name",
                values_from = "sampled",
                values_fill = 0,
                names_prefix = "ProjectUnit_") |>
    data.frame()

  colnames(macro5) <- gsub(" ", "_", colnames(macro5))

  # order projectunit columns
  macro_names <- names(macro5[!grepl("ProjectUnit_", names(macro5))])
  proj_names1 <- names(macro5[grepl("ProjectUnit_", names(macro5))])
  proj_names <-
    if(any(names(macro5) %in% "ProjectUnit_Park")){
    c("ProjectUnit_Park",
      sort(proj_names1[!proj_names1 %in% "ProjectUnit_Park"]))
    } else {
        sort(proj_names1)
      }

  MacroPlots <- data.frame(macro5[order(macro5$MacroPlot_Name),
                           c(macro_names, proj_names)])

  #---- SampleEvents View ----
  #### Compile Sample Event Data ####
  tryCatch(
    monstat <- get("MonitoringStatus", envir = env) |>
      select(-datasource),
    error = function(e){
      stop("MonitoringStatus table not found. Please import NGPN FFI data tables.")
      })
  tryCatch(
    mm_monstat_se <- get("MM_MonitoringStatus_SampleEvent", envir = env) |>
      select(-datasource),
    error = function(e){
      stop("MM_MonitoringStatus_SampleEvent table not found. Please import NGPN FFI data tables.")
      })
  tryCatch(
    sampev <- get("SampleEvent", envir = env) |>
      select(-datasource),
    error = function(e){
      stop("SampleEvent table not found. Please import NGPN FFI data tables.")
      })

  # Use to make some tables smaller before join
  macro_guids <- unique(MacroPlots$MacroPlot_GUID)

  # Fix typos in MonitoringStatus_Name and MonitoringStatus_Base
  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2009_Plant Community"] <- "2009_PlantCommunity"

  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2018_Plant Community"] <- "2018_PlantCommunity"

  monstat$MonitoringStatus_Name[monstat$MonitoringStatus_Name == "2024_Plant Community"] <- "2024_PlantCommunity"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base == "2016_"] <- "PlantCommunity"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("Plant Community",
                                                                     "_PlantCommunity")] <- "PlantCommunity"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("2018_PlantCommunity",
                                                                     "2019_PlantCommunity")] <- "PlantCommunity"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_Riparian")] <- "Riparian"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_ForestStructure")] <- "ForestStructure"

  monstat$MonitoringStatus_Base[monstat$MonitoringStatus_Base %in% c("_FirePlantCommunity")] <- "FirePlantCommunity"

  sampev2 <- left_join(MacroPlots, sampev, by = c("MacroPlot_GUID" = "SampleEvent_Plot_GUID"),
                       relationship = 'many-to-many') #MM b/c plots are used for multiple projects

  monstat_join <- inner_join(mm_monstat_se, monstat,
                             by = c("MM_MonitoringStatus_GUID" = "MonitoringStatus_GUID"))

  sampev3 <- left_join(sampev2, monstat_join,
                       by = c("SampleEvent_GUID" = "MM_SampleEvent_GUID"),
                       relationship = 'many-to-many')

  sampev3$SampleEvent_Date <-
    format(as.Date(substr(sampev3$SampleEvent_Date, 1, 10), format = "%Y-%m-%d"),
           "%Y-%m-%d")

  sampev3$year <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%Y")
  sampev3$month <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%m")
  sampev3$doy <- format(as.Date(sampev3$SampleEvent_Date, format = "%Y-%m-%d"), "%j")

  # drop plots with no associated sample events
  # distinct(sampev4$MacroPlot_Name[is.na(sampev4$SampleEvent_GUID)]) # Plots with no sample events
  sampev4 <- sampev3[!is.na(sampev3$SampleEvent_GUID),]

  names(sampev4)[names(sampev4) == "SampleEvent_GUID"] <- "SampleEventGUID"
  names(sampev4)[names(sampev4) == "SampleEvent_Date"] <- "SampleEventDate"
  names(sampev4)[names(sampev4) == "SampleEvent_UV1"] <- "SampleEventUV1"
  names(sampev4)[names(sampev4) == "SampleEvent_Comment"] <- "SampleEventComment"

  # Drop table names from most column names for easier coding
  names(sampev4) <-
    gsub("^SampleEvent_", "", names(sampev4))

  # Add the _ back
  names(sampev4) <- gsub("SampleEvent", "SampleEvent_", names(sampev4))

  # Drop data before 2011
  sampev5 <- sampev4[sampev4$year >= 2011,]

  keep_cols_samp <-
    c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose", "MacroPlot_Type",
      "SampleEvent_Date", "year", "month", "doy", "UTM_X", "UTM_Y", "UTMzone",
      "Elevation", "Azimuth", "Aspect", "SlopeHill", "SlopeTransect",
      "SampleEvent_UV1", "DefaultMonitoringStatus", "TreatmentUnit",
      "MonitoringStatus_Prefix", "MonitoringStatus_Base",
      "MonitoringStatus_Suffix", "MonitoringStatus_Name",
      "MonitoringStatus_Comment", "SampleEvent_Comment", "SampleEvent_GUID",
      "MM_MonitoringStatus_GUID", "RegistrationUnit_GUID", "MacroPlot_GUID")

  SampleEvents <- data.frame(sampev5[order(sampev5$MacroPlot_Name,
                                           sampev5$SampleEvent_Date),
                             keep_cols_samp])

  sampev_unique1 <- SampleEvents |>
    select(-MonitoringStatus_Comment, -MM_MonitoringStatus_GUID) |>
    distinct()

  # Convert "" to NA so unique actually works
  mon_cols <- c("DefaultMonitoringStatus",
                "MonitoringStatus_Prefix",
                "MonitoringStatus_Base",
                "MonitoringStatus_Suffix",
                "MonitoringStatus_Name")

  sampev_unique1[,mon_cols][sampev_unique1[,mon_cols] == ""] <- NA_character_

  sampev_unique <- distinct(sampev_unique1)

  # Prevents some duplication of data until monitoring status is fixed in database.
  sampev_guids <- unique(sampev_unique$SampleEvent_GUID)

  #---- Taxa_Table View----
  setTxtProgressBar(pb,2)
  tryCatch(localspp <- get("LocalSpecies", envir = env),
           error = function(e){
             stop("LocalSpecies table not found. Please import NGPN FFI data tables.")
             })
  tryCatch(mastspp <- get("MasterSpecies", envir = env) |>
             distinct(),
           error = function(e){
             stop("MasterSpecies table not found. Please import NGPN FFI data tables.")
             })
  tryCatch(auxspp <- get("AuxSpecies", envir = env),
           error = function(e){
             stop("AuxSpecies table not found. Please import NGPN FFI data tables.")
             })
  tryCatch(lifeform <- get("LU_LifeForm", envir = env) |>
             distinct(),
           error = function(e){
             stop("LU_LifeForm table not found. Please import NGPN FFI data tables.")
             })
  # NGPN does not appear to use the SpeciesPickList, so not including here.
  # lifecycle doesn't appear to be used much by NGPN, so not including it here.

  # Table joins
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
  names(spp4)[names(spp4) == "LocalSpecies_Description"] <- "Species_Description"
  names(spp4)[names(spp4) == "LocalSpecies_Comment"] <- "Species_Comment"

  # Drop table names from column names for easier coding
  names(spp4) <-
    gsub("^MacroPlot_|^LocalSpecies_|^MasterSpecies_|^LU_|^AuxSpecies_|^Registration",
         "", names(spp4))

  names(spp4)[names(spp4) == "UnitGUID"] <- "RegistrationUnitGUID"

  keep_cols_taxa <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName",
                      "Family", "Genus", "Nativity", "Invasive", "Cultural",
                      "Concern", "LifeCycle", "LifeForm_Name", "NotBiological",
                      "UserAdded", "Species_UV1", "IsUnknown", "IsUnlisted",
                      "Species_Description", "Species_Comment", "Unit_Name",
                      "SymbolKey", "Synonym_SymbolKey", "Spp_GUID",
                      "RegistrationUnitGUID", "MasterSpeciesGUID",
                      "AuxSpeciesGUID", "PLANTS_GUID")

  Taxa_Table <- data.frame(spp4[order(spp4$Symbol, spp4$Unit_Name),
                                keep_cols_taxa])

  #---- Cover_Points_Metric View ----
  setTxtProgressBar(pb,3)

  covpts_samp1 <-   tryCatch(get("Cover_Points_metric_Sample", envir = env),
                             error = function(e){NULL})

  covpts_attr1 <- tryCatch(get("Cover_Points_metric_Attribute", envir = env),
                           error = function(e){NULL})

  if(class(covpts_samp1) == "data.frame"){

  # Making tables smaller before joins
  covpts_samp <- covpts_samp1[covpts_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(covpts_samp$SampleData_SampleRow_GUID)
  covpts_attr2 <- covpts_attr1[covpts_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]
  # Drop records where Index is blank b/c causes issues in the join
  #covpts_attr <- covpts_attr2[!is.na(covpts_attr2$Index),]

  samp_covs1 <- left_join(sampev_unique, covpts_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  # drop records with blank SampleData_SampleRow_GUID
  samp_covs <- samp_covs1[!is.na(samp_covs1$SampleData_SampleRow_GUID),] # works same as Visited == T

  samp_cova <- left_join(samp_covs, covpts_attr2,
                         by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                "datasource"),
                         relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_cov_spp <- left_join(samp_cova, Taxa_Table,
                            by = c("Spp_GUID", "Unit_Name",
                                   "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_view_start <- c("MacroPlot_Name", "Unit_Name", "MacroPlot_Purpose",
                       "UTM_X", "UTM_Y", "UTMzone", "Elevation", "Azimuth",
                       "Aspect", "SlopeHill", "SlopeTransect", "SampleEvent_Date",
                       "year", "month", "doy")

  cols_view_end <- c("UV1Desc", "UV2Desc", "UV3Desc", "SaComment",
                     "DefaultMonitoringStatus", "MonitoringStatus_Base",
                     "MacroPlot_GUID", "SampleEvent_GUID", #"MM_MonitoringStatus_GUID",
                     "RegistrationUnit_GUID", "Spp_GUID")

  cols_taxa_start <- c("Symbol", "ITIS_TSN", "ScientificName", "CommonName")

  cols_taxa_end <- c("Nativity", "Invasive", "Cultural", "Concern", "LifeCycle",
                     "LifeForm_Name", "NotBiological", "Species_Comment")

  cols_covpt <- c("Visited", "NumTran", "TranLen", 'NumPtsTran', "Offset",
                  "Index", "Transect", "Point", "Tape", "Order", "Height",
                  "CanopyLayer", "Status", "Comment")

  Cover_Points_metric <- distinct(data.frame(
    samp_cov_spp[order(samp_cov_spp$MacroPlot_Name,
                       samp_cov_spp$year,
                       samp_cov_spp$Index,
                       samp_cov_spp$ScientificName),
                 c(cols_view_start,
                   cols_taxa_start,
                   cols_covpt,
                   cols_taxa_end,
                   cols_view_end)]))
  }
  #---- Cover_Species_Composition View ----
  setTxtProgressBar(pb,4)
  covcomp_samp1 <-   tryCatch(get("Cover_SpeciesComposition_metric_Sample",
                                  envir = env),
                              error = function(e){NULL})
  covcomp_attr1 <- tryCatch(get("Cover_SpeciesComposition_metric_Attribute",
                                envir = env),
                            error = function(e){NULL})

  if(class(covcomp_samp1) == "data.frame"){
  # Making tables smaller before joins
  covcomp_samp <- covcomp_samp1[covcomp_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(covcomp_samp$SampleData_SampleRow_GUID)
  covcomp_attr2 <- covcomp_attr1[covcomp_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_comps1 <- left_join(sampev_unique, covcomp_samp,
                           by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  # drop records with blank SampleData_SampleRow_GUID
  samp_comps <- samp_comps1[!is.na(samp_comps1$SampleData_SampleRow_GUID),] # works same as Visited == T too

  samp_compa <- left_join(samp_comps, covcomp_attr2,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_comp_spp <- left_join(samp_compa, Taxa_Table,
                             by = c("Spp_GUID", "Unit_Name",
                                    "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_covcomp <- c("Visited", "Index", "Status", "SizeCl", "AgeCl", "Cover",
                    "Height", "Comment", "UV1", "UV2", "UV3")

  Cover_Species_Composition <- distinct(
    data.frame(
    samp_comp_spp[order(samp_comp_spp$MacroPlot_Name,
                        samp_comp_spp$year,
                        samp_comp_spp$Index,
                        samp_comp_spp$ScientificName),
                  c(cols_view_start,
                    cols_taxa_start,
                    cols_covcomp,
                    cols_taxa_end,
                    cols_view_end)]))
  }
  #---- Density_Belts_Metric View ----
  setTxtProgressBar(pb,5)
  densbelt_samp1 <-   tryCatch(get("Density_Belts_metric_Sample", envir = env),
                               error = function(e){NULL})
  densbelt_attr1 <- tryCatch(get("Density_Belts_metric_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(densbelt_samp1) == "data.frame"){
  # Making tables smaller before joins
  densbelt_samp <- densbelt_samp1[densbelt_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(densbelt_samp$SampleData_SampleRow_GUID)
  densbelt_attr2 <- densbelt_attr1[densbelt_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_densbs1 <- left_join(sampev_unique, densbelt_samp,
                            by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  # drop records with blank SampleData_SampleRow_GUID
  samp_densbs <- samp_densbs1[!is.na(samp_densbs1$SampleData_SampleRow_GUID),] # works same as Visited == T

  samp_densba <- left_join(samp_densbs, densbelt_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_densb_spp <- left_join(samp_densba, Taxa_Table,
                              by = c("Spp_GUID", "Unit_Name",
                                     "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_densbelt <- c("Visited", "NumTran", "NumSubbelt", "TranLen", "TranWid",
                     "Area", "Index", "Transect", "Subbelt", "Status", "SizeCl",
                     "AgeCl", "Count", "Height", "SubFrac", "Comment", "UV1",
                     "UV2", "UV3")

  Density_Belts_metric <- distinct(data.frame(
    samp_densb_spp[order(samp_densb_spp$MacroPlot_Name,
                         samp_densb_spp$year,
                         samp_densb_spp$Index,
                         samp_densb_spp$ScientificName),
                   c(cols_view_start,
                     cols_taxa_start,
                     cols_densbelt,
                     cols_taxa_end,
                     cols_view_end)]))
  }
  #---- Density_Quadrats_Metric View ----
  setTxtProgressBar(pb,6)

  densquad_samp1 <- tryCatch(get("Density_Quadrats_metric_Sample", envir = env),
                             error = function(e){NULL})
  densquad_attr1 <- tryCatch(get("Density_Quadrats_metric_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(densquad_samp1) == "data.frame"){
  # Making tables smaller before joins
  densquad_samp <- densquad_samp1[densquad_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(densquad_samp$SampleData_SampleRow_GUID)
  densquad_attr2 <- densquad_attr1[densquad_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_densqs1 <- left_join(sampev_unique, densquad_samp,
                            by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  # drop records with blank SampleData_SampleRow_GUID
  samp_densqs <- samp_densqs1[!is.na(samp_densqs1$SampleData_SampleRow_GUID),] # works same as Visited == T

  samp_densqa <- left_join(samp_densqs, densquad_attr2,
                           by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                  "datasource"),
                           relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_densq_spp <- left_join(samp_densqa, Taxa_Table,
                              by = c("Spp_GUID", "Unit_Name",
                                     "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_densquad <- c("Visited", "NumTran", "NumQuadTran", "QuadLen", "QuadWid",
                     "Area", "Index", "Transect", "Quadrat", "Status", "SizeCl",
                     "AgeCl", "Count", "Height", "SubFrac", "Comment", "UV1",
                     "UV2", "UV3")

  Density_Quadrats_metric <- distinct(data.frame(
    samp_densq_spp[order(samp_densq_spp$MacroPlot_Name,
                         samp_densq_spp$year,
                         samp_densq_spp$Index,
                         samp_densq_spp$ScientificName),
                   c(cols_view_start,
                     cols_taxa_start,
                     cols_densquad,
                     cols_taxa_end,
                     cols_view_end)]))
  }
  #---- Disturbance_History View ----
  setTxtProgressBar(pb,7)
  disthist_samp1 <-   tryCatch(get("DisturbanceHistory_Sample", envir = env),
                               error = function(e){NULL})
  disthist_attr1 <- tryCatch(get("DisturbanceHistory_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(disthist_samp1) == "data.frame"){
  # Making tables smaller before joins
  disthist_samp <- disthist_samp1[disthist_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(disthist_samp$SampleData_SampleRow_GUID)
  disthist_attr2 <- disthist_attr1[disthist_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_dists1 <- left_join(sampev_unique, disthist_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  # drop records with blank SampleData_SampleRow_GUID
  samp_dists <- samp_dists1[!is.na(samp_dists1$SampleData_SampleRow_GUID),] # works same as Visited == T

  samp_dista <- left_join(samp_dists, disthist_attr2,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_dist <- c("Visited", "Index", "ChAgent", "SevCode", "StartYr", "StartMo",
                 "StartDy", "EndYr", "EndMo", "EndDy", "DatePrec", "ChgDesc",
                 "Comment", "UV1", "UV2", "UV3")

  cols_view_end_nospp <- cols_view_end[!cols_view_end %in% "Spp_GUID"]

  Disturbance_History <- distinct(data.frame(
    samp_dista[order(samp_dista$MacroPlot_Name,
                     samp_dista$year,
                     samp_dista$Index),
               c(cols_view_start,
                 cols_dist,
                 cols_view_end_nospp)]
  ))
  }
  #---- Surface_Fuels_1000Hr View ----
  setTxtProgressBar(pb,8)
  surf1000_samp1 <- tryCatch(get("SurfaceFuels_1000Hr_Sample", envir = env),
                             error = function(e){NULL})
  surf1000_attr1 <- tryCatch(get("SurfaceFuels_1000Hr_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surf1000_samp1) == "data.frame"){
  # Making tables smaller before joins
  surf1000_samp <- surf1000_samp1[surf1000_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surf1000_samp$SampleData_SampleRow_GUID)
  surf1000_attr2 <- surf1000_attr1[surf1000_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surf1000s1 <- left_join(sampev_unique, surf1000_samp,
                               by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  samp_surf1000s <- samp_surf1000s1[!is.na(samp_surf1000s1$SampleData_SampleRow_GUID == TRUE),]

  samp_surf1000a <- left_join(samp_surf1000s, surf1000_attr2,
                              by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                     "datasource"),
                              relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_surf1000 <- c("Visited", "NumTran", "TranLen", "Index", "Transect",
                     "Slope", "LogNum", "Dia", "DecayCl", "CWDFuConSt",
                     "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_1000Hr <- distinct(data.frame(
    samp_surf1000a[order(samp_surf1000a$MacroPlot_Name,
                         samp_surf1000a$year,
                         samp_surf1000a$Index),
                   c(cols_view_start,
                     cols_surf1000,
                     cols_view_end_nospp)]))
  }
  #---- Surface_Fuels_Fine View ----
  setTxtProgressBar(pb,9)
  surffine_samp1 <-   tryCatch(get("SurfaceFuels_Fine_Sample", envir = env),
                               error = function(e){NULL})
  surffine_attr1 <- tryCatch(get("SurfaceFuels_Fine_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surffine_samp1) == "data.frame"){

  # Making tables smaller before joins
  surffine_samp <- surffine_samp1[surffine_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surffine_samp$SampleData_SampleRow_GUID)
  surffine_attr2 <- surffine_attr1[surffine_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surffines1 <- left_join(sampev_unique, surffine_samp,
                               by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  samp_surffines <- samp_surffines1[!is.na(samp_surffines1$SampleData_SampleRow_GUID == TRUE),]

  samp_surffinea <- left_join(samp_surffines, surffine_attr2,
                              by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                     "datasource"),
                              relationship = 'many-to-many') # b/c multiple projects/macroplot

  names(samp_surffinea)[names(samp_surffinea) == "Azimuth.x"] <- "Azimuth"
  names(samp_surffinea)[names(samp_surffinea) == "Azimuth.y"] <- "Azimuth_Fuels"

  cols_surffine <- c("Visited", "NumTran", "OneHrTranLen", "TenHrTranLen",
                     "HunHrTranLen", "Index", "Transect", "Azimuth_Fuels",
                     "Slope", "OneHr", "TenHr", "HunHr", "FWDFuConSt",
                     "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_Fine <- distinct(data.frame(
    samp_surffinea[order(samp_surffinea$MacroPlot_Name,
                         samp_surffinea$year,
                         samp_surffinea$Index),
                   c(cols_view_start,
                     cols_surffine,
                     cols_view_end_nospp)]))

  }

  #---- Surface_Fuels_Duff View ----
  setTxtProgressBar(pb,10)
  surfduff_samp1 <-   tryCatch(get("SurfaceFuels_Duff_Litter_Sample", envir = env),
                               error = function(e){NULL})
  surfduff_attr1 <- tryCatch(get("SurfaceFuels_Duff_Litter_Attribute", envir = env),
                             error = function(e){NULL})

  if(class(surfduff_samp1) == "data.frame"){
  # Making tables smaller before joins
  surfduff_samp <- surfduff_samp1[surfduff_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(surfduff_samp$SampleData_SampleRow_GUID)
  surfduff_attr2 <- surfduff_attr1[surfduff_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  samp_surfduffs1 <- left_join(sampev_unique, surfduff_samp,
                               by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))

  samp_surfduffs <- samp_surfduffs1[!is.na(samp_surfduffs1$SampleData_SampleRow_GUID == TRUE),]


  samp_surfduffa <- left_join(samp_surfduffs, surfduff_attr2,
                              by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                     "datasource"),
                              relationship = 'many-to-many') # b/c multiple projects/macroplot

  cols_surfduff <- c("Visited", "NumTran", "Index", "Transect", "SampLoc",
                     "OffSet", "LittDep", "DuffDep", "FuelbedDep", "DLFuConSt",
                     "Comment", "UV1", "UV2", "UV3")

  Surface_Fuels_Duff <- distinct(data.frame(
    samp_surfduffa[order(samp_surfduffa$MacroPlot_Name,
                         samp_surfduffa$year,
                         samp_surfduffa$Index),
                   c(cols_view_start,
                     cols_surfduff,
                     cols_view_end_nospp)]))
  }
  #---- Trees_Metric ----
  setTxtProgressBar(pb,11)
  tree_samp1 <- tryCatch(get("Trees_Individuals_metric_Sample", envir = env),
                         error = function(e){NULL})
  tree_attr1 <- tryCatch(get("Trees_Individuals_metric_Attribute", envir = env),
                         error = function(e){NULL})

  if(class(tree_attr1) == "data.frame"){
  # Making tables smaller before joins
  tree_samp <- tree_samp1[tree_samp1$SampleData_SampleEvent_GUID %in% sampev_guids,]
  samprow_guids <- unique(tree_samp$SampleData_SampleRow_GUID)
  tree_attr <- tree_attr1[tree_attr1$AttributeData_SampleRow_GUID %in% samprow_guids,]

  # Not all parks/plots have tree data associated, making the left_joins bring in a bunch of blank rows.
  # Using all plots with a tree recorded in the tree_samp1 to filter out non-tree plots
  samp_treesrj <- right_join(sampev_unique, tree_samp,
                             by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID"))
  samp_treearj <- right_join(samp_treesrj, tree_attr,
                             by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                    "datasource"),
                             relationship = 'many-to-many') # b/c multiple projects/macroplot

  tree_samp_plots <- sort(unique(samp_treearj$MacroPlot_Name))

  samp_trees1 <- left_join(sampev_unique, tree_samp,
                          by = c("SampleEvent_GUID" = "SampleData_SampleEvent_GUID")) |>
    filter(MacroPlot_Name %in% tree_samp_plots)

  samp_trees <- samp_trees1[!is.na(samp_trees1$SampleData_SampleRow_GUID == TRUE),]

  samp_treea <- left_join(samp_trees, tree_attr,
                          by = c("SampleData_SampleRow_GUID" = "AttributeData_SampleRow_GUID",
                                 "datasource"),
                          relationship = 'many-to-many') # b/c multiple projects/macroplot

  samp_tree_spp <- left_join(samp_treea, Taxa_Table,
                             by = c("Spp_GUID", "Unit_Name",
                                    "RegistrationUnit_GUID" = "RegistrationUnitGUID"))

  cols_tree <- c("Visited", "MacroPlotSize", "SnagPlotSize", "BrkPntDia", "QTR",
                 "SubFrac", "TagNo", "Status", "DBH", "CrwnCl", "LiCrBHt",
                 "CrwnRad", "DRC", "Comment", "UV1", "UV2", "UV3")

  # tree columns not used by NGPN
  #c("Ht", "CrwnRto", "CrFuBHt", "Age", "GrwthRt", "Mort", "DecayCl", "LaddBaseHt", "LaddMaxHt",
  # "NuLiStems", "NuDeStems", "EqDia", "XCoord", "YCoord", "CKR", "CharHt",
  # "ScorchHt", "CrScPct", "DamCd1", "DamSev1", "DamCd2", "DamSev2", "DamCd3",
  # "DamSev3", "DamCd4", "DamSev4", "DamCd5", "DamSev5")

  Trees_metric <- distinct(data.frame(
    samp_tree_spp[order(samp_densq_spp$MacroPlot_Name,
                        samp_densq_spp$year,
                        samp_densq_spp$Index,
                        samp_densq_spp$ScientificName),
                  c(cols_view_start,
                    cols_taxa_start,
                    cols_tree,
                    cols_taxa_end,
                    cols_view_end)]))

  }

  #---- Add views to VIEWS_NGPN ----
  view_names <- c("Cover_Points_metric", "Cover_Species_Composition",
                  "Density_Belts_metric", "Density_Quadrats_metric",
                  "Disturbance_History", "MacroPlots", "SampleEvents",
                  "Surface_Fuels_1000Hr", "Surface_Fuels_Fine",
                  "Surface_Fuels_Duff", "Taxa_Table", "Trees_metric")

  if(new_env == TRUE){VIEWS_NGPN <<- new.env()}
  env_views <- if(new_env == TRUE){VIEWS_NGPN} else {.GlobalEnv}
  if(exists("Cover_Points_metric")){
     assign("Cover_Points_metric", Cover_Points_metric, envir = env_views)
    }
  if(exists("Cover_Species_Composition")){
     assign("Cover_Species_Composition", Cover_Species_Composition, envir = env_views)
    }
  if(exists("Density_Belts_metric")){
     assign("Density_Belts_metric", Density_Belts_metric, envir = env_views)
    }
  if(exists("Density_Quadrats_metric")){
     assign("Density_Quadrats_metric", Density_Quadrats_metric, envir = env_views)
    }
  if(exists("Disturbance_History")){
     assign("Disturbance_History", Disturbance_History, envir = env_views)
    }
  assign("MacroPlots", MacroPlots, envir = env_views)
  assign("SampleEvents", SampleEvents, envir = env_views)
  if(exists("Surface_Fuels_1000Hr")){
    assign("Surface_Fuels_1000Hr", Surface_Fuels_1000Hr, envir = env_views)
    }
  if(exists("Surface_Fuels_Fine")){
    assign("Surface_Fuels_Fine", Surface_Fuels_Fine, envir = env_views)
    }
  if(exists("Surface_Fuels_Duff")){
    assign("Surface_Fuels_Duff", Surface_Fuels_Duff, envir = env_views)
    }
  assign("Taxa_Table", Taxa_Table, envir = env_views)
  if(exists("Trees_metric")){
    assign("Trees_metric", Trees_metric, envir = env_views)
    }

  views_final <- view_names[view_names %in% names(env_views)]

  if(length(views_final) != length(view_names)){
    warning(paste0("The following views were not created because specified ",
                   "database did not include relevant tables: ",
                   paste0(view_names[!view_names %in% names(env_views)],
                          collapse = ", ")))
  }

  close(pb)

  if(export_views == TRUE){
    dir.create(tmp <- tempfile())
    invisible(lapply(seq_along(views_final), function(x){
      temp_tbl = get(views_final[x], envir = env_views)
      write.csv(temp_tbl,
                paste0(tmp, "\\", views_final[x], ".csv"),
                row.names = FALSE)
    }))

    view_list <- list.files(tmp)

    zip_name = paste0("NGPN_FFI_views_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
              root = tmp,
              files = view_list)
    noquote(paste0("Export of views complete and saved to ", export_pathn, "\\",
                   zip_name))
  }

  } # end of function

