# ---- Params for troubleshooting ----
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
#
# # If SQL not set up, use import below instead, and update the import_path:
# importData(type = 'csv',
#            import_path = "./data/NGPN_FFI_table_export_20260616.zip",
#            keep_tables = T)

# Start of source code ----
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
  if(nrow(df) > 0){
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
    } else {NULL}
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

## Loading data ----
# Species list
# or UPDATE PATH
#tab4_spp <- read.csv("./data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv")
try(tab4_spp <- read.csv("./data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"), silent = TRUE)
try(tab4_spp <- read.csv("../data/NGPN_PCM_Table_4_Tree_shrub_species_list.csv"), silent = TRUE)

#### START OF REPORTING TABLES ####

## Taxa ----
# Does not remove "only active" plots

### Missing Values ----
# Check for species with inconsistent scientific names (eg genus only, genus spp.) but the same symbol
taxa <- unique(VIEWS_NGPN$Taxa_Table[,c("Symbol", "ITIS_TSN", "ScientificName",
                                        "Unit_Name", "NotBiological")])

taxa_wide <- taxa |>
  mutate(present = 1) |>
  arrange(Unit_Name) |>
  pivot_wider(names_from = Unit_Name,
              values_from = present,
              values_fill = 0)

#### Blanks in ScientificName ----
taxa_miss_sci <- taxa_wide |>
  filter(is.na(ScientificName)) |>
  arrange(Symbol)

QC_table <- QC_check(df = taxa_miss_sci,
                     meas_type = "Taxa",
                     tab = "Blank SciName",
                     check = "Records that have a blank ScientificName column.",
                     chk_type = 'check')

dt_taxa_miss_sci <- make_dt(df = taxa_miss_sci,
                            cap = "Records that have a blank ScientificName column")

#### Blank symbols ----
taxa_miss_sym <- taxa_wide |>
  filter(is.na(Symbol)) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_sym,
                           meas_type = "Taxa",
                           tab = "Blank Symbol",
                           check = "Records that have a blank Symbol column.",
                           chk_type = 'check'))

kbl_taxa_miss_sym <- make_kable(df = taxa_miss_sym,
                                cap = "Records that have a blank Symbol column")

#### Blank TSN ----
taxa_miss_tsn <- taxa_wide |>
  filter(is.na(ITIS_TSN)) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_tsn,
                           meas_type = "Taxa",
                           tab = "Blank ITIS_TSN",
                           check = "Records that have a blank TSN column.",
                           chk_type = 'check'))

dt_taxa_miss_tsn <- make_dt(df = taxa_miss_tsn,
                            cap = "Records that have a blank TSN column")

#### NotBiological blank ----
taxa_miss_nb <- taxa_wide |>
  filter(is.na(NotBiological)) |>
  arrange(Symbol) |>
  filter(!is.na(Symbol)) # drops record that's all NAs across species columns and picked up in other checks

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_miss_nb,
                           meas_type = "Taxa",
                           tab = "Blank NotBio",
                           check = "Records that have a blank NotBiological column.",
                           chk_type = 'check'))

kbl_taxa_miss_nb <- make_kable(df = taxa_miss_nb,
                               cap = "Records that have a blank NotBiological column")

### Inconsistent names ----

#### Different scientific names, common names or TSNs ----
taxa2 <- unique(VIEWS_NGPN$Taxa_Table[,c("Symbol", "ITIS_TSN",
                                         "ScientificName", "CommonName",
                                         "Unit_Name", "NotBiological")])

taxa_dups <- taxa2 |>
  mutate(present = 1) |>
  arrange(Unit_Name) |>
  pivot_wider(names_from = Unit_Name,
              values_from = present,
              values_fill = 0) |>
  group_by(Symbol) |>
  mutate(num_tsn = sum(!is.na(ITIS_TSN)),
         num_sci = sum(!is.na(ScientificName)),
         num_com = sum(!is.na(CommonName)),
         num_dup = num_tsn + num_sci + num_com) |>
  filter(num_dup > 3) |>
  arrange(Symbol,
          ScientificName) |>
  data.frame()

taxa_dup_notbio <- taxa_dups |>
  filter(NotBiological == TRUE) |>
  select(Symbol:CommonName, AGFO:WICA) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_dup_notbio,
                           meas_type = "Taxa",
                           tab = "Inconsistent NotBio",
                           check = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = T",
                           chk_type = 'check'))

dt_taxa_dup_notbio <- make_dt(taxa_dup_notbio,
                              cap = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = T")

#### Inconsistent Species ----
taxa_dup_bio <- taxa_dups |>
  filter(NotBiological == FALSE) |>
  select(Symbol:CommonName,
         AGFO:WICA) |>
  arrange(Symbol)

QC_table <- rbind(QC_table,
                  QC_check(df = taxa_dup_bio,
                           meas_type = "Taxa",
                           tab = "Inconsistent Species",
                           check = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = F",
                           chk_type = 'check'))

dt_taxa_dup_bio <- make_dt(taxa_dup_bio, cap = "Symbols with inconsistent TSN, ScientificName, or CommonName values across parks where NotBiological = F")

### Taxa Tab Creation ----
# check if Taxa - missing checks returned at least 1 record to determine whether to include tab
taxa_check <- QC_table |>
  filter(Type %in% "Taxa" & Num_Records > 0)

taxa_include <- tab_include(taxa_check)

## Cover Point Data ----
point_int1 <- getCoverPoints() |>
  mutate(park = substr(MacroPlot_Name, 1, 4),
         year = as.numeric(year))

point_int <- point_int1 |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         TranLen,
         NumPtsTran,
         Transect,
         Point,
         Tape,
         Order,
         Height,
         ScientificName,
         Status,
         NotBiological) #|> unique()

#### Ground Hits ----
num_ground <- point_int |>
  filter(Order == 0) |>
  distinct() |>
  summarize(num_ground = n(),
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    NumPtsTran,
                    Transect)) |>
  filter(NumPtsTran != num_ground)|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         NumPtsTran,
         num_ground) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date,
          Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = num_ground,
                           tab = "Incorrect Ground",
                           meas_type = "Point Intercept",
                           check = "Transects where number of ground hits doesn't match number of points sampled.",
                           chk_type = "error"))

dt_num_ground <- make_dt(num_ground,
                         cap = "Transects where number of ground hits doesn't match number of points sampled.")

#### Order > 0 have heights ----
ht_check <- point_int |>
  distinct() |>
  summarize(num_orders = n(),
            Height = sum(Height, na.rm = T), # sum much faster than max
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    Transect,
                    Point,
                    Tape)) |>
  filter(num_orders > 1 & Height == 0) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date)

ht_check$Height[ht_check$Height == 0] <- NA_real_ # Adding NA back in due to sum

QC_table <- rbind(QC_table,
                  QC_check(df = ht_check,
                           tab = "Missing Height",
                           meas_type = "Point Intercept",
                           check = "Points with more than 1 order missing a height for the top hit.",
                           chk_type = "error"))

dt_ht_check <- make_dt(ht_check,
                       cap = "Points with more than 1 order missing a height for the top hit.")

### Duplicate orders within a transect ----
dup_order <- point_int |>
  distinct() |>
  summarize(num_hits = sum(!is.na(ScientificName)),
            .by = c(MacroPlot_Name,
                    Unit_Name,
                    SampleEvent_Date,
                    year,
                    Transect,
                    Point,
                    Tape,
                    Order)) |>
  filter(num_hits > 1) |>
  arrange(MacroPlot_Name,
          SampleEvent_Date)

QC_table <- rbind(QC_table,
                  QC_check(df = dup_order,
                           tab = "Duplicate Order",
                           meas_type = "Point Intercept",
                           check = "Points with duplicate orders on the same transect.",
                           chk_type = "error"))

kbl_dup_order <- make_kable(dup_order,
                            cap = "Points with Order = 1 and blank Height value.")

### Check for heights > 2m ----
ht_oor <- point_int |>
  filter(Height > 2.0) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         Point,
         Tape,
         Order,
         Height,
         ScientificName) |>
  distinct()

QC_table <- rbind(QC_table,
                  QC_check(df = ht_oor,
                           tab = "Height over 2m",
                           meas_type = "Point Intercept",
                           check = "Points with a Height > 2.0m.",
                           chk_type = "error"))

kbl_ht_oor <- make_kable(ht_oor, cap = "Points with a Height > 2.0m.")

### Find heights > 99.8% ever recorded ----
point_ht99 <- quantile(point_int$Height,
                       probs = 0.998,
                       na.rm = T)

point99 <- point_int |>
  filter(Height > point_ht99) |>
  mutate(Height99 = point_ht99) |>
  arrange()

QC_table <- rbind(QC_table,
                  QC_check(point99,
                           tab = "Heights over 99.8pct",
                           meas_type = "Point Intercept",
                           check = "Heights greater than 99.8pct ever recorded.",
                           chk_type = 'check'))

kbl_point99 <- make_dt(point99,
                       cap = "Heights greater than 99.8pct ever recorded.")

#### Transect numbers that aren't 1 or 2 ----
trans12 <- point_int |>
  filter(!Transect %in% c(1, 2)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = trans12,
                           tab = "Odd Transects",
                           meas_type = "Point Intercept",
                           check = "Transects not numbered 1 or 2",
                           chk_type = 'error'))

kbl_trans12 <- make_kable(trans12, cap = "Transects not numbered 1 or 2")

### Point Intersect Tab Creation ----
# check if Point Intercept checks returned at least 1 record to determine whether to include tab
pint_check <- QC_table |>
  filter(Type %in% "Point Intercept" & Num_Records > 0)

pint_include <- tab_include(pint_check)


## Quadrats ----
densb <- getDensityBelts(years = year_range, purpose = "NGPN_PCM") |>
  mutate(year = as.numeric(year)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         MacroPlot_Purpose,
         year,
         month,
         NumTran,
         TranLen,
         TranWid,
         Area,
         Transect,
         Subbelt,
         SubFrac,
         Status,
         Count,
         Symbol,
         ITIS_TSN,
         ScientificName,
         UV1,
         UV2,
         UV3)

### Transect numbers that aren't 1 or 2 ----
dtrans12 <- densb |>
  filter(!Transect %in% c(1, 2)) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = dtrans12,
                           tab = "Odd Transects",
                           meas_type = "Quadrats",
                           check = "Transects not numbered 1 or 2",
                           chk_type = 'error'))

kbl_dtrans12 <- make_kable(dtrans12,
                           cap = "Transects not numbered 1 or 2")

### Transects != 1m ----
dtrans1 <- densb |>
  filter(TranLen != 1) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         TranLen,
         Transect)

QC_table <- rbind(QC_table,
                  QC_check(df = dtrans1,
                           tab = "Transect Lengths not = 1",
                           meas_type = "Quadrats",
                           check = "Transects that are not = 1",
                           chk_type = 'error'))

kbl_dtrans1 <- make_kable(dtrans1, cap = "Transect lengths that are not = 1")

### Subfractions that don't match ----
subfracs <- c(0.01, 0.1, 1, 10)

subfrac_imp <- densb |>
  filter(!SubFrac %in% subfracs) |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         Transect,
         SubFrac)

QC_table <- rbind(QC_table,
                  QC_check(df = subfrac_imp,
                           tab = "SubFrac Typo",
                           meas_type = "Quadrats",
                           check = "Subfractions that are outside acceptible values",
                           chk_type = 'error'))

kbl_subfrac_imp <- make_kable(subfrac_imp,
                              "Subfractions that are outside acceptible values")

### Species Counts != 1 ----
spp_count <- densb |>
  filter(Count != 1) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         Subbelt,
         SubFrac,
         Symbol,
         ScientificName,
         Count)

QC_table <- rbind(QC_table,
                  QC_check(df = spp_count,
                           tab = "Count != 1",
                           meas_type = "Quadrats",
                           check = "Recorded count value != 1",
                           chk_type = "error"))

kbl_spp_count <- make_kable(spp_count, cap = "Recorded count value != 1")

### Species count > 1 ----
spp_dup <- densb |>
  filter(Count == 1) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         Transect,
         Subbelt,
         SubFrac,
         Symbol,
         ScientificName,
         Count) |>
  unique() |>
  pivot_wider(names_from = SubFrac,
              values_from = Count,
              names_prefix = "SubFrac_",
              values_fill = 0)

subfrac_cols <- c("SubFrac_0.01",
                  "SubFrac_0.1",
                  "SubFrac_1",
                  "SubFrac_10")

missing <- setdiff(subfrac_cols, names(spp_dup))

spp_dup[missing] <- 0

spp_dup$Count <- spp_dup$SubFrac_0.01 + spp_dup$SubFrac_0.1 + spp_dup$SubFrac_1 + spp_dup$SubFrac_10

spp_dup2 <- spp_dup |> filter(Count > 1)

QC_table <- rbind(QC_table,
                  QC_check(spp_dup2, tab = "Duplicate Spp.",
                           meas_type = "Quadrats",
                           check = "Species with more than one count per subbelt",
                           chk_type = "error"))

kbl_spp_dup2 <- make_kable(spp_dup2, cap = "Species with more than one count per subbelt.")

### Quadrats Tab Creation ----
# check if Nested Quadrats checks returned at least 1 record to determine whether to include tab
densb_check <- QC_table |>
  filter(Type %in% "Quadrats" & Num_Records > 0)

densb_include <- tab_include(densb_check)

## Trees and Poles ----
trees1 <- getTrees(years = year_range, purpose = "NGPN_PCM") |>
  select(MacroPlot_Name,
         Unit_Name,
         SampleEvent_Date,
         year,
         month,
         MacroPlotSize,
         SnagPlotSize,
         BrkPntDia,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         CrwnCl,
         LiCrBHt,
         CrwnRad,
         DRC,
         UV1,
         UV2)

trees <- left_join(trees1,
                   tab4_spp,
                   by = c("ScientificName",
                          "Symbol")) |>
 mutate(diam = pmax(DBH,
                    DRC,
                    na.rm = T))

### DBH/DRC ----
# Check that species that are DRC are correctly sampled as DRC (taken from Table 4 of NGPN PCM SOPs (p94))
drc_spp <- tab4_spp$Symbol[tab4_spp$Diam_Loc == "Root Collar"]

#### Wrong DRC vs DBH ----
trees_wrong_drc <- trees |>
  filter(Symbol %in% drc_spp & !is.na(DBH)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(trees_wrong_drc, tab = "Wrong Diam - DRC",
                           meas_type = "Trees and Poles",
                           check = "Trees that should have had DRC instead of DBH measured.",
                           chk_type = 'error'))

dt_trees_wrong_drc <- make_dt(trees_wrong_drc,
                              cap = "Trees that should have had DRC instead of DBH measured, based on Table 4 in SOPs.")

#### Wrong DBH vs DRC ----
trees_wrong_dbh <- trees |>
  filter((!Symbol %in% drc_spp) & !is.na(DRC))|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         Status,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(trees_wrong_dbh,
                           tab = "Wrong Diam - DBH",
                           meas_type = "Trees and Poles",
                           check = "Trees that should have had DBH instead of DRC measured.",
                           chk_type = 'error'))

dt_trees_wrong_dbh <-
  make_dt(trees_wrong_dbh, cap = "Trees that should have had DBH instead of DRC measured, based on Table 4 in SOPs.")



#### Check trees > 80 cm for possible errors ----
bigt <- trees |>
  filter(diam > 80) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         DBH,
         DRC,
         diam)

QC_table <- rbind(QC_table,
                  QC_check(bigt, tab = "Big Trees", meas_type = "Trees and Poles",
                           check = "Trees greater than 80cm DBH or DRC to check for possible errors.",
                           chk_type = "check"))

dt_bigt <- make_dt(bigt |>
                     select(-diam),
                   cap = "Trees greater than 60cm DBH or DRC to check for possible errors")

#### Check tree Diam >99% of ever recorded ----
tree_dbh99 <- quantile(trees$DBH, probs = 0.99, na.rm = T)

trees99 <- trees |>
  filter(DBH > tree_dbh99)

QC_table <- rbind(QC_table,
                  QC_check(trees99,
                           tab = "DBH over 99pct",
                           meas_type = "Trees and Poles",
                           check = "Tree DBH measurements greater than 99pct ever recorded.",
                           chk_type = 'check'))

dt_trees99 <- make_dt(trees99,
                      cap = "Tree DBH measurements greather than 99pct ever recorded. An alternate way of checking for out of range values.")

#### Check trees > 30 cm DRC for possible errors ----
bigdrc <- trees |>
  filter(DRC > 30) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(bigdrc,
                           tab = "Big DRCs",
                           meas_type = "Trees and Poles",
                           check = "Trees greater than 30cm DRC to check for possible errors.",
                           chk_type = "check"))

dt_bigdrc <- make_dt(bigdrc,
                     cap = "Trees greater than 30cm DRC to check for possible errors.")


#### Check tree Diam >99% of ever recorded ---
tree_drc99 <- quantile(trees$DRC, probs = 0.99, na.rm = T)

trees_drc99 <- trees |>
  filter(DRC > tree_drc99)

QC_table <- rbind(QC_table,
                  QC_check(trees_drc99,
                           tab = "DRC over 99pct",
                           meas_type = "Trees and Poles",
                           check = "Tree DRC measurements greater than 99pct ever recorded.",
                           chk_type = 'check'))

kbl_trees_drc99 <- make_kable(trees_drc99,
                              "Tree DRC measurements greather than 99pct ever recorded. An alternate way of checking for out of range values.")

#### Check for Trees < 2.54 DBH ----
small_dbh <- trees |>
  filter(diam <= 2.54) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(small_dbh,
                           tab = "DBH under 2.54",
                           meas_type = "Trees and Poles",
                           check = "Stems with DBH or DRC <= 2.54cm",
                           chk_type = 'error'))

kbl_small_dbh <- make_kable(small_dbh, cap = "Stems with DBH or DRC <= 2.54cm")

### Check for missing tree data ----
#### Trees >15cm DBH should have UV1 In/Out and UV2 Condition Code column and that are Trees (not shrubs) ----
tree_miss_uv <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(is.na(UV1) | is.na(UV2)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(tree_miss_uv, tab = "Trees missing UV",
                           meas_type = "Trees and Poles",
                           check = "Trees > 15cm DBH or DRC missing UV1 and or UV2 values.",
                           chk_type = "check"))

dt_tree_miss_uv <- make_dt(tree_miss_uv, cap = "Trees > 15cm DBH or DRC missing UV1 and or UV2 values.")

#### Inconsistent IN/OUT in UV1 ----
tree_incon_UV1 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(!UV1 %in% c("IN", "OUT") & !is.na(UV1)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1)

QC_table <- rbind(QC_table,
                  QC_check(tree_incon_UV1,
                           tab = "Trees incon. UV1",
                           meas_type = "Trees and Poles",
                           check = "Trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.",
                           chk_type = "check"))

dt_tree_incon_UV1 <- make_dt(tree_incon_UV1, cap = "Trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.")

#### Check that UV2 tree conditions match SOP conditions (page 85/103 of SOP) ----
tree_cond_list <- c( "BKN", "CAMB", "DBK", "DEC", "DIS", "INS", "MPBB", "MPBG", "ROOT",
                     "SCAR", "SCORCH1", "SCORCH2", "SCORCH3", "SCORCH4", "SND")

tree_conds_UV2 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "L") |>
  filter(!UV2 %in% tree_cond_list & !is.na(UV2)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(tree_conds_UV2,
                           tab = "Trees incon. UV2",
                           meas_type = "Trees and Poles",
                           check =
                           paste0(
                           "Live trees > 15cm DBH or DRC with UV2 values that don't match codes in SOP: ",
                           paste0(tree_cond_list, collapse = ", "),
                           "."),
                           chk_type = "check"))

dt_tree_conds_UV2 <- make_dt(tree_conds_UV2, cap = "Live trees > 15cm DBH or DRC with UV1 values that don't perfectly match 'IN' or 'OUT'.")

#### Check if poles have UV1 or UV2 entered ----
pole_uv <- trees |>
  filter(diam <= 15) |>
  filter(!is.na(UV1) | !is.na(UV2))|>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC,
         UV1,
         UV2)

QC_table <- rbind(QC_table,
                  QC_check(pole_uv, tab = "Poles with UV",
                           meas_type = "Trees and Poles",
                           check = "Poles <= 15cm DBH or DRC with UV1 or UV2 values.",
                           chk_type = "check"))

dt_pole_uv <- make_dt(pole_uv, cap = "Poles <= 15cm DBH or DRC with UV1 or UV2 values.")

### Inconsistent data ----
#### Inconsistent tree status codes ---
tree_statcodes <- trees |>
  filter(!Status %in% c("L", "D") & !is.na(Status)) |>
  select(MacroPlot_Name,
         SampleEvent_Date,
         year,
         QTR,
         SubFrac,
         TagNo,
         Symbol,
         ScientificName,
         GrowthForm,
         DBH,
         DRC)

QC_table <- rbind(QC_table,
                  QC_check(tree_statcodes,
                           tab = "Incons. Status Codes",
                           meas_type = "Trees and Poles",
                           check = "Status codes that don't exactly match L or D",
                           chk_type = 'error'))

kbl_tree_statcodes <- make_kable(tree_statcodes, cap = "Status codes that don't exactly match L or D")

#### Check UV2 for dead trees ----
dead_tree_codes <- c("CS", "LS", "RS")

dead_con <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "D") |>
  filter(!UV2 %in% dead_tree_codes & !is.na(UV2))

QC_table <- rbind(QC_table,
                  QC_check(dead_con,
                           tab = "Incons. Dead UV2",
                           meas_type = "Trees and Poles",
                           check = "Status codes that don't exactly match dead condition codes : 'CS', 'LS', 'RS'",
                           chk_type = 'error'))

dt_dead_con <- make_dt(dead_con,
                       cap = "Status codes that don't exactly match dead condition codes : 'CS', 'LS', 'RS'")

#### Check for missing UV2 for dead trees ----
dead_miss_uv2 <- trees |>
  filter(diam > 15) |>
  filter(GrowthForm == "Tree") |>
  filter(Status == "D") |>
  filter(is.na(UV2))

QC_table <- rbind(QC_table,
                  QC_check(dead_miss_uv2,
                           tab = "Dead missing UV2",
                           meas_type = "Trees and Poles",
                           check = "Dead trees > 15cm DBH or DRC missing UV2 values.",
                           chk_type = 'error'))

kbl_dead_miss_uv2 <- make_kable(dead_miss_uv2,
                           cap = "Dead trees > 15cm DBH or DRC missing UV2 values.")

### Tree Tab Creation ----
# check if tree checks returned at least 1 record to determine whether to include tab
tree_check <- QC_table |>
  filter(Type %in% "Trees and Poles" & Num_Records > 0)

tree_include <- tab_include(tree_check)

# Compile final QC Table ----
# revise for different color combos for checks (99 vs 90)? Drop for checks vs. errors?
QC_cap <- 
if(all_years == TRUE){
"The table below documents Quality Control checks performed on NGPN
Plant Community Monitoring data that are stored in the FFI database. This report
primarily checks data that are entered annually. If records are returned for a given check, the row is
highlighted yellow for errors and blue for records that aren't necessarily
errors, but need further review (e.g., large DBH measurements). A separate tab
corresponding to each check that returned results by protocol module (e.g.
Point Intercept, Quadrats, etc.) is printed to the right of this tab. Only
MacroPlots with samples that fall on the panel schedule sampling scheme are
included in this report."
} else if(all_years == FALSE){
  paste0("The table below documents Quality Control checks performed on NGPN
Plant Community Monitoring data that are stored in the FFI database for year: ", 
year_curr, ". This report
primarily checks data that are entered annually. If records are returned for a given check, the row is
highlighted yellow for errors and blue for records that aren't necessarily
errors, but need further review (e.g., large DBH measurements). A separate tab
corresponding to each check that returned results by protocol module (e.g.
Point Intercept, Quadrats, etc.) is printed to the right of this tab. Only
MacroPlots with samples that fall on the panel schedule sampling scheme are
included in this report.")
}

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
