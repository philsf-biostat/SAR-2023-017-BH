# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
library(haven)
# library(foreign)
library(naniar)
library(lubridate)
library(labelled)

demographics <- c(
  "Mod1id",
  "Birth",
  "SexF",
  "Race",
  "Mar",
  "AGE",
  "ZipDis",
  "ZipF",
  "ZipInj",
  "PROBLEMUse",
  "EDUCATION",
  "EMPLOYMENT",
  # "PTADays",
  "RURALdc",
  "DCIDistressScore",
  "DCIQuintile",
  "DCIDecile")

clinical <- c(
  "Mod1id",
  "PriorSeiz",
  "SCI",
  "Injury",
  "Cause",
  # "AcutePay1",
  "RehabPay1",
  "RehabDis",
  "ResDis",
  "DAYStoREHABdc",
  # "DRSd",
  # "FIMTOTD",
  "FIMMOTD",
  "FIMCOGD",
  "DeathF",
  "FollowUpPeriod",
  "IntStatus",
  "Followup")

num_vars <- c("AGE", "DAYStoREHABdc", "FIMMOTD", "FIMCOGD", "FollowUpPeriod")

# participants data -------------------------------------------------------

set.seed(42)

# read first patient data table
data.raw <- read_sav("dataset/Form1_20221017.sav")

# join second patient data table
data.raw <- data.raw %>%
  left_join(
    read_sav("dataset/Form2_20221017.sav"),
    by = "Mod1id",
    suffix = c("_Form1", "_Form2"),
  )

# Original N of individuals
Nobs_orig_id <- data.raw %>% distinct(Mod1id) %>% nrow()

na_zip <- c("66666", "88888", "99999", "")
na_date <- c("4444-04-04", "5555-05-05", "7777-07-07", "8888-08-08", "9999-09-09") %>%
  as.POSIXct(tz = "UTC")
# na_fct <- c("Unknown")

# save var labels before processing
labs <- data.raw %>% var_label()

# missing data treatment: explicit NA
data.raw <- data.raw %>%
  replace_with_na(replace = list(
   # replace NA in all Zipcodes
   ZipInj = na_zip,
   ZipDis = na_zip,
   ZipF = na_zip,
   # replace NA in all Dates
   Birth = na_date,
   Death = na_date,
   DeathF = na_date,
   Followup = na_date,
   # replace NA in other covariates
   SexF = c(99),
   Race = c(99),
   Mar = c(99),
   ResDis = c(888, 999),
   PriorSeiz = c(66, 77, 99),
   SCI = c(99),
   Cause = c(999),
   AcutePay1 = c(888, 999),
   RehabPay1 = c(888,999),
   AGE = c(9999),
   PROBLEMUse = c(77, 99),
   DAYStoREHABdc = c(8888, 9999),
   DRSd = c(999),
   EDUCATION = c(999),
   EMPLOYMENT = c(888, 999),
   FIMMOTD = c(999),
   FIMTOTD = c(9999),
   FIMCOGD = c(999),
   PTADays = c(888, 9999),
   # RURALdc = c(),
   # FollowUpPeriod = c(),
   # IntStatus = c(),
   RehabDis = na_date
  )) #%>%
# this takes TOO LONG to run - switch to manual replacing (above)
  # # replace NA in all Dates
  # replace_with_na_if(
  #   .predicate = is.POSIXt,
  #   condition = ~.x %in% na_date
  #   )

# data cleaning -----------------------------------------------------------

# # create shadow vars before imputation
# data.raw <- data.raw %>%
#   mutate(ZipDis.Shadow = is.na(ZipDis), .after = ZipDis)
# 
# # impute Zip codes
# data.raw <- data.raw %>%
#   mutate(
#     # Fill missing ZipDis with ZipInj
#     ZipDis = if_else(is.na(ZipDis), ZipInj, ZipDis),
#     # Fill missing ZipF with (filled) ZipDis
#     ZipF = if_else(is.na(ZipF), ZipDis, ZipF),
#     # Fill Death dates from Form1
#     DeathF = if_else(is.na(DeathF), Death, DeathF),
#     # keep FU as numeric, for later filtering
#     FollowUpPeriod = as.numeric(FollowUpPeriod),
#   )
# 
# # which values were imputed?
# imputed <- data.raw %>%
#   # select(starts_with("ZipD")) %>%
#   filter(ZipDis.Shadow, !is.na(ZipDis))

# SES data ----------------------------------------------------------------

# join Zipcodes table
data.raw <- data.raw %>%
  left_join(
    # read all columns as character, so Zipcodes are kept as originally encoded
    read_excel("dataset/DCI.xlsx", col_types = c("text")),
    by = c("ZipDis" = "Zipcode")
  )

# size of original dataset, before filtering
Nvar_orig <- data.raw %>% ncol
Nobs_orig <- data.raw %>% nrow

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    all_of(demographics),
    all_of(clinical),
  ) %>%
  mutate(
    # convert DCI values back to numeric
    across(starts_with("DCI"), as.numeric),
    # simplify dates
    across(where(is.POSIXt), as_date),
  )

# keep labelled numeric vars before wholesale conversion to factor
data.raw <- data.raw %>%
  mutate(
    across(all_of(num_vars), as.numeric),
  )

# convert haven_labelled to factor (missing value codes are used automatically)
data.raw <- data.raw %>%
  mutate(
    across(where(is.labelled), as_factor),
  )

# convert back (from factor) the variable types
data.raw <- data.raw %>%
  mutate(
    across(starts_with("Zip"), as.character),
  )

# clear unused factor levels
data.raw <- data.raw %>%
  droplevels()

# restore original labels - intersect is used to get only valid colnames
var_label(data.raw) <- labs[intersect(colnames(data.raw), names(labs))]

# data saving -------------------------------------------------------------

save(data.raw, Nobs_orig, Nvar_orig, Nobs_orig_id, demographics, clinical, file = "dataset/brennan_data.rds")
