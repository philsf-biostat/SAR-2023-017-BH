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
  # "FIMMOTF",
  # "FIMCOGF",
  "DeathF",
  "FollowUpPeriod",
  "IntStatus",
  "Followup")

num_vars <- c("AGE", "DAYStoREHABdc", "FIMMOTD", "FIMCOGD", "FollowUpPeriod")

na_zip <- c("66666", "88888", "99999", "")
na_date <- c("4444-04-04", "5555-05-05", "6666-06-06", "7777-07-07", "8888-08-08", "9999-09-09") %>%
  as.Date()
# na_fct <- c("Unknown")

# read DCI/SES data
# read all columns as character, so Zipcodes are kept as originally encoded
DCI <- read_excel("dataset/DCI.xlsx", col_types = c("text")) %>%
  select(Zipcode, starts_with("DCI")) # only DCI* data from table

# participants data -------------------------------------------------------

set.seed(42)

# read first patient data table
cc <- read_sav("dataset/Form1_20221017.sav") %>%
  # join second patient data table
  left_join(
    read_sav("dataset/Form2_20221017.sav"),
    by = "Mod1id",
    suffix = c("_Form1", "_Form2"),
  ) %>%
  select(any_of(c(demographics, clinical))) # reduce amount of data collected

# Original N of individuals
# Nobs_orig_id <- data.raw %>% distinct(Mod1id) %>% nrow()

# save var labels before processing
labs <- cc %>% var_label()

# data cleaning -----------------------------------------------------------

# missing data treatment: explicit NA
cc <- cc %>%
  mutate(
    # simplify dates
    across(where(is.POSIXt), as_date),
  ) %>%
  replace_with_na(replace = list(
   # replace NA in all Zipcodes
   ZipInj = na_zip,
   ZipDis = na_zip,
   ZipF = na_zip,
   # replace NA in all Dates
   Birth = na_date,
   # Death = na_date,
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
   # AcutePay1 = c(888, 999),
   RehabPay1 = c(888,999),
   AGE = c(9999),
   PROBLEMUse = c(77, 99),
   DAYStoREHABdc = c(8888, 9999),
   # DRSd = c(999),
   EDUCATION = c(999),
   EMPLOYMENT = c(888, 999),
   FIMMOTD = c(999),
   FIMCOGD = c(999),
   # FIMMOTF = c(999),
   # FIMCOGF = c(999),
   # PTADays = c(888, 9999),
   # RURALdc = c(),
   # FollowUpPeriod = c(),
   # IntStatus = c(),
   RehabDis = na_date
  ))

# prepare base dataset ----------------------------------------------------

# This section will break the orig data into 4 datasets
# - data with consecutive followup, for location data (cc, and its siblings)
# - date of death, with follow indicator (Deaths)
# - participant constant data (d)
# - individuals with no followup identifiers (lost)
# data.raw will be reconstructed from multiple datasets that undergone different imputation procedures with the previous structure

# prepare colnames for wrangling operations
cc <- cc %>%
  # standard naming convention
  rename(Date_Inj = Injury, Zip_Inj = ZipInj, Date_Dis = RehabDis, Zip_Dis = ZipDis, Zip = ZipF) %>%
  # create empty columns before spreading/collapsing operations
  mutate(outcome_Inj=0, outcome_Dis=0,) %>%
  # keep a copy of RehabDis, for the inclusion/exclusion criteria
  mutate(RehabDis = Date_Dis) %>%
  # keep a copy of Injury, for the AGE calculation
  mutate(Injury = Date_Inj) %>%
  # merge DeathF and Followup into a single Date
  mutate(
    # create new Date with either DeathF OR Followup - prioritize Deaths over Followup when both are present
    Date = if_else(is.na(DeathF), Followup, DeathF),
  )

# add Inj/Dis into Date/Zip: spread wide then collapse into long format
cc <- cc %>%
  # spread FollowUpPeriod across all 3 variables (Date, Zip, IntStatus)
  pivot_wider(names_from = FollowUpPeriod, values_from = c(Date, Zip, IntStatus
                                                           # , outcome # will be time-varying, but not created yet
                                                           # , Time # will be time-varying, but not created yet
                                                           , DeathF # time-varying
                                                           , Followup # time-varying
                                                           # , FIMMOTF, FIMCOGF # time-varying
                                                           )) %>% # side-effect: creates Date_NA, Zip_NA, etc
  select(-ends_with("NA")) %>% # drop Date_NA, Zip_NA, ...
  mutate(across(starts_with("Zip"), as.character)) %>% # remove labels from Zip vars (required to match coltypes when pivoting to long format)
  # collapse all 3 variables (Date, Zip, IntStatus)
  pivot_longer(cols = starts_with(c("Date_", "Zip_", "IntStatus_" # time-varying
                                    , "outcome_" # is time-varying, but not yet defined at follow-up time points
                                    # , "Time_" # will be time-varying, but not created yet
                                    , "Followup_", "DeathF_" # time-varying
                                    # , "FIMMOTF_", "FIMCOGF_" # time-varying
                                    )), names_to = c(".value", "FollowUpPeriod"), names_sep = "_")

# define new date-based variables: Time, outcome
cc <- cc %>%
  mutate(
    # status at followup Date
    outcome = as.numeric(!is.na(DeathF)), # 0=alive, 1=dead
    # time to event (in days)
    Time = as.duration(interval(RehabDis, Date)),
    # Time = Time_d/dyears(1),
    # .after = DeathF,
  )

# sort observations by FollowUpPeriod
cc <- cc %>%
  # temporary fix for FollowUpPeriod: set numeric for ordering before imputation
  mutate(
    FollowUpPeriod = str_replace(FollowUpPeriod, "Dis", "0"),
    FollowUpPeriod = str_replace(FollowUpPeriod, "Inj", "-1"),
    FollowUpPeriod = parse_number(FollowUpPeriod),
  ) %>%
  # drop unused followups that were created when spreading the data
  drop_na(Date) %>%
  # sort follow up indicator by id
  group_by(Mod1id) %>%
  arrange(FollowUpPeriod) %>%
  ungroup()

# impute Zip codes --------------------------------------------------------

# prepare to work with multiple datasets
data.raw <- bind_rows(
  cc = cc,
  locf = cc %>% group_by(Mod1id) %>% fill(Zip, .direction = "down") %>% ungroup(),
  "locf+nocb" = cc %>% group_by(Mod1id) %>% fill(Zip, .direction = "downup") %>% ungroup(),
  .id = "dataset") %>%
  group_by(dataset) %>%
  nest()

# SES data ----------------------------------------------------------------

# join SES data
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
                      # join SES data
                      left_join(DCI, by = c("Zip" = "Zipcode")) %>%
                      # define exposure
                      mutate(exposure = DCIQuintile)
                    ))

# add separate SES at discharge
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
                      mutate() %>%
                      left_join(
                        .x %>%
                          filter(FollowUpPeriod==0) %>%
                          select(Mod1id, Zip) %>%
                          left_join(DCI %>% rename_with(~ paste0(.x, "_Dis"), .cols = starts_with("DCI")), by = c("Zip" = "Zipcode")) %>% select(-Zip),
                        by = "Mod1id"
                      )
  ))

# restore original structure ----------------------------------------------

# test: 13446 benefits from locf, outcome 1 at follow up 15
data.raw %>%
  filter(dataset == "cc") %>%
  unnest(data) %>%
  filter(Mod1id == 13444) %>% select(dataset, Mod1id, Date, Zip, FollowUpPeriod,  starts_with("exposure"), starts_with("DCI"))

# test: 13444 benefits from locf+nocb, outcome 0 at all follow ups
data.raw %>%
  filter(dataset == "cc") %>%
  unnest(data) %>%
  filter(Mod1id == 13446) %>% select(dataset, Mod1id, Date, Zip, FollowUpPeriod,  starts_with("exposure"), starts_with("DCI"))

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
      mutate(
        # convert DCI values back to numeric
        across(starts_with("DCI"), as.numeric),
        # keep labelled numeric vars before wholesale conversion to factor
        across(any_of(num_vars), as.numeric),
        # convert haven_labelled to factor (missing value codes are used automatically)
        across(where(is.labelled), as_factor),
        # convert back (from factor) the variable types
        across(starts_with("Zip"), as.character),
      ) %>%
      # clear unused factor levels
      droplevels()
  ))

# restore original labels - intersect is used to get only valid colnames
data.raw <- data.raw %>%
  mutate(
    data = map(data, ~ .x %>% set_variable_labels(.labels = labs[intersect(colnames(.x), names(labs))]) )
    )

# data saving -------------------------------------------------------------

# save(data.raw, Nobs_orig, Nvar_orig, Nobs_orig_id, demographics, clinical, file = "dataset/brennan_data_17.rds")
write_rds(data.raw, "dataset/brennan_data_17.rds")
