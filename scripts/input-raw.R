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
data.raw <- read_sav("dataset/Form1_20221017.sav") %>%
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
labs <- data.raw %>% var_label()

# data cleaning -----------------------------------------------------------

# missing data treatment: explicit NA
data.raw <- data.raw %>%
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
   # FIMTOTD = c(9999),
   FIMCOGD = c(999),
   # PTADays = c(888, 9999),
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

# prepare base dataset ----------------------------------------------------

# This section will break the orig data into 4 datasets
# - data with consecutive followup, for location data (cc, and its siblings)
# - date of death, with follow indicator (Deaths)
# - participant constant data (d)
# - individuals with no followup identifiers (lost)
# data.raw will be reconstructed from multiple datasets that undergone different imputation procedures with the previous structure

# start fresh
# merge DeathF and Followup into a single Date + create Status/Time at each date
cc <- data.raw %>%
  mutate(
    # create new Date with either DeathF OR Followup - prioritize Deaths over Followup when both are present
    Date = if_else(is.na(DeathF), Followup, DeathF),
    # status at followup Date
    outcome = as.numeric(!is.na(DeathF)), # 0=alive, 1=dead
    # time to event (in days)
    Time = as.duration(interval(RehabDis, Date)),
    # Time = Time_d/dyears(1),
    .after = DeathF,
  ) %>% select(-DeathF, -Followup) # Death and Followup are no longer needed: dropping

# add Inj/Dis into Date/Zip: spread wide then collapse into long format
cc <- cc %>%
  # select(-c(SexF, Race, Mar, AGE, PriorSeiz, PROBLEMUse, EDUCATION, EMPLOYMENT, RURALdc, SCI, Cause, RehabPay1, DAYStoREHABdc, FIMMOTD, FIMCOGD, ResDis)) %>%
  rename(Date_Inj = Injury, Zip_Inj = ZipInj, Date_Dis = RehabDis, Zip_Dis = ZipDis, Zip = ZipF) %>%
  # spread FollowUpPeriod across all 3 variables (Date, Zip, IntStatus)
  pivot_wider(names_from = FollowUpPeriod, values_from = c(Date, Zip, IntStatus, Time, outcome)) %>%
  select(-ends_with("NA")) %>% # Date_NA and Zip_NA
  #filter(Mod1id == 13446) %>%
  mutate(across(starts_with("Zip"), as.character)) %>% # remove labels from Zip vars
  # collapse all 3 variables (Date, Zip, IntStatus)
  pivot_longer(cols = starts_with(c("Date", "Zip", "IntStatus", "Time", "outcome")), names_to = c(".value", "FollowUpPeriod"), names_sep = "_") #%>%
  # replace_with_na(list(FollowUpPeriod = "NA"))

# # lost to follow up
# lost <- data.raw %>%
#   filter(is.na(FollowUpPeriod))

# temporary fix for FollowUpPeriod: set numeric for ordering before imputation
cc <- cc %>%
  mutate(
    FollowUpPeriod = str_replace(FollowUpPeriod, "Dis", "0"),
    FollowUpPeriod = str_replace(FollowUpPeriod, "Inj", "-1"),
    FollowUpPeriod = parse_number(FollowUpPeriod),
  ) %>%
  # drop unused followups that were created when spreading the data
  drop_na(Date) %>%
  group_by(Mod1id) %>%
  # sort follow up indicator by id
  arrange(FollowUpPeriod) %>%
  ungroup() %>%
  # temporary fix for FollowUpPeriod: revert levels after ordering
  mutate(
    FollowUpPeriod = as.character(FollowUpPeriod),
    FollowUpPeriod = str_replace(FollowUpPeriod, "^-1$", "Inj"),
    FollowUpPeriod = str_replace(FollowUpPeriod, "^0$", "Dis"),
  )

# impute Zip codes --------------------------------------------------------

# # impute LOCF
# locf <- cc %>%
#   group_by(Mod1id) %>%
#   fill(Zip, .direction = "down") %>%
#   ungroup()
# 
# # imput LOCF 2 (down + up)
# downup <- cc %>%
#   group_by(Mod1id) %>%
#   fill(Zip, .direction = "downup") %>%
#   ungroup()

# prepare to work with multiple datasets
data.raw <- bind_rows(
  cc = cc,
  locf = cc %>% group_by(Mod1id) %>% fill(Zip, .direction = "down") %>% ungroup(),
  downup = cc %>% group_by(Mod1id) %>% fill(Zip, .direction = "downup") %>% ungroup(),
  .id = "dataset") %>%
  group_by(dataset) %>%
  nest()

# define exposure + fill Time/outcome at baseline
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
                      # fill Time at Dis
                      mutate(Time = if_else(FollowUpPeriod == "Dis", duration(0), Time)) %>%
                      # fill outcome at Inj and Dis
                      replace_na(replace = list(outcome = 0))
                    ))

# # temporary fix for FollowUpPeriod: revert levels after imputation
# data.raw <- data.raw %>%
#   mutate(data = map(data, ~ .x %>%
#                       mutate(
#                         FollowUpPeriod = as.character(FollowUpPeriod),
#                         FollowUpPeriod = str_replace(FollowUpPeriod, "^-1$", "Inj"),
#                         FollowUpPeriod = str_replace(FollowUpPeriod, "^0$", "Dis"),
#                       )
#                     ))

# SES data ----------------------------------------------------------------

# Nvar_1 <- data.raw %>% ncol()

# join SES data
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
                      # join SES data
                      left_join(DCI, by = c("Zip" = "Zipcode")) %>%
                      # define exposure
                      mutate(exposure = DCIQuintile)
                    ))

# reshape after getting SES data: extract Inj/Dis from the Zip and Date as separate cols (we could drop_na(Date) after this)
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>% # operate on data.raw multiple times
                      pivot_wider(names_from = FollowUpPeriod, values_from = c(Date, Zip, starts_with("DCI"), IntStatus, Time, outcome, exposure)) %>%
                      pivot_longer(c(starts_with(c("Date", "Zip", "DCI", "IntStatus", "Time", "outcome", "exposure")), -ends_with(c("Inj", "Dis"))), names_to = c(".value", "FollowUpPeriod"), names_sep = "_") %>%
                      # IntStatus was spread, but there was never data for it: dropping
                      select(-IntStatus_Dis, -IntStatus_Inj, , -Time_Inj, - Time_Dis, -outcome_Inj, -outcome_Dis) %>%
                      # # DCI data was spread, and could be useful, but we don't need those: dropping
                      # select(-DCIQuintile_Inj, -DCIQuintile_Dis, -DCIDecile_Inj, -DCIDecile_Dis, -DCIDistressScore_Inj, -DCIDistressScore_Dis) %>%
                      # ensure exposure works for previous analysis code
                      # mutate(exposure_Dis = DCIQuintile_Dis) %>%
                      mutate(FollowUpPeriod = parse_number(FollowUpPeriod)) %>%
                      # drop unused followups that were created when spreading the data
                      drop_na(Date)
                    ))

# restore original structure ----------------------------------------------

# include deaths and individual characteristics
data.raw <- data.raw %>%
  mutate(data = map(data, ~ .x %>%
                      # rename back columns to previous names
                      rename(
                        ZipF = Zip,
                        ZipDis = Zip_Dis,
                        RehabDis = Date_Dis,
                        Followup = Date,
                        ZipInj = Zip_Inj,
                        Injury = Date_Inj,
                      )
                    ))

# # restore lost to followup
# # not required / redundant / non-informative
# data.raw <- data.raw %>%
#   mutate(data = map(data, ~
#                       .x %>%
#                       # left_join(lost, by = c("Mod1id", "FollowUpPeriod", "IntStatus"))
#                       bind_rows(lost)
#   ))

# test: 13446 benefits from locf, outcome 1 at follow up 15
data.raw %>%
  filter(dataset == "cc") %>%
  unnest(data) %>%
  filter(Mod1id == 13446) %>% select(dataset, Mod1id, Injury, RehabDis, Followup, FollowUpPeriod, ZipDis, starts_with("exposure"), ZipInj, ZipF)

# test: 13444 benefits from downup, outcome 0 at all follow ups
data.raw %>%
  filter(dataset == "cc") %>%
  unnest(data) %>%
  filter(Mod1id == 13444) %>% select(dataset, Mod1id, Injury, RehabDis, Followup, FollowUpPeriod, ZipDis, starts_with("exposure"), ZipInj, ZipF)

# size of original dataset, before filtering
# Nvar_orig <- data.raw %>% ncol
# Nobs_orig <- data.raw %>% nrow

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
