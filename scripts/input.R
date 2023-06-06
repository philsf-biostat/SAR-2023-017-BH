# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
# library(readxl)
# library(haven)
# library(foreign)
library(lubridate)
# library(naniar)
library(labelled)

study_period <- c("2010-01-01", "2018-12-31") %>%
  as.Date()

# data loading ------------------------------------------------------------
set.seed(42)
load(file = "dataset/brennan_data.rds")

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    everything(),
  ) %>%
  rename(
    id = Mod1id,
    exposure = DCIQuintile,
  ) %>%
  mutate(
    # create new Date with either DeathF OR Followup - prioritize Deaths over Followup when both are present
    Date = if_else(is.na(DeathF), Followup, DeathF),
    # status at followup Date
    outcome = as.numeric(!is.na(DeathF)), # 0=alive, 1=dead
    # time to event (in days)
    Time_d = as.duration(interval(RehabDis, Date)),
    Time = Time_d/dyears(1),
  ) %>%
  filter(
  )

# rename selecting vars
demographics <- str_replace(demographics, "Mod1id", "id")
demographics <- str_replace(demographics, "DCIQuintile", "exposure")
clinical <- str_replace(clinical, "Mod1id", "id")
clinical <- str_replace(clinical, "DCIQuintile", "exposure")

# exclusion criteria: COVID is a possible confounder, use outcome Status Date to exclude
data.raw <- data.raw %>%
  filter(
    Date <= as.Date("2019-12-31") # last date (status)
  )

# exclusion criteria: before 2020
Nobs_incl_per <- data.raw %>% nrow()

# inclusion criteria: up to 10yr of follow up
data.raw <- data.raw %>%
  filter(
    FollowUpPeriod <= 10,
  )

# exclusion criteria: redundant participant observations: pick last date of follow up
data.raw <- data.raw %>%
  group_by(id) %>%
  filter(
    FollowUpPeriod == max(FollowUpPeriod, na.rm = TRUE),
  ) %>%
  ungroup()

# inclusion criteria: 10yr follow up + unique IDs
Nobs_incl_id <- data.raw %>% nrow()

# inclusion criteria: study period
data.raw <- data.raw %>%
  filter(
    between(RehabDis, study_period[1], study_period[2]), # discharge date
  )

# inclusion criteria: valid times
data.raw <- data.raw %>%
  filter(Time>0)

# remove invalid observations (outcome at time 0 or below)
Nobs_invalid <- data.raw %>% nrow()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = as.character(id), # or as.factor
    # label SES quintiles
    exposure = factor(exposure, labels = c("Prosperous", "Comfortable", "Mid-Tier", "At-Risk", "Distressed")),
    # age at time of injury
    AGE = if_else(is.na(AGE), floor(as.duration(interval(Birth, Injury))/dyears(1)), AGE),
    # reduce number of categories
    Race = fct_collapse(Race,
                        White = "White",
                        Black = "Black",
                        Hispanic = "Hispanic Origin",
                        other_level = "Other",
                        ),
    EDUCATION = fct_collapse(EDUCATION,
                             "Less Than High School" = c("8th Grade or Less", "9th - 11th Grade"),
                             "High School/GED" = c("GED", "HS", "HS/GED", "777"),
                             # "21" is presumed to be "Trade"
                             "Greater Than High School" = c("Trade", "21", "Some College", "Associate", "Bachelors", "Masters", "Doctorate"),
                             # other_level = "Other",
                             ),
    EMPLOYMENT = fct_collapse(EMPLOYMENT,
                              Employed = "Employed",
                              Unemployed = "Unemployed",
                              other_level = "Other",
                              ),
    RehabPay1 = fct_collapse(RehabPay1,
                             "Private Insurance" = c("Private Insurance", "Workers Compensation", "Auto Insurance"),
                             "Public Insurance" = c("Medicaid", "Medicare", "State or County"),
                             other_level = "Other",
                             ),
    ResDis = fct_collapse(ResDis,
                          "Private Residence" = "Private Residence",
                          other_level = "Other",
                          ),
    Cause = fct_collapse(Cause,
                         Vehicular = c("Motor Vehicle", "Motorcycle", "Bicycle", "All-Terrain Vehicle (ATV) and All-Terrain Cycle (ATC)", "Other Vehicular: Unclassified"),
                         Falls = "Fall",
                         Violence = c("Gunshot Wound", "Assaults With Blunt Instrument", "Other Violence"),
                         other_level = "Other",
                         ),
    SCI = as.numeric(SCI == "Yes"),
    PROBLEMUse = as.numeric(PROBLEMUse == "Yes"),
    # relevel variables
    SexF = fct_relevel(SexF, "Male"),
    RehabPay1 = fct_relevel(RehabPay1, "Private Insurance"),
    EDUCATION = fct_relevel(EDUCATION, "Greater Than High School"),
    RURALdc = fct_relevel(RURALdc, "Suburban"),
    FIMMOTD4 = cut(FIMMOTD, breaks = c(0, quantile(FIMMOTD, probs = c(.25, .50, .75), na.rm = TRUE), 100), labels = c("Q1", "Q2", "Q3", "Q4")), #, labels = c("Q1", "Q2", "Q3", "Q4"), right = FALSE
    FIMCOGD4 = cut(FIMCOGD, breaks = c(0, quantile(FIMCOGD, probs = c(.25, .50, .75), na.rm = TRUE), 100), labels = c("Q1", "Q2", "Q3", "Q4")),
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    exposure = "SES quintiles",
    outcome = "Mortality",
    AGE = "Age at injury",
    Time = "Time of follow up (years)",
    Date = "Date of last follow up",
    FIMMOTD4 = str_replace(attr(data.raw$FIMMOTD, "label"), ":", " quartiles"),
    FIMCOGD4 = str_replace(attr(data.raw$FIMCOGD, "label"), ":", " quartiles"),
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    id,
    exposure,
    outcome,
    Date,
    Time,
    everything(),
    -starts_with("Zip"),
    -starts_with("DCI"),
    -where(is.Date),
    -IntStatus,
    -FollowUpPeriod,
    -Time_d,
  )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
