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
analytical <- read_rds("dataset/brennan_data_17.rds")


# save labels before processing
labs <- var_label(analytical$data[[1]])

# data cleaning -----------------------------------------------------------

analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  select(
    everything(),
  ) %>%
  rename(
    id = Mod1id,
    # # last known location at each follow_up: original analysis uses SES at baseline (discharge)
    # exposure_last = exposure,
  ) %>%
  mutate(
    # Date = Followup,
    # # reproduce original analysis: constant exposure, SES at baseline (discharge)
    # exposure = exposure_Dis,
    # new data: time-varying exposure
  ) %>%
  filter(
  )
  ))

# rename selecting vars
# demographics <- str_replace(demographics, "Mod1id", "id")
# demographics <- str_replace(demographics, "DCIQuintile", "exposure")
# clinical <- str_replace(clinical, "Mod1id", "id")
# clinical <- str_replace(clinical, "DCIQuintile", "exposure")

# inclusion criteria: select observations starting at discharge
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
                      filter(
                        FollowUpPeriod > -1 # Injury date is -1
                      )
  ))

# exclusion criteria: COVID is a possible confounder, use outcome Status Date to exclude
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  filter(
    Date <= as.Date("2019-12-31") # last date (status)
  )
  ))

# # exclusion criteria: before 2020
# Nobs_incl_per <- analytical %>% nrow()

# inclusion criteria: up to 10yr of follow up
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  filter(
    FollowUpPeriod <= 10,
  )
  ))

# keep multiple observations before exclusion criteria: mult_*
analytical2 <- analytical

# rename datasets
analytical[1:3, 1] <- str_c("sing_", pull(analytical[1:3, 1]))
analytical2[1:3, 1] <- str_c("mult_", pull(analytical2[1:3, 1]))

# # use time-varying SES in mult_*
# analytical2 <- analytical2 %>%
#   mutate(data = map(data, ~.x %>%
#                       mutate(
#                         exposure = exposure_last,
#                       )
#   ))

# exclusion criteria: redundant participant observations: pick last date of follow up
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  group_by(id) %>%
  filter(
    FollowUpPeriod == max(FollowUpPeriod, na.rm = TRUE),
  ) %>%
  ungroup()
  ))

# single observation dataset uses exposure at discharge
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
                      mutate(
                        # reproduce original analysis: constant exposure, SES at baseline (discharge)
                        exposure = DCIQuintile_Dis,
                      )
  ))

analytical <- bind_rows(analytical, analytical2)
rm(analytical2)

# Relevel dataset factor in semantic order
analytical <- analytical %>%
  mutate(
    dataset = factor(dataset),
    dataset = fct_shift(dataset, n = 3),
  )

# # inclusion criteria: 10yr follow up + unique IDs
# Nobs_incl_id <- analytical %>% nrow()

# inclusion criteria: study period
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  filter(
    between(RehabDis, study_period[1], study_period[2]), # discharge date
  )
  ))

# inclusion criteria: valid times
analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  filter(Time>=0)
  ))

# # remove invalid observations (outcome at time 0 or below)
# Nobs_invalid <- analytical %>% nrow()

# data wrangling ----------------------------------------------------------

analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  mutate(
    id = as.character(id), # or as.factor
    # label SES quintiles
    exposure = factor(exposure, labels = c("Prosperous", "Comfortable", "Mid-Tier", "At-Risk", "Distressed")),
    across(starts_with("exposure"), ~ factor(.x, labels = c("Prosperous", "Comfortable", "Mid-Tier", "At-Risk", "Distressed"))),
    # convert Time to years
    Time = Time/dyears(1),
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
  ))

# labels ------------------------------------------------------------------

labs <- list(
  labs,
  exposure = "SES quintiles",
  outcome = "Mortality",
  Time = "Time of follow up (years)",
  Date = "Date of last follow up",
  FIMMOTD4 = labs$FIMMOTD,
  FIMCOGD4 = labs$FIMCOGD
)

analytical <- analytical %>%
  mutate(data = map(data, ~.x %>%
  set_variable_labels(
    # restore original labels - intersect is used to get only valid colnames
    .labels = labs[intersect(colnames(.x), names(labs))],
  )
  ))

# analytical dataset ------------------------------------------------------

analytical <- analytical %>%
  mutate(analytical = map(data, ~.x %>%
  # select analytic variables
  select(
    id,
    exposure,
    outcome,
    # Date,
    Time,
    everything(),
    -starts_with("Zip"),
    -starts_with("DCI"),
    -where(is.Date),
    -IntStatus,
    # -FollowUpPeriod,
    # -Time_d,
    # -FIMMOTF,
    # -FIMCOGF,
  )
  ))

Nvar_final <- analytical %>% unnest(analytical) %>% ungroup() %>% filter(dataset=="sing_cc") %>% select(-dataset) %>% ncol
Nobs_final <- analytical %>% unnest(analytical) %>% ungroup() %>% filter(dataset=="sing_cc") %>% select(-dataset) %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical$analytical[[1]] %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")

# model data --------------------------------------------------------------

analytical <- analytical %>%
  mutate(md = map(analytical, ~
                    .x %>%
                    select(
                      -PriorSeiz,
                      # -exposure_Inj,
                      # -exposure_Dis,
                      # -exposure_last,
                    ) %>%
                    drop_na()
  ))
