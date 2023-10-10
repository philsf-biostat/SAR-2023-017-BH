# setup -------------------------------------------------------------------

# library(Hmisc) # describe
# library(skimr) # skim
# library(gmodels) # CrossTable
library(gtsummary)
library(gt)
# library(effectsize)
# library(finalfit) # missing_compare

# setup gtsummary theme

theme_ff_gtsummary()
theme_gtsummary_compact()
# theme_gtsummary_language(language = "pt") # traduzir

# exploratory -------------------------------------------------------------

# overall description
# analytical %>% skimr::skim()

# minimum detectable effect size
# interpret_cohens_d(0.5)
# cohens_d(outcome ~ exposure, data = analytical) %>% interpret_cohens_d()
# interpret_icc(0.7)

analytical %>% unnest(data) %>% select(exposure) %>% miss_var_summary()

analytical %>% unnest(analytical) %>% distinct(id) %>% count()

lost <- analytical %>% unnest(md) %>% distinct(id) %>% summarise(n = n()-7409)

# tables ------------------------------------------------------------------

sing <- "Single observation per individual"
mult <- "Multiple observations per individual"
cc <- "CC = complete-cases (not imputed)"
locf <- "LOCF = last observation carried forward"
nocb <- "NOCB = next observation carried backward"

# table is constructed in tables-save and loaded here
tab_desc <- read_rds("dataset/tab_desc_017.rds")

# number of deaths
deaths <- analytical %>%
  unnest(md) %>%
  filter(outcome==1) %>%
  count(outcome) %>%
  select(-outcome) %>%
  ungroup()
