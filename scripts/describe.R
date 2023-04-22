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

# tables ------------------------------------------------------------------

tab_desc <- analytical %>%
  tbl_summary(
    include = -id,
    # by = exposure,
  ) %>%
  # modify_caption(caption = "**Tabela 1** Características demográficas") %>%
  # modify_header(label ~ "**Características dos pacientes**") %>%
  # pretty format categorical variables
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "center")
