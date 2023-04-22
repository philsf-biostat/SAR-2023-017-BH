# setup -------------------------------------------------------------------
# library(infer)

# tables ------------------------------------------------------------------

# template p-value table
tab_inf <- analytical %>%
  tbl_summary(
    include = -id,
    by = exposure,
  ) %>%
  # include study N
  add_overall() %>%
  # bring home the bacon! (options: add_p or add_difference)
  # add_p: quick and dirty
  add_p(
    # use Fisher test (defaults to chi-square)
    test = all_categorical() ~ "fisher.test",
    # use 3 digits in pvalue
    pvalue_fun = function(x) style_pvalue(x, digits = 3),
  ) %>%
  # # diff: alternative to add_p
  #   add_difference(
  #     test = all_categorical() ~ "fisher.test",
  #     # use 3 digits in pvalue
  #     pvalue_fun = function(x) style_pvalue(x, digits = 3),
  #     # ANCOVA
  #     # adj.vars = c(sex, age, bmi),
  #   ) %>%
  # # EN
  # modify_footnote(update = c(estimate, ci, p.value) ~ "ANCOVA (adjusted by sex, age and BMI)") %>%
  # # PT
  # modify_header(estimate ~ '**Diferença ajustada**') %>%
  # modify_footnote(update = c(estimate, ci, p.value) ~ "ANCOVA (ajustada por sexo, idade e IMC)") %>%
  # pretty format categorical variables
  bold_labels() %>%
  modify_table_styling(columns = "label", align = "center") %>%
  # bold significant p values
  bold_p()

# Template Cohen's D table (obs: does NOT compute p)
# tab_inf <- analytical %>%
#   # select
#   select(
#     -id,
#   ) %>%
#   tbl_summary(
#     by = exposure,
#   ) %>%
#   add_difference(
#     test = all_continuous() ~ "cohens_d",
#     # ANCOVA
#     adj.vars = c(sex, age, bmi),
#   ) %>%
#   modify_header(estimate ~ '**d**') %>%
#   bold_labels()
