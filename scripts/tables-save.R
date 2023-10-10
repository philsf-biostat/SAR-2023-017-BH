# this script creates the tables from describe.R and inference.R, an saves them to disk, for optimum performance

tab <- function(data) {
  data %>%
    tbl_summary(
      by = dataset,
      statistic = outcome ~ "{n}",
      # statistic = all_categorical() ~ "{n}",
      missing_text = "Missing",
    ) %>%
    modify_footnote(
      stat_1 ~ cc,
      stat_2 ~ locf,
      stat_3 ~ nocb,
      abbreviation = TRUE,
    )
  }

# Table 1 -----------------------------------------------------------------

temp <- analytical %>%
  select(dataset, analytical) %>%
  unnest(analytical) %>%
  ungroup() %>%
  separate(dataset, c("obs", "dataset"), sep = "_") %>%
  mutate(dataset = str_to_upper(dataset)) %>%
  select(obs, dataset, outcome, exposure)

tab_desc <- list(
  temp %>% filter(obs=="sing") %>% select(-obs) %>% tab(), # sing
  temp %>% filter(obs=="mult") %>% select(-obs) %>% tab()  # mult
) %>%
  tbl_merge(tab_spanner = c(
    sing,
    mult
  ))

tab_desc %>% write_rds("dataset/tab_desc_017.rds")

# Table 2 -----------------------------------------------------------------


mod <- mod %>%
  # select(model:summ) %>%
  mutate(tab = map(model, ~
                     tbl_regression(.x,
                                    exp = TRUE,
                                    intercept = TRUE,
                                    include=exposure,
                                    # conf.int = FALSE,
                     ) %>% bold_labels() %>% bold_p(),
  ))

# merged table
tab_inf <- mod %>%
  pull(tab) %>%
  tbl_merge(tab_spanner = c(
    "**Single CC***",
    # "**Single LOCF***",
    # "**Single LOCF+NOCB***",
    # "**Multiple CC**",
    "**Multiple LOCF**"
    # , "**Multiple LOCF+NOCB**"
  )) %>%
  modify_footnote(
    # c(estimate_1, estimate_2, estimate_3) ~ note_fim,
    estimate_1 ~ note_fim,
  ) %>%
  modify_footnote(
    # c(estimate_1, estimate_4) ~ cc,
    # c(estimate_2, estimate_5) ~ locf,
    # c(estimate_3, estimate_6) ~ nocb,
    estimate_1 ~ cc,
    estimate_2 ~ locf,
    abbreviation = TRUE,
  )

# # stacked table
# tab_sing <- mod %>%
#   filter(obs == "sing") %>%
#   pull(tab) %>%
#   tbl_merge(tab_spanner = c("CC", "LOCF", "LOCF+NOCB")) %>%
#   modify_footnote(
#     # c(estimate_1, estimate_2, estimate_3) ~ note_fim,
#     c(p.value_1, p.value_2, p.value_3) ~ note_fim, # remember to edit this out in final DOCX!
#   ) %>%
#   modify_footnote(
#     estimate_1 ~ cc,
#     estimate_2 ~ locf,
#     estimate_3 ~ nocb,
#     abbreviation = TRUE,
#   )
# tab_mult <- mod %>%
#   filter(obs == "mult") %>%
#   pull(tab) %>%
#   tbl_merge(tab_spanner = c("CC", "LOCF", "LOCF+NOCB")) %>%
#   modify_footnote(
#     estimate_1 ~ cc,
#     estimate_2 ~ locf,
#     estimate_3 ~ nocb,
#     abbreviation = TRUE,
#   )
#
# tab_inf2 <- list(tab_sing, tab_mult) %>%
#   tbl_stack(
#     group_header = c(paste(sing, "*"), mult)
#   )
# # (tab_spanner = c("**cs_cc***", "**cs_locf***", "**cs_locf+nocb***", "**tv_cc**", "**tv_locf**", "**tv_locf+nocb**")

tab_inf %>% write_rds("dataset/tab_inf_017.rds")

