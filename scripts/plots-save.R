# setup -------------------------------------------------------------------
height <- 12
width <- 12
units <- "cm"

# publication ready tables ------------------------------------------------

# Don't need to version these files on git
# tab_inf %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2023-017-BH-v01-T2.rtf")

# save plots --------------------------------------------------------------

ggsave(filename = "figures/outcome.png", plot = gg.outcome, height = height, width = width, units = units)
ggsave(filename = "figures/distr_age.png", plot = gg.age, height = height, width = width, units = units)
