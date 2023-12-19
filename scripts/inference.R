# setup -------------------------------------------------------------------
# library(infer)

# tables ------------------------------------------------------------------

note_fim <- "\\* FIM scores violate PH assumption"

# table is constructed in tables-save and loaded here
tab_inf <- read_rds("dataset/tab_inf_017.rds")

# Schoenfeld test ---------------------------------------------------------

sch.df <- mod %>%
  transmute(dataset, ph = map(res,
                             ~ .x$table %>%
                               as.data.frame() %>%
                               rownames_to_column("term") %>%
                               mutate(p = style_pvalue(p))
  )) %>%
  unnest(ph) %>%
  select(dataset, term, p) %>%
  pivot_wider(names_from = term, values_from = p) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame()
colnames(sch.df) <- sch.df[1, ]
sch.df <- sch.df[-1, ]
sch.df <- sch.df %>% rownames_to_column("term")
