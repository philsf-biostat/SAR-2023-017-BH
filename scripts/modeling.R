# setup -------------------------------------------------------------------
# library(moderndive)
library(broom)
# library(lmerTest)
# library(broom.mixed)
# library(simputation)
# library(mice)
library(survival)

# model coefficients from SAR-2023-016
# update(mod.social.geo.clinical, . ~ . + FIMMOTD4 + FIMCOGD4) %>% tidy() %>% dput()
model_16 <- structure(list(
  term = c("exposureComfortable", "exposureMid-Tier", "exposureAt-Risk", "exposureDistressed", "SexFFemale", "RaceBlack", "RaceHispanic", "RaceOther", "AGE", "EDUCATIONLess Than High School", "EDUCATIONHigh School/GED", "EMPLOYMENTUnemployed", "EMPLOYMENTOther", "ResDisOther", "RURALdcRural", "RURALdcUrban", "RehabPay1Public Insurance", "RehabPay1Other", "SCI", "PROBLEMUse", "FIMMOTD4Q2", "FIMMOTD4Q3", "FIMMOTD4Q4", "FIMCOGD4Q2", "FIMCOGD4Q3", "FIMCOGD4Q4"),
  estimate = c(-0.00777706175480185, 0.0926488999790922, 0.111418189475191, 0.203165769712993, -0.361083267782009, -0.212382922290373, -0.512761278373466, -0.34227050437073, 0.0383490595642642, 0.147758153368633, 0.307764691121985, 0.561763002430481, 0.601989751531276, 0.357012680579183, 0.0355581507963198, 0.120743010427145, 0.332995498091183, 0.10279106170517, 0.257385993549379, 0.245203871483331, -0.386588188688364, -0.421618486785988, -0.587966852134675, -0.171887564649647, -0.197749522981139, -0.430771178633132),
  std.error = c(0.121312204762617, 0.130446285730059, 0.126746719391713, 0.127796207056313, 0.0893475223934483, 0.119808628213012, 0.167832954126442, 0.2077503083739, 0.00263631231948754, 0.115891235315523, 0.0885897546633758, 0.1422158498437, 0.105616902249082, 0.0885275682682966, 0.115951096030979, 0.0961988447205793, 0.0949963558016463, 0.185114761933753, 0.174954727725915, 0.0938421494309754, 0.100501089278951, 0.119311549049286, 0.139911102146464, 0.104141030354517, 0.110566428486197, 0.127166481115921),
  statistic = c(-0.064107826331406, 0.71024559618981, 0.879061722543297, 1.58976369011851, -4.04133498175756, -1.77268470107821, -3.0551883034077, -1.64750900756656, 14.5464781546515, 1.27497263245447, 3.47404383601052, 3.95007309697109, 5.69974822885422, 4.03278535221029, 0.306665068407976, 1.25513992166805, 3.50535023455512, 0.55528289927497, 1.47115769259234, 2.6129396328852, -3.84660695184456, -3.53376089863542, -4.20243170923754, -1.65052682947833, -1.78851325568345, -3.38745850992334),
  p.value = c(0.948884370155935, 0.47755185021006, 0.37936781049767, 0.111888081282029, 5.3147774429833e-05, 0.0762809675788584, 0.00224919257739181, 0.0994534663024795, 6.14881588629178e-48, 0.202318928861904, 0.000512677269587384, 7.81273295428896e-05, 1.19984497544939e-08, 5.51196222369424e-05, 0.75909832079322, 0.20942797039188, 0.000456006976506958, 0.57870117378935, 0.141248475725221, 0.00897671564025996, 0.000119764864294495, 0.000409691520248712, 2.64062924160004e-05, 0.0988352305378624, 0.0736932389666159, 0.000705433860069405)),
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -26L))

# raw estimate ------------------------------------------------------------

f.crude <- Surv(Time, outcome) ~ exposure

# adjusted ----------------------------------------------------------------

f.final_16 <- Surv(Time, outcome) ~ exposure + SexF + Race + AGE + EDUCATION +
  EMPLOYMENT + ResDis + RURALdc + RehabPay1 + SCI + PROBLEMUse +
  FIMMOTD4 + FIMCOGD4

# many models approach (LCW) ----------------------------------------------

# mod_data <- bind_rows(
#   cc = data.raw,
#   locf = data.raw %>% group_by(id) %>% fill(exposure) %>% ungroup(),
#   .id = "dataset") %>%
#   group_by(dataset) %>%
#   nest()

mod <- analytical %>%
  # filter(dataset != "mult_cc") %>% # remove mult_cc: not enough outcomes due to not collecting Zip at deaths
  filter(dataset %in% c("sing_cc", "mult_locf")) %>%
  separate(dataset, c("obs", "dataset"), sep = "_") %>%
  # mutate(model = map(md, ~ coxph(f.final_16, data = .x))) %>%
  # mutate(model = map(md, ~ coxph(f.final_16, data = .x, id = id))) %>%
  mutate(model = map(md, ~ coxph(Surv(Time, outcome) ~ exposure + SexF + Race + AGE + EDUCATION + EMPLOYMENT + RehabPay1 + SCI + DAYStoREHABdc + PROBLEMUse + ResDis + RURALdc + FIMMOTD4 + FIMCOGD4, data = .x, id = id))) %>%
  mutate(coef = map(model, tidy)) %>%
  mutate(res = map(model, cox.zph)) %>%
  mutate(summ = map(model, glance)) %>%
  select(-data, -analytical, -md) # drop data from models object

# test original "final" model ---------------------------------------------

# testthat::expect_equal(
#   mod %>%
#     filter(obs=="sing", dataset=="cc") %>%
#     unnest(coef) %>%
#     # filter(str_detect(term, "^exposure")) %>%
#     # mutate(estimate = exp(estimate)) %>%
#     ungroup() %>%
#     arrange(term) %>%
#     select(term:p.value, -dataset),
#   model_16 %>% arrange(term))
