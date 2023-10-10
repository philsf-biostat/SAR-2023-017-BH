# setup -------------------------------------------------------------------
# library(moderndive)
library(broom)
# library(lmerTest)
# library(broom.mixed)
# library(simputation)
# library(mice)
library(survival)

# model coefficients from SAR-2023-016
model_16 <- structure(list(
  term = c("exposureComfortable", "exposureMid-Tier", "exposureAt-Risk", "exposureDistressed", "SexFFemale", "RaceBlack", "RaceHispanic", "RaceOther", "AGE", "EDUCATIONLess Than High School", "EDUCATIONHigh School/GED", "EMPLOYMENTUnemployed", "EMPLOYMENTOther", "RehabPay1Public Insurance", "RehabPay1Other", "SCI", "PROBLEMUse", "FIMMOTD4Q2", "FIMMOTD4Q3", "FIMMOTD4Q4", "FIMCOGD4Q2", "FIMCOGD4Q3", "FIMCOGD4Q4", "ResDisOther", "RURALdcRural", "RURALdcUrban"),
  estimate = c(0.992940739213542, 1.11305273260294, 1.13472784908265, 1.23786157278012, 0.703379049043012, 0.821286718964551, 0.585826290154214, 0.712248160875171, 1.03419467558036, 1.20312166043624, 1.36194286922328, 1.67536752997069, 1.75014538778394, 1.32154797658208, 1.15331897541019, 1.38245157415139, 1.26393672444356, 0.667608159200727, 0.637175680406797, 0.540720314102421, 0.850013425321032, 0.836500473512451, 0.666842929311413, 1.39578905925968, 1.04759085700639, 1.06937936751564),
  std.error = c(0.121748381516957, 0.131017942633534, 0.127939627476899, 0.129247035628928, 0.0895316525942851, 0.121892794675243, 0.169167733644265, 0.207626851385504, 0.00289219826337649, 0.116128069582823, 0.088984866677644, 0.143536088598769, 0.105673181132325, 0.0959624321549732, 0.185531788048793, 0.175864767629003, 0.0945004405844065, 0.100769220189462, 0.120191803214081, 0.14057740515454, 0.104100861150974, 0.111047946380275, 0.12786690634643, 0.0886916050863731, 0.116268379204629, 0.0972046308497822), statistic = c(-0.0581880035343513, 0.817494518818241, 0.98791003327187, 1.65098837099374, -3.92999944106428, -1.61521441558078, -3.16095720510462, -1.63432082833669, 11.6254245244166, 1.59237610177415, 3.47151456357405, 3.59514159207929, 5.29650813215694, 2.90534277615226, 0.768837797327808, 1.84151965415535, 2.47862584817009, -4.00969527022967, -3.74992184129174, -4.37376911757379, -1.56101624261784, -1.60766766348081, -3.16892588289544, 3.75976834465845, 0.399877476315683, 0.690074631686849),
  p.value = c(0.953598876324771, 0.41364588036885, 0.323196711222857, 0.0987409494758209, 8.49460590401823e-05, 0.106264269825654, 0.00157251600143109, 0.102191502493946, 3.06068068199501e-31, 0.111300222674474, 0.000517531258756882, 0.000324215195736847, 1.18038084137154e-07, 0.00366851102629947, 0.441989607529208, 0.0655454430983082, 0.0131889574810452, 6.07971577647098e-05, 0.000176889695325723, 1.22119534802501e-05, 0.118519918534483, 0.107907984309348, 0.00153003406860897, 0.000170070766408068, 0.689246762619386, 0.490147255427147)),
  row.names = c(NA, -26L), class = c("tbl_df", "tbl", "data.frame"))

# raw estimate ------------------------------------------------------------

f.crude <- Surv(Time, outcome) ~ exposure

# adjusted ----------------------------------------------------------------

f.final_16 <- Surv(Time, outcome) ~ exposure + SexF + Race + AGE + EDUCATION + 
  EMPLOYMENT + strata(Cause) + RehabPay1 + SCI + PROBLEMUse +
  FIMMOTD4 + FIMCOGD4 + ResDis + RURALdc

# many models approach (LCW) ----------------------------------------------

# mod_data <- bind_rows(
#   cc = data.raw,
#   locf = data.raw %>% group_by(id) %>% fill(exposure) %>% ungroup(),
#   .id = "dataset") %>%
#   group_by(dataset) %>%
#   nest()

analytical <- analytical %>%
  # mutate(model = map(md, ~ coxph(f.final_16, data = .x))) %>%
  # mutate(model = map(md, ~ coxph(f.final_16, data = .x, id = id))) %>%
  mutate(model = map(md, ~ coxph(Surv(Time, outcome) ~ exposure + SexF + Race + AGE + EDUCATION + EMPLOYMENT + strata(Cause) + RehabPay1 + SCI + PROBLEMUse + FIMMOTD4 + FIMCOGD4 + ResDis + RURALdc, data = .x, id = id))) %>%
  mutate(coef = map(model, tidy)) %>%
  mutate(res = map(model, cox.zph)) %>%
  mutate(summ = map(model, glance))

# test original "final" model ---------------------------------------------

testthat::expect_equal(
  analytical %>%
    filter(dataset=="cc") %>%
    unnest(coef) %>%
    # filter(str_detect(term, "^exposure")) %>%
    mutate(estimate = exp(estimate)) %>%
    ungroup() %>%
    select(term:p.value, -dataset),
  model_16)
