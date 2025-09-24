## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| label: fig-pso-network-plot
#| echo: false
#| message: false
#| fig-cap: "Network of studies comparing treatments for moderate-to-severe plaque psoriasis."

library(multinma)
options(mc.cores = parallel::detectCores())

library(dplyr)

pso_ipd <- mutate(plaque_psoriasis_ipd,
                  trtclass = case_when(trtn == 1 ~ "Placebo",
                                          trtn %in% c(2, 3, 5, 6) ~ "IL-17 blocker",
                                          trtn == 4 ~ "TNFa blocker",
                                          trtn == 7 ~ "IL-12/23 blocker"))

pso_agd <- mutate(plaque_psoriasis_agd,
                  trtclass = case_when(trtn == 1 ~ "Placebo",
                                          trtn %in% c(2, 3, 5, 6) ~ "IL-17 blocker",
                                          trtn == 4 ~ "TNFa blocker",
                                          trtn == 7 ~ "IL-12/23 blocker"))

pso_net <- combine_network(
  set_ipd(pso_ipd,
    study = studyc,
    trt = trtc,
    r = pasi75,
    trt_class = trtclass),
  set_agd_arm(pso_agd,
    study = studyc,
    trt = trtc,
    r = pasi75_r,
    n = pasi75_n,
    trt_class = trtclass)
)

# Network plot
plot(pso_net, weight_nodes = TRUE, weight_edges = TRUE, show_trt_class = TRUE) +
  ggplot2::guides(size = ggplot2::guide_legend(ncol = 4),
                  fill = ggplot2::guide_legend(ncol = 2, override.aes = list(size = 3)),
                  edge_color = ggplot2::guide_legend(ncol = 2, override.aes = list(edge_width = 2)),
                  edge_width = ggplot2::guide_legend(ncol = 4)
                  ) +
  ggplot2::theme(legend.margin = ggplot2::margin(0, 1, 0, 1, "lines"),
                 plot.margin = ggplot2::margin(0, 0, 0, 3, "lines"))



## ----pso-inspect-data, warning=FALSE------------------------------------------------------------------------------------------------------
# Load the multinma package to access the data
library(multinma)

# Inspect the datasets
head(plaque_psoriasis_ipd)

head(plaque_psoriasis_agd)


## .widetable {
##   width: 150%;
##   overflow-x: auto;
##   white-space: nowrap;
## }

## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| echo: false
library(tidyr)

# Create table of baseline characteristics by study
f_meansd <- function(x, digits = 1, format = "f", ...) {
  m <- formatC(mean(x), digits = digits, format = format, ...)
  s <- formatC(sd(x), digits = digits, format = format, ...)
  paste0(m, " (", s, ")")
}

f_prop <- function(x, digits = 1, format = "f", ...) {
  formatC(mean(x) * 100, digits = digits, format = "f", ...)
}

f_wt_meansd <- function(m, s, weight, digits = 1, format = "f", ...) {
  mbar <- weighted.mean(m, weight)
  sbar <- sqrt(weighted.mean(s^2 + m^2, weight) - mbar^2)
  mc <- formatC(mbar, digits = digits, format = format, ...)
  sc <- formatC(sbar, digits = digits, format = format, ...)
  paste0(mc, " (", sc, ")")
}

f_wt_prop <- function(p, weight, digits = 1, format = "f", ...) {
  formatC(weighted.mean(p, weight), digits = digits, format = format, ...)
}

get_ag_meansd <- function(data, x, weight = "sample_size_w0", digits = 1, format = "f", ...) {
  data %>% select(m = paste0(x, "_mean"), s = paste0(x, "_sd"), w = !! weight) %>%
    # summarise(m1 = m %*% w / sum(w), s1 = s %*% w / sum(w), 
    #           mc = formatC(m1, digits = digits, format = format, ...),
    #           sc = formatC(s1, digits = digits, format = format, ...),
    #           txt = paste0(mc, " (", sc, ")")) %>%
    summarise(txt = f_wt_meansd(m, s, w, digits = digits, format = format, ...)) %>%
    transmute(vars = x, txt)
}

get_ag_prop <- function(data, x, weight = "sample_size_w0", digits = 1, format = "f", ...) {
  data %>% select(p = !! x, w = !! weight) %>%
    # summarise(p1 = p %*% w / sum(w),
    #           txt = formatC(p1, digits = digits, format = format, ...)) %>%
    summarise(txt = f_wt_prop(p, w, digits = digits, format = format, ...)) %>%
    transmute(vars = x, txt)
}

# Get complete cases
pso_ipd <- plaque_psoriasis_ipd %>% 
  select(studyc, trtc, trtc_long, age, male, weight, durnpso, pasi_w0, prevsys, bsa, psa) %>%
  dplyr::filter(complete.cases(.))

# Calculate sample sizes
ssize <- pso_ipd %>% 
  group_by(studyc) %>% 
  summarise(N = n()) %>%
  bind_rows(
    plaque_psoriasis_agd %>% group_by(studyc) %>% summarise(N = sum(sample_size_w0))
  ) %>% 
  mutate(N = ifelse(studyc %in% c("UNCOVER-1", "UNCOVER-2", "UNCOVER-3", "IXORA-S"), paste0("(IPD N = ", N, ")"), paste0("(AgD N = ", N, ")"))) %>%
  spread(studyc, N) %>%
  mutate(#"Chiricozzi 2019" = "(AgD N = 330)",
        PROSPECT = "(AgD N = 1509)",
        #PsoBest = "(AgD N = 2556)"
        )

trt_labels <- c(
  "Placebo",
  "Etanercept",
  "Ixekizumab Q2W",
  "Ixekizumab Q4W",
  "Secukinumab 150 mg",
  "Secukinumab 300 mg",
  "Ustekinumab")

tab=bind_rows(
  pso_ipd %>% 
    group_by(studyc) %>%
    do({rbind(
      tibble(vars = "_Treatments", txt = paste(sort(unique(factor(.$trtc_long, levels = trt_labels))), collapse = "<br>")),
      gather(., vars, value, age, weight, durnpso, pasi_w0, bsa) %>%
        group_by(vars) %>%
        summarise(txt = f_meansd(value)),
      gather(., vars, value, male, prevsys, psa) %>%
        group_by(vars) %>%
        summarise(txt = f_prop(value))
        )}),
  plaque_psoriasis_agd %>% 
    group_by(studyc) %>% 
    do({bind_rows(
      tibble(vars = "_Treatments", txt = paste(sort(unique(factor(.$trtc_long, levels = trt_labels))), collapse = "<br>")),
      get_ag_meansd(., "age"),
      get_ag_meansd(., "weight"),
      get_ag_meansd(., "durnpso"),
      get_ag_meansd(., "pasi_w0"),
      get_ag_meansd(., "bsa"),
      get_ag_prop(., "male"),
      get_ag_prop(., "prevsys"),
      get_ag_prop(., "psa")
    )}),
  # Registry target population
  read.csv(here::here("data/registry_summaries.csv")) %>%
    dplyr::filter(study == "PROSPECT") %>%
    rowwise() %>% 
    transmute(studyc = study, vars = covariate, 
              txt = if_else(is.na(sd), f_wt_prop(mean * 100, 1), f_wt_meansd(mean, sd, 1))) %>%
    add_row(studyc = "PROSPECT", vars = "_Treatments", txt = "Secukinumab 300 mg")
) %>%
  spread(studyc, txt) %>%
  # Put PROSPECT target population on RHS
  relocate(PROSPECT, .after = last_col()) %>%
  # Add EM indicators
  # mutate(isEM = if_else(vars %in% c("durnpso", "prevsys", "weight", "bsa", "psa"), "*", "")) %>%
  # Make nice row labels
  mutate(vars = recode(vars,
    "_Treatments" = "Treatment arms",
    male = "Male (%)",
    age = "Age, years",
    durnpso = "Duration of psoriasis, years",
    pasi_w0 = "Baseline PASI score",
    prevsys = "Previous systemic treatment (%)",
    weight = "Weight, kg",
    bsa = "Body surface area, per cent",
    psa = "Psoriatic arthritis (%)"
  )) %>%
  # Add sample sizes
  add_row(!!! ssize, .after = 0) %>%
  # select(isEM, vars, everything()) %>%
  select(vars, everything()) %>%
  # Remove row headings for EM indicators and labels
  # setNames(c("", "", paste(names(.)[-(1:2)], .[1, -(1:2)], sep = "<br>"))) %>%
  # {.[-1, ]} %>%
  setNames(c("", paste(names(.)[-1], .[1, -1], sep = "<br>"))) %>%
  {.[-1, ]} 
### tab |> tinytable::tt() |> print("markdown")

# trials=colnames(tab)[-1]
# tab=tab %>% t()  
# colnames(tab)=tab[1,]
# tab=tab[-1,] |> as_tibble() |> mutate(Trial=trials) |> select(Trial,everything())
# tab |> tinytable::tt()

# Output to table
knitr::kable(tab[,1:6],escape = FALSE, table.attr = "class = \'widetable\'", format = "html")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
knitr::kable(tab[,c(1,7:11)],escape = FALSE, table.attr = "class = \'widetable\'", format = "html")


## ----maic-prep-data, warning=FALSE--------------------------------------------------------------------------------------------------------
# Select relevant rows
fixture_agd <- subset(
  plaque_psoriasis_agd,studyc == "FIXTURE" & trtc %in% c("ETN", "SEC_300")
)

uncover_ipd <- subset(
  plaque_psoriasis_ipd,
  studyc %in% c("UNCOVER-2", "UNCOVER-3") & trtc %in% c("ETN", "IXE_Q2W")
)

# Check for missing data in IPD
all(complete.cases(
  uncover_ipd[, c("pasi75", "durnpso", "prevsys", "bsa", "weight", "psa")])
)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
# FIXTURE AgD covariate summaries (first and second moments)
X_agd <- with(fixture_agd,
              cbind(durnpso_mean, durnpso_mean^2 + durnpso_sd^2,
                    prevsys / 100,
                    bsa_mean, bsa_mean^2 + bsa_sd^2,
                    weight_mean, weight_mean^2 + weight_sd^2,
                    psa / 100))

# We were given summaries by arm, so convert these to overall summaries
X_agd <- apply(
  X_agd, MARGIN = 2, FUN = weighted.mean, w = fixture_agd$sample_size_w0
)

# Create matrix of centred UNCOVER IPD covariates
X_ipd <- sweep(with(uncover_ipd, 
                    cbind(durnpso, durnpso^2,
                          prevsys,
                          bsa, bsa^2,
                          weight, weight^2,
                          psa)), 
               MARGIN = 2, STATS = X_agd, FUN = '-')


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
# Objective function
objfn <- function(a1, X){
  sum(exp(X %*% a1))
}

# Gradient function
gradfn <- function(a1, X){
  colSums(sweep(X, 1, exp(X %*% a1), "*"))
}


## ----maic-optim, warning=FALSE------------------------------------------------------------------------------------------------------------
study_ipd <- uncover_ipd$studyc

# Estimate weights for UNCOVER-2
opt_uncover2 <- optim(par = rep(0, ncol(X_ipd)), 
                      fn = objfn, gr = gradfn, 
                      X = X_ipd[study_ipd == "UNCOVER-2", ], 
                      method = "BFGS")

a1_uncover2 <- opt_uncover2$par
wt_uncover2 <- exp(X_ipd[study_ipd == "UNCOVER-2", ] %*% a1_uncover2)

# Normalise to sum to ESS for interpretation
wt_uncover2 <- wt_uncover2 * sum(wt_uncover2) / sum(wt_uncover2^2)

# Estimate weights for UNCOVER-3
opt_uncover3 <- optim(par = rep(0, ncol(X_ipd)), 
                      fn = objfn, gr = gradfn, 
                      X = X_ipd[study_ipd == "UNCOVER-3", ], 
                      method = "BFGS")

a1_uncover3 <- opt_uncover3$par
wt_uncover3 <- exp(X_ipd[study_ipd == "UNCOVER-3", ] %*% a1_uncover3)

# Normalise to sum to ESS for interpretation
wt_uncover3 <- wt_uncover3 * sum(wt_uncover3) / sum(wt_uncover3^2)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| label: fig-maic-histogram
#| fig-cap: "Histograms of MAIC weights in each IPD study. For interpretation, weights were normalised to sum to the effective sample size."
#| fig-subcap:
#|   - "UNCOVER-2"
#|   - "UNCOVER-3"
#| layout-ncol: 2
hist(wt_uncover2, freq = FALSE, breaks = 50, main = NULL, xlab = "Weight")
hist(wt_uncover3, freq = FALSE, breaks = 50, main = NULL, xlab = "Weight")

# Effective sample sizes
sum(wt_uncover2)^2 / sum(wt_uncover2^2)
sum(wt_uncover3)^2 / sum(wt_uncover3^2)


## ----include = FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------
# Check that the reweighted samples do indeed match the AgD summary statistics
with(uncover_ipd[study_ipd == "UNCOVER-2",],
  c(durnpso_mean = weighted.mean(durnpso, wt_uncover2), 
    durnpso_sd = sqrt(weighted.mean((durnpso - weighted.mean(durnpso, wt_uncover2))^2, wt_uncover2)), 
    prevsys = weighted.mean(prevsys, wt_uncover2),
    bsa_mean = weighted.mean(bsa, wt_uncover2), 
    bsa_sd = sqrt(weighted.mean((bsa - weighted.mean(bsa, wt_uncover2))^2, wt_uncover2)),
    weight_mean = weighted.mean(weight, wt_uncover2), 
    weight_sd = sqrt(weighted.mean((weight - weighted.mean(weight, wt_uncover2))^2, wt_uncover2)),
    psa = weighted.mean(psa, wt_uncover2)
  )
)

with(uncover_ipd[study_ipd == "UNCOVER-3",],
  c(durnpso_mean = weighted.mean(durnpso, wt_uncover3), 
    durnpso_sd = sqrt(weighted.mean((durnpso - weighted.mean(durnpso, wt_uncover3))^2, wt_uncover3)), 
    prevsys = weighted.mean(prevsys, wt_uncover3),
    bsa_mean = weighted.mean(bsa, wt_uncover3), 
    bsa_sd = sqrt(weighted.mean((bsa - weighted.mean(bsa, wt_uncover3))^2, wt_uncover3)),
    weight_mean = weighted.mean(weight, wt_uncover3), 
    weight_sd = sqrt(weighted.mean((weight - weighted.mean(weight, wt_uncover3))^2, wt_uncover3)),
    psa = weighted.mean(psa, wt_uncover3)
  )
)

fixture_agd[, c("durnpso_mean", "durnpso_sd", "prevsys", "bsa_mean", "bsa_sd", 
                "weight_mean", "weight_sd", "psa")]


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
# Load the sandwich package for robust standard errors
library(sandwich)

# IXE Q2W vs. ETN from UNCOVER-2
fit_uncover2 <- glm(
  cbind(pasi75, 1 - pasi75) ~ trtc,
  data = uncover_ipd[study_ipd == "UNCOVER-2",],
  family = binomial(link = "probit"),
  weights = wt_uncover2
)

# probit difference
d_uncover2 <- coef(fit_uncover2)["trtcIXE_Q2W"]                     
# sandwich variance
var_uncover2 <- vcovHC(fit_uncover2)["trtcIXE_Q2W", "trtcIXE_Q2W"]  

# IXE Q2W vs. ETN from UNCOVER-3
fit_uncover3 <- glm(cbind(pasi75, 1 - pasi75) ~ trtc,
                    data = uncover_ipd[study_ipd == "UNCOVER-3",],
                    family = binomial(link = "probit"),
                    weights = wt_uncover3)

# probit difference
d_uncover3 <- coef(fit_uncover3)[["trtcIXE_Q2W"]]                   
# sandwich variance
var_uncover3 <- vcovHC(fit_uncover3)["trtcIXE_Q2W", "trtcIXE_Q2W"]  

# Obtain inverse-variance weighted estimate and standard error
(d_ETN_IXE <- weighted.mean(c(d_uncover2, d_uncover3),
                            c(1 / var_uncover2, 1 / var_uncover3)))
(se_ETN_IXE <- sqrt(1 / sum(1 / var_uncover2, 1 / var_uncover3)))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
fit_fixture <- glm(cbind(pasi75_r, pasi75_n - pasi75_r) ~ trtc,
    data = fixture_agd,
    family = binomial(link = "probit"))

(d_ETN_SEC <- coef(fit_fixture)[["trtcSEC_300"]])
(se_ETN_SEC <- sqrt(vcov(fit_fixture)["trtcSEC_300", "trtcSEC_300"]))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
(d_SEC_IXE <- d_ETN_IXE - d_ETN_SEC)
(se_SEC_IXE <- sqrt(se_ETN_IXE^2 + se_ETN_SEC^2))


## ----unanchored-maic, echo = FALSE, warning=FALSE-----------------------------------------------------------------------------------------
# Select relevant rows
fixture_SEC_agd <- subset(plaque_psoriasis_agd,
                      studyc == "FIXTURE" & trtc == "SEC_300")

uncover_IXE_ipd <- subset(
  plaque_psoriasis_ipd,
  studyc %in% c("UNCOVER-2", "UNCOVER-3") & trtc  == "IXE_Q2W"
)

# FIXTURE AgD covariate summaries (first and second moments)
X_SEC_agd <- with(fixture_SEC_agd,
              cbind(durnpso_mean, durnpso_mean^2 + durnpso_sd^2,
                    prevsys / 100,
                    bsa_mean, bsa_mean^2 + bsa_sd^2,
                    weight_mean, weight_mean^2 + weight_sd^2,
                    psa / 100))

# We were given summaries by arm, so convert these to overall summaries
X_SEC_agd <- apply(
  X_SEC_agd, MARGIN = 2, FUN = weighted.mean, 
  w = fixture_SEC_agd$sample_size_w0
)

# Create matrix of centred UNCOVER IPD covariates
X_IXE_ipd <- sweep(with(uncover_IXE_ipd, 
                    cbind(durnpso, durnpso^2,
                          prevsys,
                          bsa, bsa^2,
                          weight, weight^2,
                          psa)), 
               MARGIN = 2, STATS = X_SEC_agd, FUN = '-')

study_IXE_ipd <- uncover_IXE_ipd$studyc

# Estimate weights for UNCOVER-2
opt_IXE_uncover2 <- optim(par = rep(0, ncol(X_IXE_ipd)), 
                      fn = objfn, gr = gradfn, 
                      X = X_IXE_ipd[study_IXE_ipd == "UNCOVER-2", ], 
                      method = "BFGS")

a1_IXE_uncover2 <- opt_IXE_uncover2$par
wt_IXE_uncover2 <- exp(
  X_IXE_ipd[study_IXE_ipd == "UNCOVER-2", ] %*% a1_IXE_uncover2
)

# Normalise to sum to ESS for interpretation
wt_IXE_uncover2 <- wt_IXE_uncover2 * sum(wt_IXE_uncover2) / 
  sum(wt_IXE_uncover2^2)

# Estimate weights for UNCOVER-3
opt_IXE_uncover3 <- optim(par = rep(0, ncol(X_IXE_ipd)), 
                      fn = objfn, gr = gradfn, 
                      X = X_IXE_ipd[study_IXE_ipd == "UNCOVER-3", ], 
                      method = "BFGS")

a1_IXE_uncover3 <- opt_IXE_uncover3$par
wt_IXE_uncover3 <- exp(
  X_IXE_ipd[study_IXE_ipd == "UNCOVER-3", ] %*% a1_IXE_uncover3
)

# Normalise to sum to ESS for interpretation
wt_IXE_uncover3 <- wt_IXE_uncover3 * sum(wt_IXE_uncover3) / 
  sum(wt_IXE_uncover3^2)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
# IXE Q2W probability from UNCOVER-2
fit_IXE_uncover2 <- glm(
  cbind(pasi75, 1 - pasi75) ~ 1,
  data = subset(uncover_ipd, studyc == "UNCOVER-2" & trtc == "IXE_Q2W"),
  family = binomial(link = "probit"),
  weights = wt_IXE_uncover2
)

# probit probability
p_IXE_uncover2 <- coef(fit_IXE_uncover2)[["(Intercept)"]]  
# sandwich variance
var_IXE_uncover2 <- vcovHC(fit_IXE_uncover2)["(Intercept)", "(Intercept)"]  

# IXE Q2W probability from from UNCOVER-3
fit_IXE_uncover3 <- glm(
  cbind(pasi75, 1 - pasi75) ~ 1,
  data = subset(uncover_ipd, studyc == "UNCOVER-3" & trtc == "IXE_Q2W"),
  family = binomial(link = "probit"),
  weights = wt_IXE_uncover3
)

# probit probability
p_IXE_uncover3 <- coef(fit_IXE_uncover3)[["(Intercept)"]]  
# sandwich variance
var_IXE_uncover3 <- vcovHC(fit_IXE_uncover3)["(Intercept)", "(Intercept)"]  

# Obtain inverse-variance weighted estimate and standard error
(p_IXE <- weighted.mean(c(p_IXE_uncover2, p_IXE_uncover3), 
                        c(1 / var_IXE_uncover2, 1 / var_IXE_uncover3)))
(se_IXE <- sqrt(1 / sum(1 / var_IXE_uncover2, 1 / var_IXE_uncover3)))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
fit_SEC_fixture <- glm(cbind(pasi75_r, pasi75_n - pasi75_r) ~ 1,
    data = fixture_agd,
    family = binomial(link = "probit"))

(p_SEC <- coef(fit_SEC_fixture)[["(Intercept)"]])
(se_SEC <- sqrt(vcov(fit_SEC_fixture)["(Intercept)", "(Intercept)"]))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
(d_un_SEC_IXE <- p_IXE - p_SEC)               # probit difference
(se_un_SEC_IXE <- sqrt(se_IXE^2 + se_SEC^2))  # standard error


## ----setup_mlnmr, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------
library(multinma)
options(mc.cores = parallel::detectCores())


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
# IPD studies
pso_ipd <- transform(plaque_psoriasis_ipd,
    # Variable transformations
    bsa = bsa / 100,
    weight = weight / 10,
    durnpso = durnpso / 10,
    prevsys = as.numeric(prevsys),
    psa = as.numeric(psa),
    # Treatment classes
    trtclass = 
      ifelse(trtc == "PBO", "Placebo",
      ifelse(trtc %in% c(
        "IXE_Q2W", "IXE_Q4W", "SEC_150", "SEC_300"
      ),  "IL-17 blocker",
      ifelse(trtc == "ETN", "TNFa blocker",
      ifelse(trtc == "UST", "IL-12/23 blocker", NA)))),
    # Check complete cases for covariates of interest
    is_complete = complete.cases(durnpso, prevsys, bsa, weight, psa)
  ) 

# Only a very small proportion of incomplete rows; we simply remove these
pso_ipd <- subset(pso_ipd, is_complete)

# AgD studies
pso_agd <- transform(plaque_psoriasis_agd,
    # Variable transformations
    bsa_mean = bsa_mean / 100, bsa_sd = bsa_sd / 100,
    weight_mean = weight_mean / 10, weight_sd = weight_sd / 10,
    durnpso_mean = durnpso_mean / 10, durnpso_sd = durnpso_sd / 10,
    prevsys = prevsys / 100,
    psa = psa / 100,
    # Treatment classes
    trtclass = 
      ifelse(trtc == "PBO", "Placebo",
        ifelse(trtc %in% c(
          "IXE_Q2W", "IXE_Q4W", "SEC_150", "SEC_300"
        ),  "IL-17 blocker",
        ifelse(trtc == "ETN", "TNFa blocker",
        ifelse(trtc == "UST", "IL-12/23 blocker", NA))))
    )


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
pso_net <- combine_network(
  set_ipd(pso_ipd,
    study = studyc,
    trt = trtc,
    r = pasi75,
    trt_class = trtclass),
  set_agd_arm(pso_agd,
    study = studyc,
    trt = trtc,
    r = pasi75_r,
    n = pasi75_n,
    trt_class = trtclass)
)


## ----eval=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------
# plot(pso_net, weight_nodes = TRUE, weight_edges = TRUE, show_trt_class = TRUE)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
pso_net <- add_integration(pso_net,
  durnpso = distr(qgamma, mean = durnpso_mean, sd = durnpso_sd),
  prevsys = distr(qbern, prob = prevsys),
  bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
  psa = distr(qbern, prob = psa))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| label: pso-mlnmr-fit
#| cache: true
pso_fit_FE <- nma(pso_net, 
                  trt_effects = "fixed",
                  link = "probit", 
                  likelihood = "bernoulli2",
                  regression = ~(durnpso + prevsys + bsa + weight + psa)*.trt,
                  class_interactions = "common",
                  prior_intercept = normal(scale = 10),
                  prior_trt = normal(scale = 10),
                  prior_reg = normal(scale = 10),
                  init_r = 0.1,
                  QR = TRUE)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
print(pso_fit_FE, pars = "d")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| eval: false
#| # Population-average conditional effects
# relative_effects(pso_fit_FE)
# # Average event probabilities
# predict(pso_fit_FE, type = "response")
# # Population-average marginal effects
# marginal_effects(pso_fit_FE, mtype = "link")
# # Output not shown


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
prospect_dat <- data.frame(
  studyc = "PROSPECT",
  durnpso = 19.6 / 10, durnpso_sd = 13.5 / 10,
  prevsys = 0.9095,
  bsa = 18.7 / 100, bsa_sd = 18.4 / 100,
  weight = 87.5 / 10, weight_sd = 20.3 / 10,
  psa = 0.202)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------
# relative_effects(
#   pso_fit_FE, newdata = prospect_dat, study = studyc, all_contrasts = TRUE
# )


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
prospect_dat <- add_integration(prospect_dat,
  durnpso = distr(qgamma, mean = durnpso, sd = durnpso_sd),
  prevsys = distr(qbern, prob = prevsys),
  bsa = distr(qlogitnorm, mean = bsa, sd = bsa_sd),
  weight = distr(qgamma, mean = weight, sd = weight_sd),
  psa = distr(qbern, prob = psa),
  cor = pso_net$int_cor)


## ----warning=FALSE, results='hide'--------------------------------------------------------------------------------------------------------
prospect_pred <- predict(pso_fit_FE, 
                         type = "response", 
                         newdata = prospect_dat,
                         study = studyc,
                         baseline = distr(qbeta, 1156, 1509-1156),
                         baseline_type = "response",
                         baseline_level = "aggregate",
                         baseline_trt = "SEC_300")
prospect_pred


## -----------------------------------------------------------------------------------------------------------------------------------------
#> ------------------------------------------------------------- Study: PROSPECT ---- 
#> 
#>                         mean   sd 2.5%  25%  50%  75% 97.5% Bulk_ESS Tail_ESS Rhat
#> pred[PROSPECT: PBO]     0.04 0.01 0.02 0.03 0.04 0.04  0.06     4284     3237    1
#> pred[PROSPECT: ETN]     0.44 0.04 0.36 0.41 0.44 0.47  0.52     7005     3384    1
#> pred[PROSPECT: IXE_Q2W] 0.87 0.02 0.83 0.86 0.88 0.89  0.91     5965     3425    1
#> pred[PROSPECT: IXE_Q4W] 0.77 0.03 0.70 0.75 0.77 0.79  0.83     6156     3693    1
#> pred[PROSPECT: SEC_150] 0.66 0.03 0.60 0.64 0.66 0.68  0.71     6374     3528    1
#> pred[PROSPECT: SEC_300] 0.77 0.01 0.74 0.76 0.77 0.77  0.79     4115     3913    1
#> pred[PROSPECT: UST]     0.67 0.06 0.55 0.63 0.67 0.72  0.79     4393     2993    1


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------
#| label: fig-pso-prospect-pred
#| fig-cap: "Population-average probabilities of achieving PASI 75 response on each treatment, in the PROSPECT target population. Estimates are posterior medians, with 66% and 95% Credible Intervals."
#| fig-height: 3
plot(prospect_pred)


## ----warning=FALSE, results='hide'--------------------------------------------------------------------------------------------------------
marginal_effects(pso_fit_FE, 
                 mtype = "link",
                 newdata = prospect_dat,
                 study = studyc,
                 baseline = distr(qbeta, 1156, 1509-1156),
                 baseline_type = "response",
                 baseline_level = "aggregate",
                 baseline_trt = "SEC_300",
                 all_contrasts = TRUE)
#> ------------------------------------------------------- Study: PROSPECT ---- 
#> 
#>                                      mean   sd  2.5%   25%   50%   75% 97.5%
#> marg[PROSPECT: ETN vs. PBO]          1.66 0.11  1.44  1.58  1.66  1.73  1.87
#> marg[PROSPECT: IXE_Q2W vs. PBO]      2.95 0.11  2.74  2.88  2.95  3.02  3.16
#> marg[PROSPECT: IXE_Q4W vs. PBO]      2.54 0.10  2.33  2.47  2.54  2.61  2.74
#> marg[PROSPECT: SEC_150 vs. PBO]      2.21 0.12  1.98  2.13  2.21  2.29  2.45
#> marg[PROSPECT: SEC_300 vs. PBO]      2.53 0.12  2.30  2.45  2.53  2.61  2.77
#> marg[PROSPECT: UST vs. PBO]          2.26 0.19  1.90  2.13  2.26  2.39  2.63
#>   ...
#> marg[PROSPECT: SEC_300 vs. IXE_Q2W] -0.42 0.10 -0.62 -0.49 -0.42 -0.36 -0.23
#>   ... output abbreviated ...

