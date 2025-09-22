## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
load("data/icl_data_long.rda")
tab=icl_data_long
colnames(tab)=c(
  "Study","Trt","\\(r\\)","\\(n\\)","Contamination level","Surgery type"
)
tab=rbind(tab[1:5,],rep("$\\ldots$",6))
tinytable::tt(tab)


## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------
library(multinma)
# To set up parallel processing
options(mc.cores = parallel::detectCores())


## -----------------------------------------------------------------------------------------------------------------------------------------
icl_network <- set_agd_arm(
  icl_data_long, 
  study = study,
  trt = trt,
  r = r, 
  n = n,
  trt_class = as.numeric(trt != "nonantibacterial")
)

# Print a summary of the network
icl_network


## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------
# plot(icl_network, weight_nodes = TRUE, weight_edges = TRUE)

## ----fig-icl_nma_network, echo = FALSE, warning = FALSE, message = FALSE------------------------------------------------------------------
#| fig.cap: RCT evidence network on ICL for prevention of SSI
plot(icl_network, weight_nodes = TRUE, weight_edges = TRUE) + 
  ggplot2::theme(legend.position = "bottom", legend.box = "vertical")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: icl_fe_re
#| results: false
#| message: false
#| cache: true
icl_fit_fe <- nma(icl_network, 
                  trt_effects = "fixed",
                  prior_intercept = normal(scale = 100),
                  prior_trt = normal(scale = 100))

icl_fit_re <- nma(icl_network, 
                trt_effects = "random",
                prior_intercept = normal(scale = 100),
                prior_trt = normal(scale = 100),
                prior_het = half_normal(scale = 2.5))


## -----------------------------------------------------------------------------------------------------------------------------------------
print(icl_fit_fe)
print(icl_fit_re)


## -----------------------------------------------------------------------------------------------------------------------------------------
(icl_dic_fe <- dic(icl_fit_fe))
(icl_dic_re <- dic(icl_fit_re))



## -----------------------------------------------------------------------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 6
#| out-width: "75%"
plot(icl_dic_re)


## -----------------------------------------------------------------------------------------------------------------------------------------
(icl_relative_effects_re <- relative_effects(
  icl_fit_re, trt_ref = "nonantibacterial"
))

# Store as an array and exponentiate from log odds ratios to odds ratios
lor_array <- as.array(icl_relative_effects_re)
or_array <- exp(lor_array)

(icl_or_re <- summary(or_array))


## -----------------------------------------------------------------------------------------------------------------------------------------
icl_or_re_forest <- as.data.frame(icl_or_re)
icl_or_re_forest$estci <- sprintf("%.2f (%.2f, %.2f)", 
                                  icl_or_re_forest$mean,
                                  icl_or_re_forest$`2.5%`,
                                  icl_or_re_forest$`97.5%`)

forestplot::forestplot(icl_or_re_forest, 
                       mean = mean, lower = `2.5%`, upper = `97.5%`,
                       labeltext = c(parameter, estci),
                       boxsize = .1,
                       # Axis ticks on log scale
                       xlog = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Mean ranks and other summary statistics
(icl_ranks_re <- posterior_ranks(icl_fit_re))
# Probability of occupying each rank
(icl_probs_re <- posterior_rank_probs(icl_fit_re))
# Plot the rankogram
plot(icl_probs_re)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| label: cum-effects
# Cumulative probabilities of occupying each rank
(icl_cum_probs_re <- posterior_rank_probs(icl_fit_re, cumulative = TRUE))
# Plot the cumulative rankograms
plot(icl_cum_probs_re)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| results: false
#| message: false
#| label: nma-random
icl_fit_re_contamination <- nma(icl_network, 
                                trt_effects = "random",
                                regression = ~.trt:contamination_level,
                                class_interactions = "common",
                                prior_intercept = normal(scale = 100),
                                prior_trt = normal(scale = 100),
                                prior_reg = normal(scale = 100),
                                prior_het = half_normal(scale = 2.5))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| label: dic-model
# Inspect the regression coefficients and model fit statistics
print(icl_fit_re_contamination)
dic(icl_fit_re_contamination)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| results: false
#| message: false
#| label: nma-random2
# Testing inconsistency using an unrelated mean effects (inconsistency) model
icl_fit_re_ume <- nma(icl_network, 
                      consistency = "ume",
                      trt_effects = "random",
                      prior_intercept = normal(scale = 100),
                      prior_trt = normal(scale = 100),
                      prior_het = normal(scale = 2.5),
                      # Increase max_treedepth to avoid warnings
                      control = list(max_treedepth = 15))


## -----------------------------------------------------------------------------------------------------------------------------------------
# Compare the DIC and residual deviance to the consistency model
(icl_dic_re_ume <- dic(icl_fit_re_ume))
(icl_dic_re)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| label: plot_re
plot(icl_dic_re, icl_dic_re_ume, show_uncertainty = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| results: false
#| message: false
#| label: nma-random3
# Testing for inconsistency using node splitting
icl_fit_re_nodesplit <- nma(icl_network, 
                            consistency = "nodesplit",
                            trt_effects = "random",
                            prior_intercept = normal(scale = 100),
                            prior_trt = normal(scale = 100),
                            prior_het = half_normal(scale = 2.5),
                            # Increase max_treedepth to avoid warnings
                            control = list(max_treedepth = 15))


## -----------------------------------------------------------------------------------------------------------------------------------------
# Summarise nodesplits
summary(icl_fit_re_nodesplit)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| fig-height: 7
#| fig-width: 8
#| out-width: "75%"
# Plot direct and indirect estimates
plot(icl_fit_re_nodesplit) + 
  theme(legend.position=c(.85,.2))


## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------
library(netmeta)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Load the icl_data list with matrices r, n and t for events, patients, and 
# treatments, and the vector na with number of arms per study.
load("data/icl_data.rda")
t_names <- c("nonantibacterial",  "no irrigation",  "antiseptic",  "antibiotic")

# Netmeta does not accept independent arms on the same treatment within a trial
# Need to merge arms on the same treatment
for(i_study in 1:icl_data$ns) {
  # Which treatments are studied in this study?
  study_treatments <- unique(icl_data$t[i_study, ])
  for(i_treatment in study_treatments[!is.na(study_treatments)]) {
    # Index the arms on this treatment
    arm_index <- icl_data$t[i_study, ] == i_treatment
    arm_index[is.na(arm_index)] <- FALSE
    if(sum(arm_index) > 1) {
      # Merge to just one arm on this treatment
      icl_data$t[i_study, arm_index] <- 
        c(i_treatment, rep(NA, sum(arm_index) - 1))
      icl_data$na[i_study] <- icl_data$na[i_study] - sum(arm_index) + 1
      # r and n are the sum of events and patients across all arms
      icl_data$r[i_study, arm_index] <- 
        c(sum(icl_data$r[i_study, arm_index]), rep(NA, sum(arm_index) - 1))
      icl_data$n[i_study, arm_index] <- 
        c(sum(icl_data$n[i_study, arm_index]), rep(NA, sum(arm_index) - 1))
    }
  }
}

# Netmeta can take strings as the treatment names
temp <- matrix("", dim(icl_data$t)[1], dim(icl_data$t)[2])
rownames(temp) <- rownames(icl_data$t)
for(i_study in 1:dim(temp)[1]) {
  temp[i_study, ] <- t_names[icl_data$t[i_study, ]]
}
icl_data$t <- temp


## -----------------------------------------------------------------------------------------------------------------------------------------
# Transform data from arm-based format to contrast-based format
# Studlab is repeated for studies with >2 arms (e.g. Oleson 1980 with 3 arms)
icl_network <- pairwise(treat = list(t[, 1], t[, 2], t[, 3]),
                        event = list(r[, 1], r[, 2], r[, 3]),
                        n = list(n[, 1], n[, 2], n[, 3]),
                        data = icl_data, sm = "OR",
                        studlab = names(na))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
tb=icl_network[1:5,c("TE", "seTE", "studlab", "treat1", "treat2")] |> as_tibble() |> 
  bind_rows(tibble(TE=NA,seTE=NA,studlab=NA,treat1=NA,treat2=NA))
tinytable::tt(tb) |> tinytable::format_tt(replace = "$\\ldots$",digits=3)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| cache: true
#| results: false
#| message: false
#| label: netmeta
# Output the results of a fixed effects NMA
icl_fit_fe <- netmeta(TE, seTE, treat1, treat2, studlab, data = icl_network,
                      common = TRUE, random = FALSE,
                      reference.group = "nonantibacterial")

# Output the results of a random effects NMA
icl_fit_re <- netmeta(TE, seTE, treat1, treat2, studlab, data = icl_network,
                      common = FALSE, random = TRUE,
                      reference.group = "nonantibacterial")



## -----------------------------------------------------------------------------------------------------------------------------------------
icl_fit_fe

icl_fit_re


## -----------------------------------------------------------------------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 5
#| out-width: "75%"
netgraph(icl_fit_re, points = TRUE, cex.points = 3, cex = 1.25,
         multiarm = TRUE, number.of.studies = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 4
#| out-width: "85%"
# P-score probabilities 
netrank(icl_fit_re, small.values = "good")
# Plot the rankograms
plot(rankogram(icl_fit_re, small.values = "good"))


## -----------------------------------------------------------------------------------------------------------------------------------------
# Can also estimate SUCRA with sampling
netrank(icl_fit_re, small.values = "good", method = "SUCRA")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 5
#| out-width: "85%"
netheat(icl_fit_re, nchar.trts = 7)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Compare to decomp.design results (esp. Between-designs Q section)
decomp.design(icl_fit_re)

