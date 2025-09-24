## -----------------------------------------------------------------------------------------------------------------------------------------
load(file = "data/msm_colon.rda")
dim(msm_colon)

# Add numeric strategy that will be used in survival and multistate modelling
msm_colon$strategy_id <- as.numeric(msm_colon$rx)

# Look at the contents
head(msm_colon)

# Give a description of the transition numbers in msm_colon
transition_names <- c("RF to R", "RF to OCD", "R to CD", "R to OCD")



## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------
library(survival)
library(flexsurv)
set.seed(2549788)


## -----------------------------------------------------------------------------------------------------------------------------------------
distribution_names <- c(
  "exp", "weibull", "gompertz", "lognormal", "llogis", "gamma"
)
transition_names <- c("RF to R", "RF to OCD", "R to CD", "R to OCD")



## -----------------------------------------------------------------------------------------------------------------------------------------

# Matrix to store summaries of the clock-reset distributions
survival_aic_cr <- 
  matrix(nrow = length(distribution_names), 
         ncol = length(transition_names),
         dimnames = list(distribution_names, transition_names))

# Lists to store actual survival models for use in multistate models
survival_models_cr <- list()




## -----------------------------------------------------------------------------------------------------------------------------------------
# List of distributions fit to time-in-state (years column of msm_colon)
survival_formula_cr <- list()
# Set the transitions that depend on treatment
survival_formula_cr[["RF to R"]] <- as.formula(
  "Surv(years, status) ~ factor(strategy_id)"
)
# Set the transitions that are independent of treatment
survival_formula_cr[["RF to OCD"]] <- as.formula("Surv(years, status) ~ 1")
survival_formula_cr[["R to CD"]] <- as.formula("Surv(years, status) ~ 1")
survival_formula_cr[["R to OCD"]] <- as.formula("Surv(years, status) ~ 1")



## ----warning = FALSE----------------------------------------------------------------------------------------------------------------------
for(i_trans in 1:length(transition_names)) {
  survival_models_cr[[transition_names[i_trans]]] <- list()
  for(i_dist in 1:length(distribution_names)) {
    # Fit the survival distribution for this transition
    survival_models_cr[[i_trans]][[distribution_names[i_dist]]] <- 
      flexsurvreg(survival_formula_cr[[i_trans]],
                  subset = (trans == i_trans), 
                  data = msm_colon, dist = distribution_names[i_dist])
          
    # Save the AIC for model comparison
    survival_aic_cr[i_dist, i_trans] <- 
      survival_models_cr[[i_trans]][[distribution_names[i_dist]]]$AIC
  }
}



## -----------------------------------------------------------------------------------------------------------------------------------------
# Which models have the lowest AIC?
min_aic_table <- matrix(NA, nrow = 2, ncol = length(transition_names))
rownames(min_aic_table) <- c("CR distribution", "CR AIC")
colnames(min_aic_table) <- transition_names
min_aic_table["CR distribution", ] <- distribution_names[
  apply(survival_aic_cr, c(2), which.min)
]
min_aic_table["CR AIC", ] <- round(
  apply(survival_aic_cr, c(2), min), digits = 2
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
knitr::kable(min_aic_table)


## ----message=FALSE------------------------------------------------------------------------------------------------------------------------
# Load the msm library
library(msm)
load("data/panel_colon_data.rda")
# Look at the contents
head(panel_colon)

# Summarise the transitions
statetable.msm(state, id, data = panel_colon)


## ----cache=TRUE---------------------------------------------------------------------------------------------------------------------------
colon_qmatrix <- rbind(
  c(0, 1, 0, 1), 
  c(0, 0, 1, 1),
  c(0, 0, 0, 0), 
  c(0, 0, 0, 0)
)

rownames(colon_qmatrix) <- colnames(colon_qmatrix) <-
  c("Recurrence-free", "Recurrence", "Dead (Cancer)", "Dead (Other cause)")

# Estimate the transition rates 
colon_msm_fit <- msm(state ~ years, subject = id, data = panel_colon, 
                    covariates = list (
                      "1-2" = ~ strategy_id
                    ),
                    deathexact = c(3, 4),
                    qmatrix = colon_qmatrix, 
                    gen.inits = TRUE,
                    center = FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: true
# Create a customised version of the print method for msm so the output doesn't spill over the pdf page
my.print.msm = function (x, covariates = NULL, digits = 4, ...) 
{
    ret <- NULL
    if (!x$foundse & !attr(x, "fixed")) {
        cat("Optimisation probably not converged to the maximum likelihood.\noptim() reported convergence but estimated Hessian not positive-definite.\n")
    }
    else {
        if (is.null(x$cl)) {
            cl <- 0.95
            warning("Found msm object saved before version 1.3. Models will need to be refitted under the newer version for output functions to work")
        }
        else cl <- x$cl
        if (!attr(x, "fixed")) {
            if (is.null(covariates)) {
                covvalue <- (if (x$center) 
                  "their means"
                else "0")
                covariates <- if (x$center) 
                  "mean"
                else 0
            }
            else covvalue <- "the values supplied in \"covariates\""
            covmessage <- if (attr(x$data$mf, "ncovs") == 0) 
                ""
            else paste0("\nBaselines are with covariates set to ", 
                covvalue)
            cat("Maximum likelihood estimates", covmessage, "\n", 
                sep = "")
            if (x$qcmodel$ncovs > 0) 
                hrmessage <- paste0(" with hazard ratios for each covariate")
            else hrmessage <- ""
            q.header <- paste0("Transition intensities", hrmessage, 
                "\n")
            ret <- msm.form.qoutput(x, covariates, cl = cl, digits = digits, 
                ...)
            fres <- attr(ret, "formatted")
            cat("\n")
            cat(q.header)
            print(fres, quote = FALSE)
            if (x$emodel$misc) {
                ormessage <- if (x$ecmodel$ncovs > 0) 
                  paste0(" with odds ratios for each covariate")
                else ""
                e.header <- paste0("Misclassification probabilities", 
                  ormessage, "\n")
                rete <- msm.form.eoutput(x, covariates, cl = cl, 
                  digits = digits, ...)
                frese <- attr(rete, "formatted")
                cat("\n")
                cat(e.header)
                print(frese, quote = FALSE)
                if (any(x$paramdata$plabs[x$paramdata$optpars] == 
                  "initp")) {
                  i.header <- paste0("Initial state occupancy probabilities\n")
                  cat("\n")
                  cat(i.header)
                  print(x$hmodel$initprobs)
                  if (any(x$hmodel$nicovs > 0)) {
                    ic.header <- "Covariates on logit initial state probabilities\n"
                    cat("\n")
                    cat(ic.header)
                    print(x$hmodel$icoveffect)
                  }
                }
            }
            else if (x$hmodel$hidden && (is.null(x$hmodel$phase.only) || 
                !x$hmodel$phase.only)) {
                cat("\n")
                print(x$hmodel)
            }
            if (!is.null(x$qmodel$phase.states)) {
                cat("\nPhase-type model\n")
                print(phasemeans.msm(x))
            }
        }
    }
    cat("\n-2 * log-likelihood: ", x$minus2loglik, "\n")
    invisible(ret)
}

print_msm_wrapped <- function(x, ...) {
  # Print the wrapped call manually
  cat("Call:\n")
  wrapped_call <- strwrap(deparse(x$call), width = 90)
  cat(paste(wrapped_call, collapse = "\n"), "\n\n")
  
  # Remove the call from the object's attributes (rather than setting it to NULL)
  x2 <- x
  x2$call <- NULL
  attr(x2, "call") <- NULL
  
  # Now print using msm's method, no duplicate Call: NULL
  my.print.msm(x2, ...)
}

# Use it
print_msm_wrapped(colon_msm_fit)


## -----------------------------------------------------------------------------------------------------------------------------------------
pmatrix.msm(colon_msm_fit, covariates = list(strategy_id = 1))


## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------
# data.table package provides the data structures for hesim
library(data.table)
library(flexsurv)
library(hesim)
library(ggplot2)
library(BCEA)

# Global variables that specify the model
state_names <- c(
  "Recurrence-free", "Recurrence", "Dead (Cancer)", "Dead (Other cause)"
)
treatment_names <- c("Observation", "Lev", "Lev+5FU")
n_patients <- 1000
n_samples <- 1000
n_states <- 4
n_treatments <- 3


## -----------------------------------------------------------------------------------------------------------------------------------------
# Four transitions: 1 (RF to R), 2 (RF to OCD), 3 (R to CD), 4 (R to OCD)
transition_matrix <- rbind(
  c(NA, 1, NA, 2),
  c(NA, NA, 3, 4),
  c(NA, NA, NA, NA),
  c(NA, NA, NA, NA)
)
colnames(transition_matrix) <- rownames(transition_matrix) <- state_names



## -----------------------------------------------------------------------------------------------------------------------------------------
# Treatments to compare
strategies <- data.table(
  strategy_id = c(1, 2, 3),
  strategy_name = treatment_names
)

# Individuals are randomly sampled with replacement from the colon dataset
# Sample patient IDs to preserve correlation between age and sex
patient_id_sample <- sample(1:n_patients, size = n_patients, replace = TRUE)
patients <- data.table(
  patient_id = 1:n_patients,
  age = msm_colon$age[patient_id_sample],
  sex = msm_colon$sex[patient_id_sample]
)

# Only need to specify the non-death (i.e., non-absorbing) states as hesim adds
# death/absorbing states automatically based on the transition matrix
states <- data.table(
  state_id = c(1:2),
  state_name = rownames(transition_matrix)[1:2]
)



## -----------------------------------------------------------------------------------------------------------------------------------------
# hesim data specification
hesim_data_ <- hesim_data(
  strategies = strategies,
  patients = patients, 
  states = states
)

# Duplicate the individual data for each treatment strategy
transition_model_data <- expand(hesim_data_, 
  by = c("strategies", "patients")
)


# Use hesim to generate model labels
hesim_labels <- get_labels(hesim_data_)
hesim_labels$transition_id <- c(
  "RF to R" = 1, 
  "RF to OCD" = 2,
  "R to CD" = 3,
  "R to OCD" = 4
)

hesim_labels$state_id <- c(1:n_states)
names(hesim_labels$state_id) <- state_names



## -----------------------------------------------------------------------------------------------------------------------------------------
# List of survival models for each transition
hesim_survival_cr <- flexsurvreg_list(
  survival_models_cr[["RF to R"]][["gompertz"]],
  survival_models_cr[["RF to OCD"]][["gompertz"]],
  survival_models_cr[["R to CD"]][["llogis"]],
  survival_models_cr[["R to OCD"]][["exp"]]
)




## -----------------------------------------------------------------------------------------------------------------------------------------
# Calling the constructor for IndivCtstmTrans
transition_model_cr <- create_IndivCtstmTrans(
  hesim_survival_cr, transition_model_data,
  trans_mat = transition_matrix, 
  n = n_samples,
  clock = "reset",
  start_age = patients$age
)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Two time periods for utilities
n_utility_times <- 2

# We sample these utilities and probabilities separately and combine into a 
# table of "custom" utilities for hesim
# Number of states is the number of states minus the number of absorbing states
# which is 2 in this case.

utility_table <- stateval_tbl(
  data.table(
    strategy_id = rep(
      # Table defines values for each treatment 
      c(1:n_treatments),
      each = length(states$state_id) * n_utility_times * n_samples
      ),
    sample = rep(
      # Table then gives values for each sample 
      rep(c(1:n_samples), 
        each = length(states$state_id) * n_utility_times
        ), 
      # Sample values repeated for each treatment
      times = n_treatments 
      ),
    state_id = rep(
      # Values for each state 
      rep(1:length(states$state_id), 
        each = n_utility_times
        ), 
      # State values repeated for each treatment and state
      times = n_treatments * n_samples
      ), 
    time_start = rep(
      # Values for each timepoint
      c(0, 1), 
      # Timepoint values repeated for each treatment, sample, and state.
      times = n_treatments * n_samples * length(states$state_id)
      ),
    value = 1
  ), 
  dist = "custom"
)

head(utility_table)


## -----------------------------------------------------------------------------------------------------------------------------------------

# Normally distributed state utilities
utility_recurrence_free <- rnorm(n_samples, 0.8, sd = 0.1 * 0.8)
utility_recurrence <- rnorm(n_samples, 0.6, sd = 0.1 * 0.6)

# Probability and disutility of toxicity
# No SD reported so use 10% of the mean
p_tox_lev <- rnorm(n_samples, mean = 0.20, sd = 0.1 * 0.20)
p_tox_lev5fu <- rnorm(n_samples, mean = 0.40, sd = 0.1 * 0.40)
disutility_toxicity <- rnorm(n_samples, mean = -0.1, sd = 0.1 * 0.1)



## -----------------------------------------------------------------------------------------------------------------------------------------

# Fill in the blank utility table

# Utilities for Observation
# Not affected by disutility and the same for first and subsequent years
utility_table[strategy_id == 1 & state_id == 1, "value"] <- 
  c(
    utility_recurrence_free,
    utility_recurrence_free
  )
utility_table[strategy_id == 1 & state_id == 2, "value"] <- 
  c(
    utility_recurrence,
    utility_recurrence
  )

# Utilities for Lev
utility_table[strategy_id == 2 & state_id == 1, "value"] <- 
  c(
    utility_recurrence_free + p_tox_lev * disutility_toxicity, 
    utility_recurrence_free
  )
utility_table[strategy_id == 2 & state_id == 2, "value"] <- 
  c(
    utility_recurrence + p_tox_lev * disutility_toxicity, 
    utility_recurrence
  )

# Utilities for Lev+5FU
utility_table[strategy_id == 3 & state_id == 1, "value"] <- 
  c(
    utility_recurrence_free + p_tox_lev5fu * disutility_toxicity, 
    utility_recurrence_free
  )
utility_table[strategy_id == 3 & state_id == 2, "value"] <- 
  c(
    utility_recurrence + p_tox_lev5fu * disutility_toxicity, 
    utility_recurrence
  )


## -----------------------------------------------------------------------------------------------------------------------------------------
utility_model <- create_StateVals(utility_table, 
  hesim_data = hesim_data_,
  n = n_samples
)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Treatment costs depend on the costs of adverse events which are a one-off
treatment_cost_table <- stateval_tbl(
  data.table(
    strategy_id = rep(c(1:n_treatments), each = n_samples),
    sample = rep(c(1:n_samples), times = n_treatments),
    value = 1
  ), 
  dist = "custom"
)

# State costs are zero except for one-off cost of advanced treatment
# in recurrence state
n_cost_times <- 2

state_cost_table <- stateval_tbl(
  data.table(
    state_id = rep(states$state_id, each = n_cost_times),
    time_start = rep(c(0, 1), times = length(states$state_id)),
    est = c(0, 0, 40000, 0)
  ),
  dist = "fixed"
)



## -----------------------------------------------------------------------------------------------------------------------------------------

# Normally distributed cost of toxicity
cost_toxicity <- rnorm(n_samples, mean = 2000, sd = 0.1 * 2000)


treatment_cost_table[strategy_id == 1, "value"] <- 0
treatment_cost_table[strategy_id == 2, "value"] <- 5000 + 
  p_tox_lev * cost_toxicity 
treatment_cost_table[strategy_id == 3, "value"] <- 10000 + 
  p_tox_lev5fu * cost_toxicity 



## -----------------------------------------------------------------------------------------------------------------------------------------
treatment_cost_model <- create_StateVals(treatment_cost_table, 
  n = n_samples, 
  hesim_data = hesim_data_
)
state_cost_model <- create_StateVals(state_cost_table, 
  n = n_samples, 
  hesim_data = hesim_data_
)
cost_models <- list(Drug = treatment_cost_model,
  Medical = state_cost_model
)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Build the combined economic models of progression, costs, and utilities
economic_model_cr <- IndivCtstm$new(
  trans_model = transition_model_cr,
  utility_model = utility_model,
  cost_models = cost_models
)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Disease modelling
economic_model_cr$sim_disease()

head(economic_model_cr$disprog_)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Simulate discounted costs and qalys using the simulated disease progression
economic_model_cr$sim_costs(dr = 0.03)
economic_model_cr$sim_qalys(dr = 0.03)


## -----------------------------------------------------------------------------------------------------------------------------------------

# State occupancy probabilities for clock-reset
economic_model_cr$sim_stateprobs(t = seq(0, 20 , 1/12)) 


## -----------------------------------------------------------------------------------------------------------------------------------------
state_probabilities_cr <- economic_model_cr$stateprobs_[
  , .(prob_mean = mean(prob)),by = c("strategy_id", "state_id", "t")
]

autoplot(economic_model_cr$stateprobs_, labels = hesim_labels)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Generate matrices suitable for use by BCEA
hesim_summary_cr <- economic_model_cr$summarize()


## -----------------------------------------------------------------------------------------------------------------------------------------
# Convert costs and QALYs into a matrix usable by BCEA
# n_samples rows and n_strategies columns
costs_matrix_cr <- effects_matrix_cr <- 
  matrix(NA, 
  nrow = n_samples, 
  ncol = n_treatments,
  dimnames = list(NULL, treatment_names)
)


for(i_treat in 1:n_treatments) {
  costs_matrix_cr[, i_treat] <- with(
    hesim_summary_cr$costs, 
    costs[category =="total" & 
      strategy_id == i_treat & 
      dr == 0.03]
  )
  
  effects_matrix_cr[, i_treat] <- with(
    hesim_summary_cr$qalys, 
    qalys[strategy_id == i_treat & 
    dr == 0.03]
  )
  
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Generate and return the BCEA object
colon_bcea_cr <- bcea(
  eff = effects_matrix_cr,
  cost = costs_matrix_cr,
  ref = 1,
  interventions = treatment_names,
  Kmax = 200000
)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Quick check of costs and effects
apply(
  costs_matrix_cr, 2, function(x) {
    c(mean(x), quantile(x, probs = c(0.025, 0.975)))
  }
)
apply(
  effects_matrix_cr, 2, function(x) {
    c(mean(x), quantile(x, probs = c(0.025, 0.975)))
  }
)



## -----------------------------------------------------------------------------------------------------------------------------------------

# CE Analysis using clock-reset model
summary(colon_bcea_cr, wtp = 100000)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Change object so that comparisons are against Lev+5FU
# Generate and return the BCEA object
colon_bcea_lev5fu_cr <- bcea(
  eff = effects_matrix_cr,
  cost = costs_matrix_cr,
  ref = 3,
  interventions = treatment_names,
  Kmax = 200000
)

# Plot the cost-effectiveness plane for Lev+5FU vs Observation
ceplane.plot(colon_bcea_lev5fu_cr, 
  wtp = 100000, 
  comparison = 1, 
  xlim = c(-2, 4), 
  ylim = c(-20000, 160000),
  graph = "ggplot2"
) +
  ylab("Incremental costs ($)")  +
  xlab("Incremental effects (QALYs)")


## -----------------------------------------------------------------------------------------------------------------------------------------
# Plot a CEAC
colon_multi_cr <- multi.ce(colon_bcea_cr)
ceac.plot(colon_multi_cr, 
  graph = "ggplot",
  line = list(color = c("red", "green", "blue")),
  pos = c(0, 0.50)
) + 
  xlab("Willingness-to-pay ($)")


## -----------------------------------------------------------------------------------------------------------------------------------------
# Sample from the multivariate Normal for log hazard rates estimated by msm
# using panel data
transition_parameter_names <- paste(
  rep(c("RF to R", "RF to OCD", "R to CD", "R to OCD"), 3),
  rep(c("Intercept", "Lev", "Lev+5FU"), each = 4)
)

panel_parameter_samples <- MASS::mvrnorm(
  n = n_samples,mu = colon_msm_fit$estimates,Sigma = colon_msm_fit$covmat
)
colnames(panel_parameter_samples) <- transition_parameter_names
panel_parameter_samples <- as.data.frame(panel_parameter_samples)

for (i in paste("RF to CD", c("Intercept", "Lev", "Lev+5FU")))
  panel_parameter_samples[[i]] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
# Transition model parameters based on msm panel data
transition_model_parameters <- params_surv_list(
  # Distribution for first transition "RF to R"
  params_surv(
    coefs = list(data.frame(
      intercept = panel_parameter_samples[, "RF to R Intercept"],
      "Lev" = panel_parameter_samples[, "RF to R Lev"],
      "Lev+5FU" = panel_parameter_samples[, "RF to R Lev+5FU"])
    ), 
    dist = "exp"),
  # Distribution for second transition "RF to OCD"
  params_surv(
    coefs = list(data.frame(
      intercept = panel_parameter_samples[, "RF to OCD Intercept"],
      "Lev" = panel_parameter_samples[, "RF to OCD Lev"],
      "Lev+5FU" = panel_parameter_samples[, "RF to OCD Lev+5FU"])
    ), 
    dist = "exp"),
  # Distribution for third transition "R to CD"
  params_surv(
    coefs = list(data.frame(
      intercept = panel_parameter_samples[, "R to CD Intercept"],
      "Lev" = panel_parameter_samples[, "R to CD Lev"],
      "Lev+5FU" = panel_parameter_samples[, "R to CD Lev+5FU"])
    ), 
    dist = "exp"),
  # Distribution for fourth transition "R to OCD"
  params_surv(
    coefs = list(data.frame(
      intercept = panel_parameter_samples[, "R to OCD Intercept"],
      "Lev" = panel_parameter_samples[, "R to OCD Lev"],
      "Lev+5FU" = panel_parameter_samples[, "R to OCD Lev+5FU"])
    ), 
    dist = "exp")
)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Need to have data matching the terms of transition_model_parameters
transition_model_data$"intercept" <- 1
transition_model_data$"Lev" <- transition_model_data$strategy_name == "Lev"
transition_model_data$"Lev.5FU" <- 
  transition_model_data$strategy_name == "Lev+5FU"

# msm fit using factor for strategy but hesim data for strategy is numeric
transition_model_data$strategy_id <- 
  as.numeric(transition_model_data$strategy_id)

# Use the panel data transition models
transition_model_panel <- create_IndivCtstmTrans(transition_model_parameters, 
  input_data = transition_model_data,
  trans_mat = transition_matrix,
  clock = "forward",
  start_age = patients$age
)


## -----------------------------------------------------------------------------------------------------------------------------------------

# Combine with the same cost and utility models used for the clock-reset model
economic_model_panel  <- IndivCtstm$new(
  trans_model = transition_model_panel,
  utility_model = utility_model,
  cost_models = cost_models
)

# Simulate disease, costs, effects, and state probabilities
economic_model_panel$sim_disease()
economic_model_panel$sim_costs(dr = 0.03)
economic_model_panel$sim_qalys(dr = 0.03)
economic_model_panel$sim_stateprobs(t = seq(0, 20 , 1/12)) 


## -----------------------------------------------------------------------------------------------------------------------------------------
# Explore state probabilities
state_probabilities_panel <- economic_model_panel$stateprobs_[
  , .(prob_mean = mean(prob)),by = c("strategy_id", "state_id", "t")
]

autoplot(economic_model_panel$stateprobs_, labels = hesim_labels)



## -----------------------------------------------------------------------------------------------------------------------------------------
hesim_summary_panel <- economic_model_panel$summarize()

# Convert costs and QALYs into a matrix usable by BCEA
# n_samples rows and n_strategies columns
costs_matrix_panel <- effects_matrix_panel <- 
  matrix(NA, nrow = n_samples, ncol = n_treatments,
         dimnames = list(NULL, treatment_names))


for(i_treat in 1:n_treatments) {
  costs_matrix_panel[, i_treat] <- with(
    hesim_summary_panel$costs, 
    costs[category =="total" & 
    strategy_id == i_treat & 
    dr == 0.03]
  )
  
  effects_matrix_panel[, i_treat] <- with(
    hesim_summary_panel$qalys, 
    qalys[strategy_id == i_treat & 
    dr == 0.03]
  )
  
}

# Quick check of costs and effects
apply(
  costs_matrix_cr, 2, function(x) {
    c(mean(x), quantile(x, probs = c(0.025, 0.975)))
  }
)
apply(
  effects_matrix_cr, 2, function(x) {
    c(mean(x), quantile(x, probs = c(0.025, 0.975)))
  }
)



## -----------------------------------------------------------------------------------------------------------------------------------------
# Generate and return the BCEA object
colon_bcea_panel <- bcea(
  eff = effects_matrix_panel,
  cost = costs_matrix_panel,
  ref = 1,
  interventions = treatment_names,
  Kmax = 200000
)

# CE Analysis using panel data model
summary(colon_bcea_panel, wtp = 100000)

# Plot a CEAC
colon_multi_panel <- multi.ce(colon_bcea_panel)
ceac.plot(colon_multi_panel, 
  graph = "ggplot",
  line = list(color = c("red", "green", "blue")),
  pos = c(0, 0.50) 
) + 
  xlab("Willingness-to-pay ($)")

