## -----------------------------------------------------------------------------------------------------------------------------------------
n_treatments <- 3
treatment_names <- c("Observation", "Lev", "Lev+5FU")


## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------------------------------------
library(heemod)   # For illustrating state-transition diagram of Markov models
library(BCEA)     # For analyzing the costs and effects of the model
library(MASS)     # For multivariate normal
library(survival) # For colon cancer dataset
library(flexsurv) # For Gompertz survival functions
library(diagram)  # For visualizing graphs


## -----------------------------------------------------------------------------------------------------------------------------------------
# number of health states 
n_states <- 4
# names of the health states
state_names <- c(
  "Recurrence-free", "Recurrence", "Dead (All-cause)", "Dead (Cancer)"
) 


## ----message = FALSE----------------------------------------------------------------------------------------------------------------------
health_states_diagram <-
  define_transition(
    state_names = c(
      "Recurrence-free", "Recurrence", "Dead (All-cause)", "Dead (Cancer)"
    ),
    X1, X2, X3, 0,
    0,  X4, X5, X6,
    0,  0,  X7, 0,
    0,  0,  0,  X8
  )


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| message: false
health_states_diagram <-
  define_transition(
    state_names = c(
      "Recurrence-\nfree", "Recurrence", "Dead\n (All-cause)", "Dead\n (Cancer)"
    ),
    X1, X2, X3, 0,
    0,  X4, X5, X6,
    0,  0,  X7, 0,
    0,  0,  0,  X8
  )


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-heemod-plot
#| fig-cap: "The `heemod` representation of the graph associated with the transition matrix"
#| fig-height: 4.2
#| fig-width: 4
# Note: the arguments box.size, relsize and self.cex are case specific. 
# The basic code to plot would be: plot(health_states_diagram) 
plot(health_states_diagram, box.size = 0.18, relsize = 0.75, self.cex = 0.5)


## -----------------------------------------------------------------------------------------------------------------------------------------
n_cycles <- 50


## -----------------------------------------------------------------------------------------------------------------------------------------
r <- -log(1 - 0.5)/5
prob_1_month<- 1 - exp(-r * 1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Function to translate log odds to probabilities
expit <- function(logO) {
  return(exp(logO)/(1 + exp(logO)))
}

# Function to translate probabilities to log odds
logit <- function(p) {
  return(log(p/(1-p)))
}

## -----------------------------------------------------------------------------------------------------------------------------------------
expit(0.5)
logit(0.6224593)


## ----results = FALSE----------------------------------------------------------------------------------------------------------------------
transition_matrices_homogeneous <- array(
  dim = c(n_treatments, n_states, n_states),
  dimnames = list(treatment_names, state_names, state_names)
)

# For the Observation option
transition_matrices_homogeneous["Observation", "Recurrence-free", ]  <-  
  c(NA, 0.064, 0.13, 0)
transition_matrices_homogeneous["Observation", "Recurrence", ]       <-  
  c(0, NA, 0.13, 0.44)
transition_matrices_homogeneous["Observation", "Dead (All-cause)", ] <-  
  c(0, 0, NA, 0)
transition_matrices_homogeneous["Observation", "Dead (Cancer)", ]    <-  
  c(0, 0, 0, NA)

# For the Lev treatment option
transition_matrices_homogeneous["Lev", "Recurrence-free", ]  <-  
  c(NA, 0.067, 0.13, 0)
transition_matrices_homogeneous["Lev", "Recurrence", ]       <-  
  c(0, NA, 0.13, 0.40)
transition_matrices_homogeneous["Lev", "Dead (All-cause)", ] <-  
  c(0, 0, NA, 0)
transition_matrices_homogeneous["Lev", "Dead (Cancer)", ]    <-  
  c(0, 0, 0, NA)

# For the Lev+5FU treatment option
transition_matrices_homogeneous["Lev+5FU", "Recurrence-free", ] <-  
  c(NA, 0.046, 0.13, 0)
transition_matrices_homogeneous["Lev+5FU", "Recurrence", ]      <-  
  c(0, NA, 0.13, 0.26)
transition_matrices_homogeneous["Lev+5FU", "Dead (All-cause)", ] <-  
  c(0, 0, NA, 0)
transition_matrices_homogeneous["Lev+5FU", "Dead (Cancer)", ]    <-  
  c(0, 0, 0, NA)

# Ensure the rows sum to 1
# Probability of remaining in current state is 1 minus probability of leaving
for(i_state in 1:length(state_names)) {
  transition_matrices_homogeneous[, i_state, i_state] <- 1 - 
    rowSums(transition_matrices_homogeneous[, i_state, -i_state])
}


## -----------------------------------------------------------------------------------------------------------------------------------------
transition_matrices_homogeneous["Observation", , ]
head(apply(transition_matrices_homogeneous, c(1), rowSums))


## -----------------------------------------------------------------------------------------------------------------------------------------
# Proportion of cohort in each state during first cycle
cohort_vector_first <- c(1, 0, 0, 0)
cohort_vector_second <- cohort_vector_first %*% 
  transition_matrices_homogeneous["Observation", , ] 
cohort_vector_third <- cohort_vector_second %*% 
  transition_matrices_homogeneous["Observation", , ] 


## -----------------------------------------------------------------------------------------------------------------------------------------
# Proportion of cohort in each state during first cycle
cohort_vector_first <- c(1, 0, 0, 0)
cohort_vector_second <- cohort_vector_first %*% 
  transition_matrices_homogeneous["Observation", , ] 
cohort_vector_third <- cohort_vector_second %*% 
  transition_matrices_homogeneous["Observation", , ] 


## -----------------------------------------------------------------------------------------------------------------------------------------
cohort_vector_second
cohort_vector_third


## -----------------------------------------------------------------------------------------------------------------------------------------
Markov_trace_homogeneous <- array(
  0, dim = c(n_treatments, n_cycles, n_states),
  dimnames = list(treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), 
                  state_names)
)

# All individuals start in Recurrence-free
Markov_trace_homogeneous[, 1, "Recurrence-free"] <- 1

# Use transition matrix to update Markov trace at each cycle
for (i_cycle in 1:(n_cycles - 1)){
  for(i_treatment in 1:n_treatments) {
    Markov_trace_homogeneous[i_treatment, i_cycle + 1, ] <-
      Markov_trace_homogeneous[i_treatment, i_cycle, ] %*%
      transition_matrices_homogeneous[i_treatment, , ]   
  }
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Present the first six cycles of the Markov trace for the observation arm
head(Markov_trace_homogeneous["Observation", , ])

# The rows of each cycle sum to 1, which means that all individuals occupy a 
# health state
head(apply(Markov_trace_homogeneous, c(1), rowSums)) 


## -----------------------------------------------------------------------------------------------------------------------------------------
state_costs <- array(0, dim = c(n_treatments, n_states), 
                   dimnames = list(treatment_names, state_names))

state_costs[ , "Recurrence-free"] <- 
  transition_matrices_homogeneous[, "Recurrence-free", "Recurrence"] * 40000
state_costs[, "Recurrence"]       <- 0
state_costs[, "Dead (Cancer)"]    <- 0
state_costs[, "Dead (All-cause)"] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
state_costs


## -----------------------------------------------------------------------------------------------------------------------------------------
state_qalys <- array(dim = c(n_states), dimnames = list(state_names))

# QALY associated with 1-year in each health state is the same as the utility
# for each of these states
state_qalys["Recurrence-free"]  <- 0.8
state_qalys["Recurrence"]       <- 0.6
state_qalys["Dead (Cancer)"]    <- 0
state_qalys["Dead (All-cause)"] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
state_qalys


## -----------------------------------------------------------------------------------------------------------------------------------------
# Probabilities of toxicity on each treatment
# Plus cost and disutility of toxicity
p_tox_lev           <- 0.20
p_tox_lev5fu        <- 0.40
cost_toxicity       <- 2000
disutility_toxicity <- -0.1

# Treatment Costs
treatment_costs <-
  array(dim = c(n_treatments), dimnames = list(treatment_names))

treatment_costs["Observation"] <- 0
treatment_costs["Lev"]         <- 5000  + p_tox_lev    * cost_toxicity
treatment_costs["Lev+5FU"]     <- 10000 + p_tox_lev5fu * cost_toxicity

# Treatment QALYs
treatment_qalys <- 
  array(dim = c(n_treatments), dimnames = list(treatment_names))

treatment_qalys["Observation"] <- 0
treatment_qalys["Lev"]         <- p_tox_lev * disutility_toxicity
treatment_qalys["Lev+5FU"]     <- p_tox_lev5fu * disutility_toxicity


## -----------------------------------------------------------------------------------------------------------------------------------------
treatment_costs
treatment_qalys


## -----------------------------------------------------------------------------------------------------------------------------------------
# Use a context specific discount factor
discount_factor <- 1.03

# Assign memory for the costs and QALYs for each treatment in each cycle
cycle_costs <-  array(
  dim = c(n_treatments, n_cycles), 
  dimnames = list(treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "))
)
cycle_qalys <-  array(
  dim = c(n_treatments, n_cycles), 
  dimnames = list(treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "))
)

# Assign memory for the total costs and QALYs for each treatment
total_costs <-  array(
  dim = c(n_treatments), dimnames = list(treatment_names)
)
total_qalys <-  array(
  dim = c(n_treatments), dimnames = list(treatment_names)
)

for(i_treatment in 1:n_treatments) {

  # State costs depend on treatment as depend on risk of recurrence
  cycle_costs[i_treatment, ] <- Markov_trace_homogeneous[i_treatment, , ] %*% 
    state_costs[i_treatment, ]
  # State QALYs do not depend on treatment
  cycle_qalys[i_treatment, ] <-  Markov_trace_homogeneous[i_treatment, , ] %*% 
    state_qalys[]

  # Combine the cycle_costs and treatment_costs to get total costs
  # Apply the discount factor 
  # (1 in first year,  1.03 in second,  1.03^2 in third,  and so on)
  total_costs[i_treatment] <-  treatment_costs[i_treatment] + 
    cycle_costs[i_treatment, ] %*% 
    (1 / discount_factor) ^ rep(c(0 : (n_cycles / 2 - 1)), each = 2)
  

  # Combine the discounted cycle_qalys and treatment_qalys to get total qalys
 total_qalys[i_treatment] <- treatment_qalys[i_treatment] +
   cycle_qalys[i_treatment, ] %*%
   (1 / discount_factor) ^ rep(c(0 : (n_cycles / 2 - 1)), each = 2)

}

v_nhb <-  100000 * total_qalys - total_costs
# Create a dataframe with the costs and effects.
# Round the costs to no decimals and effects (QALYs) to 3
df_results_cost_effects <- data.frame("Costs"   = round(total_costs),
                                      "Effects" = round(total_qalys, 3),
                                      "NHB"     = round(v_nhb))


## -----------------------------------------------------------------------------------------------------------------------------------------
df_results_cost_effects


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Incremental costs and effects relative to Observation (reference)
# Don't need incremental results for first/reference treatment as this is 
# the reference; both are more expensive than Observation
incremental_costs  <-  total_costs[-1] - total_costs["Observation"]
# Only Lev+5FU outperforms has greater QALYs than Observation
# Lev is dominated by Observation
incremental_effects <-  total_qalys[-1] - total_qalys["Observation"]

# The ICER for Lev+5FU is less than $100,000/QALY willingness to pay 
# Indicating that it is cost-effective compared to Observation
ICER <-  ifelse(
  incremental_costs / incremental_effects < 0, 
  NA, incremental_costs / incremental_effects
)

# Incremental net benefit at the $100,000/QALY willingness to pay
# Lev+5FU has greatest net benefit
incremental_net_benefit <-  100000 * incremental_effects - incremental_costs

# Now combine the results 
df_results <- data.frame(
  "Costs"       = round(total_costs),
  "Effects"     = round(total_qalys, 3),
  "incr_costs"   = c("-", round(incremental_costs)),
  "incr_effects" = c("-", round(incremental_effects,3)),
  "ICER"         = c("-", round(ICER)),
  "NHB"          = round(v_nhb),
  "incr_NHB"     = c("-", round(incremental_net_benefit))
)


## -----------------------------------------------------------------------------------------------------------------------------------------
incremental_costs
incremental_effects
incremental_net_benefit
df_results


## ----results = FALSE----------------------------------------------------------------------------------------------------------------------
# We initialize with all transitions set to zero
transition_matrices <- array(
  0, 
  dim = c(n_treatments, n_cycles, n_states, n_states), 
  dimnames = list(
    treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), 
    state_names, state_names
  )
)


## -----------------------------------------------------------------------------------------------------------------------------------------
# The shape of the Gompertz function that describes the all cause mortality 
shape_D_all_cause <- 0.088457189 
# The rate of the Gompertz functions that described the all-cause mortality
rate_D_all_cause  <- 0.008098087 

# Transition probability during first cycle
transition_matrices[, 1, "Recurrence-free", "Dead (All-cause)"] <- 
  transition_matrices[, 1, "Recurrence", "Dead (All-cause)"] <- 
  1 - exp(-Hgompertz(1, shape = shape_D_all_cause, rate = rate_D_all_cause))

# Transition probability during subsequent cycles
for(i_cycle in 2:n_cycles) {
  # Same probability of all-cause death in both states
  transition_matrices[, i_cycle, "Recurrence-free", "Dead (All-cause)"] <-
  transition_matrices[, i_cycle, "Recurrence", "Dead (All-cause)"] <- 
    # Conditional probability of death during cycle
    # Hgompertz is the cumulative hazard of a Gompertz distribution
    # 1 - exp(-Hgompertz) gives the survival probability up to each cycle
    1- exp(-Hgompertz(
      i_cycle, shape = shape_D_all_cause, rate = rate_D_all_cause
    )) / 
    exp(-Hgompertz(
      i_cycle - 1, shape = shape_D_all_cause, rate = rate_D_all_cause
    ))
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Define the mean values for parameters of the log-logistic cure model
recurrence_mean <- list()
# First is log odds of cure (theta), then are shape and scale of log-logistic
recurrence_mean[["Observation"]] <- c(-0.4397653, 0.4596922, 0.1379289 )
recurrence_mean[["Lev"]]         <- c(-0.3660914, 0.5413504, 0.1007300)
recurrence_mean[["Lev+5FU"]]     <- c(0.2965285,  0.5154467, 0.2704233)


for(treatment_name in treatment_names) {
  # name the items in the list
  names(recurrence_mean[[treatment_name]]) <- c("theta", "shape", "scale") 

  # Transition probability during first cycle
  transition_matrices[treatment_name, 1, 
                      "Recurrence-free", "Recurrence"] <- 
    # Probability of not being cured
    (1 - expit(recurrence_mean[[treatment_name]]["theta"])) *
    # Probability of recurrence using cumulative hazard of recurrence 
    (1 - exp(-Hllogis(
      1,shape = exp(recurrence_mean[[treatment_name]]["shape"]),
      scale = exp(recurrence_mean[[treatment_name]]["scale"])
    )))

  # Transition probabilities during subsequent cycles
  for(i_cycle in 2:n_cycles) {
    transition_matrices[treatment_name, i_cycle,
                        "Recurrence-free", "Recurrence"] <- 
      (1 - expit(recurrence_mean[[treatment_name]]["theta"]))  *
      # Recurrence probability conditional on no recurrence up to i_cycle-1
      (1 - (exp(-Hllogis(
        i_cycle, 
        shape = exp(recurrence_mean[[treatment_name]]["shape"]), 
        scale = exp(recurrence_mean[[treatment_name]]["scale"])
      )) / 
        exp(-Hllogis(
          i_cycle - 1, 
          shape = exp(recurrence_mean[[treatment_name]]["shape"]), 
          scale = exp(recurrence_mean[[treatment_name]]["scale"])
        ))))
  }
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Log rates of mortality on Observation and log rate ratios for interventions
log_cancer_mortality_obs <- -0.57335844
log_cancer_mortality_rr_lev <- 0.05479799
log_cancer_mortality_rr_lev5fu <- 0.05479799
# Probabilities of death due to cancer are fixed over time
# Calculated with log rate of mortality and log rate ratios
transition_matrices["Observation", , "Recurrence", "Dead (Cancer)"] <- 
  1 - exp(-exp(log_cancer_mortality_obs))
transition_matrices["Lev", , "Recurrence", "Dead (Cancer)"] <- 
  1 - exp(-exp(log_cancer_mortality_obs + log_cancer_mortality_rr_lev))
transition_matrices["Lev+5FU", , "Recurrence", "Dead (Cancer)"] <- 
  1 - exp(-exp(log_cancer_mortality_obs + log_cancer_mortality_rr_lev5fu))


# Ensure the rows sum to 1
# Probability of remaining in current state is 1 minus probability of leaving
for(i_state in 1:length(state_names)) {
  transition_matrices[, , i_state, i_state] <- 1 - 
    apply(
      transition_matrices[, , i_state, -i_state], c(1, 2), sum, na.rm = TRUE
    )
}

# we could use the code below to check that all rows sum to 1 
# apply(transition_matrices, c(1, 2), rowSums)



## -----------------------------------------------------------------------------------------------------------------------------------------
Markov_trace_inhomogeneous <- array(
  0, dim = c(n_treatments, n_cycles, n_states), 
  dimnames = list(
    treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), state_names
  )
)

# All individuals start in Recurrence-free
Markov_trace_inhomogeneous[, 1, 1] <- 1

# Use transition matrix to update Markov trace at each cycle
for (i_cycle in 1:(n_cycles - 1)){
  for(i_treatment in 1:n_treatments) {
    Markov_trace_inhomogeneous[i_treatment, i_cycle + 1, ] <-
      Markov_trace_inhomogeneous[i_treatment, i_cycle, ] %*%
      transition_matrices[i_treatment, i_cycle, , ]   
  }
}


## -----------------------------------------------------------------------------------------------------------------------------------------
head(Markov_trace_inhomogeneous["Observation", , ])


## -----------------------------------------------------------------------------------------------------------------------------------------
state_costs <- array(
  0, dim = c(n_treatments, n_cycles, n_states), 
  dimnames = list(
    treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), state_names
  )
)

state_costs[, , "Recurrence-free"] <- 
  transition_matrices[, , "Recurrence-free", "Recurrence"] * 40000
state_costs[, , "Recurrence"]       <- 0
state_costs[, , "Dead (Cancer)"]    <- 0
state_costs[, , "Dead (All-cause)"] <- 0



## -----------------------------------------------------------------------------------------------------------------------------------------
# Assign memory for the costs and QALYs for each treatment in each cycle
cycle_costs <- array(
  dim = c(n_treatments, n_cycles), 
  dimnames = list(treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "))
)
cycle_qalys <-array(
  dim = c(n_treatments, n_cycles), 
  dimnames = list(treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "))
)

# Assign memory for the total costs and QALYs for each treatment
total_costs <-  array(dim = c(n_treatments), 
                      dimnames = list(treatment_names))
total_qalys <-  array(dim = c(n_treatments), 
                      dimnames = list(treatment_names))

for(i_treatment in 1:n_treatments) {
# State costs depend on treatment & cycle; need sum of rows of an outer product
  cycle_costs[i_treatment, ] <- rowSums(
    Markov_trace_inhomogeneous[i_treatment, , ] *
    state_costs[i_treatment, , ]
  )
# State QALYs do not depend on treatment or cycle
  cycle_qalys[i_treatment, ] <- Markov_trace_inhomogeneous[i_treatment, , ] %*% 
    state_qalys[]
# Combine the cycle_costs and treatment_costs to get total costs. Apply the 
# discount factor (1 in first year, 1.03 in second, 1.03^2 in third, ...)
  total_costs[i_treatment] <-  treatment_costs[i_treatment] + 
    cycle_costs[i_treatment, ] %*% 
    (1 / discount_factor)^rep(c(0:(n_cycles / 2 - 1)), each = 2)
# Combine the discounted cycle_qalys and treatment_qalys to get total qalys
 total_qalys[i_treatment] <- treatment_qalys[i_treatment] +
   cycle_qalys[i_treatment, ] %*%
   (1 / discount_factor)^rep(c(0:(n_cycles / 2 - 1)), each = 2)
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Both are more expensive
(incremental_costs <-  total_costs[-1] - total_costs["Observation"])

# But only Lev+5FU outperforms Lev
(incremental_effects <-  total_qalys[-1] - total_qalys["Observation"])

# ICER and INB show Lev+5FU is a cost-effective option at $100,000/QALY
(ICER <- ifelse(
  incremental_costs / incremental_effects <0, 
  NA, 
  incremental_costs / incremental_effects)
)
(incremental_net_benefit <-  100000 * incremental_effects - incremental_costs)


## -----------------------------------------------------------------------------------------------------------------------------------------
n_samples <- 1000
# Set a random seed (starting seed itself arbitrary)
set.seed(25634773)


## -----------------------------------------------------------------------------------------------------------------------------------------
# There is one transition matrix for each treatment,cycle and sample
# Store them in an array with (before filling in below) NA entries
transition_matrices <- array(
  dim = c(n_treatments, n_cycles, n_samples, n_states, n_states), 
	dimnames = list(
	  treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), 
	  NULL, state_names, state_names
	)
)


## -----------------------------------------------------------------------------------------------------------------------------------------
# Recurrence follows a log-logistic 'cure' model and is treatment/time-dependent
recurrence_mean <- recurrence_cov <- recurrence_samples <- list()
recurrence_mean[["Observation"]] <- c(-0.4397653, 0.4596922, 0.1379289)

recurrence_cov[["Observation"]]  <- matrix(
  c(0.018536978, 0.003472046, -0.003681193,
    0.003472046,  0.006251446, -0.002619486,
    -0.003681193, -0.002619486,  0.008996133),
  nrow = 3, ncol = 3
)

recurrence_mean[["Lev"]] <- c(-0.3660914, 0.5413504, 0.1007300)

recurrence_cov[["Lev"]]  <- matrix(
  c(0.016490815, 0.002455212, -0.002092227,
    0.002455212, 0.006079560, -0.001808315,
    -0.002092227, -0.001808315, 0.007070765),
  nrow = 3, ncol = 3
)

recurrence_mean[["Lev+5FU"]] <- c(0.2965285, 0.5154467, 0.2704233)

recurrence_cov[["Lev+5FU"]]  <- matrix(
  c(0.017204092, 0.003652515, -0.003404183,
    0.003652515, 0.009739810, -0.003570708,
    -0.003404183, -0.003570708, 0.011417611),
  nrow = 3, ncol = 3
)


## -----------------------------------------------------------------------------------------------------------------------------------------
for(treatment_name in treatment_names) {
# Multivariate normal distribution for underlying parameters of recurrence model
  recurrence_samples[[treatment_name]] <- mvrnorm(
    n_samples, 
    mu = recurrence_mean[[treatment_name]],
    Sigma = recurrence_cov[[treatment_name]]
  )
  # Give the samples meaningful names to reduce risk of error
  names(recurrence_mean[[treatment_name]]) <- 
    colnames(recurrence_cov[[treatment_name]]) <- 
    rownames(recurrence_cov[[treatment_name]]) <- 
    colnames(recurrence_samples[[treatment_name]]) <- 
    c("theta", "shape", "scale")
  
  transition_matrices[treatment_name, 1, , "Recurrence-free", "Recurrence"] <- 
    (1 - expit(recurrence_samples[[treatment_name]][, "theta"])) *
    (1 - exp(
      -Hllogis(1, shape = exp(recurrence_samples[[treatment_name]][, "shape"]),
               scale = exp(recurrence_samples[[treatment_name]][, "scale"]))
    ))
  for(i_cycle in 2:n_cycles) {
    transition_matrices[
      treatment_name, i_cycle, , "Recurrence-free", "Recurrence"
    ] <- 
      (1 - expit(recurrence_samples[[treatment_name]][, "theta"]))  *
      (1 - (exp(-Hllogis(
        i_cycle, shape = exp(recurrence_samples[[treatment_name]][, "shape"]), 
        scale = exp(recurrence_samples[[treatment_name]][, "scale"])
      )) /
        exp(-Hllogis(
          i_cycle - 1, 
          shape = exp(recurrence_samples[[treatment_name]][, "shape"]), 
          scale = exp(recurrence_samples[[treatment_name]][, "scale"]))
        )))
  }
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Background mortality follows a Gompertz distribution fitted to US lifetables
# As the sample size is so large, we follow the common assumption that there is
# no uncertainty. It is the same regardless of treatment and recurrence but 
# is time-dependent. Probability death during  first cycle is 1 minus 
# probability of surviving 1 year
transition_matrices[, 1, , "Recurrence-free", "Dead (All-cause)"] <- 
  transition_matrices[, 1, , "Recurrence",      "Dead (All-cause)"] <- 
  1 - exp(-Hgompertz(1, shape = 0.088457189, rate = 0.008098087))

for(i_cycle in 2:n_cycles) {
  # Probability of death during cycle i is 1 minus probability of surviving from 
  # cycle i - 1 to cycle i
  transition_matrices[, i_cycle, , "Recurrence-free", "Dead (All-cause)"] <-
  transition_matrices[, i_cycle, , "Recurrence",      "Dead (All-cause)"] <- 
    1 - exp(-Hgompertz(i_cycle, shape = 0.088457189, rate = 0.008098087)) / 
    exp(-Hgompertz(i_cycle - 1, shape = 0.088457189, rate = 0.008098087))
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Cancer specific survival follows an exponential distribution and is 
# time-independent. These are correlated between treatments and we represent 
# their uncertainty using a multivariate Normal distribution
# Mean of log rate and log ratios
crr_lmean <- c(
  log_cancer_mortality_obs, log_cancer_mortality_rr_lev, 
  log_cancer_mortality_rr_lev5fu
)
# Covariance on log scale
crr_lcov <- matrix(c( 0.006451612, -0.006451612, -0.006451612,
                     -0.006451612,  0.013074127,  0.006451612,
                     -0.006451612,  0.006451612,  0.015710870), 
                   nrow = 3, ncol = 3)

# Sample from multivariate Normal to preserve correlation
crr_lsamples <- mvrnorm(n_samples, mu = crr_lmean, Sigma = crr_lcov)

# Give samples meaningful names to avoid errors
names(crr_lmean) <- colnames(crr_lcov) <- 
  rownames(crr_lcov) <- colnames(crr_lsamples) <- c(
    "Obs log rate", "Lev log RR", "Lev+5FU log RR"
  )

# Exponential distribution with parameters on log scale. Every cycle should 
# have the same probability but every sample should be different
transition_matrices["Observation", , , "Recurrence", "Dead (Cancer)"] <-
  rep(exp(-exp(crr_lsamples[, "Obs log rate"])), each = n_cycles)
transition_matrices["Lev", , , "Recurrence", "Dead (Cancer)"] <-
  rep(
    exp(-exp(rowSums(crr_lsamples[, c("Obs log rate", "Lev log RR")]))), 
    each = n_cycles
  )
transition_matrices["Lev+5FU", , , "Recurrence", "Dead (Cancer)"] <-
  rep(
    exp(-exp(rowSums(crr_lsamples[, c("Obs log rate", "Lev+5FU log RR")]))), 
    each = n_cycles
  )

# Cancer specific mortality is zero for individuals without recurrence
transition_matrices[, , , "Recurrence-free", "Dead (Cancer)"] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
# Ensure sum of probabilities of types of death do not exceed 1
# Add the death probabilities together
sum_death_probabilities <- apply(transition_matrices[, , , "Recurrence", 
                          c("Dead (All-cause)", "Dead (Cancer)")], 
      c(1,2,3), sum)
# Only apply a scale factor if this sum is greater than 1
# Otherwise set to 1 so no effect
sum_death_probabilities[sum_death_probabilities <= 1] <- 1

# Scale the probabilities for each type of death by the total 
transition_matrices[, , , "Recurrence", "Dead (All-cause)"] <- 
  transition_matrices[, , , "Recurrence", "Dead (All-cause)"] /
  sum_death_probabilities
transition_matrices[, , , "Recurrence", "Dead (Cancer)"] <-
  transition_matrices[, , , "Recurrence", "Dead (Cancer)"] /
  sum_death_probabilities


## -----------------------------------------------------------------------------------------------------------------------------------------
# Model is irreversible so no recovery from recurrence
transition_matrices[, , , "Recurrence", "Recurrence-free"] <- 0

# No transitions from dead states and all individuals stay dead
transition_matrices[, , , "Dead (Cancer)", ] <- 
  transition_matrices[, , , "Dead (All-cause)", ] <- 0

transition_matrices[, , , "Dead (Cancer)", "Dead (Cancer)"] <- 
  transition_matrices[, , , "Dead (All-cause)", "Dead (All-cause)"] <- 1

# Due to numerical instability, some probabilities will end up being -e-16
# So set to zero for safety
transition_matrices[transition_matrices < 0] <- 0


# Ensure probabilities from the state sum to one
for(i_state in 1:length(state_names)) {
  transition_matrices[, , , i_state, i_state] <- 1 - 
    apply(transition_matrices[, , , i_state, -i_state], 
          c(1, 2, 3), sum, na.rm = TRUE)
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# Build an array to store the cohort vector at each cycle
# Each cohort vector has 4 ( = n_states) elements
# There is one cohort vector for each treatment, cycle and PSA sample
Markov_trace <- array(0, dim = c(n_treatments, n_cycles, n_samples, n_states), 
                      dimnames = list(treatment_names, 
                                      paste("Cycle", 0:(n_cycles-1), 
                                            sep = " "), NULL, state_names))

# Assume that everyone starts in Recurrence-free
Markov_trace[, 1, , "Recurrence-free"] <- 1 

# Loop over the treatment options
for (i_treatment in 1:n_treatments) {
	# Loop over the samples
	for (i_sample in 1:n_samples) {
		# Loop over the cycles
		# Cycle 1 is already defined so only need to update cycles 2:n_cycles
		for (i_cycle in 2:n_cycles) {
			# Markov update
			Markov_trace[i_treatment, i_cycle, i_sample, ] <- 
				Markov_trace[i_treatment, i_cycle - 1, i_sample, ] %*%
				transition_matrices[i_treatment, i_cycle, i_sample, , ]
		}  # close loop for cycles
	}
}


## -----------------------------------------------------------------------------------------------------------------------------------------
# These depend on cycle and treatment as the Recurrence-free cost depends
# on the (cycle and treatment dependent) probability of recurrence
state_costs<-array(0, dim = c(n_treatments, n_cycles, n_samples, n_states), 
                   dimnames = list(treatment_names, 
                                   paste("Cycle", 0:(n_cycles-1), 
                                         sep = " "), NULL, state_names))

# Cost of Recurrence-free is cost of one-off advanced treatment 
# by probability of experiencing recurrence
# This probability is uncertain so this uncertainty must be propagated
state_costs[, , , "Recurrence-free"] <- 
  transition_matrices[, , , "Recurrence-free", "Recurrence"] * 40000
state_costs[, , , "Recurrence"]       <- 0
state_costs[, , , "Dead (Cancer)"]    <- 0
state_costs[, , , "Dead (All-cause)"] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
# Now define the QALYS associated with the states per cycle
# There is one for each PSA sample and each state
# Store in an NA array and then fill in below
state_qalys <- array(dim = c(n_samples, n_states), 
                     dimnames = list(NULL, state_names))

# QALY associated with 1-year in each health state
# No SD reported so use 10% of mean
# Utility disease free is 0.8
state_qalys[, "Recurrence-free"]  <- rnorm(n_samples, 0.8, sd = 0.1 * 0.8)
# Utility during advanced treatment is 0.6
state_qalys[, "Recurrence"]       <- rnorm(n_samples, 0.6, sd = 0.1 * 0.6)
state_qalys[, "Dead (Cancer)"]    <- 0
state_qalys[, "Dead (All-cause)"] <- 0


## -----------------------------------------------------------------------------------------------------------------------------------------
# Define the treatment costs
# One for each  sample and each treatment
treatment_costs <- treatment_qalys <- 
  array(
    dim = c(n_treatments, n_samples), dimnames = list(treatment_names, NULL)
  )

# Probabilities, costs and disutility of toxicity
# No SD reported so use 10% of the mean
p_tox_lev           <- rnorm(n_samples, mean = 0.20, sd = 0.1 * 0.20)
p_tox_lev5fu        <- rnorm(n_samples, mean = 0.40, sd = 0.1 * 0.40)
cost_toxicity       <- rnorm(n_samples, mean = 2000, sd = 0.1 * 2000)
disutility_toxicity <- rnorm(n_samples, mean = -0.1, sd = 0.1 * 0.1)

# Assign treatment costs and qalys for all samples simultaneously
treatment_costs["Observation", ] <- 0
treatment_costs["Lev", ]         <- 5000  + p_tox_lev   * cost_toxicity
treatment_costs["Lev+5FU", ]     <- 10000 + p_tox_lev5fu * cost_toxicity

treatment_qalys["Observation", ] <- 0
treatment_qalys["Lev", ]         <- p_tox_lev    * disutility_toxicity
treatment_qalys["Lev+5FU", ]     <- p_tox_lev5fu * disutility_toxicity


## -----------------------------------------------------------------------------------------------------------------------------------------
# Build an array to store costs & QALYs accrued per cycle, one for each 
# treatment & each PSA sample, for each cycle. Will be filled below in the main 
# model code & discounted + summed to contribute to total costs & total QALYs
cycle_costs <- array(
  dim = c(n_treatments, n_cycles, n_samples), 
  dimnames = list(
    treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), NULL
  )
)
cycle_qalys <- array(
  dim = c(n_treatments, n_cycles, n_samples), 
	dimnames = list(
	  treatment_names, paste("Cycle", 0:(n_cycles-1), sep = " "), NULL
	)
)
# Build arrays to store total costs & QALYs, one for each treatment & PSA 
# sample, filled below using cycle_costs, treatment_costs and cycle_qalys
total_costs <- array(dim = c(n_treatments, n_samples), 
	dimnames = list(treatment_names, NULL))
total_qalys <- array(dim = c(n_treatments, n_samples), 
	dimnames = list(treatment_names, NULL))

# Main model code
# Loop over the treatment options
for (i_treatment in 1:n_treatments) {
# Loop over the PSA samples
	for (i_sample in 1:n_samples) {
# Loop over the cycles. 
# Cycle 1 is already defined so only need to update cycles 2:n_cycles

# Now use the cohort vectors to calculate total costs for each cycle
		cycle_costs[i_treatment, , i_sample] <- 
		  x <- rowSums(Markov_trace[i_treatment, , i_sample, ] * 
		  state_costs[i_treatment, , i_sample, ])
# And total QALYs for each cycle
		cycle_qalys[i_treatment, , i_sample] <- 
		  Markov_trace[i_treatment, ,i_sample, ] %*% state_qalys[i_sample, ]

# Combine cycle_costs & treatment_costs to get total costs
# Discount factor: (1 in first year, 1_035 in second, 1_035^2 in third, ...)
		total_costs[i_treatment, i_sample] <- 
		  treatment_costs[i_treatment, i_sample] + 
		  cycle_costs[i_treatment, , i_sample] %*%
			(1 / discount_factor) ^ c(0:(n_cycles - 1))

# Combine the discounted cycle_qalys to get total_qalys
		total_qalys[i_treatment, i_sample] <- 
		  treatment_qalys[i_treatment, i_sample] +
		  cycle_qalys[i_treatment, , i_sample]%*%
		  (1 / discount_factor) ^ c(0:(n_cycles - 1))
	} # close loop for PSA samples
} # close loop for treatments


## -----------------------------------------------------------------------------------------------------------------------------------------
# Use base R functions to summarise results
# Both interventions have higher costs than observation
(incremental_costs <- rowMeans(total_costs[-1, ] - total_costs[1, ]))
# Only Lev+5FU has greater effects than observation, so Lev is again dominated
(incremental_effects <- rowMeans(total_qalys[-1, ] - total_qalys[1, ]))
# The ICER and incremental net benefit indicate Lev+5FU is cost-effective
# compared to observation at $100,000/QALY
(ICER <- ifelse(
  incremental_costs / incremental_effects <0, NA, 
  incremental_costs / incremental_effects
))
(incremental_net_benefit <- incremental_effects * 100000 - incremental_costs)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: true
#| label: fig-bcea
#| fig-width: 7
#| fig-height: 5
#| fig-pos: "H"
#| layout-ncol: 2
#| fig-cap: 'Graphical output from `BCEA`'
#| fig-subcap: 
#|   - Cost-effectiveness plane
#|   - Cost-effectiveness acceptability curve

colon_bcea <- bcea(
  e = t(total_qalys), c = t(total_costs), ref = 1, 
  interventions = treatment_names, Kmax = 200000
) 
ceplane.plot(colon_bcea, wtp = 100000, graph = "ggplot2")
colon_multi_ce <- multi.ce(colon_bcea)
ceac.plot(
  colon_multi_ce, graph = "ggplot2",
  line = list(color = c("red", "green", "blue")),
  pos = c(1, 0.50)
)

