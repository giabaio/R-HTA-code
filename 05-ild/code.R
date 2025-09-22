## ----warning = F, message = F-------------------------------------------------------------------------------------------------------------
#| echo: false

## Load required packages
library(tidyverse)

## Specify file location 
wd <- getwd()
dir_data <- file.path(wd, "data")

## Read in dataset; sample of first 5 participant data
df <- read.csv(file.path(dir_data, "10TT_synth_280921.csv")) %>% 
  ## Convert categorical variables to factors
  mutate(across(c("arm", "sex", "bmicat"), as.factor))


## ----warning = F, message = F-------------------------------------------------------------------------------------------------------------
#| echo: false
df |> as_tibble() |> print(n=5)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true

tableone::CreateTableOne(
  vars = c("sex", "age", "bmicat"), strata = "arm", data = df, test = F
)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true

## Create new data frame df_label for demonstration
df_label <- df 

## Factorise treatment arm variable
## Assign labels and order to respective levels
df_label$arm <- factor(
  df_label$arm, levels = c("1", "0"),
  labels = c("10TT", "Usual Care"), ordered = T
)

tableone::CreateTableOne(
  vars = c("sex", "age", "bmicat"), strata = "arm", 
  data = df_label, test = F
)


## ----fig.cap = "Trend in QoL over time in each treatment arm", message = F----------------------------------------------------------------
#| echo: true
#| fig-width: 5
#| fig-height: 3
df_qol <- df %>% 
  ## select columns of interest
  select(id, arm, contains("qol")) %>% 
  ## convert dataframe from wide to long format
  pivot_longer(
    contains("qol"), names_to = "month", names_prefix = "qol_", 
    names_transform = list(month = as.integer), values_to = "qol"
  ) 

df_qol %>% 
  ## group by treatment arm and month 
  group_by(arm, month) %>% 
  ## calculate mean and standard error by treatment arm and month 
  summarise(
    qol_mean = mean(qol, na.rm = T), 
    qol_se = sqrt(var(qol, na.rm = T)/length(qol))
  ) %>% 
  ## Applies 'ggplot' to this dataset
  ggplot(aes(color = arm, group = arm)) + 
  ## add data points and error bars
  geom_point(aes(x = month, y = qol_mean, shape = arm)) + 
  geom_errorbar(
    aes(x = month, ymin = qol_mean - 1.96 * qol_se, 
        ymax = qol_mean + 1.96 * qol_se)
  ) + 
  ## add lines to connect points 
  geom_line(aes(x = month, y = qol_mean, group = arm, linetype = arm)) + 
  ## add labels and break points for axes
  labs(x = "Month", y = "Mean QoL at each study visit (95% CI)") + 
  scale_x_continuous(breaks = c(0, 3, 6, 12, 18, 24))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true

## Function to perform discounting. Default discount rate set at 3.5% 
disc <- function(x, year, disc_rate = 0.035) {
  x / ((1 + disc_rate)^(year - 1))
}

## Apply discounting to QoL data
df_qol_disc <- df_qol %>% 
  ## convert month to follow-up year; set baseline to be in year 1
  mutate(year = pmax(1, ceiling(month/12))) %>% 
  ## apply discounting
  mutate(qol_disc = disc(qol, year)) 

## Example of QoL data for participant id 3
df_qol_disc %>% 
  dplyr::filter(id == 3)


## ----fig.cap="Example of utility profile for participant ID 3. QALYs is the shaded region under the utility profile."---------------------
#| label: fig-utility-id3
#| fig-width: 5
#| fig-height: 3

## Plot QoL data over time for participant id 3
ggplot(data = df_qol %>% dplyr::filter(id == 3)) + 
  geom_line(aes(x = month, y = qol)) + 
  ## add shading below line
  geom_ribbon(
    aes(x = month, ymax = qol, ymin = 0), fill = "red", alpha = 0.2
  ) + 
  ## add labels and break points for axes
  labs(x = "Month", y = "QoL") + 
  scale_x_continuous(breaks = c(0, 3, 6, 12, 18, 24))



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
df_qaly <- df_qol %>% 
  ## group data by participant id, and subsequent manipulation 
  ## is done for each participant 
  group_by(id) %>% 
  ## exclude participants with missing qol at any visit
  dplyr::filter(!any(is.na(qol))) %>% 
  ## calculate area under the utility profile (using DescTools::AUC)
  summarise(
    qaly = DescTools::AUC(x = month, y = qol, method = "trapezoid") / 12
  )
summary(df_qaly$qaly)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
df_qol_analysis <- df_qol %>% 
  select(id, month, qol) %>% 
  ## convert dataframe from long to wide format
  pivot_wider(
    names_from = "month", values_from = "qol", names_prefix = "qol_"
  ) %>% 
  ## merge with qalys calculated before
  left_join(df_qaly, by = "id")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
## Example of cost data
df_cost_analysis <- df %>% 
  select(id, totalcost) %>% 
  rename(cost = totalcost)


## -----------------------------------------------------------------------------------------------------------------------------------------
all.equal(df$gpvis * 45 + df$costint + df$costoth, df$totalcost)


## ----fig.cap= "Distribution of total cost", warning = F-----------------------------------------------------------------------------------
#| echo: false
#| fig-pos: "H"
#| fig-width: 5
#| fig-height: 3
ggplot(data = df_cost_analysis) + 
  geom_histogram(aes(x = cost), binwidth = 250) + 
  labs(x = "Total cost (GBP)")



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
df_analysis_withmissing <- df %>% 
  ## create a dataframe with baseline characteristics
  select(id, arm, sex, age, bmicat) %>% 
  ## merge quality of life data
  left_join(df_qol_analysis, by = "id") %>% 
  ## merge cost data
  left_join(df_cost_analysis, by = "id")

# Checks for missing data
df_analysis_withmissing %>% 
  summarise(across(c("qaly", "cost"), ~ sum(is.na(.))))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
df_analysis <- df_analysis_withmissing %>% 
  drop_na() 

tableone::CreateTableOne(
  strata = "arm", data = df_analysis %>% select(-id), test = F
)



## ----fig.cap= "Total cost and QALYs over 2 years of follow-up stratified by treatment arm"------------------------------------------------
#| echo: false
#| label: fig-qaly-cost-marginals
#| fig-width: 5
#| fig-height: 3
p <- ggplot(data = df_analysis, aes(group = arm, color = arm, fill = arm)) + 
  geom_point(aes(x = cost, y = qaly)) + 
  theme(legend.position = "bottom") + 
  labs(x = "Total cost (GBP)", y = "QALYs")

ggExtra::ggMarginal(p, groupColour = T, groupFill = T, alpha = 0.25)


## ----unadjustedCost-----------------------------------------------------------------------------------------------------------------------
#| echo: true
# run model regressing cost on the trial arm
m_cost <- lm(cost ~ arm, data = df_analysis)
# display coefficients
summary(m_cost)$coefficients


## ----unadjustedQoL------------------------------------------------------------------------------------------------------------------------
#| echo: true
# modelling difference in utilities
m_qol <- lm(qaly ~ arm, data = df_analysis)
summary(m_qol)$coefficients


## ----unadjustedManual---------------------------------------------------------------------------------------------------------------------
#| echo: false
# calculate mean costs & QALYs in each arm
df_analysis %>%
  group_by(arm) %>%
  summarise(mean(qaly), mean(cost))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: true
myprint.htest = function (x, digits = getOption("digits"), prefix = "\t", ...) {
    cat("\n")
    cat(strwrap(x$method, prefix = prefix), sep = "\n")
    cat("\n")
    cat("data:  ", x$data.name, "\n", sep = "")
    out <- character()
    if (!is.null(x$statistic)) 
        out <- c(out, paste(names(x$statistic), "=", format(x$statistic, 
            digits = max(1L, digits - 2L))))
    if (!is.null(x$parameter)) 
        out <- c(out, paste(names(x$parameter), "=", format(x$parameter, 
            digits = max(1L, digits - 2L))))
    if (!is.null(x$p.value)) {
        fp <- format.pval(x$p.value, digits = max(1L, digits - 
            3L))
        out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", 
            fp)))
    }
    cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
    if (!is.null(x$alternative)) {
        cat("alternative hypothesis: \n  ")
        if (!is.null(x$null.value)) {
            if (length(x$null.value) == 1L) {
                alt.char <- switch(x$alternative, two.sided = "not equal to", 
                  less = "less than", greater = "greater than")
                cat("true ", names(x$null.value), " is ", alt.char, 
                  " ", x$null.value, "\n", sep = "")
            }
            else {
                cat(x$alternative, "\nnull values:\n", sep = "")
                print(x$null.value, digits = digits, ...)
            }
        }
        else cat(x$alternative, "\n", sep = "")
    }
    if (!is.null(x$conf.int)) {
        cat(format(100 * attr(x$conf.int, "conf.level")), " percent confidence interval:\n", 
            " ", paste(format(x$conf.int[1:2], digits = digits), 
                collapse = " "), "\n", sep = "")
    }
    if (!is.null(x$estimate)) {
        cat("sample estimates:\n")
        print(x$estimate, digits = digits, ...)
    }
    cat("\n")
    invisible(x)
}

## ----imbalances---------------------------------------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

# ### Check for imbalances
# # imbalance in sex between treatment arms
# t.test(as.numeric(sex) ~ arm, data = df_analysis)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
myprint.htest(t.test(as.numeric(sex) ~ arm, data = df_analysis))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # imbalance in baseline age between treatment arms
# t.test(age ~ arm, data = df_analysis)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
myprint.htest(t.test(age ~ arm, data = df_analysis))

## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # imbalance in baseline utilities between treatment arms
# t.test(qol_0 ~ arm, data = df_analysis)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
myprint.htest(t.test(qol_0 ~ arm, data = df_analysis))


## ----adjusted-----------------------------------------------------------------------------------------------------------------------------
#| echo: true

### Adjusted analysis
# modelling difference in costs (adjusted for age & gender)
m_cost <- lm(cost ~ arm + sex + age, data = df_analysis)
summary(m_cost)$coefficients

# modelling difference in utilities (adjusted for age & gender)
m_qol <- lm(qaly ~ arm + sex + age + qol_0, data = df_analysis)
summary(m_qol)$coefficients


## ----dfInc--------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# name resulting variable df_inc
df_inc <- df_analysis %>%
  # predict cost & qaly from regressions
  # use command `predict` applied to an lm object
  mutate(cost_predict = predict(m_cost),
         qaly_predict = predict(m_qol)) %>%
  # produce summary statistics by the treatment arm variable
  group_by(arm) %>%
  # summarise mean cost and utility differences
  summarise(cost = mean(cost_predict), qaly = mean(qaly_predict),
            .groups = 'drop') %>%
  # reshape into a wide format
  pivot_longer(cols = c("cost", "qaly")) %>%
  pivot_wider(names_from = c(name, arm), values_from = value) %>%
  # calculate incremental cost and utilities
  mutate(cost_inc = cost_1 - cost_0,
         qaly_inc = qaly_1 - qaly_0
  )


## ----nb-----------------------------------------------------------------------------------------------------------------------------------
#| echo: true

# vector with willingness to pay thresholds
r <- c(20000, 30000)

# transform r into a dataframe for subsequent merging
df_r <- tibble(r = r)

# name resulting variable df_nb
df_nb <- merge(df_inc, df_r) %>%
  # calculate NMB
  mutate(nb = qaly_inc * r - cost_inc)

# view results
df_nb


## ----set_seed-----------------------------------------------------------------------------------------------------------------------------
#| echo: true
set.seed(123)



## ----functions----------------------------------------------------------------------------------------------------------------------------
#| echo: true
get_inc <- function(df) {
  # function to summarise cost and utility differences by arm
  # Argument: df (dataframe, e.g. df_analysis)
  # Here, the dataframe must contain "arm" with values 1 & 0, 
  # "cost_disc" and "qaly_disc" columns
  # However all that can be parameterised to create a more flexible function
  df_inc <- df %>%
    # produce summary statistics by the treatment arm variable
    group_by(arm) %>%
    # summarise mean cost and utility differences
    summarise(cost = mean(cost), qaly = mean(qaly),
              .groups = 'drop') %>%
    # reshape into a wide format
    pivot_longer(cols = c("cost", "qaly")) %>%
    pivot_wider(names_from = c(name, arm), values_from = value) %>%
    # calculate incremental cost and utilities
    mutate(cost_inc = cost_1 - cost_0,
           qaly_inc = qaly_1 - qaly_0
    )
  return(df_inc)
}

# function to summarise cost and utility differences by arm
# Arguments: df_inc (dataframe; as generated by get_inc); 
# r (vector of thresholds)

get_nb <- function(df_inc, r) {
  df_r <- tibble(r = r)
  df_nb <- merge(df_inc, df_r) %>%
    mutate(nb = qaly_inc * r - cost_inc)
  return(df_nb)
}



## ----bootstrap----------------------------------------------------------------------------------------------------------------------------
#| echo: true
# number of simulations
n_sim <- 200

# initialise output dataset
df_boot <- NULL

# loop across simulations s in 1:n_sim
for (s in 1:n_sim) {
  # sample patients from df_analysis
  df_analysis_s <- slice_sample(
    df_analysis, n = nrow(df_analysis), replace = TRUE
  )
  # calculate incremental cost & utility differences by simulation
  df_inc_s <- get_inc(df = df_analysis_s) %>%
    # add simulation number
    mutate(sim = s) %>%
    # re-arrange columns
    select(sim, everything())
  # add to the output
  df_boot <- rbind(df_boot, df_inc_s)
}
head(df_boot, 3)


## ----nbBoot-------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# vector with thresholds
r <- seq(from = 0, to = 100000, by = 100)

# calculate NMB
df_boot <- get_nb(df_inc = df_boot,  r = r)

# view result
head(df_boot, 3)



## ----bootstrapCI--------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Function: get confidence interval from a vector using a percentile method
# Arguments: vector with outcomes (vec; e.g. cost_delta_0); confidence level 
# (level; optional argument with default value of 0.95)
get_bootstrap_ci <- function(vec, level = 0.95) {
  # order the vector
  vec <- sort(vec)
  n <- length(vec)
  # calculate percentile to be cut off
  temp <- (1 - level) / 2
  # read off values at respective coordinates
  lower <- vec[max(floor(n * temp), 1)]
  upper <- vec[min(ceiling(n * (1 - temp)), n)]
  return(list(l = lower, u = upper))
}



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# create a copy of the original dataset (optional step)
df_inc_with_ci <- df_inc

# loop through all columns of df_inc
for (colname in colnames(df_inc)) {
  # uncertainty estimate for a given columns
  v_ci <- get_bootstrap_ci(df_boot[, colname])
  # numbers of digits to round the output to
  # 0 for cost columns; 1 for QALY columns
  # could re-write this as a function too
  digits <- if (grepl("cost", colname)) 0 else
    if (grepl("qaly", colname)) 1
  # add uncertainty estimate to the point estimate
  df_inc_with_ci[, colname] <- str_c(
    round(df_inc_with_ci[, colname], digits = digits), 
    " (", 
    round(v_ci$l, digits = digits), 
    "; ", 
    round(v_ci$u, digits = digits),
    ")"
  )
}

# view the output and potentially save as a .csv
df_inc_with_ci |> t()


## ----cePlane------------------------------------------------------------------------------------------------------------------------------
#| echo: false
xmax <- max(abs(df_boot$qaly_inc))
ymax <- max(abs(df_boot$cost_inc))
p <- ggplot(df_boot, aes(x = qaly_inc, y = cost_inc)) + 
  geom_point() + 
  xlab("Incremental QALYs") + ylab("Incremental Cost") + 
  coord_cartesian(xlim = c(-xmax, xmax), ylim = c(-ymax, ymax)) + 
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  theme_bw()
p


## ----ceacCE-------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: false
# # for the simulation to be cost-effective, need
# # (1) EITHER QALY effect to be positive and NMB to be non-negative
# # (2) OR QALY effect to be negative and NMB to be non-positive
# df_boot <- df_boot %>%
#   mutate(ce = (qaly_inc > 0 & nb <= 0) | (qaly_inc < 0 & nb >= 0))
# head(df_boot, 3)


## ----ceacP--------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# calculate probability of being cost-effective for each threshold r
# equivalent to the proportion of CE column entries that are equal to 1
# Since CE column entries are either 1 or 0, this is equivalent to the mean
df_ceac <- df_boot %>%
  group_by(r) %>%
  summarise(p_ce = mean(nb>0))
head(df_ceac, 3)


## ----ceacPlot-----------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 5
#| out-width: "75%"
p <- ggplot(df_ceac, aes(x = r, y = p_ce)) +
  # connect observations
  geom_line() +
  # change y-axis limits
  coord_cartesian(ylim = c(0, 1)) +
  # change axis titles
  xlab("willingness to pay threshold") +
  ylab("Probability of the treatment being cost-effective") +
  # display probability labels as percentages
  scale_y_continuous(labels = scales::percent)
p



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
library(rjags)
data <- read.csv("data/10TT_synth_280921.csv")
set.seed(301031)

# adjusted analysis
# clean up jags code
# cross-check my Bayesian model with missing data chapter

# flesh out text



## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| label: fig-hist-qqplot
#| fig-cap: "Histogram and QQplot for the distribution of total costs"
#| fig-subcap: 
#|   - ""
#|   - ""
#| layout-ncol: 2
data$log_totalcost <- log(data$totalcost)
hist(data$log_totalcost,main="Histogram of log-costs",xlab="log costs")
qqnorm(data$log_totalcost)
qqline(data$log_totalcost)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| label: fig-hist-qqplot2
#| fig-cap: "Histogram and QQplot for the distribution of total costs"
#| fig-subcap: 
#|   - ""
#|   - ""
#| layout-ncol: 2
hist(data$qol_0,main="Histogram of QoL",xlab="QoL")
qqnorm(data$qol_0); qqline(data$qol_0)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
QOL <- cbind(
  data$qol_0,data$qol_3,data$qol_6,data$qol_12,data$qol_18,data$qol_24
)
Upper <- matrix(as.numeric(QOL==1),ncol=6)
for(j in 1:6) {
  QOL[QOL[,j]==1,j] <- NA
}


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
Outcomes0 <- cbind(data$log_totalcost,QOL)[data$arm==0,]
Outcomes1 <- cbind(data$log_totalcost,QOL)[data$arm==1,]

Upper0 <- Upper[data$arm==0,]
Upper1 <- Upper[data$arm==1,]


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
QOL_inits <- array(dim=dim(QOL))
for(j in 1:6) {
  QOL_inits[Upper[,j]==1,j] <- 1 + runif(1,0,1)/10
}
QOL_inits0 <- QOL_inits[data$arm==0,]
QOL_inits1 <- QOL_inits[data$arm==1,]
Outcomes_inits0 <- cbind(rep(NA,nrow(Outcomes0)), QOL_inits0)
Outcomes_inits1 <- cbind(rep(NA,nrow(Outcomes1)), QOL_inits1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
#| eval: false

# model{
#   # Arm 0
#   for(id in 1:n0){    # Loops over the `n0` patients in arm 0
#     Outcomes0[id,1] ~ dnorm(mu_0[1],tau0[1])
#     for(col in 2:7){
#       Outcomes0[id,col] ~ dnorm(mu_0cond[id,col],tau0cond[col])
#     }
#     for(int in 2:7){
#       # captures whether qol_0 is at the upper bound of 1
#       Upper_0[id,int-1] ~ dinterval(Outcomes0[id,int], 1)
#     }
#   }
#   # conditional means and variances
#   for(id in 1:n0){
#     mu_0cond[id,1] <- mu_0[1]
#     mu_0cond[id,2] <- mu_0[2] + (Outcomes0[id,1] - mu_0[1])*
#       Sigma_0[2,1]/Sigma_0[1,1]
#     for(col in 3:7){
#       mu_0cond[id,col] <- mu_0[col] + Sigma_0[col,1:(col-1)]%*%
#         inverse(Sigma_0[1:(col-1),1:(col-1)])%*%(
#           Outcomes0[id,1:(col-1)] - mu_0[1:(col-1)]
#         )
#     }
#   }
#   # conditional *precisions* (to feed to the `dnorm` call above)
#   tau0cond[1] <- tau0[1];
#   tau0cond[2] <- 1/(Sigma_0[2,2] - Sigma_0[2,1]*Sigma_0[1,2]/Sigma_0[1,1])
#   for(col in 3:7){
#     tau0cond[col] <- 1/(Sigma_0[col,col] - Sigma_0[col,1:(col-1)]%*%inverse(
#       Sigma_0[1:(col-1),1:(col-1)]
#     ) %*% Sigma_0[1:(col-1),col])
#   }
#   # Compute mean QoL
#   for(int in 1:6){
#     qol_0_mean[int] = 1 - (1-mu_0[int+1])*pnorm(1,mu_0[int+1],tau0[int+1]) -
#       sigma0[int+1]*dnorm(1,mu_0[int+1],tau0[int+1])
#   }
#   # Compute total mean QoL
#   qaly_0 <- 0.5*(qol_0_mean[1] + qol_0_mean[2])*3/12 +
#     0.5*(qol_0_mean[2] + qol_0_mean[3])*3/12 +
#     0.5*(qol_0_mean[3] + qol_0_mean[4])*6/12 +
#     0.5*(qol_0_mean[4] + qol_0_mean[5])*(6/12)+
#     0.5*(qol_0_mean[5] + qol_0_mean[6])*(6/12)
#   # Compute mean total cost
#   cost_0 <- exp(mu_0[1] + 0.5*Sigma_0[1,1])
#   # Priors
#   mu_0[1] ~ dnorm(10,0.01)
#   for(i in 2:7){
#     mu_0[i] ~ dnorm(0.5,16)
#   }
#   for(i in 1:7){
#     D_0[i,i] ~ dgamma(1,1); sqrt.D0[i,i] <- sqrt(D_0[i,i])
#   }
#   for(i in 1:6){
#     for(j in (i+1):7){
#       D_0[i,j] <- 0;  L_0[i,j] <- 0;  sqrt.D0[i,j] <- 0
#     }
#   }
#   L_0[1,1] <- 1;  L_0[2,1] <- cos(phi0[1,2]);  L_0[2,2] <- sin(phi0[1,2])
#   D_0[2,1] <- 0;  sqrt.D0[2,1] <- 0
#   for(i in 3:7){
#     L_0[i,1] <- cos(phi0[i-1,2]);    D_0[i,1] <- 0; sqrt.D0[i,1] <- 0
#     for(j in 2:(i-1)){
#       D_0[i,j] <- 0
#       L_0[i,j] <- prod(sin(phi0[i-1,2:j]))*cos(phi0[i-1,j+1])
#       sqrt.D0[i,j] <- 0
#     }
#     L_0[i,i] <- prod(sin(phi0[i-1,2:i]))
#   }
#   for(i in 1:6){
#     for(j in 1:7){
#       phi0[i,j] ~ dunif(0,3.1415)
#     }
#   }
#   Sigma_0 <- sqrt.D0%*%L_0%*%t(L_0)%*%sqrt.D0
#   # Compute variance matrix
#   for(i in 1:7){
#     sigma0[i] <- sqrt(Sigma_0[i,i]);    tau0[i] <- 1/Sigma_0[i,i]
#   }
#   R_0 <- L_0%*%t(L_0)
# 
#   # Now repeat for Arm 1 (change suffix `_0` to `_1`)
#   ...
# 
#   # Then compute incremental costs and effects
#   qaly_inc <- qaly_1 - qaly_0;      cost_inc <- cost_1 - cost_0
# }


## ----cache=FALSE,eval=FALSE---------------------------------------------------------------------------------------------------------------
# # Sets the initial values for the model parameters
# jags.inits <- list(Outcomes0=Outcomes_inits0,Outcomes1=Outcomes_inits1)
# # Defines the data as a named 'list'
# jags.data <- list(
#   Outcomes0=Outcomes0,Outcomes1=Outcomes1,n0=nrow(Outcomes0),
#   n1=nrow(Outcomes1),Upper_0=Upper0,Upper_1=Upper1
# )
# # Runs JAGS in the background
# model <- jags.model(
#   "jags.script.txt", data=jags.data, inits=jags.inits, n.chains=1
# )
# # Updates the model with 50000 iterations
# update(model, n.iter=50000)


## ----cache=FALSE,eval=FALSE---------------------------------------------------------------------------------------------------------------
# # Saves the results of 1000 iterations in an object 'samples'
# samples <- coda.samples(
#   model,variable.names=c(
#     "phi0[3,1]","phi0[6,4]","phi1[2,1]","phi1[4,2]","sigma0[1]","sigma0[4]",
#     "sigma1[2]","sigma1[7]","cost_0","qaly_0","cost_1","qaly_1","qaly_inc",
#     "cost_inc"
#   ),n.iter=1000
# )
# # Stores the MCMC samples in a R dataset
# dput(samples,"data/samples0.Rdata")


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
#| echo: false
samples <- dget("data/samples0.Rdata")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
#| eval: false
# # Geweke diagnostics
# coda::geweke.diag(samples,frac1=0.1,frac2=0.5)
# 
# # Raftery-Lewis Diagnostic, 2.5th and 97.5 percentile, respectively
# coda::raftery.diag(samples,r=1,q=0.025)
# coda::raftery.diag(samples,r=1,q=0.975)
# 
# # Heidelberger-Welch Diagnostic
# coda::heidel.diag(samples)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: false
# print("Geweke Diagnostic"); geweke.diag(samples,frac1=0.1,frac2=0.5)
# print("Raftery-Lewis Diagnostic, 2.5th percentile"); raftery.diag(samples,r=1,q=0.025)
# print("Raftery-Lewis Diagnostic, 97.5th percentile");raftery.diag(samples,r=1,q=0.975)
# print("Heidelberger-Welch Diagnostic"); heidel.diag(samples)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------
#| echo: true
# # Run the model for 4000 more iterations, with thinning
# samples2 <- coda.samples(
#   model, variable.names=c(
#     "cost_0","qaly_0","cost_1","qaly_1","qaly_inc","cost_inc"
#   ),
#   n.iter=4000,thin=20
# )
# # Show some summary statistics
# summary(samples2)

## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
#| echo: false
samples2 <- dget("data/samples2.Robject")

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
# Show some summary statistics
summary(samples2)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# And the actual first few simulated values for each model parameter
samples2[[1]][1:6,]


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| label: fig-ceplane-bayesian-completecase
#| fig-cap: Cost-effectiveness plane
#| fig-width: 6
#| fig-height: 5
results <- as.data.frame(cbind(samples2[[1]][,3],samples2[[1]][,6]))
names(results) <- c("Incremental_Costs","Incremental_QALYs")
plot(results$Incremental_QALYs,results$Incremental_Costs,ylim=c(-2000,2000),xlim=c(-0.3,0.3),pch=16,xlab="Incremental QALYs",ylab="Incremental Costs")
segments(0,-3000,0,3000); segments(-0.4,0,0.4,0)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| label: fig-inb-bayesian
#| fig-cap: "Posterior distribution of the INB, for $k$ set at 30,000 GBP/QALY"
#| fig-width: 6
#| fig-height: 5
lambda <- 30000
results$INB <- lambda*results$Incremental_QALYs - results$Incremental_Costs
hist(results$INB,main="Historgram of the INB distribution",xlab="Posterior INB")
# qqnorm(results$INB)
# qqline(results$INB)
INB.est <- mean(results$INB)
INB.se <- sd(results$INB)
INB.CrI <- c(INB.est-1.96*INB.se,INB.est+1.96*INB.se)
#paste("INB estimate=",round(INB.est))
#paste("95% CrI for INB: (", round(INB.CrI[1]),", ",round(INB.CrI[2]),")",sep="")
lambda <- (1:10000)*10
INB <- sweep(outer(results$Incremental_QALYs,lambda,"*"),1,results$Incremental_Costs,"-")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| eval: false
#| label: fig-ceac-bayesian
#| fig-cap: "Cost-effectiveness acceptability curve"
# CEAC <- apply(INB>0,2,mean)
# #summary(apply(INB,2,mean)); summary(Prob.CostEffective)
# plot(lambda,CEAC,xlab="willingness to pay threshold (GBP)",main="Cost-effectiveness acceptability curve",type="l",ylim=c(0,0.05))

