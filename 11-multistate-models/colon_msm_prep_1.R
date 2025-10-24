# Script to simulate time-to-event data for colon cancer
# STATES: RF = Recurrence free, R = Recurrence, CD = Cancer death, OCD = Other cause death 
# Four transitions: 1 (RF to R), 2 (RF to OCD), 3 (R to CD), 4 (R to OCD)
# Howard Thom 14-May-2023

# Adapted from "2_basic_survival_analysis.R"

# TODO: Simulate panel data
# TODO: Simulate output from a fractional polynomials NMA
# TODO: Move to a project on GitHub


set.seed(245234545)

expit <- function(logO) {
  return(exp(logO)/(1 + exp(logO)))
}
logit <- function(p) {
  return(log(p/(1-p)))
}

### 1. INITIALISATION ----

# Packages
library(dplyr);           
library(survival);        
library(flexsurv);        
library(flexsurvcure);    
library(survParamSim)

# Data
load(file = "df_colon.RData")

head(df_colon);

# There are some "ties" in the data with a recurrence being recorded at the same 
# day as death, which suggests an overall survival after progression of zero.Given
# that a zero is problematic for fitting time-to-even distributions these are 
# manually corrected so that the event of recurrence happens 2 week (14 days)
# before the event of death.
count(df_colon, t_R_D == 0);

df_colon <- df_colon %>% 
  mutate(
    correct_t_R_D = (t_R_D == 0),
    t_RF_R = if_else(!is.na(correct_t_R_D) & correct_t_R_D, t_RF_R - 14, t_RF_R),
    t_R_D  = if_else(!is.na(correct_t_R_D) & correct_t_R_D, 14, t_R_D)
  );

head(df_colon %>% filter(correct_t_R_D));

# Changing time from days to years
days_in_year <- 365.25;
df_colon <- df_colon %>% 
  mutate(
    t_RF_R = t_RF_R / days_in_year,
    t_RF_D = t_RF_D / days_in_year,
    t_R_D  = t_R_D  / days_in_year
  );

# All-cause death follows a Gompertz distribution
# shape =  0.088457189, rate = 0.008098087
all_cause_survival <- data.frame(matrix(NA, nrow = dim(df_colon)[1], ncol = 2,
                                        dimnames = list(NULL, c("years", "status"))))
all_cause_survival$years <- rgompertz( dim(df_colon)[1],  shape =  0.088457189, rate = 0.008098087)
# For a test just assume all patients observed to die
all_cause_survival$status <- 1
# Does this give the same shape and scale? (yes)
all_cause_model <- flexsurvreg(Surv(years, status) ~ 1, dist = "gompertz", data = all_cause_survival)

# Multistate data for colon cancer 
# Current names
#c("id", "rx", "age", "sex", "c_RF_R", "t_RF_R", "c_RF_D", "c_RF_D", "t_RF_D", "c_R_D", "t_R_D")

# Desired column names
# c( "id", "from", "rx", "age", "sex", "to", "Tstart", "Tstop", "years", "status", "trans")
# And five rows for each id with trans = 1, ..., 5.
# STATES: RF = Recurrence free, R = Recurrence, CD = Cancer death, OCD = Other cause death 
# Four transitions: 1 (RF to R), 2 (RF to OCD), 3 (R to CD), 4 (R to OCD)

msm_colon <- data.frame(
  matrix(NA, nrow = 4 * dim(df_colon)[1], ncol = 11,
         dimnames = list(NULL, 
                         c( "id", "rx", "age", "sex", "from", "to", "Tstart", "Tstop", "years", "status", "trans")))
)

msm_colon$id <- rep(df_colon$id, each = 4)
msm_colon$rx <- rep(df_colon$rx, each = 4)
msm_colon$age <- rep(df_colon$age, each = 4)
msm_colon$sex <- rep(df_colon$sex, each = 4)
msm_colon$trans <- c(1:4)


# Loop through the patients and construct the data
for(i_id in 1:dim(df_colon)[1]) {
  # 1 (RF to R)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("from", "to")] <- c(1, 2)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("years", "status")] <-
    df_colon[df_colon$id == i_id, c("t_RF_R", "c_RF_R")]
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("Tstart")] <- 0
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("Tstop")] <- 
    df_colon[df_colon$id == i_id, c("t_RF_R")]
  # Check if all-cause survival time is before this transition is observed
  if (all_cause_survival$years[i_id] < msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("Tstop")]) {
    # If yes then event is censored
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, c("status")] <- 0
  }
  
  # 2 (RF to OCD)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, c("from", "to")] <- c(1, 4)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, c("years", "status")] <-
    all_cause_survival[i_id, ]
  # If recurrence check which event occurred first
  if(msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "status"]) {
    # Status of time to recurrence
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "status"] <-
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "years"] <
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, "years"]
    # Status of time to Other cause death
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, "status"] <-
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "years"] >
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, "years"]
    # Set the time to the minimum
    msm_colon[msm_colon$id == i_id & (msm_colon$trans == 1 | msm_colon$trans == 2), "years"] <-
      min(msm_colon[msm_colon$id == i_id & (msm_colon$trans == 1 | msm_colon$trans == 2), "years"] )      
  } else {
    # Ensure censoring time for recurrence is minimum of recurrence censoring and death time 
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "years"] <-
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "Tstop"] <-
      min(msm_colon[msm_colon$id == i_id & (msm_colon$trans == 1 | msm_colon$trans == 2), "years"] )      
  }
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, c("Tstart")] <- 0
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, c("Tstop")] <- 
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 2, "years"]
  
  # 3 (R to CD)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("from", "to")] <- c(2, 3)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("years", "status")] <-
    df_colon[df_colon$id == i_id, c("t_R_D", "c_R_D")]
  # Only observed if recurrence is not censored
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstart")] <- 
    ifelse(msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "status"],
           msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "Tstop"],
           NA)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstop")] <- 
    ifelse(msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "status"],
           msm_colon[msm_colon$id == i_id & msm_colon$trans == 1, "Tstop"] +
             msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "years"],
           NA)
  
  
  # 4 (R to OCD)
  msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, c("from", "to")] <- c(2, 4)
  # Only observed if a patient transitions to R
  if(!is.na(msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "status"]) &
     msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "status"] == 1){
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, c("status")] <-
      all_cause_survival[i_id, "status"]
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, c("years")] <-
      all_cause_survival[i_id, "years"] - msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "Tstart"]
    
    # If cancer death check which event occurred first
    if(msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "status"]) {
      # Status of time to cancer death
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "status"] <-
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "years"] <
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, "years"]
      # Status of time to Other cause death
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, "status"] <-
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "years"] >
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, "years"]
      # Set the time to the minimum
      msm_colon[msm_colon$id == i_id & (msm_colon$trans == 3 | msm_colon$trans == 4), "years"] <-
        min(msm_colon[msm_colon$id == i_id & (msm_colon$trans == 3 | msm_colon$trans == 4), "years"] )      
      # Update the stop time
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstop")] <- 
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstart")] +
        msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, "years"]  
    }
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, c("Tstart")] <- 
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstart")]
    msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, c("Tstop")] <- 
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 3, c("Tstart")] +
      msm_colon[msm_colon$id == i_id & msm_colon$trans == 4, "years"]
  }
} # End loop over i_id

# Remove the NA rows
msm_colon <- msm_colon[-which(is.na(msm_colon$status)), ]

save(msm_colon, file = "msm_colon.rda")

