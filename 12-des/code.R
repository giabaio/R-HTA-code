## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| message: false
#| warning: false
library(simmer)
library(simmer.plot)
library(dampack)
library(tidyverse,quietly = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # Defining the trajectory
# traj <- trajectory() %>%
# 
#   # Nurse determines whether doctor should be visited
#   seize(resource = 'Nurse') %>%
#   timeout(task = function() rweibull(n = 1, shape = 5, scale = 10)) %>%
#   set_attribute(
#     keys = 'SeeDoctor', values = function() rbinom(n = 1, size = 1, prob = 0.3)
#   ) %>%
#   release(resource = 'Nurse') %>%
# 
#   # Only those who need to go through to the Doctor
#   branch(
#     option = function() get_attribute(.env = sim, keys = 'SeeDoctor'),
#     continue = T,
#     # Sub-trajectory for the doctor
#     trajectory() %>%
#       seize(resource = 'Doctor') %>%
#       timeout(task = function() rweibull(n = 1, shape = 5, scale = 10)) %>%
#       release(resource = 'Doctor')
#   ) %>%
# 
#   # Some patients will have to be re-examined by the Nurse
#   simmer::rollback(
#     target = 5, check = function() rbinom(n = 1, size = 1, prob = 0.05)
#   )
# 
# # Plots a diagram with the trajectory
# plot(traj)
# 
# 
# # Defining the simulation environment
# sim <- simmer() %>%
#   add_generator(
#     name_prefix = 'Patient ',
#     trajectory = traj,
#     distribution = function() rexp(n = 1, rate = 0.1),
#     mon = 2
#   ) %>%
#   add_resource(name = 'Nurse', capacity = 2) %>%
#   add_resource(name = 'Doctor', capacity = 1)
# 
# 
# # Running the simulation
# set.seed(123)
# sim %>% reset() %>% run(until = 4 * 60)
# 
# 
# # Extracting the outcomes
# df_arrivals <- get_mon_arrivals(sim)
# df_resources <- get_mon_resources(sim)
# df_attributes <- get_mon_attributes(sim)
# 
# # how many visits to the doctor's office?
# df_attributes%>%
#   filter(key=="SeeDoctor") %>%
#   summarize(sum(value==1))
# 
# # Can visualise the first few rows of the resulting simulations
# head(df_arrivals)
# head(df_resources)
# head(df_attributes)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # Main trajectory
# traj_main <- trajectory(name = "traj_main") %>%
# 
#   ## INITIALIZATION ##
# 
#   # 1) Record the treatment arm, BS and RFS, and initialize counters
#   set_attribute(keys   = c("TreatmentArm", "BS", "RFS", "AdjuvantCycles",
#                            "Toxicities", "dCosts", "dQALYs"),
#                 values = function() fn_initialisation())


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# ## ADJUVANT TREATMENT ##
# 
# # 2) Will the individual receive treatment?
# # If TreatmentArm is 1 or 2, the value provided to the option argument is
# # TRUE/1 and the individual will enter the branch. If TreatmentArm is 0,
# # it will be FALSE/0 and the individual will skip the branch.
# branch(
#   option = function()
#     get_attribute(.env = sim,
#                   keys = "TreatmentArm") %in% c(1, 2),
#   continue = TRUE,
# 
#   # Trajectory defining what happens in the branch
#   trajectory()
#   %>%
# 
#   # 3) Record the event and duration for the treatment cycle
#   set_attribute(
#     keys = c("AdjuvantCycleEvent", "AdjuvantCycleTime"),
#     values = function()
#       fn_adjuvant_cycle(
#         attrs = get_attribute(.env = sim, keys = c("BS", "RFS")),
#         t_now = now(.env = sim)
#       )
#   ) %>%
# 
#   # 4) Update the number of cycles, toxicities, costs, and QALYs
#   set_attribute(
#     keys = c("AdjuvantCycles", "Toxicities", "dCosts", "dQALYs"),
#     values = function()
#       fn_adjuvant_impact(
#         attrs = get_attribute(
#           .env = sim,
#           keys = c("TreatmentArm", "AdjuvantCycleTime")
#         ),
#         t_now = now(.env = sim)
#       ),
#     mod = "+"
#   ) %>%
# 
# # 5) Delay for the duration of that treatment cycle
#   timeout_from_attribute(key = "AdjuvantCycleTime") %>%
# 
# # 6) Check what will happen next based on AdjuvantCycleEvent:
# #    1 = death during cycle
# #    2 = recurrence during cycle
# #    3 = no recurrence or death during cycle
# ## 6.1) Is the individual alive?
#   branch(
#     option = function() {
#       get_attribute(.env = sim, keys = "AdjuvantCycleEvent") == 1
#     },
#     continue = FALSE,
#     traj_death
#   ) %>%
#   ## 6.2) Is the individual free of cancer recurrence?
#   branch(
#     option = function() {
#       get_attribute(.env = sim, keys = "AdjuvantCycleEvent") == 2
#     },
#     continue = FALSE,
#     traj_recurrence
#   ) %>%
#   ## 6.3) Was the maximum amount of cycles reached?
#   rollback(
#     target = 5,
#     check = function() {
#       get_attribute(.env = sim, keys = "AdjuvantCycles") < n_max_adjuvant_cycles
#     }
#   )
# )


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# ## LONG-TERM FOLLOW UP ##
# 
# # 7) Record the time until recurrence or non-cancer death
# # Here the events are defined as follows:
# #   1 = death
# #   2 = recurrence
# set_attribute(
#   keys = c("FollowUpTime", "FollowUpEvent"),
#   values = function() c(
#    FollowUpTime  = min(get_attribute(.env = sim, keys = c("BS", "RFS"))) -
#      now(.env = sim),FollowUpEvent = which.min(
#        get_attribute(.env = sim, keys = c("BS", "RFS"))
#      )
#    )
#   ) %>%
# 
# # 8) Update the QALYs
# set_attribute(keys   = "dQALYs",
#                 mod    = "+",
#                 values = function() fn_discount_QALYs(
#                   utility    = u_diseasefree,
#                   t_start    = now(.env = sim),
#                   t_duration = get_attribute(.env = sim, keys = "FollowUpTime"))
#   ) %>%
# 
#   # 9) Delay until recurrence or non-cancer death
#   timeout_from_attribute(key = "FollowUpTime") %>%
# 
# # 10) Was the event death or cancer recurrence?
# # There are two options here and, hence, two different trajectories the
# # individual can go in the branch. What happens is stored in FollowUpEvent:
# #   1 = death
# #   2 = recurrence
# branch(option   = function() get_attribute(.env = sim, keys = "FollowUpEvent"),
#          continue = FALSE,
# 
#          # option = 1 -> death/BS
#          traj_death,
# 
#          # option = 2 -> recurrence/RFS
#          traj_recurrence)
# 
# # This was the end of the main trajectory.


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # Sub-trajectory for treatment of recurrence/advanced disease
# traj_recurrence <- trajectory(name = "traj_recurrence") %>%
# 
#   ## TREATMENT OF ADVANCED DISEASE ##
# 
#   # 11) Record the time until death (cancer or non-cancer)
#   set_attribute(keys   = "CSS",
#                 values = function() fn_advanced_time(
#                   attrs = get_attribute(.env = sim,
#                                         keys = c("TreatmentArm", "BS"))
#                 )
#   ) %>%
# 
#   # 12) Update the costs and QALYs
#   set_attribute(keys   = c("dCosts", "dQALYs"),
#                 values = function() fn_advanced_impact(
#                   attrs = get_attribute(.env = sim, keys = "CSS"),
#                   t_now = now(.env = sim)),
#                 mod    = "+") %>%
# 
#   # 13) Delay until death
#   timeout_from_attribute(key = "CSS") %>%
# 
#   # Death: go to traj_death sub-trajectory
#   join(traj_death)
# 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # Sub-trajectory to record survival upon death
# traj_death <- trajectory(name = "traj_death") %>%
# 
#   # 14) Record survival
#   set_attribute(.env = sim, keys = "OS", values = function() now(.env = sim))
# 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| eval: false
# # Define the simulation environment
# n_individuals <- 50000
# 
# sim <- simmer() %>%
#   add_generator(name_prefix  = "Patient_",
#                 trajectory   = traj_main,
#                 distribution = at(rep(x = 0, times = n_individuals)),
#                 mon          = 2)
# 
# # Run the simulation and summarize outcomes for strategy: Obs (0)
# treatment_arm <- 0; set.seed(1); sim %>% reset() %>% run(until = Inf)
# df_0 <- fn_summarise(df_sim = get_mon_attributes(sim))
# 
# # Run the simulation and summarize outcomes for strategy: Lev (1)
# treatment_arm <- 1; set.seed(1); sim %>% reset() %>% run(until = Inf)
# df_1 <- fn_summarise(df_sim = get_mon_attributes(sim))
# 
# # Run the simulation and summarize outcomes for strategy: Lev+5FU (2)
# treatment_arm <- 2; set.seed(1); sim %>% reset() %>% run(until = Inf)
# df_2 <- fn_summarise(df_sim = get_mon_attributes(sim))
# 
# 
# 
# 
# v_costs <- c(mean(df_0$dCosts),mean(df_1$dCosts),mean(df_2$dCosts))
# v_effs <-  c(mean(df_0$dQALYs),mean(df_1$dQALYs),mean(df_2$dQALYs))
# v_strategies <- c("Obs", "Lev", "Lev+5FU")
# df_ce <- data.frame(Costs = v_costs,
#                     Effs = v_effs,
#                     Strategies = v_strategies)
# 
# 
# res <- calculate_icers(df_ce$Costs,df_ce$Effs,df_ce$Strategies)
# plot(res)

