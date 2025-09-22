## ----setup_packages, echo=FALSE,warning=FALSE,message=FALSE-------------------------------------------------------------------------------
library(survival)
library(flexsurv)
library(flexsurvcure)
library(survHE)
library(dplyr)
library(here)
library(knitr)
library(survminer)
library(muhaz)
library(broom)
library(data.table)
library(readxl)
library(openxlsx)
library(rjags)
library(tidyverse,quietly = TRUE)
library(darthtools)


## ----echo = T-----------------------------------------------------------------------------------------------------------------------------
#| label: fig-example-survival1
#| fig-cap: "Kaplan-Meier estimates for the recurrence-free survival probability, as a function of the treatment group"
#| echo: true
#| message: false
#| warning: false
#| fig-width: 6
#| fig-height: 5
#| out-width: "95%"

# extract RFS and CSS data
# the dataset is structured so that each row represents a possible outcome.
# the variable etype defines if the outcome is recurrence (etype = 1) or death
# (etype = 2). We first create the composite outcome of recurrence or death in 
# order to create a recurrence free survival outcome. 

colon_all <- inner_join(x = subset(colon, etype ==1),
                        y = subset(colon[,c("id","time","status","etype")],
                                   etype ==2),
                        by = "id",suffix=c("RFS","OS"))
colon_all[colon_all$statusOS == 1                 &
          colon_all$timeOS   <= colon_all$timeRFS,"statusRFS"] = 1  

colon_all      <- colon_all %>% mutate(yearsRFS = timeRFS / 365.25)
colon_all      <- colon_all %>% mutate(yearsOS  = timeOS / 365.25)

RFS_data   <- colon_all %>% dplyr::select(id, rx, statusRFS, yearsRFS)%>%
                            mutate(years = yearsRFS, status = statusRFS)
RFS_lev    <- RFS_data %>% dplyr::filter(rx=="Lev")
RFS_obs    <- RFS_data %>% dplyr::filter(rx=="Obs")
RFS_lev5fu <- RFS_data %>% dplyr::filter(rx=="Lev+5FU")
KM_RFS     <- survfit(Surv(time = years, event = status) ~ rx, data = RFS_data)

# plot KM for RFS
pal <- c("gray8", "gray87", "gray56")
ggsurvplot(
  KM_RFS,
  data = RFS_data,
  size = 1,                  # change line size
  palette = pal,             # custom color palettes
  conf.int = FALSE,          # Add confidence interval
  pval = FALSE,              # Add p-value
  risk.table = TRUE,         # Add risk table
  risk.table.height = 0.25,  # Useful to change when you have multiple groups
  ggtheme = theme_bw(),      # Change ggplot2 theme
  xlab     = 'Years since randomisation',     # Change X-axis label
  title    = "Survival curve for Recurrence Free Survival",
  subtitle = "Based on Kaplan-Meier estimates"
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-example-survival2
#| fig-cap: "Estimates for the hazard function in each treatment group"
#| echo: false
#| message: false
#| warning: false
muhaz_boot <- function(times, delta, B=100, ...){
  n <- length(times)
  args_extra <- list(...)
  args <- c(list(times=times, delta=delta), args_extra)
  haz_est <- do.call("muhaz", args)
  nhaz <- length(haz_est$est.grid)
  res <- matrix(nrow=nhaz, ncol=B)
  for (i in 1:B){
    sam <- sample(1:n, replace=TRUE)
    args <- c(list(times=times[sam], delta=delta[sam], 
                   min.time=haz_est$pin$min.time, max.time=haz_est$pin$max.time), 
              args_extra)
    haz_rep <- do.call("muhaz", args)
    res[,i] <- haz_rep$haz.est
  }
  resci <- apply(res, 1, quantile, c(0.025, 0.975))
  summ <- data.frame(t=haz_est$est.grid, est = haz_est$haz.est, 
                     lcl = resci[1,], ucl=resci[2,])
  summ
}

haz_obs <- muhaz_boot(RFS_obs$years, RFS_obs$status) 
haz_lev <- muhaz_boot(RFS_lev$years, RFS_lev$status) 
haz_lev5fu <- muhaz_boot(RFS_lev5fu$years, RFS_lev5fu$status) 
haz_all <- rbind(haz_obs %>% mutate(Treatment="Observation"),
                 haz_lev %>% mutate(Treatment="Levamisole"),
                 haz_lev5fu %>% mutate(Treatment="Levamisole+5FU"))
group.colors <- c(
  "Observation" = "black","Levamisole" = "gray50", "Levamisole+5FU" = "gray90"
)
ggplot(haz_all, aes(x=t, y=est, col=Treatment)) +
   scale_colour_grey()+
 geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=Treatment), alpha=0.3)+
   scale_fill_manual(values=group.colors) + 
 geom_line(lwd=1.3) + xlab("Years after randomisation") + ylab("Hazard rate")+
 labs(title="Plot of the hazard rate of recurrence  ")+
 theme_bw()



## ----warning=FALSE,echo=FALSE-------------------------------------------------------------------------------------------------------------
med_surv <- surv_median(KM_RFS) %>%
            dplyr::filter(strata=="rx=Lev") %>% 
            pull(median) %>%
            round(1)


## ----flexsurvreg-example1-----------------------------------------------------------------------------------------------------------------
fit_RFS_lev <- flexsurvreg(
  Surv(years, status) ~ 1, data=RFS_lev, dist="gengamma"
)
fit_RFS_lev
plot(fit_RFS_lev, col = "gray80") # survival function
plot(fit_RFS_lev, type="hazard",ylim = c(0,0.5), col = "gray75") # hazard function



## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-example-survHE
#| fig-cap: 'Examples of graphical output from the `survHE` package'
#| fig-subcap: 
#|   - Survival curves for Recurrence Free survival
#|   - "Survival curves for the 'Obs' group"
#| fig-width: 6
#| fig-height: 6
#| layout-ncol: 2
#| echo: false
#| message: false
#| warning: false
mods <- c("weibull", "gengamma", "lnorm", "llogis", "gompertz", "exponential", "gamma")
mods <- c("gengamma", "lnorm", "gompertz", "exponential", "gamma")
fit.survHE.RFS.lev <- fit.models(formula = Surv(years, status) ~ 1, data = RFS_lev, distr = mods)
plot(fit.survHE.RFS.lev, add.km = T, 
     legend.position = c(0.75, 0.6), legend.text = element_text(size = 9),
     legend.title = element_text(size = 11)) +
     theme(plot.margin=unit(c(1,3.5,0.5,0.5), "cm")) + # top, right, bottom, left
     theme(legend.position=c(1.14,0.5)) +
     labs(title = "Fitted survival curves for RFS -Lev")+
 scale_color_grey()

fit.survHE.RFS.obs <- fit.models(formula = Surv(years, status) ~ 1,
                                 data = RFS_obs, distr = mods)
plot(fit.survHE.RFS.obs, add.km = T, 
     legend.position = c(0.75, 0.6), legend.text = element_text(size = 9),
     legend.title = element_text(size = 11)) +
     theme(plot.margin=unit(c(1,3.5,0.5,0.5), "cm")) + # top, right, bottom, left
     theme(legend.position=c(1.14,0.5)) +
     labs(title = "Fitted survival curves for RFS - Obs")+
  scale_color_grey()

tfit <- seq(0, 15, by=0.1)


## ----aic_lev------------------------------------------------------------------------------------------------------------------------------
aicres.lev <- data.frame(
  model = names(fit.survHE.RFS.lev$models), 
  AIC = fit.survHE.RFS.lev$model.fitting$aic,
  BIC = fit.survHE.RFS.lev$model.fitting$bic
)
aicres.lev %>% arrange(AIC)

aicres.obs <- data.frame(
  model = names(fit.survHE.RFS.lev$models), 
  AIC = fit.survHE.RFS.lev$model.fitting$aic,
  BIC = fit.survHE.RFS.lev$model.fitting$bic
)
aicres.obs %>% arrange(AIC)


## -----------------------------------------------------------------------------------------------------------------------------------------
fit.survHE.RFS.lev <- fit.models( formula = Surv(years, status) ~ 1,
                                  data = RFS_lev, distr = mods )
best.RFS.lev       <- fit.survHE.RFS.lev$models[[aicres.lev$mod[1]]] 
best.RFS.obs       <- fit.survHE.RFS.obs$models[[aicres.obs$mod[1]]] 
times <- max()  
df_HR_PFS          <- boot_hr(surv_model1 = best.RFS.lev,
                              surv_model2 = best.RFS.obs,
                              times = tfit[-1], B = 100)  
df_HR_PFS %>%  ggplot(
  aes(x = time, y = med)) + geom_line(size = 0.8, color = "black"
) + geom_ribbon(
  aes(ymin=lcl, ymax=ucl), colour = NA, fill = "gray50", alpha=0.1
) + scale_x_continuous(breaks = c(1:max(tfit[-1]))) +  
  labs(title = "Hazard ratio of Lev vs. Obs",
       subtitle = "Median [2.5%, 97.5%]",
       x = "Time in years", y = "Hazard ratio") +  
  theme_bw() + scale_y_continuous(limits = c(0,2)) + geom_hline(yintercept = 1)




## -----------------------------------------------------------------------------------------------------------------------------------------
cox_RFS <- coxph(Surv(years, status) ~ rx, data = RFS_data)
summary(cox_RFS)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
cf   <- coef(summary(cox_RFS))
est1 <- round(cf[1,"exp(coef)"],2)
est2 <- round(cf[2,"exp(coef)"],2)
se1  <- round(cf[1,"se(coef)"],2)
se2  <- round(cf[2,"se(coef)"],2)


## ----fig.height = 7, fig.width = 9--------------------------------------------------------------------------------------------------------
weib_RFS <- flexsurvreg(
  Surv(years, status) ~ rx, data = RFS_data,dist = "weibull"
)
weib_RFS
plot(
  weib_RFS, main = "AFT model for Recurrence free survival- all treatments", 
  xlab = "Years ", ylab ="Survival probability", col = c("black", "gray80", "gray50")
)



## -----------------------------------------------------------------------------------------------------------------------------------------
ggt_aft <- flexsurvreg(Surv(years, status) ~ rx, data=RFS_data, dist="gengamma")
ggt2    <- flexsurvreg(Surv(years, status) ~ rx, anc=list(sigma=~rx), 
                    data=RFS_data, dist="gengamma")
ggt3    <- flexsurvreg(Surv(years, status) ~ rx, anc=list(sigma=~rx, Q=~rx),
                    data=RFS_data, dist="gengamma")
aics <- AIC(ggt_aft, ggt2, ggt3)  # middle one is best 
aics


## -----------------------------------------------------------------------------------------------------------------------------------------
t_extrap <- seq(0, 15, by=0.1)
haz_aft <- summary(ggt_aft, type="hazard", B=100, t=t_extrap, tidy = TRUE) %>% 
  mutate(model="AFT")
haz2 <- summary(ggt2, type="hazard", B=100, t=t_extrap, tidy = TRUE) %>% 
  mutate(model="GG2")
haz3 <- summary(ggt3, type="hazard", B=100, t=t_extrap, tidy = TRUE) %>% 
  mutate(model="GG3")
haz_fitted <- rbind(haz_aft, haz2, haz3)  %>% 
  rename(estimate=est) %>% 
  mutate(class="param")

haz_obs <- tidy(muhaz(RFS_obs$years, RFS_obs$status)) %>% 
  mutate(rx="Obs")
haz_lev <- tidy(muhaz(RFS_lev$years, RFS_lev$status))  %>% 
  mutate(rx="Lev")
haz_lev5fu <- tidy(muhaz(RFS_lev5fu$years, RFS_lev5fu$status)) %>% 
  mutate(rx="Lev+5FU")
haz_np <- rbind(haz_obs, haz_lev, haz_lev5fu) %>% 
  mutate(model="Nonparametric", class="nonpar") 
haz_all <- haz_np %>%
  full_join(haz_fitted)

ggplot(haz_all %>% dplyr::filter(
  model!="GG3", 
  rx!="Lev"), 
  aes(x=time, y=estimate, lty=rx, col=model, size=class,  
      group=interaction(rx,model))) + 
  # geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=model, colour=NA)) + 
  geom_line() + 
  xlab("Years") + ylab("Hazard") + 
  xlim(0, 15) + 
  scale_size_manual(breaks=c("nonpar","param"), 
                    values=c(1.5, 1)) +
  # Might want to customise the colours 
  scale_color_manual(breaks=c("Nonparametric","AFT","GG2"),
                     values =  c("black", "gray39", "gray87")) + 
  guides(lwd="none", fill="none",
         col = guide_legend(title="Model"),
         lty = guide_legend(title="Treatment group")) 




## -----------------------------------------------------------------------------------------------------------------------------------------
summary(ggt_aft, type="rmst", t=15, tidy=TRUE)
summary(ggt2, type="rmst", t=15, tidy=TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
t <- seq(0.2, 15, 0.1)
hrs1 <- hr_flexsurvreg(
  ggt_aft, t=t, newdata = data.frame(rx=c("Obs","Lev+5FU"))
) %>%
  mutate(model="AFT") 
hrs2 <- hr_flexsurvreg(
  ggt2, t=t, newdata = data.frame(rx=c("Obs","Lev+5FU"))
) %>%
  mutate(model="GG2")
hrs <- rbind(hrs1, hrs2)
ggplot(hrs, aes(x=t, y=est, col=model)) + 
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="lightgray") + 
  geom_line() + 
  ylab("Hazard ratio") + xlab("Years")+
 scale_color_grey(start = 0, end = 0.4)


## -----------------------------------------------------------------------------------------------------------------------------------------
    spl1 <- flexsurvspline(Surv(years, status) ~ rx, data=RFS_data, k=2) 


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------
#     spl2 <- flexsurvspline(
#       Surv(years, status) ~ rx, anc=list(gamma1=~rx), data=RFS_data, k=1
#     )
# 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "75%"
t_extrap <- seq(0, 15, by=0.1)
haz_spline <- summary(spl1, type="hazard", B=100, t=t_extrap, tidy = TRUE) %>% 
  mutate(model="Spline")
haz2 <- summary(ggt2, type="hazard", B=100, t=t_extrap, tidy = TRUE) %>% 
  mutate(model="GG2")
haz_fitted <- rbind(haz_spline, haz2)  %>% 
  rename(estimate=est) %>% 
  mutate(class="param")

haz_obs <- tidy(muhaz(RFS_obs$years, RFS_obs$status)) %>% 
  mutate(rx="Obs")
haz_lev <- tidy(muhaz(RFS_lev$years, RFS_lev$status))  %>% 
  mutate(rx="Lev")
haz_lev5fu <- tidy(muhaz(RFS_lev5fu$years, RFS_lev5fu$status)) %>% 
  mutate(rx="Lev+5FU")
haz_np <- rbind(haz_obs, haz_lev, haz_lev5fu) %>% 
  mutate(model="Nonparametric", class="nonpar") 
haz_all <- haz_np %>%
  full_join(haz_fitted)

ggplot(haz_all %>% dplyr::filter(
  model!="GG3", 
  rx!="Lev"), 
  aes(x=time, y=estimate, lty=rx, col=model, size=class,  
      group=interaction(rx,model))) + 
  # geom_ribbon(aes(ymin=lcl, ymax=ucl, fill=model, colour=NA)) + 
  geom_line() + 
  xlab("Years") + ylab("Hazard") + 
  xlim(0, 15) + 
  scale_size_manual(breaks=c("nonpar","param"), 
                    values=c(1.5, 1)) +
  # Might want to customise the colours 
  scale_color_manual(breaks=c("Nonparametric","Spline","GG2"),
                     values =  c("black", "gray39", "gray87")) + 
  guides(lwd="none", fill="none",
         col = guide_legend(title="Model"),
         lty = guide_legend(title="Treatment group")) 
    AIC(ggt2, spl1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "75%"
#| fig-width: 5
#| fig-height: 4
library(flexsurvcure)
cur1 <- flexsurvcure(Surv(years, status) ~ rx,data = RFS_data,dist="gengamma")
plot(cur1, main = "fit of g-gamma mixture cure model on RFS", 
     ylab = "Time", xlab="Survival probability", 
     col = c("black", "gray40", "gray75"))


cur2 <- flexsurvcure(Surv(years, status) ~ rx,data = RFS_data,dist="gengamma",
                     anc=list(sigma=~rx, mu=~rx))
plot(cur1, main = "fit of g-gamma mixture cure model on RFS", 
     ylab = "Time", xlab="Survival probability",
     col = c("black", "gray30", "gray65"))
lines(cur2,col = "gray80")
AIC(cur1, cur2)



## -----------------------------------------------------------------------------------------------------------------------------------------
#| out-width: "75%"
#| fig-width: 5
#| fig-height: 4
cur1_nm <- flexsurvcure(
  Surv(years, status) ~ rx,data = RFS_data,dist="gengamma", mixture = F
)
plot(cur1_nm, main = "fit of g-gamma non-mixture cure model on RFS", 
     ylab = "Time", xlab="Survival probability",
     col = c("black", "gray30", "gray65"))


cur2_nm  <- flexsurvcure(
  Surv(years, status) ~ rx,data = RFS_data,dist="gengamma",
  anc=list(sigma=~rx, mu=~rx), mixture = F
)
plot(cur1_nm, main = "fit of g-gamma non-mixture cure model on RFS", 
     ylab = "Time", xlab="Survival probability",
     col = c("black", "gray30", "gray65"))
lines(cur2_nm,col ="gray80")
AIC(cur1_nm, cur2_nm)


## -----------------------------------------------------------------------------------------------------------------------------------------
# TRIAL SPECIFIC ESTIMATES OF TRANSFORMED PARAMETERS 

#clear workspace 
rm(list = ls())

# load data
workfolder <- "MV NMA/"
datafolder <- paste(workfolder,"Data/", sep="")
survparamdatafolder <- paste(workfolder,"Data/survparamdata/", sep="")
outfolder      <- paste(workfolder,"Output/NMA_results/", sep="")
jagscodefolder <- paste(workfolder,"Jagscode/", sep="")

model1   <- c('weibull')
dataname <- paste(survparamdatafolder, 'parametric survival data.xlsx', sep="")
dataset2 <- as.data.frame(read_excel(dataname, sheet=paste('data2',model1)))



## ----cache=TRUE,results="hide"------------------------------------------------------------------------------------------------------------
# PERFORM MULTIVARIATE NMA TO OBTAIN TREATMENT SPECIFIC RELATIVE 
# TREATMENT ESTIMATES DESCRIBING THE TIME VARYING HAZARD RATIO
dataset1 <- as.data.frame(read_excel(dataname, sheet="data1"))
trt      <- as.data.frame(read_excel(dataname, sheet="treatments"))


# prepping data to run in jags 
N1 <- dim(dataset2)[1]              #no of data points
N2 <- dim(dataset1)[1]              #no of studies x no of outcomes (10x2=20) 
# no of studies
ns <- length(unique(dataset1$studyid))              
#no of outcomes
no <- num_o <- dim(
  dataset2[, grep('y',substr(colnames(dataset2),1,1))] 
)[2]                                
# no of interventions for each outcome
nt.total <- rep(length(unique(dataset2$treatment)),no)  
# no. of arms in each study
nastudy  <-  c(table(dataset2$studyid))                

studyid <- dataset2$studyid
study   <- dataset2$study
y       <- as.matrix(dataset2[,grep('y\\.',colnames(dataset2))])
se      <- as.matrix(dataset2[,grep('se\\.',colnames(dataset2))])
arm     <- dataset2$arm
cov11   <- dataset2$cov11
cov22   <- dataset2$cov22
cov12   <- dataset2$cov12


studyid1 <- dataset1$studyid1
t <- cbind(dataset1$t1,dataset1$t2,dataset1$t3)
s <- dataset1$s	
o <- dataset1$o	
out <- dataset1$out	
na <- dataset1$na

datause <- list(
  N1       = N1,
  N2       = N2,
  ns       = ns,
  no       = no,
  nt.total = nt.total,
  nastudy  = nastudy,
  studyid  = as.numeric(as.factor(studyid)),
  study    = study,
  arm      = arm,
  y        = y,
  cov11    = cov11,
  cov22    = cov22,
  cov12    = cov12,
  studyid1 = as.numeric(as.factor(studyid1)),
  s        = s,
  t        = t,
  o        = o,
  out      = out,
  na       = na         
)

# run NMA model
iterate <- 50000
NTHIN <- 2
iterate1 <- iterate*NTHIN

MODELFILE <- paste(jagscodefolder, "MV NMA RE.bugs", sep="" ) 
set.seed(1)

jagsRE <- NULL
jagsRE <- suppressWarnings(
  jags.model(file = MODELFILE, data =datause, n.chains = 2, n.adapt = iterate)
)
update(jagsRE,iterate)
jagsRE.out <- coda.samples(jagsRE, c("d"), n.iter = iterate1, thin = NTHIN)
summaryRE <- summary(jagsRE.out)


## ----results="hide"-----------------------------------------------------------------------------------------------------------------------
# the function digitise creates IPD Data based on digitized aggregate estimates
digitise(surv_inp   = "data/OS.txt", 
         nrisk_inp  = "data/OS_risk.txt", 
         km_output  = "data/KMdata_OS.txt", 
         ipd_output = "data/IPDdata_OS.txt")

# make.ipd is the function that creates IPD from the txt files generated from
# the digitise function

IPD_OS  <- make.ipd(ipd_files = c("data/IPDdata_OS.txt"), ctr = 1, 
                    var.labs  = c("time","event","arm"))


