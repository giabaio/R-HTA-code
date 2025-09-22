## ----pkgs, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------
library(foreign)
library(mice)
library(pan)
library(VIM)
library(kableExtra)


## ----data, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------
# 10TT trial data
data<-read.csv("data/10TT_synth_280921_mg.csv")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-missingplot
#| echo: true 
#| eval: true
#| message: false
#| warning: false
#| error: false 
#| fig.pos: 'H'
#| out.width: '75%'
#| out.extra: ''
#| fig.cap: "Missing data propostions and patterns in the 10TT trial"

# missing data patterns
mice_plot <- VIM::aggr(
  data[,7:12], col=c('white','grey'), numbers=TRUE, sortVars=FALSE, 
  cex.numbers=0.6, labels=names(data[,7:12]), cex.axis=.7, gap=3, 
  ylab=c("Missing data","Pattern")
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-stats0
#| tbl-cap: "Patients' characteristics according to their missingness status"
#| echo: false 
#| eval: true
#| message: false
#| warning: false
#| error: false 

options(scipen=999)
library(gtsummary)
library(gt)
# missing data indicator: 1 if observed, 0 otherwise
data$r<-as.numeric(complete.cases(data$hrql_3 & data$hrql_6 & data$hrql_12 & data$hrql_18 & data$hrql_24 & data$totalcost))
data$r = factor(data$r,levels = c(0,1),labels = c("Mis", "Obs"))
data$sex = factor(data$sex,levels = c(1,2),labels = c("M", "F"))
data$bmi = factor(data$bmi,levels = c(1,2),labels = c("< 35", ">= 35"))
data$arm = factor(data$arm,levels = c(0,1),labels = c("control", "treatment"))

# Characteristics of the individuals with missing data versus fully observed (by trial arm)
table0 <- subset(data[data$arm=="control",], select=c(age, sex, bmi, hrql_0, r))

  t1<-tbl_summary(
   table0,
    by = r, # split table by group
    missing = "no", # don't list missing data separately
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels() 
    
table1 <- subset(data[data$arm=="treatment",], select=c(age, sex, bmi, hrql_0, r))
  
 t2<-tbl_summary(
    table1,
    by = r, # split table by group
    missing = "no", # don't list missing data separately
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
  ) %>%
    add_n() %>% # add column with total number of non-missing observations
    modify_header(label = "**Variable**") %>% # update the column header
    bold_labels()

 tbl_merge(
   tbls = list(t1, t2),
   tab_spanner = c("**Control**", "**Treatment**")
 ) %>% 
   as_gt() |> tab_options(table.font.size = pct(9))


## ----qaly, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------
# Calculate QALY based on the individual HRQL scores 
# (using the 'area under the curve' approach)
data$qaly <- 0.125*data$hrql_0 + 0.25*data$hrql_3 + 0.375*data$hrql_6 + 
                  0.5*data$hrql_12 + 0.5*data$hrql_18 + 0.25*data$hrql_24

# 1. We apply a simple linear transformation so that the QALY variable 
#    shifts from left-skewed to right-skewed to facilitate the use of GLMs
data$qaly<- 3 - data$qaly


## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------------
# Create a subset of the data that includes variables to be imputed and 
# predictors sort data by treatment arm (1 - treatment, 0 - control)
data <- data[order(data$arm),] 
data0 <- subset(
  data[data$arm=="control",],
  select=c(arm, sex, age, bmi, hrql_0, hrql_3, hrql_6, hrql_12, 
           hrql_18, hrql_24, totalcost, qaly)
)
data1 <- subset(
  data[data$arm=="treatment",],
  select=c(arm, sex, age, bmi, hrql_0, hrql_3, hrql_6, hrql_12,
           hrql_18, hrql_24, totalcost, qaly)
)

ini  <- mice(data0, maxit=0)   #initial values
pred <- ini$pred 	# 1 - used as predictor; 0 otherwise
meth <- ini$meth 


## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE----------------------------------------------------------------------
# Modify imputation method for 'qaly' to perform 'Passive Imputation'
meth["qaly"] <- "~I(3 - (0.125*data$hrql_0 + 0.25*data$hrql_3 + 
                    0.375*data$hrql_6 + 0.5*data$hrql_12 + 
                    0.5*data$hrql_18 + 0.25*data$hrql_24))"

# exclude 'qaly' from predictors to prevent feedback
pred[c("hrql_0", "hrql_3", "hrql_6", "hrql_12", "hrql_18", 
       "hrql_24", "totalcost"), "qaly"] <- 0  

# exclude 'arm' from predictors as we are already imputing by arm
pred[c("hrql_0", "hrql_3", "hrql_6", "hrql_12", "hrql_18", 
       "hrql_24", "totalcost"), "arm"]  <- 0 


## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------------------------
# Perform multiple imputation using mice()
M <-5		# Change this to your desired number of imputations
imp0 <- mice(data0, m=M, meth=meth, seed=1710, pred=pred, maxit=10)
imp1 <- mice(data1, m=M, meth=meth, seed=1710, pred=pred, maxit=10)

imp <- rbind(imp0,imp1)   # combine imputations from the 2 arms


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: imputationconverg
#| echo: true 
#| eval: false
#| message: false
#| warning: false
#| error: false 
#| out.width: '90%'
#| fig.pos: 'H'
#| out.extra: ''
#| fig.cap: "Convergence diagnostics of the mice MCMC chains for key parameters"

# # Check convergence of MCMC chains
# plot(imp, totalcost + qaly ~ .it | .ms, layout = c(2, 2))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: imputationdiagnostics
#| echo: true 
#| eval: true
#| message: false
#| warning: false
#| error: false 
#| out.width: '75%'
#| fig.pos: 'H'
#| out.extra: ''
#| fig.cap: "Density plot of the imputed values generated through mice"
# Assess plausibility of imputed values
densityplot(imp, ~totalcost + qaly, layout = c(1, 2))


## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------------------------
# Apply substantive model to data
mod.cost <- with(imp, glm(totalcost ~ arm, family = Gamma(link = "identity")))
mod.qaly <- with(
  imp, glm(qaly ~ arm + hrql_0, family = Gamma(link = "identity"))
)


## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------------
# # Apply Rubin's rules and estimate parameters of interest
# # (incremental cost and QALY)
# summary(pool(mod.cost), conf.int = TRUE)

## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------------
# Apply Rubin's rules and estimate parameters of interest 
# (incremental cost and QALY)
s1=summary(pool(mod.cost), conf.int = TRUE)
s2=summary(pool(mod.qaly), conf.int = TRUE)
print(s1,digits=3)


## ----echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------------
# summary(pool(mod.qaly), conf.int = TRUE)

## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------------------------
print(s2,digits=3)


## ----echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------------------------
post <- ini$post       # post option
d <- seq(0, 0.1,0.02)  # sensitivity parameter
est <- vector("list", length(d)) # list object to store results

# Re-run (post-process) MI for each MNAR scenario
for (i in 1:length(d)) {
  post["hrql_3"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post["hrql_6"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post["hrql_12"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post["hrql_18"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post["hrql_24"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  imp0 <- mice(data0, m=M, post=post, meth=meth, seed=1710, pred=pred, maxit=10)
  imp1 <- mice(data1, m=M, post=post, meth=meth, seed=1710, pred=pred, maxit=10)
  imp<-rbind(imp0,imp1)
  fit <- with(imp, glm(qaly ~  arm + hrql_0, family = Gamma(link = "identity")))
  est[[i]] <- summary(pool(fit), conf.int = TRUE) 
}


## ----echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------------------------
sa.base<-data.frame(d=d,
     delta.q=round( c(-est[[1]][2,2],-est[[2]][2,2],-est[[3]][2,2],
                      -est[[4]][2,2],-est[[5]][2,2],-est[[6]][2,2]), 3),
     l.ci<-round( c(-est[[1]][2,8],-est[[2]][2,8],-est[[3]][2,8],
                    -est[[4]][2,8],-est[[5]][2,8],-est[[6]][2,8]), 3),
     u.ci<-round( c(-est[[1]][2,7],-est[[2]][2,7],-est[[3]][2,7],
                    -est[[4]][2,7],-est[[5]][2,7],-est[[6]][2,7]), 3))
sa.base[1,1]<-"0 (MAR)"
names(sa.base)<-c("d","Inc QALY","Lower CI", "Upper CI")

# sensitivity analysis (MNAR) 
# Different k by treatment arm
post0 <- ini$post
post1 <- ini$post
est <- vector("list", length(d))

# Imputation using 
for (i in 1:length(d)) {
  post0["hrql_3"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post0["hrql_6"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post0["hrql_12"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post0["hrql_18"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post0["hrql_24"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", d[i])
  post1["hrql_3"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", 0.5*d[i])
  post1["hrql_6"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", 0.5*d[i])
  post1["hrql_12"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", 0.5*d[i])
  post1["hrql_18"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", 0.5*d[i])
  post1["hrql_24"] <- paste("imp[[j]][,i] <- imp[[j]][,i] -", 0.5*d[i])
  imp0 <- mice(data0, m=M, post=post0, meth=meth, seed=1710, pred=pred, maxit=10)
  imp1 <- mice(data1, m=M, post=post1, meth=meth, seed=1710, pred=pred, maxit=10)
  imp<-rbind(imp0,imp1)
  fit <- with(imp, glm(qaly ~  arm + hrql_0, family = Gamma(link = "identity")))
  est[[i]] <- summary(pool(fit), conf.int = TRUE) }

sa.ext<-data.frame(
                delta.q=round( c(-est[[1]][2,2],-est[[2]][2,2],-est[[3]][2,2],
                                 -est[[4]][2,2],-est[[5]][2,2],-est[[6]][2,2]), 3),
                l.ci<-round( c(-est[[1]][2,8],-est[[2]][2,8],-est[[3]][2,8],
                               -est[[4]][2,8],-est[[5]][2,8],-est[[6]][2,8]), 3),
                u.ci<-round( c(-est[[1]][2,7],-est[[2]][2,7],-est[[3]][2,7],
                               -est[[4]][2,7],-est[[5]][2,7],-est[[6]][2,7]), 3))
names(sa.ext)<-c("inc QALY","lower CI", "upper CI")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-tableressensMI
#| tbl-cap: Sensitivity analysis results to departures from MAR
#| echo: false 
#| eval: true
#| message: false
#| warning: false
#| error: false 

sa <- cbind(sa.base,sa.ext)
colnames(sa)[1]="$\\delta$"
tinytable::tt(sa) |> 
  tinytable::group_tt(
    j=list(
      "Same $\\delta$ across trial arm"=2:4,
      "$\\delta$ differs by treatment arm"=5:7
    )
  ) |> 
  tinytable::style_tt(
    i=6,j=5:7,color="darkgreen",bold=TRUE
  )


## ----load_pkgs, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------------
library(denstrip)
library(foreign)
library(rjags)
library(runjags)
library(latex2exp)
library(bayesplot)
library(BCEA)
library(ggpubr)
library(ggridges)
library(HDInterval)


## ----read_data, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE-----------------------------------------------------------
tenTT <- read.csv(file="data/10TT_synth_280921_mg.csv", sep=",")


## ----background_stuff, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------
arm <- tenTT$arm+1
age <- (tenTT$age - mean(tenTT$age))/sd(tenTT$age)
bmi <- tenTT$bmicat-1
bmi.c <- (bmi - mean(bmi))/sd(bmi)
sex <- tenTT$sex-1
sex.c <- (sex - mean(sex))/sd(sex)
Xs <- cbind(age,bmi.c,sex.c)
costs <- tenTT$totalcost
tenTT.data <- list(
  Np=length(arm), Nx=dim(Xs)[2], cost=costs/1000, X=Xs, tr=arm
)


## ----cost_model_script, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------
## gamma analysis model for total costs
cost.gamma.mod <- "
# cost = total costs in £1000s
# treatment received is indexed by tr (1 = control and 2 = intervention)

model{
	for (i in 1:Np) { # loop through individuals with cost data
		cost[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
		log(mu.c[i]) <- zeta0[tr[i]] + inprod(zeta[1:Nx,tr[i]],X[i,])
		rate.c[i] <- shape.c[tr[i]]/mu.c[i]
		# predicting cost assuming all participants receive treatment 1
		pred.c1[i] <- exp(zeta0[1] + inprod(zeta[1:Nx,1],X[i,1:Nx]))
		# predicting cost assuming all participants receive treatment 2
		pred.c2[i] <- exp(zeta0[2] + inprod(zeta[1:Nx,2],X[i,1:Nx]))
		Ctot.diff[i] <- pred.c2[i] - pred.c1[i] 
		
    #get dataset predictions to compare
    cost.rep[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
	}

	for (k in 1:2) { # 2 treatment arms	
		# prior distributions
		zeta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {zeta[i,k] ~ dnorm(0,0.01)} 
		shape.c[k] ~ dgamma(0.001,0.001)
	}

	# calculating cost increment using recycled predictions
	AveC[1] <- mean(pred.c1[])*1000
	AveC[2] <- mean(pred.c2[])*1000
	Cinc <- mean(Ctot.diff[])*1000
	p.Cinc <- 1-step(Cinc) # probability favours new treatment
}
"


## ----pre_processing, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'--------------------------------------
inits1 <- list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 12345)
inits2 <- list(".RNG.name" = "base::Wichmann-Hill",".RNG.seed" = 98765)
cost.inits1 <- c(list(zeta0=rep(0.5,2),zeta=matrix(rep(c(0.1,0.1,0.1),2),3,2),
               shape.c=c(1,1)),inits1)
cost.inits2 <- c(list(zeta0=rep(1,2),zeta=matrix(rep(c(-0.1,-0.1,-0.1),2),3,2),
               shape.c=c(3,3)),inits2)
cost.pars <- c("zeta0","zeta","shape.c","AveC","Cinc","p.Cinc","cost.rep")
cost.summary.pars <- c("AveC","Cinc","p.Cinc")


## ----import_cost_res, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------
# import previously saved results to save time
g1.summary <- readRDS("data/g1.summary.rds")
c.data.rep <- readRDS("data/c.data.rep.rds")


## ----run_model_cost, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE, results='hide', cache=TRUE-------------------------
# # run model
# set.seed(123)
# g1.out <- run.jags(
#   model=cost.gamma.mod, monitor=cost.pars, data=tenTT.data, n.chains=2,
#   inits=list(cost.inits1,cost.inits2), burnin = 1000, sample = 5000,
#   adapt = 1000, thin=1, modules=c('glm','dic')
# )


## ----results_model_cost, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'---------------------------------
# # create summaries
# g1.summary <- summary(g1.out,vars=cost.summary.pars)[,1:5]


## ----results2_model_cost, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE------------------------------------------------
# show results
g1.summary


## ----pre_processing_data_qol, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------
# standardise baseline qol
bq <- tenTT$hrql_0 
bq.c <- (bq - mean(bq))/sd(bq) 

# create a matrix of EQ-5D measurements: 3, 6, 12, 18 and 24 months
qolmat <- cbind(tenTT$hrql_3,tenTT$hrql_6,tenTT$hrql_12,
                tenTT$hrql_18,tenTT$hrql_24)
qoldec <- 1-qolmat # calculate qol decrements
nts <- dim(qoldec)[2] # time-points
nps <- dim(qoldec)[1] # patients

hmat <- ifelse(qoldec==0,1,0) # hurdle indicator

epsilon <- 0.0001 # use to ensure q is positive for Gamma models
tenTT.data <- list(Np=nps, Nt=nts,  Nx=dim(Xs)[2], h=hmat, 
                   q=qoldec+epsilon, bq=bq.c, X=Xs, tr=arm)


## ----qol_model_script, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------
## hurdle model for utilities
qol.hurdle.mod <- "
# q = QoL decrements (1-QoL)
# h = 1 if q=0, 0 if q>0 (zero value indicator)
# hurdle at 0 (priors on shape (s) and mean (mu) coeffs)
# gamma model for non-zeros
# treatment arm is indexed by tr (1 = control and 2 = intervention)

model{
	for (t in 1:Nt) { # time-points
		for (i in 1:Np) { # Np individuals 
			h[i,t] ~ dbern(p[i,t]) # 1 indicates zeros model
			logit(p[i,t]) <- gamma[t,tr[i]] + omega0[tr[i]]*bq[i] + 
			inprod(omega[1:Nx,tr[i]],X[i,1:Nx])
			d[i,t] <- h[i,t]+1 # model index
			q[i,t] ~ dgamma(shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
			pred.h[i,t] ~ dbern(p[i,t])
			pred.q[i,t] <- (1-pred.h[i,t])*mu.q[i,t] # prediction is 0 if h=1
			# non-zeros model
			log(mu.q[i,t]) <- alpha[t,tr[i]] + theta[i] + beta0[tr[i]]*bq[i] + 
			inprod(beta[1:Nx,tr[i]],X[i,])
			rate.q[1,i,t] <- shape.q[1,tr[i]]/mu.q[i,t]
			# zeros model - not used in QALY increment calculation
			rate.q[2,i,t] <- shape.q[2,tr[i]]/mu.q0
			# calculate residuals
			resid.q[i,t] <- q[i,t] - pred.q[i,t]
			# predicting qol assuming all participants have treatment 1
			h1[i,t] ~ dbern(p1[i,t])
			logit(p1[i,t]) <- gamma[t,1] + omega0[1]*bq[i] + 
			inprod(omega[1:Nx,1],X[i,1:Nx])
			pred.q1[i,t] <- exp(alpha[t,1] + theta1[i] + beta0[1]*bq[i] +
			inprod(beta[1:Nx,1],X[i,1:Nx])) * (1-h1[i,t])  # 0 if h0=1 
			# predicting qol assuming all participants have treatment 2
			h2[i,t] ~ dbern(p2[i,t])
			logit(p2[i,t]) <- gamma[t,2] + omega0[2]*bq[i] + 
			inprod(omega[1:Nx,2],X[i,1:Nx]) 
			pred.q2[i,t] <- exp(alpha[t,2] + theta2[i] + beta0[2]*bq[i] +
			inprod(beta[1:Nx,2],X[i,1:Nx])) * (1-h2[i,t])  # 0 if h0=1
			diff.q[i,t] <- pred.q1[i,t] - pred.q2[i,t] 
			
		}
	}

	for (i in 1:Np) { # individual random effects for QoL marginal model
		theta[i] ~ dnorm(theta.mu[tr[i]],theta.tau[tr[i]])
		theta1[i] ~ dnorm(theta.mu[1],theta.tau[1])
		theta2[i] ~ dnorm(theta.mu[2],theta.tau[2])
	} 


	for (k in 1:2) { # 2 treatment arms	
		# prior distributions for QoL marginal sub-model
		for (t in 1:Nt) {gamma[t,k] ~ dlogis(0,1)} # fixed effects for hurdle
		omega0[k] ~ dnorm(0,0.368)
		for (i in 1:Nx) {omega[i,k] ~ dnorm(0,0.368)} 
		alpha[1,k] <- 0
		for (t in 2:Nt) {alpha[t,k] ~ dnorm(0,0.01)} # fixed effects
		beta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {beta[i,k] ~ dnorm(0,0.01)} 
		shape.q[1,k] ~ dunif(0,100)

		theta.mu[k] ~ dnorm(0,0.01)
		theta.sigma[k] ~ dunif(0,100)

		# node transformations
		theta.sigma2[k] <- pow(theta.sigma[k],2)
		theta.tau[k] <-  1/theta.sigma2[k]
	}

	# set mean and sd of zeros model to induce a spike close to 0
	mu.q0 <- 0.0001
	for (k in 1:2) {shape.q[2,k] <- 0.0001}	

	# convert to QALY diff over 2 year period
	for (i in 1:Np) { # Np individuals
		# QALYs assuming all patients receive treatment 1
		Qaly1[i] <- 0.125*bq[i] + 0.25*(1-pred.q1[i,1]) + 
		0.375*(1-pred.q1[i,2]) + 0.5*sum((1-pred.q1[i,3:4])) +
		0.25*(1-pred.q1[i,5])
		# QALYs assuming all patients receive treatment 2
		Qaly2[i] <- 0.125*bq[i] + 0.25*(1-pred.q2[i,1]) + 
		0.375*(1-pred.q2[i,2]) + 0.5*sum((1-pred.q2[i,3:4])) + 
		0.25*(1-pred.q2[i,5])
		Q.diff[i] <- Qaly2[i] - Qaly1[i] # QALY difference
	}
	
	# calculating QALY increment using recycled predictions
	AveQ[1] <- mean(Qaly1[])
	AveQ[2] <- mean(Qaly2[])
	Qinc <- mean(Q.diff[]) 
	p.Qinc <- step(Qinc) # probability favours new treatment

}
"


## ----pre_processing_qol, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'---------------------------------
qol.inits1 <- list(gamma=matrix(rep(0.1,10),5,2),omega0=rep(0.1,2),
               omega=matrix(rep(c(0.1,0.1,0.1),2),3,2),
               alpha=matrix(rep(c(NA,rep(0.1,4)),2),5,2),beta0=rep(0.1,2),
               beta=matrix(rep(c(0.1,0.1,0.1),2),3,2),
               shape.q=matrix(rep(c(1,NA),2),2,2),theta.mu=rep(0.1,2),
               theta.sigma=rep(0.05,2))
qol.inits2 <- list(gamma=matrix(rep(0.3,10),5,2),omega0=rep(-0.1,2),
               omega=matrix(rep(c(-0.1,-0.1,-0.1),2),3,2),
               alpha=matrix(rep(c(NA,rep(0.3,4)),2),5,2),beta0=rep(-0.1,2),
               beta=matrix(rep(c(-0.1,-0.1,-0.1),2),3,2),
               shape.q=matrix(rep(c(3,NA),2),2,2),theta.mu=rep(0.3,2),
               theta.sigma=rep(0.1,2))
qol.pars <- c("gamma","omega0","omega","alpha","beta0","beta","shape.q",
              "theta.mu","theta.sigma","AveQ","Qinc","p.Qinc")
qol.summary.pars <- c("AveQ","Qinc","p.Qinc")


## ----import_qol_res, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-------------------------------------
q1.summary <- readRDS("data/q1.summary.rds")


## ----run_model_qol, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE, results='hide', cache=TRUE--------------------------
# #run model
# set.seed(123)
# q1.out <- run.jags(model=qol.hurdle.mod, monitor=qol.pars, data=tenTT.data,
#                    inits=list(qol.inits1,qol.inits2),
#                    n.chains=2, burnin = 5000, sample = 1000, adapt = 1000,
#                    thin=1, modules=c('glm','dic'))


## ----results_model_q, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------------------
# # show results
# q1.summary <- summary(q1.out,vars=qol.summary.pars)[,1:5]


## ----results2_model_q, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------
# show results
q1.summary


## ----pre_processing_data_joint, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'--------------------------
# joint model
arm <- tenTT$arm+1
age <- (tenTT$age - mean(tenTT$age))/sd(tenTT$age)
bmi <- tenTT$bmicat-1
bmi.c <- (bmi - mean(bmi))/sd(bmi)
sex <- tenTT$sex-1
sex.c <- (sex - mean(sex))/sd(sex)
Xs <- cbind(age,bmi.c,sex.c)
bq <- tenTT$hrql_0 # baseline qol
bq.c <- (bq - mean(bq))/sd(bq) 
costs <- tenTT$totalcost
# creating a matrix of EQ-5D measurements: 3, 6, 12, 18 and 24 months
qolmat <- cbind(tenTT$hrql_3,tenTT$hrql_6,tenTT$hrql_12,tenTT$hrql_18,tenTT$hrql_24)
qoldec <- 1-qolmat
nts <- dim(qoldec)[2] # 5 time-points 
nps <- dim(qoldec)[1] # 535 patients
bq <- tenTT$hrql_0
bq.c <- (bq - mean(bq))/sd(bq)
hmat <- ifelse(qoldec==0,1,0) # hurdle indicator
epsilon <- 0.0001 # use to ensure q is positive for Gamma models
tenTT.data <- list(Np=nps, Nt=nts,  Nx=dim(Xs)[2], h=hmat, 
                   q=qoldec+epsilon, cost=costs/1000, bq=bq.c, X=Xs, tr=arm)


## ----joint_model_script, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'---------------------------------
# ## joint model for costs and utilities
# mar.mod1 <- "
# model{
# 	# code for QoL marginal sub-model ---
# 	# full code available in the electronic supplement
# 
# 	# Cost conditional sub-model
# 	for (i in 1:Np) { # loop through individuals with cost data
# 	  # new code to calculate QALYS
# 		# switch from QoL decrements to QoL to calculate QALYs
# 		Qtot[i] <- 0.125*bq[i] + 0.25*(1-q[i,1]) + 0.375*(1-q[i,2]) +
# 		0.5*sum((1-q[i,3:4])) + 0.25*(1-q[i,5])
# 		Qmu[i] <- 0.125*bq[i] + 0.25*(1-pred.q[i,1]) + 0.375*(1-pred.q[i,2]) +
# 		0.5*sum((1-pred.q[i,3:4])) + 0.25*(1-pred.q[i,5])
# 		
# 		# new term added to link to QoL sub-model
# 		cost[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
# 		log(mu.c[i]) <- zeta0[tr[i]] + inprod(zeta[1:Nx,tr[i]],X[i,]) +
# 		xi[tr[i]]*(Qtot[i]-Qmu[i])
# 		# remaining lines as before
# 	}
# 
# 	for (k in 1:2) { # 2 treatment arms	
# 		xi[k] ~ dnorm(0,0.01)
# 		# other priors specified as before
# 	}
# 
# 	# code for converting to QALY diff over 2 year period ---
# 	# code for calculating QALY increment using recycled predictions ---
# 	# code for calculating cost increment using recycled predictions ---
# }
# "


## ----joint_model_script_fit, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------
## joint
mar.mod1 <- "
model{
	# QoL marginal sub-model
	for (t in 1:Nt) { # 5 time-points
		for (i in 1:Np) { # Np individuals 
			h[i,t] ~ dbern(p[i,t]) # 1 indicates zeros model
			logit(p[i,t]) <- gamma[t,tr[i]] + omega0[tr[i]]*bq[i] +
			inprod(omega[1:Nx,tr[i]],X[i,1:Nx])
			d[i,t] <- h[i,t]+1 # model index
			q[i,t] ~ dgamma(shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
			pred.h[i,t] ~ dbern(p[i,t])
			pred.q[i,t] <- (1-pred.h[i,t])*mu.q[i,t] # prediction is 0 if h=1
			# non-zeros model
			log(mu.q[i,t]) <- alpha[t,tr[i]] + theta[i] + beta0[tr[i]]*bq[i] +
			inprod(beta[1:Nx,tr[i]],X[i,])
			rate.q[1,i,t] <- shape.q[1,tr[i]]/mu.q[i,t]
			# zeros model - not used in QALY increment calculation
			rate.q[2,i,t] <- shape.q[2,tr[i]]/mu.q0
			# calculate residuals
			resid.q[i,t] <- q[i,t] - pred.q[i,t]
			# predicting qol assuming all participants have treatment 1
			h1[i,t] ~ dbern(p1[i,t])
			logit(p1[i,t]) <- gamma[t,1] + omega0[1]*bq[i] + 
			inprod(omega[1:Nx,1],X[i,1:Nx])
			pred.q1[i,t] <- exp(alpha[t,1] + theta1[i] + beta0[1]*bq[i] +
			inprod(beta[1:Nx,1],X[i,1:Nx])) * (1-h1[i,t])  # 0 if h0=1 
			# predicting qol assuming all participants have treatment 2
			h2[i,t] ~ dbern(p2[i,t])
			logit(p2[i,t]) <- gamma[t,2] + omega0[2]*bq[i] + 
			inprod(omega[1:Nx,2],X[i,1:Nx]) 
			pred.q2[i,t] <- exp(alpha[t,2] + theta2[i] + beta0[2]*bq[i] +
			inprod(beta[1:Nx,2],X[i,1:Nx])) * (1-h2[i,t])  # 0 if h0=1
			diff.q[i,t] <- pred.q1[i,t] - pred.q2[i,t] 
			
			#get log densities to assess fit
		  ldens.q[i,t] <- logdensity.gamma(q[i,t],
		  shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
		  #get dataset predictions to compare
		  q.rep[i,t] ~ dgamma(shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
		}
	}

	for (i in 1:Np) { # individual random effects for QoL marginal model
		theta[i] ~ dnorm(theta.mu[tr[i]],theta.tau[tr[i]])
		theta1[i] ~ dnorm(theta.mu[1],theta.tau[1])
		theta2[i] ~ dnorm(theta.mu[2],theta.tau[2])
	}

	# Cost conditional sub-model
	for (i in 1:Np) { # individuals with cost data
		# switch from QoL decrements to QoL to calculate QALYs
		Qtot[i] <- 0.125*bq[i] + 0.25*(1-q[i,1]) + 0.375*(1-q[i,2]) + 
		0.5*sum((1-q[i,3:4])) + 0.25*(1-q[i,5])
		Qmu[i] <- 0.125*bq[i] + 0.25*(1-pred.q[i,1]) + 0.375*(1-pred.q[i,2]) +
		0.5*sum((1-pred.q[i,3:4])) + 0.25*(1-pred.q[i,5])
		cost[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
		log(mu.c[i]) <- zeta0[tr[i]] + inprod(zeta[1:Nx,tr[i]],X[i,]) +
		xi[tr[i]]*(Qtot[i]-Qmu[i])
		rate.c[i] <- shape.c[tr[i]]/mu.c[i]
		# calculate residuals
		resid.c[i] <- cost[i] - mu.c[i] 
		# predicting cost assuming all participants receive treatment 1
		pred.c1[i] <- exp(zeta0[1] + inprod(zeta[1:Nx,1],X[i,1:Nx]))
		# predicting cost assuming all participants receive treatment 2
		pred.c2[i] <- exp(zeta0[2] + inprod(zeta[1:Nx,2],X[i,1:Nx]))
		Ctot.diff[i] <- pred.c2[i] - pred.c1[i] 
		
		#get log densities to assess fit
		ldens.cost[i] <- logdensity.gamma(cost[i],shape.c[tr[i]],rate.c[i])
		#get dataset predictions to compare
		cost.rep[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
	}

	for (k in 1:2) { # 2 treatment arms	
		# prior distributions for QoL marginal sub-model
		for (t in 1:Nt) {gamma[t,k] ~ dlogis(0,1)} # time fixed effects
		omega0[k] ~ dnorm(0,0.368)
		for (i in 1:Nx) {omega[i,k] ~ dnorm(0,0.368)} 
		alpha[1,k] <- 0
		for (t in 2:Nt) {alpha[t,k] ~ dnorm(0,0.01)} # time fixed effects
		beta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {beta[i,k] ~ dnorm(0,0.01)} 
		shape.q[1,k] ~ dunif(0,100)

		theta.mu[k] ~ dnorm(0,0.01)
		theta.sigma[k] ~ dunif(0,100)

		# prior distributions for cost conditional sub-model
		zeta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {zeta[i,k] ~ dnorm(0,0.01)} 
		xi[k] ~ dnorm(0,0.01)
		shape.c[k] ~ dgamma(0.001,0.001)

		# node transformations
		theta.sigma2[k] <- pow(theta.sigma[k],2)
		theta.tau[k] <-  1/theta.sigma2[k]

	}

	# set mean and sd of zeros model to induce a spike close to 0
	mu.q0 <- 0.0001
	for (k in 1:2) {shape.q[2,k] <- 0.0001}

	# convert to QALY diff over 2 year period

	for (i in 1:Np) { # Np individuals
		# QALYs assuming all patients receive treatment 1
		Qaly1[i] <- 0.125*bq[i] + 0.25*(1-pred.q1[i,1]) + 
		0.375*(1-pred.q1[i,2]) + 0.5*sum((1-pred.q1[i,3:4])) + 
		0.25*(1-pred.q1[i,5])
		# QALYs assuming all patients receive treatment 2
		Qaly2[i] <- 0.125*bq[i] + 0.25*(1-pred.q2[i,1]) + 
		0.375*(1-pred.q2[i,2]) + 0.5*sum((1-pred.q2[i,3:4])) + 
		0.25*(1-pred.q2[i,5])
		Q.diff[i] <- Qaly2[i] - Qaly1[i] # QALY difference
	}
	
	# calculating QALY increment using recycled predictions
	AveQ[1] <- mean(Qaly1[])
	AveQ[2] <- mean(Qaly2[])
	Qinc <- mean(Q.diff[]) 
	p.Qinc <- step(Qinc) # probability favours new treatment	

	# calculating cost increment using recycled predictions
	AveC[1] <- mean(pred.c1[])*1000
	AveC[2] <- mean(pred.c2[])*1000
	Cinc <- mean(Ctot.diff[])*1000
	p.Cinc <- 1-step(Cinc) # probability favours new trt

} 
"


## ----pre_processing_joint, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'--------------------------------
joint.inits1 <- c(cost.inits1,qol.inits1)
joint.inits1$xi <- c(-1,-1)
joint.inits2 <- c(cost.inits2,qol.inits2)
joint.inits2$xi <- c(-1,-1)
joint.pars <- c(cost.pars,qol.pars,"xi")
joint.summary.pars <- c(cost.summary.pars,qol.summary.pars)


## ----import_joint_res, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------------
# import previously saved results to save time
j1.summary <- readRDS("data/j1.summary_new.rds")


## ----run_model_joint, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE, results='hide'------------------------------------
# #run model
# set.seed(123)
# j1.out <- run.jags(
#   model=mar.mod1, monitor=joint.pars, data=tenTT.data, n.chains=2,
#   inits=list(joint.inits1,joint.inits2),burnin = 5000, sample = 1000,
#   adapt = 1000,thin=1,modules=c('glm','dic')
# )


## ----results_model_joint, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'--------------------------------
# # show results
# j1.summary <- summary(j1.out,vars=joint.summary.pars)[,1:5]


## ----results2_model_joint, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE-----------------------------------------------
# show results
j1.summary


## ----selection_model_script_full, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'------------------------
## selection model for utilities
mnar.mod1 <- "
model{
	# code for QoL marginal sub-model --
	
	for (t in 1:Nt) { # 5 time-points
		for (i in 1:Np) { # Np individuals 
			h[i,t] ~ dbern(p[i,t]) # 1 indicates zeros model
			logit(p[i,t]) <- gamma[t,tr[i]] + omega0[tr[i]]*bq[i] +
			inprod(omega[1:Nx,tr[i]],X[i,1:Nx])
			d[i,t] <- h[i,t]+1 # model index
			q[i,t] ~ dgamma(shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
			pred.h[i,t] ~ dbern(p[i,t])
			pred.q[i,t] <- (1-pred.h[i,t])*mu.q[i,t] # prediction is 0 if h=1
			# non-zeros model
			log(mu.q[i,t]) <- alpha[t,tr[i]] + theta[i] + beta0[tr[i]]*bq[i] +
			inprod(beta[1:Nx,tr[i]],X[i,])
			rate.q[1,i,t] <- shape.q[1,tr[i]]/mu.q[i,t]
			# zeros model - not used in QALY increment calculation
			rate.q[2,i,t] <- shape.q[2,tr[i]]/mu.q0
			# calculate residuals
			resid.q[i,t] <- q[i,t] - pred.q[i,t]
			# predicting qol assuming all participants have treatment 1
			h1[i,t] ~ dbern(p1[i,t])
			logit(p1[i,t]) <- gamma[t,1] + omega0[1]*bq[i] + 
			inprod(omega[1:Nx,1],X[i,1:Nx])
			pred.q1[i,t] <- exp(alpha[t,1] + theta1[i] + beta0[1]*bq[i] +
			inprod(beta[1:Nx,1],X[i,1:Nx])) * (1-h1[i,t])  # 0 if h0=1 
			# predicting qol assuming all participants have treatment 2
			h2[i,t] ~ dbern(p2[i,t])
			logit(p2[i,t]) <- gamma[t,2] + omega0[2]*bq[i] + 
			inprod(omega[1:Nx,2],X[i,1:Nx]) 
			pred.q2[i,t] <- exp(alpha[t,2] + theta2[i] + beta0[2]*bq[i] +
			inprod(beta[1:Nx,2],X[i,1:Nx])) * (1-h2[i,t])  # 0 if h0=1
			diff.q[i,t] <- pred.q1[i,t] - pred.q2[i,t] 
			
			#get log densities to assess fit
		  ldens.q[i,t] <- logdensity.gamma(q[i,t],
		  shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
		  #get dataset predictions to compare
		  q.rep[i,t] ~ dgamma(shape.q[d[i,t],tr[i]],rate.q[d[i,t],i,t])
		}
	}

	for (i in 1:Np) { # individual random effects for QoL marginal model
		theta[i] ~ dnorm(theta.mu[tr[i]],theta.tau[tr[i]])
		theta1[i] ~ dnorm(theta.mu[1],theta.tau[1])
		theta2[i] ~ dnorm(theta.mu[2],theta.tau[2])
	}

	# Cost conditional sub-model
	for (i in 1:Np) { # individuals with cost data
		# switch from QoL decrements to QoL to calculate QALYs
		Qtot[i] <- 0.125*bq[i] + 0.25*(1-q[i,1]) + 0.375*(1-q[i,2]) + 
		0.5*sum((1-q[i,3:4])) + 0.25*(1-q[i,5])
		Qmu[i] <- 0.125*bq[i] + 0.25*(1-pred.q[i,1]) + 0.375*(1-pred.q[i,2]) +
		0.5*sum((1-pred.q[i,3:4])) + 0.25*(1-pred.q[i,5])
		cost[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
		log(mu.c[i]) <- zeta0[tr[i]] + inprod(zeta[1:Nx,tr[i]],X[i,]) +
		xi[tr[i]]*(Qtot[i]-Qmu[i])
		rate.c[i] <- shape.c[tr[i]]/mu.c[i]
		# calculate residuals
		resid.c[i] <- cost[i] - mu.c[i] 
		# predicting cost assuming all participants receive treatment 1
		pred.c1[i] <- exp(zeta0[1] + inprod(zeta[1:Nx,1],X[i,1:Nx]))
		# predicting cost assuming all participants receive treatment 2
		pred.c2[i] <- exp(zeta0[2] + inprod(zeta[1:Nx,2],X[i,1:Nx]))
		Ctot.diff[i] <- pred.c2[i] - pred.c1[i] 
		
		#get log densities to assess fit
		ldens.cost[i] <- logdensity.gamma(cost[i],shape.c[tr[i]],rate.c[i])
		#get dataset predictions to compare
		cost.rep[i] ~ dgamma(shape.c[tr[i]],rate.c[i])
	}

	for (k in 1:2) { # 2 treatment arms	
		# prior distributions for QoL marginal sub-model
		for (t in 1:Nt) {gamma[t,k] ~ dlogis(0,1)} # time fixed effects
		omega0[k] ~ dnorm(0,0.368)
		for (i in 1:Nx) {omega[i,k] ~ dnorm(0,0.368)} 
		alpha[1,k] <- 0
		for (t in 2:Nt) {alpha[t,k] ~ dnorm(0,0.01)} # time fixed effects
		beta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {beta[i,k] ~ dnorm(0,0.01)} 
		shape.q[1,k] ~ dunif(0,100)

		theta.mu[k] ~ dnorm(0,0.01)
		theta.sigma[k] ~ dunif(0,100)

		# prior distributions for cost conditional sub-model
		zeta0[k] ~ dnorm(0,0.01)
		for (i in 1:Nx) {zeta[i,k] ~ dnorm(0,0.01)} 
		xi[k] ~ dnorm(0,0.01)
		shape.c[k] ~ dgamma(0.001,0.001)

		# node transformations
		theta.sigma2[k] <- pow(theta.sigma[k],2)
		theta.tau[k] <-  1/theta.sigma2[k]

	}

	# set mean and sd of zeros model to induce a spike close to 0
	mu.q0 <- 0.0001
	for (k in 1:2) {shape.q[2,k] <- 0.0001}

	# convert to QALY diff over 5 year period
	for (i in 1:Np) { # Np individuals
		# QALYs assuming all patients receive treatment 1
		Qaly1[i] <- 0.125*bq[i] + 0.25*(1-pred.q1[i,1]) + 
		0.375*(1-pred.q1[i,2]) + 0.5*sum((1-pred.q1[i,3:4])) + 
		0.25*(1-pred.q1[i,5])
		# QALYs assuming all patients receive treatment 2
		Qaly2[i] <- 0.125*bq[i] + 0.25*(1-pred.q2[i,1]) + 
		0.375*(1-pred.q2[i,2]) + 0.5*sum((1-pred.q2[i,3:4])) + 
		0.25*(1-pred.q2[i,5])
		Q.diff[i] <- Qaly2[i] - Qaly1[i] # QALY difference
	}
	
	# calculating QALY increment using recycled predictions
	AveQ[1] <- mean(Qaly1[])
	AveQ[2] <- mean(Qaly2[])
	Qinc <- mean(Q.diff[]) 
	p.Qinc <- step(Qinc) # probability favours new treatment	

	# calculating cost increment using recycled predictions
	AveC[1] <- mean(pred.c1[])*1000
	AveC[2] <- mean(pred.c2[])*1000
	Cinc <- mean(Ctot.diff[])*1000
	p.Cinc <- 1-step(Cinc) # probability favours new trt

# specify response missingness model for HRQoL
# m are the missingness pattern indicators for utilities (1,2,3)

for (i in 1:Np) { #loop through individuals
 for(t in 1:Nt) { # loop through times
  count[i,t] ~ dcat(m[i,t,1:3])
  for(r in 1:3){
  m[i,t,r] <- phi[i,t,r]/sum(phi[i,t,])
  log(phi[i,t,r]) <- eta0[r,tr[i]] + inprod(eta[r,1:Nx,tr[i]],X[i,1:Nx]) + 
    lambda.val[r,tr[i]]*q[i,t]
  }
 }
}

# priors for response missingness model

 for(k in 1:2) { #2 tr arms
 eta0[1,k] <- 0
 for(i in 1:3) { eta[1,i,k] <- 0}
 for(r in 2:3){
  eta0[r,k] ~ dlogis(0,1)
  for(i in 1:3) {eta[r,i,k] ~ dnorm(0,0.01)}
  }
 }

}

"


## ----selection_model_script, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-----------------------------
# mnar.mod1<- "
# model{
# # code for the joint model --
# # full code provided in the electronic supplement
# 
# # specify response missingness model for HRQoL
# # m are the missingness pattern indicators for utilities (1,2,3)
# 
# for (i in 1:Np) { #loop through individuals
#  for(t in 1:Nt) { # loop through times
#   count[i,t] ~ dcat(m[i,t,1:3])
#   for(r in 1:3){
#   m[i,t,r] <- phi[i,t,r]/sum(phi[i,t,])
#   log(phi[i,t,r]) <- eta0[r,tr[i]] + inprod(eta[r,1:Nx,tr[i]],X[i,1:Nx]) +
#     lambda.val[r,tr[i]]*q[i,t]
#   }
#  }
# }
# 
# # priors for response missingness model
#  for(k in 1:2) { #2 tr arms
#  eta0[1,k] <- 0
#  for(i in 1:3) { eta[1,i,k] <- 0}
#  for(r in 2:3){
#   eta0[r,k] ~ dlogis(0,1)
#   for(i in 1:3) {eta[r,i,k] ~ dnorm(0,0.01)}
#   }
#  }
# }
# "


## ----sm_preprocess_p1, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE---------------------------------------------------
#create indicator for type of missingness
#pattern: 1=full, 2=interm, 3=drop
count<-qoldec
count<-ifelse(is.na(qoldec)==FALSE,1,qoldec)
count[is.na(qoldec[,1])==TRUE & is.na(qoldec[,2])==TRUE &
        is.na(qoldec[,3])==TRUE & is.na(qoldec[,4])==TRUE &
        is.na(qoldec[,5])==TRUE,]<-3
count[is.na(qoldec[,1])==FALSE & is.na(qoldec[,2])==TRUE & 
        is.na(qoldec[,3])==TRUE & is.na(qoldec[,4])==TRUE &
        is.na(qoldec[,5])==TRUE,2:5]<- 3
count[is.na(qoldec[,1])==FALSE & is.na(qoldec[,2])==FALSE &
        is.na(qoldec[,3])==TRUE & is.na(qoldec[,4])==TRUE &
        is.na(qoldec[,5])==TRUE,3:5]<-3
count[is.na(qoldec[,1])==FALSE & is.na(qoldec[,2])==FALSE &
        is.na(qoldec[,3])==FALSE & is.na(qoldec[,4])==TRUE &
        is.na(qoldec[,5])==TRUE,4:5]<- 3
count[is.na(qoldec[,1])==FALSE & is.na(qoldec[,2])==FALSE &
        is.na(qoldec[,3])==FALSE & is.na(qoldec[,4])==FALSE &
        is.na(qoldec[,5])==TRUE,5]<- 3
count[,1]<-ifelse(is.na(count[,1])==TRUE,2,count[,1])
count[,2]<-ifelse(is.na(count[,2])==TRUE & 
                    is.na(count[,3])==FALSE,2,count[,2])
count[,2]<-ifelse(is.na(count[,2])==TRUE & 
                    is.na(count[,4])==FALSE,2,count[,2])
count[,2]<-ifelse(is.na(count[,2])==TRUE & 
                    is.na(count[,5])==FALSE,2,count[,2])
count[,3]<-ifelse(is.na(count[,3])==TRUE & 
                    is.na(count[,4])==FALSE,2,count[,3])
count[,3]<-ifelse(is.na(count[,3])==TRUE & 
                    is.na(count[,5])==FALSE,2,count[,3])
count[,4]<-ifelse(is.na(count[,4])==TRUE & 
                    is.na(count[,5])==FALSE,2,count[,4])
count<-ifelse(is.na(count)==TRUE,3,count)


## ----sm_preprocess_p2, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE----------------------------------------------------
#define values for lambda in each scenario
lambda.zero.neg<-lambda.neg.zero<-lambda.neg<-lambda.mar<-matrix(NA,3,2)
lambda.zero.neg[1:2,]<-lambda.neg.zero[1:2,]<-
  lambda.mar[1:2,]<-lambda.neg[1:2,]<-0
lambda.neg.zero[3,] <- c(- 0.41, 0) # neg MNAR scenario for t=1
lambda.zero.neg[3,] <- c(0, - 0.41) # neg MNAR scenario for t=2
lambda.neg[3,] <- c(- 0.41, -0.41) # neg MNAR scenario for t=1,2
lambda.mar[3,] <- c(0, 0) # MAR scenario


## ----pre_processing_selection, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'----------------------------
#include all inputs in a list to be passed to the model
tenTT.data <- list(Np=nps, Nt=nts,  Nx=dim(Xs)[2], h=hmat, count=count, 
                   lambda.val = lambda.neg.zero,
                   q=qoldec+epsilon, cost=costs/1000, 
                   bq=bq.c, X=Xs, tr=arm)


## ----import_selection_res, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-------------------------------
mu.c_sm.mnar.list.all <- readRDS("data/mu.c_sm.mnar.list.all_new.rds")
mu.c_sm.mnar.list.mean <- readRDS("data/mu.c_sm.mnar.list.mean_new.rds")
mu.c_sm.mnar.list.quantiles <- readRDS("data/mu.c_sm.mnar.list.quantiles_new.rds")
mu.e_sm.mnar.list.all <- readRDS("data/mu.e_sm.mnar.list.all_new.rds")
mu.e_sm.mnar.list.mean <- readRDS("data/mu.e_sm.mnar.list.mean_new.rds")
mu.e_sm.mnar.list.quantiles <- readRDS("data/mu.e_sm.mnar.list.quantiles_new.rds")


## ----run_model_selection_pos, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE, results='hide'----------------------------
# #run model under negative MNAR (control)
# set.seed(123)
# sm.mnar.neg.zero1.out <- run.jags(
#   model=mnar.mod1, monitor=joint.pars, data=tenTT.data,n.chains=2,
#   inits=list(joint.inits1,joint.inits2),burnin = 5000, sample = 1000,
#   adapt = 1000,thin=1,modules=c('glm','dic')
# )


## ----run_model_selection_mar_neg, echo=TRUE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE, results='hide'------------------------
# #store model results in a list
# mnar.models.list <- vector(mode = "list", length = 4)
# mnar.models.list[[1]] <- sm.mnar.neg.zero1.out
# 
# #loop through scenarios and update lambda.val inputs
# for(i in 2:4){
#  if(i == 2){# negative MNAR (intervention)
#   tenTT.data$lambda.val <- lambda.zero.neg}
#  if(i == 3){# negative MNAR (both)
#   tenTT.data$lambda.val <- lambda.neg}
#  if(i == 4){# MAR
#   tenTT.data$lambda.val <- lambda.mar}
# 
# #run models
# set.seed(123)
# mnar.models.list[[i]] <- run.jags(
#   model=mnar.mod1, monitor=joint.pars, data=tenTT.data,n.chains=2,
#   inits=list(joint.inits1,joint.inits2),burnin = 5000,sample = 1000,
#   adapt = 1000,thin=1,modules=c('glm','dic')
# )
# }


## ----post_proc_selection, echo=FALSE, eval=FALSE, message=FALSE, warning=FALSE ,error=FALSE-----------------------------------------------
# #extract posterior mean and 95% CI for QALYs and costs
# mu.c_sm.mnar.list.all <- vector(mode = "list", length = 4)
# mu.c_sm.mnar.list.mean <- vector(mode = "list", length = 4)
# mu.c_sm.mnar.list.quantiles <- vector(mode = "list", length = 4)
# mu.e_sm.mnar.list.all <- vector(mode = "list", length = 4)
# mu.e_sm.mnar.list.mean <- vector(mode = "list", length = 4)
# mu.e_sm.mnar.list.quantiles <- vector(mode = "list", length = 4)
# 
# #loop through scenarios and save estimates
# for(i in 1:4){
# mu.c_sm.mnar.list.all[[i]] <- as.mcmc(mnar.models.list[[i]], vars = "AveC")
# mu.c_sm.mnar.list.mean[[i]] <- apply(mu.c_sm.mnar.list.all[[i]], 2, mean)
# mu.c_sm.mnar.list.quantiles[[i]] <- apply(mu.c_sm.mnar.list.all[[i]], 2,
#                                         hdi , credMass = 0.95)
# mu.e_sm.mnar.list.all[[i]] <- as.mcmc(mnar.models.list[[i]], vars = "AveQ")
# mu.e_sm.mnar.list.mean[[i]] <- apply(mu.e_sm.mnar.list.all[[i]], 2, mean)
# mu.e_sm.mnar.list.quantiles[[i]] <- apply(mu.e_sm.mnar.list.all[[i]], 2,
#                                         hdi , credMass = 0.95)
# }


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: stripplot
#| echo: false 
#| eval: false
#| message: false
#| warning: false
#| error: false 
#| results: 'hide' 
#| out.width: '90%'
#| out.extra: ''
#| fig.pos: 'H'
#| fig.cap: "Posterior density plots of the mean costs and QALYs for four alternative missingness scenarios"
#| fig.subcap: 
#|    - "Posterior density plots of the mean costs"
#|    - "Posterior density plots of the mean QALYs"

# par(mfrow=c(1,2), ps = 9.4,
#           oma = c(5,5,0,0) + 0.1,
#           mar = c(0,0,1,1) + 0.1)
# #costs
# plot(1000, xlim=c(1000,3500), ylim=c(-0.5,5.5), xlab="x", ylab="x", type="n",yaxt="n",ann = F,bty="n",axes = F)
# axis(1,at=c(1000,1500,2000,2500,3000,3500))
# axis(2,tick = F,labels = c("MNAR - (t=1)","MNAR - (t=2)","MNAR - (both)","MAR"),at=c(0.25,1.75,3.25,4.75),las=2)
# denstrip(mu.c_sm.mnar.list.all[[1]][,1], width=0.2, at=0, ticks=c(mu.c_sm.mnar.list.quantiles[[1]][1,1], mu.c_sm.mnar.list.quantiles[[1]][2,1]), mticks=mu.c_sm.mnar.list.mean[[1]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[1]][,2], width=0.2, at=0.5, ticks=c(mu.c_sm.mnar.list.quantiles[[1]][1,2], mu.c_sm.mnar.list.quantiles[[1]][2,2]), mticks=mu.c_sm.mnar.list.mean[[1]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[2]][,1], width=0.2, at=1.5, ticks=c(mu.c_sm.mnar.list.quantiles[[2]][1,1], mu.c_sm.mnar.list.quantiles[[2]][2,1]), mticks=mu.c_sm.mnar.list.mean[[2]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[2]][,2], width=0.2, at=2, ticks=c(mu.c_sm.mnar.list.quantiles[[2]][1,2], mu.c_sm.mnar.list.quantiles[[2]][2,2]), mticks=mu.c_sm.mnar.list.mean[[2]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[3]][,1], width=0.2, at=3, ticks=c(mu.c_sm.mnar.list.quantiles[[3]][1,1], mu.c_sm.mnar.list.quantiles[[3]][2,1]), mticks=mu.c_sm.mnar.list.mean[[3]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[3]][,2], width=0.2, at=3.5, ticks=c(mu.c_sm.mnar.list.quantiles[[3]][1,2], mu.c_sm.mnar.list.quantiles[[3]][2,2]), mticks=mu.c_sm.mnar.list.mean[[3]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[4]][,1], width=0.2, at=4.5, ticks=c(mu.c_sm.mnar.list.quantiles[[4]][1,1], mu.c_sm.mnar.list.quantiles[[4]][2,1]), mticks=mu.c_sm.mnar.list.mean[[4]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.c_sm.mnar.list.all[[4]][,2], width=0.2, at=5, ticks=c(mu.c_sm.mnar.list.quantiles[[4]][1,2], mu.c_sm.mnar.list.quantiles[[4]][2,2]), mticks=mu.c_sm.mnar.list.mean[[4]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# 
# #QALYs
# plot(0, xlim=c(0.5,1.5), ylim=c(-0.5,5.5), xlab="x", ylab="x", type="n",yaxt="n",ann = F,bty="n",axes = F)
# axis(1,at=c(0.5,0.75,1,1.25,1.5))
# axis(2,tick = F,labels = c("","","",""),at=c(0.25,1.75,3.25,4.75),las=2)
# denstrip(mu.e_sm.mnar.list.all[[1]][,1], width=0.2, at=0, ticks=c(mu.e_sm.mnar.list.quantiles[[1]][1,1], mu.e_sm.mnar.list.quantiles[[1]][2,1]), mticks=mu.e_sm.mnar.list.mean[[1]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[1]][,2], width=0.2, at=0.5, ticks=c(mu.e_sm.mnar.list.quantiles[[1]][1,2], mu.e_sm.mnar.list.quantiles[[1]][2,2]), mticks=mu.e_sm.mnar.list.mean[[1]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[2]][,1], width=0.2, at=1.5, ticks=c(mu.e_sm.mnar.list.quantiles[[2]][1,1], mu.e_sm.mnar.list.quantiles[[2]][2,1]), mticks=mu.e_sm.mnar.list.mean[[2]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[2]][,2], width=0.2, at=2, ticks=c(mu.e_sm.mnar.list.quantiles[[2]][1,2], mu.e_sm.mnar.list.quantiles[[2]][2,2]), mticks=mu.e_sm.mnar.list.mean[[2]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[3]][,1], width=0.2, at=3, ticks=c(mu.e_sm.mnar.list.quantiles[[3]][1,1], mu.e_sm.mnar.list.quantiles[[3]][2,1]), mticks=mu.e_sm.mnar.list.mean[[3]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[3]][,2], width=0.2, at=3.5, ticks=c(mu.e_sm.mnar.list.quantiles[[3]][1,2], mu.e_sm.mnar.list.quantiles[[3]][2,2]), mticks=mu.e_sm.mnar.list.mean[[3]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[4]][,1], width=0.2, at=4.5, ticks=c(mu.e_sm.mnar.list.quantiles[[4]][1,1], mu.e_sm.mnar.list.quantiles[[4]][2,1]), mticks=mu.e_sm.mnar.list.mean[[4]][1],colmax="red",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")
# denstrip(mu.e_sm.mnar.list.all[[4]][,2], width=0.2, at=5, ticks=c(mu.e_sm.mnar.list.quantiles[[4]][1,2], mu.e_sm.mnar.list.quantiles[[4]][2,2]), mticks=mu.e_sm.mnar.list.mean[[4]][2],colmax="blue",horiz=T,gamma = 0.7,tcol = "black",mcol = "black")


## ----cea_joint, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE-----------------------------------------------------------
cea_mar <- bcea(
  e=mu.e_sm.mnar.list.all[[4]], c=mu.c_sm.mnar.list.all[[4]], ref = 2, 
  Kmax = 100000
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: ceajointplots
#| echo: true 
#| eval: false
# cep_mar <- ceplane.plot(cea_mar, graph = "ggplot2")
# ceac_mar <- ceac.plot(cea_mar, graph = "ggplot2")


## ----cea_joint_plots_mnar, echo=FALSE, eval=TRUE, message=FALSE, warning=FALSE, error=FALSE, results='hide'-------------------------------
cea_mnar.zero.neg <- bcea(e=mu.e_sm.mnar.list.all[[1]], c=mu.c_sm.mnar.list.all[[1]], ref = 2, Kmax = 100000)
cea_mnar.neg.zero <- bcea(e=mu.e_sm.mnar.list.all[[2]], c=mu.c_sm.mnar.list.all[[2]], ref = 2, Kmax = 100000)
cea_mnar.neg <- bcea(e=mu.e_sm.mnar.list.all[[3]], c=mu.c_sm.mnar.list.all[[3]], ref = 2, Kmax = 100000)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-ceacsmsa
#| echo: false 
#| eval: true
#| message: false
#| warning: false
#| error: false 
#| fig-width: 6
#| fig-height: 5
#| out-width: "70%"
#| results: 'hide' 
#| fig.show: 'hold'
#| fig.pos: 'H'
#| fig.cap: "Cost-effectiveness acceptability curves associated with four different missing data assumption scenarios"

plot(cea_mar$k, cea_mar$ceac,  xlim=c(0,100000), ylim=c(0,.15), xlab="x", ylab="x", type="n",yaxt="n",ann = F,bty="n",axes = F)
axis(1,at=c(0,20000,40000,60000,80000,100000)) 
##axis(2,labels = c(0,0.2,0.4,0.6,0.8,1),at=c(0,0.2,0.4,0.6,0.8,1),las=2)
axis(2,labels = c(0,0.05,0.1,0.15),at=c(0,0.05,0.1,0.15),las=2)
lines(cea_mar$k,cea_mar$ceac, col="blue", lwd=2, lty=1)
lines(cea_mnar.zero.neg$k,cea_mnar.zero.neg$ceac, col="red", lwd=2, lty=2)
lines(cea_mnar.neg.zero$k,cea_mnar.neg.zero$ceac, col="red", lwd=2, lty=3)
lines(cea_mnar.neg$k,cea_mnar.neg$ceac, col="red", lwd=2, lty=4)
legend("topright", legend = c("MAR","MNAR - (t=1)","MNAR - (t=2)","MNAR - (both)"),
       lty=c(1,2,3,4), bty = "n", y.intersp = 1, col=c("blue","red","red","red","red"), cex=0.9)

