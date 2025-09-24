## This file uses R relative paths.
## Running multivariate normal NMA in jags
## Example using weibull parameters

# call libraries
library(readxl)
library(openxlsx)
library(dplyr)
library(rjags)

# clear workspace 
rm(list = ls())

# folder location
workfolder = "./"
datafolder = paste(workfolder,"Data/", sep="")
survparamdatafolder = paste(workfolder,"Data/survparamdata/", sep="")
outfolder=paste(workfolder,"Output/NMA_results/", sep="")
jagscodefolder=paste(workfolder,"Jagscode/", sep="")


# load data
model1=c('weibull')
dataname=paste(survparamdatafolder, 'parametric survival data.xlsx', sep="")
dataset1=as.data.frame(read_excel(dataname, sheet="data1"))
dataset2=as.data.frame(read_excel(dataname, sheet=paste('data2',model1)))
trt=as.data.frame(read_excel(dataname, sheet="treatments"))


# prepping data to run in jags 
N1=dim(dataset2)[1]              #no of datapoints
N2=dim(dataset1)[1]              # no of studies x no of outcomes (10x2=20) 
ns=length(unique(dataset1$studyid))              # no of studies
no=num_o= dim(dataset2[, grep('y', substr(colnames(dataset2),1,1))])[2]      #no of outcomes
nt.total =rep(length(unique(dataset2$treatment)),no)                     # no of interventions for each outcome 
nastudy = c(table(dataset2$studyid)) # no. of arms in each study

studyid=dataset2$studyid
study=dataset2$study
y=as.matrix(dataset2[,grep('y\\.',colnames(dataset2))])
se=as.matrix(dataset2[,grep('se\\.',colnames(dataset2))])
arm=dataset2$arm
cov11= dataset2$cov11
cov22= dataset2$cov22
cov12= dataset2$cov12


studyid1= dataset1$studyid1
t=cbind(dataset1$t1,dataset1$t2,dataset1$t3)
s= dataset1$s	
o= dataset1$o	
out= dataset1$out	
na= dataset1$na



datause = list(    N1      = N1,
  N2      = N2         ,
  ns      = ns         ,
  no      = no         ,
  nt.total= nt.total   ,
  nastudy = nastudy    ,
  studyid = as.numeric(as.factor(studyid))    ,
  study   = study      ,
  arm     = arm        ,
  y       = y          ,
  cov11   = cov11      ,
  cov22   = cov22      ,
  cov12   = cov12      ,
  studyid1= as.numeric(as.factor(studyid1))  ,
  s       = s          ,
  t       = t          ,
  o       = o          ,
  out     = out        ,
  na      = na         
)



# run NMA model
iterate=50000
NTHIN=2
iterate1=iterate*NTHIN

MODELFILE<-paste(jagscodefolder, "MV NMA RE.bugs", sep="" ) 
set.seed(1)

jagsRE=NULL
jagsRE<- suppressWarnings(jags.model(file = MODELFILE, data =datause, n.chains = 2, n.adapt = iterate))
update(jagsRE,iterate)
jagsRE.out <- coda.samples(jagsRE, c("d","mu", 'mu_mean','mean.y','alpha'), n.iter = iterate1, thin = NTHIN)
DICRE = dic.samples(jagsRE, iterate1, thin = NTHIN, type= "pD")
summaryRE = summary(jagsRE.out)
rawRE = data.frame(variable=rownames(summaryRE$statistics),(summaryRE$statistics)[,c(1,2)],summaryRE$quantiles, summaryRE$start, summaryRE$end)
pD = round(sum(DICRE$penalty),2)
deviance = round(sum(DICRE$deviance),2)
DIC0 =  pD + deviance
DIC=data.frame(Dbar=deviance, pD=pD, DIC=DIC0)




# export results
outputfile = paste(outfolder, "MV NMA RE.xlsx",sep="")

outdata=list(rawRE, DIC)
openxlsx::write.xlsx(outdata,outputfile,
                     sheetName= c('Jags output','DIC'),overwrite=TRUE)

