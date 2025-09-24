## This file uses R relative paths. 
## Generate parametric survival model parameters from (pseudo) IPD of each arm of each trial, all parameters are transformed to be additive on the linear predictor

# call libraries
if(!require(readxl)) {install.packages("readxl")}
if(!require(openxlsx)) {install.packages("openxlsx")}
if(!require(survival)) {install.packages("survival")}
if(!require(flexsurv)) {install.packages("flexsurv")}

library(readxl)
library(openxlsx)
library(survival)
library(flexsurv)



# clear workspace 
rm(list = ls())

# folder location
workfolder = "./"
outfolder = paste(workfolder,"Output/", sep="")
datafolder = paste(workfolder,"Data/", sep="")
kmfolder = paste(workfolder,"Data/KMdata/", sep="")


# IPD folders, each folder contains Guyot generated pseudo IPD of survival outcome data (time and event)
folders = list.dirs(kmfolder, recursive = F, full.name = F)
files_split=data.frame(matrix(unlist(strsplit(folders, " ")),ncol=5, byrow=TRUE))
colnames(files_split)[c(1,2,3,4,5)]=c('trialID','Author','year','outcome','trt')
files_split=data.frame(files_split, folders=folders )
files_split$folders1 = gsub("^.*? ","",files_split$folders)
files_split$folders1=gsub('_', " ", files_split$folders1)
files_split$trt=gsub('_', " ", files_split$trt)



# fit survival distributions
modellist=c('weibull', 'lognormal', 'llogis', 'gompertz',  'exp')
survoutlist=list()

for (k in 1:dim(files_split)[1]){
  ## 1000 bootstrap samples for covariance matrix estimates of additive parameters
  B=1000
  weibull.param=matrix(NA,ncol=2, nrow=B)
  lognormal.param=matrix(NA,ncol=2, nrow=B)
  llogis.param= matrix(NA,ncol=2, nrow=B)
  gompertz.param=matrix(NA,ncol=2, nrow=B)
  exp.param=matrix(NA,ncol=1, nrow=1)
  
  param.list=list(  weibull.param,
                    lognormal.param,
                    llogis.param,
                    gompertz.param,
                    exp.param )
  
  weibull.cov= lognormal.cov= llogis.cov= gompertz.cov= exp.cov= matrix(NA,nrow=4,ncol=4)
  cov.param=list(weibull.cov,lognormal.cov,  llogis.cov, gompertz.cov, exp.cov)
  params=list()
  
  fileuse=files_split[k,]
  folderuse=fileuse$folders

  
  datause= read.csv(file=paste(kmfolder,folderuse,"/KMdataIPD.txt", sep=""), header = TRUE, sep="\t" )
  colnames(datause)=c('time','event','x')
  AIC=data.frame(model=modellist, AIC=NA)
  km <- survfit(Surv(time, event) ~ 1, data = datause) 
  survkm= data.frame(time=km$time, est=km$surv, lcl=km$lower, ucl=km$upper)
  data_n = dim(datause)[1]  
  
  for (j in 1:4){
    model1 <- flexsurvreg(Surv(time, event) ~ 1, dist=modellist[j],data = datause) 
    model1$res
    survout=data.frame(summary(model1))
    survoutlist[[j]]=survout
    AIC$AIC[j]=model1$AIC
    
    param.list[[j]]=matrix(NA, nrow=B, ncol=dim(model1$res)[1])
    for (i in 1:B){
      set.seed(i)
      surv_model=modelx=NULL
      sampleindex =  sample(x=data_n,size=data_n,replace = TRUE)
      BSsample =  datause[sampleindex,]
      surv_model <- tryCatch(flexsurvreg(Surv(time, event) ~ 1, dist=modellist[j],data = BSsample), error=function(w) return(NA) )
      
      if (!is.na(surv_model)){
        # create additive parameters
        
        if (modellist[j]=='weibull'){
          modelx=data.frame(surv_model$res)
          modelx$param[1]=log(surv_model$res[1,1]*(1/surv_model$res[2,1])^surv_model$res[1,1])
          modelx$param[2]=surv_model$res[1,1]-1
          param.list[[j]][i,1:length(surv_model$res[,1])]=modelx$param
        }
        
        if (modellist[j]=='lognormal'){
          modelx=data.frame(surv_model$res)
          modelx$param[1]=surv_model$res[1,1]/surv_model$res[2,1]
          modelx$param[2]=-1/surv_model$res[2,1]
          param.list[[j]][i,1:length(surv_model$res[,1])]=modelx$param
        }
        
        if (modellist[j]=='llogis'){
          modelx=data.frame(surv_model$res)
          modelx$param[1]=log(1/surv_model$res[2,1])
          modelx$param[2]=surv_model$res[1,1]
          param.list[[j]][i,1:length(surv_model$res[,1])]=modelx$param
        }
        
        if (modellist[j]=='gompertz'){
          modelx=data.frame(surv_model$res)
          modelx$param[1]=log(surv_model$res[2,1])
          modelx$param[2]=surv_model$res[1,1]
          param.list[[j]][i,1:length(surv_model$res[,1])]=modelx$param
        }
      }
      print(paste('bootstrap ', i, 'complete for model ', j, modellist[j]))
    } # end bootstrap loop   
    
    params.1000=data.frame(param.list[[j]])
    colnames(params.1000)=rownames(model1$res)
    params[[j]]=data.frame(parameter=rownames(model1$res),est=colMeans(param.list[[j]], na.rm = TRUE), 
                           se=apply(param.list[[j]], MARGIN=2, FUN=sd, na.rm=TRUE), AIC=NA, loglike=NA)
    params[[j]]$AIC[1]=model1$AIC
    params[[j]]$loglike[1]=model1$loglik
    
    cov.param[[j]]=cov(params.1000,use="complete.obs")
  } # end loop of distributions
  
  ## exponential distribution only contains one parameter so no covariance matrix
  j=5
  if (modellist[j]=='exp'){
    surv_model=modelx=NULL
    surv_model <- flexsurvreg(Surv(time, event) ~ 1, dist=modellist[j],data =datause )
    modelx=data.frame(surv_model$res)
    modelx$param[1]=log(surv_model$res[1,1])
    params[[j]]=data.frame(parameter=rownames(surv_model$res),est=surv_model$res.t[1,1], 
                           se=surv_model$res.t[1,4], AIC=NA, loglike=NA)
    params[[j]]$AIC[1]=surv_model$AIC
    params[[j]]$loglike[1]=surv_model$loglik
    cov.param[[j]]=NA
    
  }
  
  
  paramexcel=paste(outfolder,folderuse, " parameters.xlsx", sep="")
  
  outlist=c(params, cov.param)
  openxlsx::write.xlsx(outlist,paramexcel,
                       sheetName= c(modellist,paste(modellist,'cov')),overwrite=TRUE)
  
} # end k loop for trials and interventions  










