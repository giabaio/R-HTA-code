## This file uses R relative paths.
## Create data set of survival distribution parameters to be used in multivariate NMA 

# call libraries
library(readxl)
library(openxlsx)
library(dplyr)

# clear workspace 
rm(list = ls())

# folder location
workfolder = "./"
outfolder = paste(workfolder,"Output/", sep="")
datafolder = paste(workfolder,"Data/", sep="")
survparamdatafolder = paste(workfolder,"Data/survparamdata/", sep="")

# import files with arm-specific parameter estimates for common survival distributions
modellist=c('weibull', 'lognormal', 'llogis', 'gompertz')

params_=list()
covs_=list()
dataset2=list()

files1 = list.files(outfolder, recursive = F, full.name = F)
files1 = files1[grep('parameters.xlsx',files1)]
files_split=data.frame(matrix((unlist(strsplit(files1, " "))),ncol=6, byrow=TRUE))
colnames(files_split)[c(1,2,3,4,5)]=c('trialID','Author','year','outcome','trt')
files_split=data.frame(files_split, files1=files1 )
files_split$files2 = gsub("^.*? ","",files_split$files1)
files_split$files2=gsub('_', " ", files_split$files2)
files_split$trt=gsub('_', " ", files_split$trt)
files_split[,1]=as.character(files_split[,1])
files_split[,2]=as.character(files_split[,2])
files_split[,3]=as.character(files_split[,3])
files_split[,4]=as.character(files_split[,4])


# creating data file with study information (dataset1)
unique_study=data.frame(studyid=unique(files_split$trialID))
unique_study$s=seq(1:length(unique_study[,1]))

unique_trt=data.frame(trt=unique(files_split$trt))
unique_trt$order=99
unique_trt$order[unique_trt$trt=='DTIC']=1
unique_trt=unique_trt[order(unique_trt$order),]
unique_trt$t=seq(1:length(unique_trt[,1]))
unique_trt=unique_trt[,c('trt','t')]
unique_id_trt=data.frame(unique(cbind(files_split$trialID,files_split$trt)))
colnames(unique_id_trt)=c("studyid","trt")

unique_id_trt1 = Reduce(function(x, y) merge(x, y, by=c('trt' ), all=TRUE), list(
  unique_id_trt, unique_trt ))
unique_id_trt1=unique_id_trt1[order(unique_id_trt1$studyid,unique_id_trt1$trt),]
unique_id_trt1$all=1

generate_a = unique_id_trt1[, c('all','studyid','trt','t')]
generate_a=generate_a[order(generate_a$studyid,generate_a$trt),]
generate_a$arm <- with(generate_a, ave(all, all, studyid, FUN = seq_along))

unique_id_trt2=reshape(generate_a, timevar="arm", idvar=c("studyid"), direction="wide")
unique_id_trt2=unique_id_trt2[,-grep('all', colnames(unique_id_trt2))]
unique_id_trt2=Reduce(function(x, y) merge(x, y, by=c('studyid' ), all=TRUE), list(
  unique_id_trt2, unique_study ))

dataset1=rbind(data.frame(unique_id_trt2,o=1,out=1),data.frame(unique_id_trt2,o=2,out=2))
dataset1=dataset1[order(dataset1$studyid),]
dataset1$na=2
dataset1$na=rowSums (data.frame(!is.na(dataset1$t.1),!is.na(dataset1$t.2),!is.na(dataset1$t.3)), na.rm = TRUE, dims = 1)
colnames(dataset1)=gsub('\\.','',colnames(dataset1))
colnames(dataset1)=gsub('studyid','studyid1',colnames(dataset1))

colnames(unique_trt)=c('treatment','t')


# creating data file with arm-specific parameter estimates  
for (j in 1:length(modellist) ){
  for (k in 1:dim(files_split)[1]){
    fileuse=NULL
    params=NULL
    covs=NULL
    covs12=NULL
    fileuse=files_split[k,]
    excelfilename=fileuse$files1
    
    paramfile=paste(outfolder,excelfilename, sep="")
    params=as.data.frame(read_excel(paramfile, sheet=modellist[j]))
    covs=as.data.frame(read_excel(paramfile, sheet=paste(modellist[j],'cov')))
    
    if (dim(params)[1]==2){
      covs12=data.frame(cov11=covs[1,1],cov22=covs[2,2],cov12=covs[1,2])
    } 
    
    filex=gsub('survival','',fileuse$files2)
    filex=gsub('.xlsx','',filex)
    params=params[,c('parameter','est','se')]
    params$studyid=fileuse$trialID
    params$treatment=fileuse$trt
    covs12$studyid=fileuse$trialID
    covs12$treatment=fileuse$trt
    
    if (k==1) {
      params_[[j]]=params
      covs_[[j]]=covs12
    } else {
      params_[[j]]=bind_rows(params_[[j]],params)
      covs_[[j]]=bind_rows(covs_[[j]],covs12)
    }
  }# end k loop
  
  
  unique_study=data.frame(studyid=unique(files_split$trialID))
  unique_study$study=seq(1:length(unique_study[,1]))
  
  studies_wide=reshape(params_[[j]], timevar="parameter", idvar=c("studyid",'treatment'), direction="wide")
  studies_wide=merge(studies_wide,unique_trt,by='treatment')
  studies_wide=studies_wide[order(studies_wide$studyid, studies_wide$t),]
  studies_wide$all=1
  
  generate_a = studies_wide[, c('all','studyid','t','treatment')]
  generate_a=generate_a[order(generate_a$studyid,generate_a$t),]
  generate_a$arm <- with(generate_a, ave(all, all, studyid, FUN = seq_along))
  
  studies_wide = Reduce(function(x, y) merge(x, y, by=c('studyid','treatment' ), all=TRUE), list(
    studies_wide, covs_[[j]] ))
  studies_wide = Reduce(function(x, y) merge(x, y, by=c('studyid','t','treatment' ), all=TRUE), list(
    studies_wide,generate_a ))
  studies_wide = Reduce(function(x, y) merge(x, y, by=c('studyid' ), all=TRUE), list(
    unique_study,studies_wide ))
  
  colnames(studies_wide)=gsub('est.','y.',colnames(studies_wide))
  studies_wide=studies_wide[,-grep('all', colnames(studies_wide))]
  
  dataset2[[j]]=studies_wide
  
  
}# end j loop  



excelout=paste(survparamdatafolder, 'parametric survival data.xlsx', sep="")


dataout=c(list(unique_trt,dataset1),dataset2)
openxlsx::write.xlsx(dataout,excelout,
                     sheetName= c('treatments','data1',paste('data2',modellist) ),overwrite=TRUE)

