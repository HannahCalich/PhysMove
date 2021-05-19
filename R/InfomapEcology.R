# Testing infomapecology package availble from: https://ecological-complexity-lab.githubio/infomap_ecology_package/

library(infomapecology)
setwd("Y:/")
if(check_infomap() !=TRUE){
  message("Cannot find infomap.exe, please set workin directory to folder containing infomap.exe file. See https://ecological-complexity-lab.github.io/infomap_ecology_package/installation for more information")
  } else {
  linklist<-read.delim("H:/My Documents/1_MyDocuments/PhD_2021/PhD/Methods Paper/R/Infomap_test/Bull_Probabilities20200210.txt",header=F)
  names(linklist)<-c("from", "to", "weight")
  linklist$from<-sub("^","Node",linklist$from)
  linklist$to<-sub("^","Node",linklist$to)

  monolayer_object<-create_monolayer_object(linklist, directed = T, bipartite = F,node_metadata = nodenames)
  infomap_object<-run_infomap_monolayer(monolayer_object, infomap_executable='infomap', flow_model='directed', silent=T, two_level=F, ...="-k")
  infomap_modules<-infomap_object$modules
  }
