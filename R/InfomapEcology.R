# Testing infomapecology package availble from: https://ecological-complexity-lab.githubio/infomap_ecology_package/

library(infomapecology)
setwd("Y:/")
if(infomapecology::check_infomap() !=TRUE){
  message("Cannot find infomap.exe, please set workin directory to folder containing infomap.exe file. See https://ecological-complexity-lab.github.io/infomap_ecology_package/installation for more information")
  } else {
  # LinkList<-read.delim("H:/My Documents/1_MyDocuments/PhD_2021/PhD/Methods Paper/R/Infomap_test/Bull_Probabilities20200210.txt",header=F)
  # names(LinkList)<-c("from", "to", "weight")
  # LinkList$from<-sub("^","Node",LinkList$from)
  # LinkList$to<-sub("^","Node",LinkList$to)

  monolayer_object<-infomapecology::create_monolayer_object(LinkList, directed = T, bipartite = F,node_metadata = nodenames)
  infomap_object<-infomapecology::run_infomap_monolayer(monolayer_object, infomap_executable='infomap', flow_model='directed', silent=T, two_level=F, ...="-k")
  infomap_modules<-infomap_object$modules
  }
