# Testing infomapecology package availble from: https://ecological-complexity-lab.githubio/infomap_ecology_package/
library(infomapecology)

# Sample monolayer directed network with hierarchical structure
# Import data
data("kongsfjorden_links")
data("kongsfjorden_nodes")
nodes <- kongsfjorden_nodes %>%
  select(node_name=Species, node_id_original=NodeID, everything())

interactions<- kongsfjorden_links %>%
  select(from=consumer, to=resource) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(weight=1)

# Prepare network objects
network_object <- create_monolayer_object(x=interactions, directed = T, bipartite = F, node_metadata = nodes)

# Run infomap, allow hierarchical modules
# Some species will have only incoming or outgoing links, so the next line will result in a warning
infomap_object <- run_infomap_monolayer(network_object, infomap_executable='Infomap',
                                        flow_model = 'directed',
                                        silent=T,trials=100, two_level=F, seed=123)

check_infomap()


linklist<-read.delim("H:/My Documents/1_MyDocuments/PhD_2021/PhD/Methods Paper/R/Infomap_test/Bull_Probabilities20200210.txt")
names(linklist)<-c("source", "target", "weight")
linklist<-as.data.frame(linklist)
linklist$source<-as.numeric(linklist$source)
linklist$target<-as.numeric(linklist$target)

test<-create_monolayer_object(linklist, directed = T, bipartite = F)#, node_metadata = nodes)
infomap_object <- run_infomap_monolayer(test, infomap_executable='Infomap',flow_model = 'directed', silent = T, two_level = F, ...="-k")
