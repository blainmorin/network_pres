

library(shiny)
library(readr)
library(tidyverse)
library(networkD3)
library(plotly)
library(statnet)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)


fed = read.csv("r73_13_recoded_flows.csv")
ref = read.csv("fed_agency_capacity_autonomy.csv")
fed = fed %>%
  filter(AGYSUB_next != "EXIT") %>%
  filter(AGYSUB_next != "UKNOWN")


years = seq(1980, 2000, by = 5)

nets = list()

df = fed %>%
  filter(n >= 5000) %>%
  filter(n <= 20000) %>%
  filter(n_nextpct <= .5)

sources = df %>%
  distinct(NAME) %>%
  rename(label = NAME)

destinations = df %>%
  distinct(label = NAME_next)

nodes = full_join(sources, destinations, by = "label")

nodes = nodes %>%
  rowid_to_column("id")

#######################################################

for (i in 1:length(years)) {
  
  df = fed %>%
    filter(yr == years[i]) %>%
    filter(n >= 5000) %>%
    filter(n <= 20000) %>%
    filter(n_nextpct <= .5)
  
  #### Add covariates to nodes
  
  ref2 = ref %>%
    filter(yr == years[i]) %>%
    select(n, agy_typ, med_sal_, agy_full) %>%
    mutate(agy_full = toupper(agy_full))
  
  nodestemp = nodes %>%
    left_join(ref2, by = c("label" = "agy_full")) %>%
    drop_na()
  
  #### Make edges
  
  agy_set = expand.grid(unique(df$NAME), unique(df$NAME_next))
  
  agy_set = agy_set %>%
    filter(as.character(Var1) != as.character(Var2)) %>%
    rename(NAME = Var1, NAME_next = Var2)
  
  df2 = agy_set %>%
    left_join(df) %>%
    mutate(connected = ifelse(n_next > 10, 1, 0)) 
  
  df2$connected = replace_na(df2$connected, 0)
  
  df2 = df2 %>%
    select(NAME, NAME_next, connected)
  
  df2 = df2 %>%
    left_join(nodestemp, by = c("NAME" = "label")) %>%
    rename(from = id)
  
  df2 = df2 %>%
    left_join(nodestemp, by = c("NAME_next" = "label")) %>%
    rename(to = id)
  
  df2 = df2 %>%
    filter(from != to) %>%
    filter(connected == 1)
  
  #### Make network
  
  temp = network(df2, vertex.attr = nodestemp, matrix.type = "edgelist")
  nets[[i]] = temp
  
  
}

sampdyn = networkDynamic(network.list = nets, vertex.pid = "vertex.names")

render.d3movie(sampdyn, 
               plot.par=list(displaylabels=T), 
               label = network.vertex.names(sampdyn))






