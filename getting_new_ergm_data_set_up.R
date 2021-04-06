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


df = ref %>%
  filter(yr != 1973) %>%
  filter(yr >= 1974) %>%
  filter(yr < 2014) %>% 
  filter(med_sal_ > 0) %>% 
  filter(n > 5000) %>%
  drop_na(med_sal_) %>%
  filter(AGYSUB != "TOTL")%>%
  filter(agy_full != "Dept. of Veterans Affairs") %>%
  filter(!grepl("NET", agy_full))

years = seq(1980,1990, by = 1)

addon = df %>%
  select(agy_full, med_sal_, agy_typ, AGYSUB, yr) %>%
  filter(yr == years[length(years)])

res = df %>%
  filter(yr %in% years) %>%
  group_by(agy_full) %>%
  summarise(ma_diff = ma_pct[which.max(yr)] - ma_pct[which.min(yr)],
            phd_diff = doc_pct[which.max(yr)] - doc_pct[which.min(yr)],
            budget_diff = (b18_roll[which.max(yr)] - b18_roll[which.min(yr)]) / b18_roll[which.min(yr)],
            dbudget_diff = (b18_d_roll[which.max(yr)] - b18_d_roll[which.min(yr)]) / b18_d_roll[which.min(yr)],
            nyear = n()) %>%
  ungroup() %>%
  filter(nyear == length(years))

fres = res %>%
  left_join(addon)


####################################################
#### NODES
######################################
#### Data Cleaning
df = fed %>%
  filter(yr == 1990) %>%
  filter(n_nextpct <= .5)


#### Make nodes
sources = df %>%
  distinct(AGYSUB) %>%
  rename(label = AGYSUB)

destinations = df %>%
  distinct(label = AGYSUB_next)

nodes = full_join(sources, destinations, by = "label")

nodes = fres %>%
  left_join(nodes, by = c("AGYSUB" = "label"))

nodes = nodes %>%
  rowid_to_column("id")


###################################3
#### Make edges
#######################


agy_set = expand.grid(unique(df$AGYSUB), unique(df$AGYSUB_next))

agy_set = agy_set %>%
  filter(as.character(Var1) != as.character(Var2)) %>%
  rename(AGYSUB = Var1, AGYSUB_next = Var2)

df2 = agy_set %>%
  left_join(df) %>%
  mutate(connected = ifelse(n_next >= 5, 1, 0))

df2$connected = replace_na(df2$connected, 0)

df2 = df2 %>%
  select(AGYSUB, AGYSUB_next, connected) %>%
  filter(connected == 1) %>%
  filter(AGYSUB %in% nodes$AGYSUB & AGYSUB_next %in% nodes$AGYSUB)

#### Make network

fednetwork = network(df2, vertex.attr = nodes, matrix.type = "edgelist")


