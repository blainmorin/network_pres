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
  filter(AGYSUB != VA00) %>% 
  filter(AGYSUB != VA0R) %>%
  filter(agy_full != "Dept. of Veterans Affairs") %>%
  filter(!grepl("NET", agy_full))

years = seq(1980,1990, by = 1)

res = df %>%
  filter(yr %in% years) %>%
  group_by(agy_full) %>%
  summarise(ma_diff = ma_pct[which.max(yr)] - ma_pct[which.min(yr)],
            phd_diff = doc_pct[which.max(yr)] - doc_pct[which.min(yr)],
            nyear = n()) %>%
  ungroup() %>%
  filter(nyear == length(years))
