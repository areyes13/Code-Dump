# Load Packages -----------------------------------------------------------
# update to match your working directory (where you have saved your files)
setwd('/Users/areyesm@us.ibm.com/Documents/Projects/MQL Routing')
# make sure you have these installed beforehand
# you can run this in the console (no quotes): install.packages(LIBRARY_NAME)
library(tidyverse)
library(dplyr)
library(readr)
library(scales)
library(readxl)
library(corrr)
library(readxl)


# 1.0) LOAD NFS WRITE OUTS ------------------------------------------------
# INTERACTION HISTORY
interaction <- read_delim("~/Documents/Projects/MQL - NFS Transfers/MQL_MODELING_MASTER_GATED_INTRCTN_Positive", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

# MQL base + pipeline, interaction, and web hist
base <- read_delim("~/Documents/Projects/MQL - NFS Transfers/MQL_MODELING_MASTER_GATED_BASE_PIPE_INTR_WEB_Positive", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)


# 1.1) Merge NFS into single table ----------------------------------------
merged <- interaction %>%
  inner_join(base, by = 'INBOUND_MARKETING_INTERACTION_KEY') 

# drop raw tables
rm(interaction, base)

all_fields <- merged_input %>% names()
# 1.2) FIND ID variables (using dictionary....) ---------------------------

dictionary <- read_excel("MQL Routing - Data Dictionary.xlsx", 
                         sheet = "Index")

ids <- dictionary %>%
  filter(SOURCE == 'BASE', 
         !VARIABLES %in% c('INBOUND_MARKETING_INTERACTION_KEY', 'DAYS_DIFF'),
         VARIABLES %in% all_fields) %>%
  pull(VARIABLES) %>%
  sort()


# 1.3) drop fields not needed for corr ------------------------------------

# drop character fields (only care about numeric for corr)
merged_input <- merged %>%
  select(-all_of(ids)) %>%
  select_if(is.numeric)


# field names
fields <- tibble(FIELD = merged_input %>% names()) %>%
  filter(!FIELD %in% c('DV_IS_OPEN.x', 'DV_IS_OPEN.y'))

# 2.1) Massive Corr Table -------------------------------------------------

corr.full <- merged_input %>%
  select(-INBOUND_MARKETING_INTERACTION_KEY, -DV_IS_OPEN.x, -DV_IS_OPEN.y) %>%
  select_if(is.numeric) %>%
  correlate()


corr.fullcut <- corr.full %>%
  stretch() %>%
  filter(abs(r) >= .85) %>%
  group_by(grp = paste(pmax(x, y), pmin(x, y), sep = "_")) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-grp) %>% 
  arrange(-r) 


# 2.2) CORR: Elimination Round 1 (pipe only) ------------------------------
# ref table of features too highly correlated (and which one we will keep instead)
# put 'x' field into 'corr99' so we know which field 'knocked out' a given variable (pref higher lvl)
elim_full <- corr.fullcut %>%
  rename(var = y,
         corr99 = x) %>%
  filter(r >= .99) %>%
  select(var, corr99, r) %>%
  arrange(var, corr99) %>%
  group_by(var) %>%
  # in case any feature was knocked out by multiple fields, keep highest level one (e.g. UT10 >>> UT20)
  filter(row_number() == 1)


# map back into ref table with extreme values and dummy sql
FULL_final <- fields %>%
  # map 
  left_join(elim_full, by = c("FIELD" = "var")) %>%
  # if there wasn't a correlated field >.99 then keep original type
  mutate(type.final = case_when(!is.na(r) ~ "DROP", 
                                T ~ 'KEEP')) %>%
  filter(type.final == 'DROP')

sink("High Corr Fields to Drop.txt")
FULL_final %>%
  arrange(FIELD) %>%
  mutate(CODE = paste0("'",FIELD,"'")) %>%
  group_by(INDEX = row_number() %/% 15) %>%
  summarise(CODE = paste(CODE, collapse = ', ')) %>%
  ungroup %>%
  summarise(CODE = paste(CODE, collapse = ',\n')) %>%
  pull(CODE) %>%
  cat()
sink()


# Full table of Corrs -----------------------------------------------------

corr_pairs_full <- corr.full %>%
  shave() %>%
  stretch(na.rm = T)

write_csv(corr_pairs_full, "MQL Modeling Base - Full correlation pairs 07072021.csv")
  




  # filter(abs(r) >= .85) %>%
  group_by(grp = paste(pmax(x, y), pmin(x, y), sep = "_")) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-grp) %>% 
  arrange(-r)  %>%
  rename(var = y,
         corr99 = x) %>%
  # filter(r >= abs(.7)) %>%
  select(var, corr99, r) %>%
  arrange(var, corr99) %>%
  group_by(var) %>%
  # in case any feature was knocked out by multiple fields, keep highest level one (e.g. UT10 >>> UT20)
  filter(row_number() == 1)
