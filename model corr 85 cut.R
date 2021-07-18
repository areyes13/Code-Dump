# Load Packages -----------------------------------------------------------
# update to match your working directory (where you have saved your files)
setwd('/Users/areyesm@us.ibm.com/Documents/Projects/MQL Routing')
# setwd("~/R Files/MQL Feature Selection")
# make sure you have these installed beforehand
# you can run this in the console (no quotes): install.packages(LIBRARY_NAME)
library(tidyverse)
library(dplyr)
library(readr)
library(scales)
library(readxl)
library(corrr)
library(readxl)

# Skip load section and just use saved working directory with input tables
# load("~/R Files/MQL Feature Selection/UT10 prep and feature select - PRE RFE.RData")

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

# 2.1) CNDP TARGET: Corr Table ** -------------------------------------------------

corr.CNDP <- input %>%
  select(-DV_IS_OPEN_x, -DV_IS_OPEN_y, -BUS_AREA_ID, -RS_MATCH_FLAG, -EXT_DTL_REVN_USD,
         -INDEX, -P_SCORE_NEW_FLAG, -NEWCO_FLG_BASE, -INBOUND_MARKETING_INTERACTION_KEY) %>%
  select_if(is.numeric) %>%
  correlate()


corr.CNDPcut <- corr.CNDP  %>%
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
elim_CNDP <- corr.CNDPcut %>%
  rename(var = y,
         corr85 = x) %>%
  # filter(r >= .99) %>%
  select(var, corr85, r) %>%
  arrange(var, corr85) %>%
  group_by(var) %>%
  # in case any feature was knocked out by multiple fields, keep highest level one (e.g. UT10 >>> UT20)
  filter(row_number() == 1)


# figure out which fields to drop and which to keep
# have to spot check a few manually to make sure we keep 'higher' level option
elim85_CNDP <- tibble(FIELD = input %>% 
         select(-DV_IS_OPEN_x, -DV_IS_OPEN_y, -BUS_AREA_ID, -RS_MATCH_FLAG, -EXT_DTL_REVN_USD,
                -INDEX, -P_SCORE_NEW_FLAG, -NEWCO_FLG_BASE) %>%
         names()) %>%
  left_join(elim_CNDP, by = c("FIELD" = "var")) %>%
  # if there wasn't a correlated field >.99 then keep original type
  mutate(type = case_when(!is.na(r) ~ "DROP", 
                          T ~ 'KEEP'),
         type.final = case_when(FIELD %in% c('AI_APP_15ITT_LEAD_INDIV_SUM_I_FLG', 'AIX_IBMI_20C0H_LEAD_INDIV_SUM_I_FLG', 
                                             'BUS_TRANS_15CPT_LEAD_INDIV_SUM_I_FLG', 'SEC_SW_15SCT_INDIV_YIELD_I', 
                                             'SEC_SW_15SCT_LEAD_INDIV_SUM_I_FLG', 'AI_APP_15ITT_LEAD_COMP_SUM_C', 
                                             'AI_APP_15ITT_WON_DTL_REVN_COMP_SUM_C', 'AIX_IBMI_20C0H_COMP_YIELD_C', 
                                             'AIX_IBMI_20C0H_LEAD_COMP_SUM_C', 'AIX_IBMI_20C0H_WON_DTL_REVN_COMP_SUM_C',
                                             'BUS_TRANS_15CPT_COMP_YIELD_C', 'BUS_TRANS_15CPT_LEAD_COMP_SUM_C', 
                                             'CLIENT_FIN_20H01_COMP_YIELD_C', 'CLIENT_FIN_20H01_LEAD_COMP_SUM_C', 
                                             'CLIENT_FIN_20H01_WON_LEAD_COMP_SUM_C', 'MAINFRM_HW_15MHW_WON_DTL_REVN_COMP_SUM_C', 
                                             'SEC_SW_15SCT_COMP_YIELD_C', 'SEC_SW_15SCT_LEAD_COMP_SUM_C', 
                                             'SEC_SW_15SCT_WON_DTL_REVN_COMP_SUM_C', 'STORAGE_15STT_LEAD_COMP_SUM_C', 
                                             'UT_LVL_15_CD_INTENT_NEW_UNKNOWN', 'AI_APP_15ITT_WON_LEAD_COMP_SUM_C',
                                             'BUS_TRANS_15CPT_WON_DTL_REVN_INDIV_SUM_I_FLG') ~ 'DROP',
                                FIELD %in% c('COGAPP_10B00_LEAD_COMP_SUM_C', 'COGAPP_10B00_LEAD_INDIV_SUM_I_FLG', 
                                             'COGAPP_10B00_WON_DTL_REVN_COMP_SUM_C', 'COGAPP_10B00_WON_LEAD_COMP_SUM_C', 
                                             'COG_SYS_15PHW_COMP_YIELD_C', 'COG_SYS_15PHW_LEAD_COMP_SUM_C', 
                                             'COG_SYS_15PHW_WON_LEAD_COMP_SUM_C', 'COG_SYS_15PHW_LEAD_INDIV_SUM_I_FLG', 
                                             'COG_SYS_15PHW_WON_DTL_REVN_COMP_SUM_C', 'GBS_10J00_COMP_YIELD_C', 
                                             'GBS_10J00_LEAD_COMP_SUM_C', 'GBS_10J00_LEAD_INDIV_SUM_I_FLG', 
                                             'GBS_10J00_WON_DTL_REVN_INDIV_SUM_I_FLG', 'GBS_10J00_WON_LEAD_COMP_SUM_C', 
                                             'GBS_10J00_WON_LEAD_INDIV_SUM_I_FLG', 'FIN_GRP_10H00_COMP_YIELD_C', 
                                             'FIN_GRP_10H00_LEAD_COMP_SUM_C', 
                                             'FIN_GRP_10H00_WON_LEAD_COMP_SUM_C', 'SYSTEMS_10C00_WON_DTL_REVN_COMP_SUM_C', 
                                             'SECURITY_10N00_COMP_YIELD_C', 'SECURITY_10N00_INDIV_YIELD_I', 
                                             'SECURITY_10N00_WON_DTL_REVN_INDIV_SUM_I_FLG', 'SECURITY_10N00_WON_LEAD_INDIV_SUM_I_FLG', 
                                             'SECURITY_10N00_LEAD_COMP_SUM_C', 'SECURITY_10N00_LEAD_INDIV_SUM_I_FLG', 
                                             'SECURITY_10N00_WON_DTL_REVN_COMP_SUM_C', 'SECURITY_10N00_WON_LEAD_COMP_SUM_C', 
                                             'SYSTEMS_10C00_LEAD_COMP_SUM_C', 'SYSTEMS_10C00_WON_LEAD_COMP_SUM_C', 
                                             'UT_LVL_10_CD_INTENT_NEW_NULL', 'RESP_QUARTER') ~ 'KEEP',
                                T ~ type)) # %>% write_csv('elim85 cndp.csv')
  

# save(corr.CNDP, corr.CNDPcut, elim85_CNDP, file = "MODELING CORR OBJ 20210714.RData")



# corr to target ----------------------------------------------------------
# TOUGH TO DECIDE ON THRESHOLD...MAYBE LEAVE AS IS FOR NOW...REVISIT IF NEEDED

corr_DROP <- elim85_CNDP %>%
  filter(type.final == "DROP") %>%
  distinct(FIELD) %>% 
  pull(FIELD)

save(corr_DROP, file = 'CORR DROP.RData')

sink(file = "CORR DROP 85 20210714.txt")
corr_DROP %>%
  as.tibble() %>%
  mutate(value = paste0("'", value, "'")) %>%
  summarise(value = paste(value, collapse = ",")) %>%
  mutate(value = paste0("c(", value,")")) %>%
  pull(value) %>%
  cat()
sink()

corr.CNDPtarget <- corr.CNDP %>%
  focus(TARGET_10A00) %>%
  filter(!term %in% corr_DROP) %>%
  mutate(absCorr = abs(TARGET_10A00)) 

save(corr.CNDPtarget, file = 'CNDP Target corr.RData')



# MULTI TARGET CORR TBL ---------------------------------------------------
# DO A CORR TABLE FOR ALL THE BINARY TARGETS
# WE CAN MAP BACK TO BORUTA OUTPUTS TO HELP DECIDE WHICH FEATURES TO KEEP
# SO WE DON'T JUST SELECT BASED ON CDNP ACCURACY

target_df <- input_master %>%
  # SELECT SINGLE BINARY TARGET (START WITH CNDP 10A00)
  select(-UT_LVL_10_CD, -UT_LVL_10_DSCR, -DV_IS_OPEN_x, -DV_IS_OPEN_y, 
         -BUS_AREA_ID, -RS_MATCH_FLAG, -EXT_DTL_REVN_USD,
         -INDEX, -P_SCORE_NEW_FLAG, -NEWCO_FLG_BASE) %>%
  select(!any_of(corr_DROP), corr_DROP[str_detect(corr_DROP, 'COMP_INST')])


corr.TARGETS <- target_df %>%
  select(-INBOUND_MARKETING_INTERACTION_KEY) %>%
  select_if(is.numeric) %>%
  correlate()


corr.TARGETS.df <- corr.TARGETS %>%
  focus(starts_with('TARGET_10'))

save(corr.TARGETS, corr.TARGETS.df, file = 'corr TARGETS.RData')

