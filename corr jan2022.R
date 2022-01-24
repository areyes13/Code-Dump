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

# 1.0) LOAD DATA ------------------------------------------------

# JAN 2022: NEW CORR INPUT (already merged & dropped; skip to corr)
input <- read_csv("mqlr corr data.csv") %>%
  select(!starts_with('URN_IDM')) %>%
  select(!starts_with('MIP_INBOUND_MK')) %>%
  select(!starts_with('OPTY_')) %>%
  select(!starts_with('MODEL_UT')) %>%
  select_if(is.numeric)

# 1.1)  ref table of field names  ------------------------------------

# field names
fields <- tibble(FIELD = input %>% names())

# 2.1) CNDP TARGET: Corr Table  -------------------------------------------------

corr <- input %>%
  correlate()


corr.cut <- corr  %>%
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
elim <- corr.cut %>%
  rename(var = y,
         corr85 = x) %>%
  # filter(r >= .99) %>%
  select(var, corr85, r) %>%
  arrange(var, corr85) %>%
  group_by(var) %>%
  # in case any feature was knocked out by multiple fields, keep highest level one (e.g. UT10 >>> UT20)
  filter(row_number() == 1)


# have to spot check a few manually to make sure we keep 'higher' level option
elim85 <- fields %>%
  left_join(elim, by = c("FIELD" = "var")) %>%
  # if there wasn't a correlated field >.99 then keep original type
  mutate(type = case_when(!is.na(r) ~ "DROP", 
                          T ~ 'KEEP'))

write_csv(elim85, 'elim85.csv')

# 2.3) CORR: manually fix drop / keep -------------------------------------
drop <- c('BJ_BUY_INDIV_CNT_1YR_FLG', 'AIX_20C0H_LEAD_COMP_SUM_C', 'AIX_20C0H_WON_DTL_REVN_COMP_SUM_C', 
          'AIX_20C0H_WON_LEAD_COMP_SUM_C_FLG', 'BUS_TRANS_15CPT_WON_DTL_REVN_COMP_SUM_C', 'ALL_OTHER_UT15_LEAD_COMP_SUM_C', 
          'ALL_OTHER_UT15_WON_DTL_REVN_COMP_SUM_C', 'ALL_OTHER_UT15_WON_LEAD_COMP_SUM_C', 'DX2SPSSST_CD_FLG', 
          'CLIENT_FIN_20H01_LEAD_COMP_SUM_C', 'CLIENT_FIN_20H01_WON_DTL_REVN_COMP_SUM_C', 'CLIENT_FIN_20H01_WON_LEAD_COMP_SUM_C', 
          'CLIENT_FIN_20H01_LEAD_COMP_SUM_C', 'CLIENT_FIN_20H01_WON_DTL_REVN_COMP_SUM_C', 'CLIENT_FIN_20H01_WON_LEAD_COMP_SUM_C', 
          'APP_MGMT_ON_PRM_20JDQ_WON_DTL_REVN_COMP_SUM_C', 'ALL_OTHER_UT15_LEAD_COMP_SUM_C', 
          'ALL_OTHER_UT15_WON_DTL_REVN_COMP_SUM_C', 'ALL_OTHER_UT15_WON_LEAD_COMP_SUM_C', 'INTR_10MISC_INDIV_FLG', 
          'INTR_10MISC_INDIV_FLG', 'INTR_MISC_COMP_CNT', 'INTR_MISC_COMP_FLG', 'DEVICE_TYPE_D', 
          'FIN_PFG_20B19_LEAD_COMP_SUM_C_FLG', 'FIN_PFG_20B19_WON_LEAD_COMP_SUM_C_FLG', 'GTS_PUB_CLOUD_20GT0_LEAD_COMP_SUM_C_FLG',
          'GTS_PUB_CLOUD_20GT0_WON_DTL_REVN_COMP_SUM_C', 'GTS_PUB_CLOUD_20GT0_WON_LEAD_COMP_SUM_C_FLG',
          'ALL_OTHER_UT15_LEAD_COMP_SUM_C', 'ALL_OTHER_UT15_WON_DTL_REVN_COMP_SUM_C', 'ALL_OTHER_UT15_LEAD_COMP_SUM_C', 
          'UT15_CODE_G1500', 'BJ_TRY_COMP_CNT_1YR', 'BJ_BUY_INDIV_CNT_1YR_FLG', 'CNTTYP_TY730_COMP_CNT_1YR', 
          'BJ_BUY_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15ISS_COMP_CNT_1YR', 'UT15_OFFER_15CPT_COMP_CNT_1YR', 
          'BJ_DSC_INDIV_CNT_1YR_FLG', 'BJ_BUY_INDIV_CNT_1YR_FLG', 'BJ_ADPT_COMP_CNT_1YR_FLG', 'BJ_BUY_INDIV_CNT_1YR_FLG', 
          'BJ_ADPT_COMP_CNT_1YR_FLG', 'BJ_ADPT_COMP_CNT_1YR_FLG', 'BJ_ADPT_COMP_CNT_1YR_FLG', 'BJ_BUY_INDIV_CNT_1YR_FLG', 
          'BJ_DSC_INDIV_CNT_1YR_FLG', 'UT20_OFFER_20B2O_COMP_CNT_1YR', 'BJ_DSC_INDIV_CNT_1YR_FLG', 
          'UT20_OFFER_20C07_COMP_CNT_1YR', 'UT20_OFFER_20C0H_COMP_CNT_1YR', 'BJ_DSC_INDIV_CNT_1YR_FLG', 
          'UT20_OFFER_20NYE_COMP_CNT_1YR', 'BJ_BUY_INDIV_CNT_1YR_FLG', 'BJ_BUY_INDIV_CNT_1YR_FLG', 'BJ_ADV_COMP_CNT_60DAYS_FLG',
          'BJ_ADPT_COMP_CNT_1YR_FLG', 'BJ_ADPT_COMP_CNT_1YR_FLG', 'DEVICE_TYPE_D')

keep <- c('AVG_RESP_DAYS_INDIV_FLG', 'COG_SYS_15PHW_LEAD_COMP_SUM_C', 'COG_SYS_15PHW_WON_DTL_REVN_COMP_SUM_C', 
          'COG_SYS_15PHW_WON_LEAD_COMP_SUM_C_FLG', 'CONSULTING_10J00_WON_DTL_REVN_COMP_SUM_C', 'DATA_AI_15ANP_LEAD_COMP_SUM_C', 
          'DATA_AI_15ANP_WON_DTL_REVN_COMP_SUM_C', 'DATA_AI_15ANP_WON_LEAD_COMP_SUM_C', 'DEVICE_TYPE_M_FLG', 
          'FIN_GRP_10H00_LEAD_COMP_SUM_C', 'FIN_GRP_10H00_WON_DTL_REVN_COMP_SUM_C', 'FIN_GRP_10H00_WON_LEAD_COMP_SUM_C', 
          'FIN_GRP_15RR4_LEAD_COMP_SUM_C', 'FIN_GRP_15RR4_WON_DTL_REVN_COMP_SUM_C', 'FIN_GRP_15RR4_WON_LEAD_COMP_SUM_C', 
          'HYBRID_CLOUD_15CAI_WON_DTL_REVN_COMP_SUM_C', 'IBM_AUTO_15IGO_LEAD_COMP_SUM_C', 
          'IBM_AUTO_15IGO_WON_DTL_REVN_COMP_SUM_C', 'IBM_AUTO_15IGO_WON_LEAD_COMP_SUM_C', 'INTR_10INFR_INDIV_FLG', 
          'INTR_10SOFT_INDIV_FLG', 'INTR_15MSC_COMP_CNT', 'INTR_CONS_COMP_FLG', 'PAGE_VIEW_CNT', 
          'PROMONTORY_15JIA_LEAD_COMP_SUM_C_FLG', 'PROMONTORY_15JIA_WON_LEAD_COMP_SUM_C_FLG', 
          'PUB_CLD_PLT_15CI9_LEAD_COMP_SUM_C_FLG', 'PUB_CLD_PLT_15CI9_WON_DTL_REVN_COMP_SUM_C', 
          'PUB_CLD_PLT_15CI9_WON_LEAD_COMP_SUM_C_FLG', 'SOFTWARE_10A00_LEAD_COMP_SUM_C', 
          'SOFTWARE_10A00_WON_DTL_REVN_COMP_SUM_C', 'SOFTWARE_10A00_WON_LEAD_COMP_SUM_C', 'UT10_CODE_G0000',
          'UT10_OFFER_10A00_COMP_CNT_1YR', 'UT10_OFFER_10A00_INDIV_CNT_1YR_FLG', 'UT10_OFFER_10C00_COMP_CNT_1YR', 
          'UT10_OFFER_10C00_INDIV_CNT_1YR_FLG', 'UT10_OFFER_10G00_COMP_CNT_1YR', 'UT10_OFFER_10J00_COMP_CNT_1YR', 
          'UT10_OFFER_10M00_INDIV_CNT_1YR_FLG', 'UT15_OFFER_153QH_INDIV_CNT_1YR_FLG', 'UT15_OFFER_156UX_COMP_CNT_1YR_FLG', 
          'UT15_OFFER_15ANP_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15CAI_COMP_CNT_1YR_FLG', 'UT15_OFFER_15CLS_COMP_CNT_1YR_FLG', 
          'UT15_OFFER_15EDG_COMP_CNT_1YR_FLG', 'UT15_OFFER_15IGO_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15ISS_INDIV_CNT_1YR_FLG', 
          'UT15_OFFER_15ITT_COMP_CNT_1YR', 'UT15_OFFER_15ITT_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15MHW_COMP_CNT_1YR', 
          'UT15_OFFER_15PHW_COMP_CNT_1YR', 'UT15_OFFER_15PHW_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15SCT_COMP_CNT_1YR', 
          'UT15_OFFER_15SCT_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15STT_INDIV_CNT_1YR_FLG', 'UT15_OFFER_15TMS_COMP_CNT_1YR_FLG', 
          'UT15_OFFER_15TSS_COMP_CNT_1YR_FLG', 'UT15_OFFER_15WCG_COMP_CNT_1YR_FLG', 'WEB_VISIT_CNT', 
          'CONSULTING_10J00_LEAD_COMP_SUM_C', 'CONSULTING_10J00_WON_LEAD_COMP_SUM_C')

cut_f <- elim85 %>%
  mutate(type = case_when(FIELD %in% drop ~ 'DROP',
                          FIELD %in% keep ~ 'KEEP',
                          T ~ type)) %>%
  filter(type == 'KEEP') %>%
  pull(FIELD)


# 3.1) CORR OUTPUT (0.85 CUT) ---------------------------------------------

based <- read_csv("mqlr corr data.csv") %>%
  select(starts_with('URN_IDM') | 
           starts_with('MIP_INBOUND_MK') | 
           starts_with('OPTY_') |
           starts_with('MODEL_UT')) %>%
  names()

model_cut <- tibble(FIELD = c(based, cut_f), 
       TABLE = 'ACS_MQLR0.MQL_MODELING_CLEAN_POSITIVE_20JAN2022')

write_csv(model_cut, 'model 85 cut F.csv')

sink(file = 'model 85 cut F.txt')
model_cut %>%
  summarise(SQL = paste0("SELECT \n",
                         paste(FIELD, collapse = ",\n"),
                         "\nFROM ", unique(TABLE))) %>%
  pull(SQL) %>% cat
sink()
