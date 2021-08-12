# Load Packages -----------------------------------------------------------
# update to match your working directory (where you have saved your files)
setwd('/Users/areyesm@us.ibm.com/Documents/Projects/MQL Modeling/UT10 Model')

# setwd("~/R Files/MQL Feature Selection")
# make sure you have these installed beforehand
# you can run this in the console (no quotes): install.packages(LIBRARY_NAME)
library(tidyverse)
library(dplyr)
library(readr)
library(scales)
library(readxl)
library(corrr)
library(caret)
library(randomForest)
library(Boruta)
library(ggthemes)
library(DataExplorer)


# load 85% corr drop -------------------------------------------------------
load("~/Documents/Projects/MQL Modeling/UT10 Model/CORR DROP.RData")

# load job feature (leftover) ---------------------------------------------
jobs <- read_csv('MQL_MODELING_MASTER_GATED_EMPL_FIELDS 20210713.csv') %>%
  mutate(JOB_CTGY_CD = coalesce(JOB_CTGY_CD, 'NULL'))

# DROP FIELDS WE WON'T NEED FOR MODELING ----------------------------------
# FROM EXCEL WORKBOOK WITH BINS...
dropper <- read_excel("~/Documents/Projects/MQL Routing/MQL MODEL - category binning summaries.xlsx", 
                      sheet = "Modeling Base") %>%
  filter(Include == 'F') %>%
  pull(VARIABLES)

# load selected fields (from final boruta) --------------------------------
# updated with new v2 selection
selector_Final <- read_excel("Feature Elimination UT10.xlsx", 
                             sheet = "BORUTA FINAL OUT")

# load data v2 from python ------------------------------------------------
# Some fields we want to force in (for now...)
maintain <- c('COMP_INST', 'UT10_OFFER_CA_COMP_CNT_60DAYS', 'UT10_OFFER_CNDP_COMP_CNT_60DAYS', 
              'UT10_OFFER_GTS_COMP_CNT_60DAYS', 'UT10_OFFER_SEC_COMP_CNT_60DAYS', 
              'UT10_OFFER_SYSWT_COMP_CNT_60DAYS', 'UT10_OFFER_WH_COMP_CNT_60DAYS_FLG') %>%
  paste(collapse = '|')

# Raw data
raw <- read_csv("full ut10 modeling data v2.csv") 


# load same quarter pipe fix & objects ------------------------------------
# load 'test_df' which has subtracted same quarter LEAD counts
# load('RLM & Same Q Pipe Update.RData')
# table of SAME QUARTER FIXED LEAD COUNTS
# pipe_SUBBED <- test_df %>%
#   ungroup() %>%
#   select(-URN_IDM_INDIV, -URN_IDM_COMP, -YEAR_QUARTER) %>%
#   select(INBOUND_MARKETING_INTERACTION_KEY,
#          all_of(originalField[originalField %in% updatedField]))
# 
# rm(test_df)
# 
# RLM_opty <- RLM_opty$INBOUND_MARKETING_INTERACTION_KEY
# 
# save(pipe_SUBBED, RLM_opty, file = 'RLM & Same Q Pipe Update 2.RData')
load('RLM & Same Q Pipe Update 2.RData')


# load lagged pipe fields (new!) ------------------------------------------
# this will now only have lagged Win Rev values 
# (tossing out yields and LEAD counts since those are coming from other table)
pipe_lag <- read_delim("~/Documents/Projects/MQL - NFS Transfers/MQL_MODELING_MASTER_GATED_BASE_PIPE_INRST_WEB_CI_LAG_POSITIVE", 
                       "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, S1_CLIENT_TEAMS, any_of(data %>% names())) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, S1_CLIENT_TEAMS, contains("_COMP_"), contains("_INDIV_")) %>%
  select(!starts_with('INTR')) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, !contains('YIELD'), -S1_CLIENT_TEAMS) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY,
         !any_of(updatedField))  %>%
  mutate(INBOUND_MARKETING_INTERACTION_KEY = INBOUND_MARKETING_INTERACTION_KEY %>% as.character)

# load missing ID fields... -----------------------------------------------
# gotta get the URN and YQ fields to map in some of our new pipe delta fields
id_fields <- read_csv('INBOUND ID FIELDS 20210729.csv')

# 1.0) BINNING: REF TABLE --------------------------------------------
# use the excel workbook where bins are already defined by conversion rates
# we can then load it in and create our dummy code for binning 
# just filter by variable and copy paste the dummy code in the next section
# might need to double check some of the OTHER and NULL buckets just in case....
bin_ref <- read_excel("~/Documents/Projects/MQL Routing/MQL MODEL - category binning summaries.xlsx", 
                      sheet = "BIN REF") %>%
  mutate(VAL = paste0("'", VAL, "'")) %>%
  group_by(VAR, BIN) %>%
  summarise(VAL = paste(VAL, collapse = ", ")) %>%
  ungroup() %>%
  mutate(VAL = paste0(VAR, " %in% c(", VAL, ") ~ '", BIN, "'")) %>%
  group_by(VAR) %>%
  summarise(VAL = paste(VAL, collapse = ',\n'))

bin_ref %>%
  filter(VAR == 'INDUSTRY') %>%
  pull(VAL) %>%
  cat()

# 1.1) BINNING - Function & Setup ----------------------------------------------
# use bin_ref to bin certain categorical fields to simplify one-hot encoding

# function to clean up bin names
name_cleaner <- function(x) {
  out <- str_replace_all(x, ' ', '_') %>%
    toupper()
  return(out)
}

# Function to bin numeric fields (NULL; 0; >= 1)
binary_bin <- function(x) {
  out <- case_when(x == -1 ~ 'NULL', 
                   x == 0 ~ 'ZERO',
                   T ~ '1PLUS')
  return(out)
} 


# bin fields and then update using 'dummify' function
# we can go ahead and drop fields that are just junk at the moment...
# if needed go back and add them back in
data <- raw %>%  
  select(!any_of(corr_DROP), corr_DROP[str_detect(corr_DROP, maintain)]) %>%
  select(!all_of(dropper)) %>%
  inner_join(jobs) %>%
  select(-COMP_REV_SEG, -EMP_CNT_SEG, -S1_CLIENT_TEAMS, 
         -CHANNEL_NAME, -CHANNEL_TYPE, -UT_LVL_10_DSCR, -TYPE_FLG, -UT_LVL_10_CD,
         -IND_SCORE_NEW, -DV_IS_OPEN_x, -DV_IS_OPEN_y) %>%
  select(!contains('YIELD')) %>%
  mutate(BUS_AREA_NAME = case_when(BUS_AREA_NAME %in% c('Held at Top Expenses', 'Communications', 'Audience Innovation', 'Portfolio Security', 'Open Marketplace', 'Other', 'Building a Smarter Business', 'IBM Master Brand', 'US Federal', 'IBM Global Financing', 'Advertising', 'Industry Marketing', 'Audience Partner', 'Journey to Cloud') ~ 'BIN_A',
                                   BUS_AREA_NAME %in% c('Watson Health', 'AI Applications', 'Security', 'Expansion Marketing', 'Blockchain and Strategic Alliances', 'Systems', 'Global Business Services') ~ 'BIN_B',
                                   BUS_AREA_NAME %in% c('Global Technology Services', 'Acoustic', 'Partner Ecosystem', 'xIBM Events', 'Talent Solutions') ~ 'BIN_C',
                                   BUS_AREA_NAME %in% c('Cloud and Data Platform', 'Developer', 'Audience Developer', 'Watson Media and Weather', 'Geography-specific Industries') ~ 'BIN_D',
                                   BUS_AREA_NAME %>% is.na ~ 'UNKNOWN',
                                   T ~ "NULL"),
         INDUSTRY = case_when(INDUSTRY %in% c('Media & Entertainment', 'Professional Services', 'Wholesale Distribution & Services') ~ 'BIN_CGHJ',
                              INDUSTRY %in% c('Consumer Products', 'Insurance', 'Retail') ~ 'BIN_FIN',
                              INDUSTRY %in% c('Education', 'Exclusions', 'Unknown') ~ 'BIN_LOW',
                              INDUSTRY %in% c('Banking', 'Energy & Utilities', 'Government, Central/Federal', 'Government, State/Provincial/Local') ~ 'BIN_NCA',
                              INDUSTRY %in% c('Healthcare', 'Life Sciences') ~ 'BIN_WAT',
                              T ~ "BIN_OTHER"),
         JOB_FUNCTION = case_when(JOB_FUNCTION %in% c('Transport & Travel Professions', 'Information Tech Professions', 'Govt & Civil Svc Professions', 'Retail/Wholesale and Services') ~ 'BIN_A',
                                  JOB_FUNCTION %in% c('Finance/Insurance Professions', 'Distribution/Storage/Logistics', 'Business Admin/Operations/Svcs') ~ 'BIN_B',
                                  JOB_FUNCTION %in% c('Sales and Marketing Professions', 'Processing/Manufac/Engineering', 'Life/Physical/Social Sciences', 'Health Discipline Professions', 'Executive Professions', 'Unknown', 'Legal Professions') ~ 'BIN_C',
                                  JOB_FUNCTION %in% c('Other', 'Educational Professions') ~ 'BIN_D',
                                  JOB_FUNCTION %>% is.na ~ 'UNKNOWN',
                                  T ~ "NULL"),
         UT_LVL_30_CD_INTENT_NEW = case_when(UT_LVL_30_CD_INTENT_NEW %in% c('30A8K') ~ 'CLOUD PAK AUTOMATION',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30AHU', '30AXQ', '30AM2', '30AXT') ~ 'CLOUD PAK DATA',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30NEB') ~ 'CLOUD PAK SEC',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30BIB') ~ 'MAAS360',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30BH6') ~ 'MAXIMO',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30AYO', '30AFV') ~ 'SPSS WATSON STUDIO',
                                             UT_LVL_30_CD_INTENT_NEW %in% c('30A1J') ~ 'WATSON ASSISTANT',
                                             UT_LVL_30_CD_INTENT_NEW %>% is.na() | UT_LVL_30_CD_INTENT_NEW == "     " ~ 'UNKNOWN',
                                             T ~  'OTHER'),
         UT_LVL_20_CD_INTENT_NEW = case_when(UT_LVL_20_CD_INTENT_NEW %in% c('20B1A') ~ 'ASSET MGMT',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20AJ5') ~ 'CLIENT CARE',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20A0X') ~ 'CLOUD PLTF',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20A11') ~ 'DATA AI PLTF',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20A0K') ~ 'DATA SCIENCE',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20B12') ~ 'MOBILE SEC',
                                             UT_LVL_20_CD_INTENT_NEW %in% c('20NYE') ~ 'SEC THREAT',
                                             UT_LVL_20_CD_INTENT_NEW %>% is.na() | UT_LVL_20_CD_INTENT_NEW == "     " ~ 'UNKNOWN',
                                             T ~ 'OTHER'),
         UT_LVL_15_CD_INTENT_NEW = case_when(UT_LVL_15_CD_INTENT_NEW %in% c('15TSS', '157Q0', '15MHW', '15SCT', '15CAI', '15CPT', '15EDG', '15PHW') ~ 'BIN_A',
                                             UT_LVL_15_CD_INTENT_NEW %in% c('15STT', '15ITT', '15IGO', '15WTH', '15STS', '15D66', '15TMS', '15CLS', '15MSC', '15WCG', '15MFT', '156UX') ~ 'BIN_B',
                                             UT_LVL_15_CD_INTENT_NEW %in% c('15ANP', '153QH', '15JHO', '15CLV', '15INP', '15TSL', '15EDC') ~ 'BIN_C',
                                             UT_LVL_15_CD_INTENT_NEW %>% is.na() ~ 'UNKNOWN',
                                             T ~ 'OTHER'),
         IBM_GBL_IMT_DSCR = case_when(IBM_GBL_IMT_DSCR %in% c('DACH', 'Central/Eastern Europe', 'BeNeLux') ~ 'DACH EEUR BENELUX',
                                      IBM_GBL_IMT_DSCR %in% c('Italy Market', 'Nordic', 'France Market') ~ 'IT NORD FR',
                                      IBM_GBL_IMT_DSCR %in% c('Japan Market', 'India/South Asia', 'Korea Market') ~ 'JP IND KOR',
                                      IBM_GBL_IMT_DSCR %in% c('US Top Market', 'Canadian Market') ~ 'NORTH AMERICA',
                                      T ~ 'OTHER'),
         PROGRAM_NAME = case_when(PROGRAM_NAME %in% c('Collaboration - Box', 'Strategic Partnerships and Alliances', 'Channel Expansion - 2017 Channel Expansion Program', 'Held at Top Expenses', 'IBM Research', 'Data Audience-Led', '2019 External Communications', 'Innovation', 'Customer Experience', 'Communications Sector', 'Core Applications', 'Sales Initiated Opportunities', 'Supply Chain', 'Communications', 'Security Conversation', 'Weather Business Solutions', 'CISO', 'IBM Services Brand', 'Channel Enablement', 'IBM Watson', 'Public Sector', 'Technology Transformation', 'Industrial Sector', 'CIO -  Enable Digital Business') ~ 'BIN_HI',
                                  PROGRAM_NAME %in% c('HR', 'Cross AI Applications', 'GTS Technology Support Services', 'Cross-Industry Activities', 'xIBM Third Party Events', 'Partner Conversation', 'Commerce', 'Watson Health Cross Program', 'Systems - IBM Z', 'TRIRIGA', 'Manage and secure networks and endpoints', 'Cloud and AI Theme Level Execution', 'Brexit Legal Notices', 'INT IBM Automation', 'Security Brand and Outcomes', 'Protect the mobile enterprise', 'WW_Threat Management', 'Retain and Grow', 'Systems - Infrastructure', 'On-site promotion for IBM Marketplace Catalog products', 'Cloud Application Innovation', 'INT Management and Platform', 'Systems - Storage', 'Security - Safer Planet', 'Cognitive Process Transformation', 'CIO - Deliver Digital Innovation', 'Watson Financial Services', 'Data and AI', 'Watson IoT - IoT Platform', 'Hybrid Data Management - Z', 'Digital Strategy and iX', 'Engineering', 'THINK 2020', 'IBM Cloud', 'Systems - Power', 'Manage application security risks', 'Hybrid Cloud Signature Moments', 'DAI DataOps', 'Global Conferences', 'Watson Health Clinical Decision Support', 'Watson Data Platform', 'Legacy Watson Marketing', 'Channel Expansion - Systems - BP Co-Marketing', 'Multi Cloud Services', 'DAI - Watson Applications and Solutions', 'DAI Hybrid Data Management', 'Blockchain', 'IBM Developer', 'DAI Business Analytics', 'IBM Collaboration Solutions', 'xIBM Think Regional Events', 'DAI Data Science', 'Channel Expansion - 2018_WW_Channel Expansion Program', 'SFT Cloud Platform Digital', 'DAI Watson Core Platform', 'Developer Conversation', 'WW_Security Services', 'DAI Cloud Pak for Data', 'Notice Program', 'Security Brand Level', 'Cross Hybrid Cloud', 'Platform', 'Strategic Account Marketing', 'Talent Solutions', 'PLT Cloud Platform F2F', 'Productivity', '2016 Cross IBM Connect Events', '2017 CA Brand Initiatives', '2017 CA Cognitive', '2017 Other Cross IBM Events', 'Activations', 'CIO - CIO Events', 'CIO - IBM MobileFirst', 'Citizenship', 'Cloud - Video', 'Cloud Storage', 'Commercial', 'Community', 'Detect Threats With Security Intelligence', 'Finance', 'Global Asset Recovery Services', 'IBM Blockchain', 'IBM Brand', 'IBM Security', 'IBM Sports', 'IBM Systems', 'IBM Watson Health', 'Inbound MKTCROSS', 'Mobile Security', 'Optimize the Security Program', 'Pillar Support', 'Security and Resiliency Narrative', 'SPGI - Israel', 'Test Program for eMessage', 'Watson Core - Discovery', 'Watson Media') ~ 'BIN_LO',
                                  PROGRAM_NAME %in% c('Automation', 'Business Operations', 'CIO - Leverage Cloud and Analytics', 'IBM Quantum', 'Cloud Ecosystems', 'IT Modernization', 'INT Cloud Aspera', 'WW_Data Security', 'Public Cloud', 'Build and Modernize Applications', 'IBM Global Financing', 'Maximo', 'Recruit', 'Managed at Top', 'IBM Healthcare', 'Protect against web fraud and cyber crime', 'Sterling Supply Chain', 'IBM Collaboration Solutions Events', 'Cloud Pak for Security', 'Cross Watson Health', 'Testing for URX Forms', 'Defense and Intelligence', 'Watson Health Management Decision Support', 'WW_Identity and Access Management', 'Advertising', 'Sterling B2B Collaboration', 'Activate', 'CA Cognitive', 'Resilient', 'Distribution Sector', 'Cross Sell', 'INT Integration and Development', 'SB Theme Level Execution', 'xIBM Geography specific Events', '2019_Co-Marketing Program') ~ 'BIN_MED',
                                  PROGRAM_NAME %in% c('Contact Module') ~ 'CONTACT MODULE',
                                  PROGRAM_NAME %>% is.na() ~ 'NULL',
                                  T ~ 'OTHER'),
         PARENT_ASSET_BUYERS_JOURNEY = case_when(PARENT_ASSET_BUYERS_JOURNEY %in% c('Adopt', 'Buy', 'Advocate') ~ 'BUY ADOPT ADVOCATE',
                                                 PARENT_ASSET_BUYERS_JOURNEY %>% is.na() ~ 'NULL',
                                                 T ~ PARENT_ASSET_BUYERS_JOURNEY)) %>%
  rowwise() %>%
  mutate(COMP_INST = sum(COMP_INST_FLG_MICROSOFT, COMP_INST_FLG_SAP, COMP_INST_FLG_HP, COMP_INST_FLG_ORACLE, 
                         COMP_INST_FLG_DELL, COMP_INST_FLG_CISCO, COMP_INST_FLG_OPENSRC, COMP_INST_FLG_AMAZON, 
                         COMP_INST_FLG_VMWARE, COMP_INST_FLG_GOOGLE, COMP_INST_FLG_IBM, COMP_INST_FLG_ADOBE, 
                         COMP_INST_FLG_AUTODESK, COMP_INST_FLG_MICROFOCUS, COMP_INST_FLG_CITRIX, COMP_INST_FLG_WORDPRESS, 
                         COMP_INST_FLG_SALESFORCE, COMP_INST_FLG_FB)) %>%
  ungroup() %>%
  mutate(COMP_INST = case_when(COMP_INST > 0 & COMP_INST <= 5 ~ '1TO5',
                               COMP_INST >= 6 ~ '6PLUS',
                               T ~ 'NULL')) %>%
  select(!contains('COMP_INST'), COMP_INST) %>%
  ungroup() %>%
  # Simple Bin for Numeric Features
  mutate(across(c(INTR_GBS_COMP_CNT, INTR_WH_COMP_CNT, INTR_15IGO_INDIV_CNT,
                  INTR_153QH_COMP_CNT, #INTR_15ANP_COMP_CNT, INTR_15ITT_COMP_CNT, 
                  INTR_15MFT_COMP_CNT, INTR_15MHW_COMP_CNT, INTR_15PHW_COMP_CNT, 
                  INTR_15STS_COMP_CNT, INTR_15ANP_INDIV_CNT, INTR_15IGO_INDIV_CNT,
                  INTR_20B2P_COMP_CNT, INTR_20A0W_COMP_CNT, INTR_20APO_COMP_CNT,
                  INTR_20A0O_COMP_CNT, INTR_20B17_COMP_CNT, INTR_20AU4_COMP_CNT, 
                  INTR_20B14_COMP_CNT, INTR_20B2L_COMP_CNT, INTR_20A0K_COMP_CNT, 
                  INTR_20GKU_COMP_CNT, INTR_20BEQ_COMP_CNT,
                  UT15_OFFER_153QH_COMP_CNT_60DAYS, UT15_OFFER_15IGO_COMP_CNT_60DAYS, UT15_OFFER_15MHW_COMP_CNT_60DAYS), 
                binary_bin)) %>%
  set_missing(list(-1L, "NULL")) %>%
  mutate(across(where(is.character), name_cleaner))
  

# 1.2) BINNING: One-Hot-Encoding ---------------------------------------------------
# once all the binning is done, we can one-hot encode our new fields
input <- data %>%
  dummify(maxcat = 10)

# bring UT10 target back in
input <- input %>%
  inner_join(raw %>% 
               select(INBOUND_MARKETING_INTERACTION_KEY, UT_LVL_10_CD))


# 1.3) Mapping Updated Pipe Fields (lag / same q) -------------------------


# USE OBJECTS FROM 'RLM & Same Q Pipe Update 2.RData'
# make vector of current features in input table
# we will need this to figure out which ones to replace with the ones from test_df
originalField <- input %>%
  names()

# drop fields that were updated in pipe_SUBBED & replace them with the new values 
# use RLM_OPTY vector to flag RLM sourced opportunities
input <- input %>%
  select(!contains('WON_DTL_REVN')) %>%
  select(!any_of(updatedField)) %>%
  mutate(INBOUND_MARKETING_INTERACTION_KEY = INBOUND_MARKETING_INTERACTION_KEY %>% as.character) %>%
  left_join(pipe_SUBBED) %>%
  left_join(pipe_lag) %>%
  mutate(RLM_OPTY_FLG = case_when(INBOUND_MARKETING_INTERACTION_KEY %in% RLM_opty ~ 1,
                                  T ~ 0)) %>%
  select(sort(current_vars())) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, UT_LVL_10_CD, RLM_OPTY_FLG, everything())


# 1.4) NUMERIC: Additional Feature Engineering ---------------------------------
# Update Pipe WON DTL REV to be a percentage of total WON DTL REV
# Hypothesis: company level breakdown of revenue more meaningful than raw numbers
# Keep ALL_WON_DTL_REV as is to keep scale of client / indiv
# No need to do a similar normalization for INDIVs (very sparse / correlated)

input <- input %>%
  mutate(across(!starts_with('ALL_WON_DTL_REV') & 
                  contains('WON_DTL') & 
                  contains('COMP') & 
                  !ends_with('_FLG'), 
                ~ (./ ALL_WON_DTL_REVN_COMP_SUM_C) %>% coalesce(0) ))



# 2.1) Prep BORUTA --------------------------------------------------------
boruta_INPUT <- input %>%
  select(!contains("ALL_OTHER")) %>%
  select(!starts_with("BJ_")) %>%
  select(!starts_with('CHN_NM')) %>%
  select(!starts_with('CHN_T')) %>%
  select(!contains('WON_DTL_REVN_INDIV')) %>%
  select(!contains('WON_LEAD_COMP')) %>%
  select(!contains('WON_LEAD_INDIV')) %>%
  select(!starts_with('PARENT_ASSET_BUYERS')) %>%
  select(!starts_with('UT_LVL_20_CD_INTENT')) %>%
  select(!starts_with('UT_LVL_30_CD_INTENT')) %>%
  select(!matches('INTR_.*FLG')) %>%
  select(-EXT_DTL_REVN_USD, -INDEX, -NEWCO_FLG_BASE, 
         -P_SCORE_NEW_FLAG, -RS_MATCH_FLAG, -AVG_RESP_DAYS)
  
# save.image("~/Documents/Projects/MQL Modeling/UT10 Model/xgb6 prep ws.RData")
# 2.2) Run BORUTA ---------------------------------------------------------
test <- boruta_INPUT %>%
  filter(RLM_OPTY_FLG == 1) %>%
  mutate(UT_LVL_10_CD = UT_LVL_10_CD %>% as.factor) %>%
  select(-INBOUND_MARKETING_INTERACTION_KEY, -RLM_OPTY_FLG)

boruta_RLM <- Boruta(UT_LVL_10_CD ~ ., 
                     data = test, 
                     maxRuns = 11,
                     doTrace = 2)

RLM_stats <- attStats(boruta_RLM) %>%
  mutate(FIELD = row.names(.)) %>%
  as_tibble() %>%
  arrange(-medianImp) %>%
  select(FIELD, everything())



