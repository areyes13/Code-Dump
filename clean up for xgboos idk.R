# Load Packages -----------------------------------------------------------
# update to match your working directory (where you have saved your files)
setwd('/Users/areyesm@us.ibm.com/Documents/Projects/MQL Modeling/UT10 Model')
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

# load 85% corr drop -------------------------------------------------------
load("~/Documents/Projects/MQL Modeling/UT10 Model/CORR DROP.RData")

# load data v2 from python ------------------------------------------------
data <- read_csv("full ut10 modeling data v2.csv") %>%
  select(!any_of(corr_DROP))

# load job feature (leftover) ---------------------------------------------
jobs <- read_csv('MQL_MODELING_MASTER_GATED_EMPL_FIELDS 20210713.csv') %>%
  mutate(JOB_CTGY_CD = coalesce(JOB_CTGY_CD, 'NULL'))


# load selected fields (from final boruta) --------------------------------
selector_Final <- read_excel("Feature Elimination UT10.xlsx", 
                       sheet = "BORUTA FINAL OUT")







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

# 1.1) BINNING: CATEGORICALS ----------------------------------------------
# use bin_ref to bin certain categorical fields to simplify one-hot encoding
# we will append this later to the main data set
cat_box <- data %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, BUS_AREA_NAME, COMP_REV_SEG, EMP_CNT_SEG, INDUSTRY, 
         JOB_FUNCTION, PARENT_ASSET_BUYERS_JOURNEY, # MARKET_INTERACTION,
         UT_LVL_30_CD_INTENT_NEW, UT_LVL_20_CD_INTENT_NEW, UT_LVL_15_CD_INTENT_NEW,
         CHANNEL_NAME, CHANNEL_TYPE, IBM_GBL_IMT_DSCR, PROGRAM_NAME) %>%
  mutate(BUS_AREA_NAME = case_when(BUS_AREA_NAME %in% c('Held at Top Expenses', 'Communications', 'Audience Innovation', 'Portfolio Security', 'Open Marketplace', 'Other', 'Building a Smarter Business', 'IBM Master Brand', 'US Federal', 'IBM Global Financing', 'Advertising', 'Industry Marketing', 'Audience Partner', 'Journey to Cloud') ~ 'BIN_A',
                                   BUS_AREA_NAME %in% c('Watson Health', 'AI Applications', 'Security', 'Expansion Marketing', 'Blockchain and Strategic Alliances', 'Systems', 'Global Business Services') ~ 'BIN_B',
                                   BUS_AREA_NAME %in% c('Global Technology Services', 'Acoustic', 'Partner Ecosystem', 'xIBM Events', 'Talent Solutions') ~ 'BIN_C',
                                   BUS_AREA_NAME %in% c('Cloud and Data Platform', 'Developer', 'Audience Developer', 'Watson Media and Weather', 'Geography-specific Industries') ~ 'BIN_D',
                                   BUS_AREA_NAME %>% is.na ~ 'UNKNOWN',
                                   T ~ "NULL"),
         COMP_REV_SEG = case_when(COMP_REV_SEG %in% c('A: 0', 'B: $1 to $2.4k', 'C: $2.5k to $4.9k', 'D: $5k to $9.9k', 'E: $10k to $19.9k', 'F: $20k to $49.9k', 'G: $50k to $99.9k', 'H: $100k to $499.9k', 'I: $500k to $999.9k', 'J: $1mm to $4.9mm') ~ '[0,5M)',
                                  COMP_REV_SEG %in% c('O: $100mm+') ~ '[100M,inf)',
                                  COMP_REV_SEG %in% c('K: $5mm to $9.9mm', 'L: $10mm to $24.9mm', 'M: $25mm to $49.9mm', 'N: $50mm to $99.9mm') ~ '[5M, 100M)',
                                  COMP_REV_SEG %>% is.na ~ 'UNKNOWN',
                                  T ~ "NULL"),
         EMP_CNT_SEG = case_when(EMP_CNT_SEG %in% c('A: 0', 'B: 1 to 99') ~ '[0,100)',
                                 EMP_CNT_SEG %in% c('C: 100 to 999', 'D: 1k to 4.9k') ~ '[100,5k)',
                                 EMP_CNT_SEG %in% c('G: 50k to 99.9k') ~ '[50k,inf)',
                                 EMP_CNT_SEG %in% c('E: 5k to 9.9k', 'F: 10k to 49.9k') ~ '[5k,50k)',
                                 EMP_CNT_SEG %in% c('H: 100k to 249.9k', 'I: 250k+') ~ 'BIN_A',
                                 EMP_CNT_SEG %>% is.na ~ 'UNKNOWN',
                                 T ~ "NULL"),
         INDUSTRY = case_when(INDUSTRY %in% c('Energy & Utilities', 'Banking', 'Government, Central/Federal', 'Government, State/Provincial/Local', 'Consumer Products', 'Insurance', 'Travel & Transportation') ~ 'BIN_A',
                              INDUSTRY %in% c('Chemicals & Petroleum', 'Aerospace & Defense', 'Automotive', 'Life Sciences', 'Financial Markets', 'Retail') ~ 'BIN_B',
                              INDUSTRY %in% c('Industrial Products', 'Telecommunications', 'Healthcare', 'Electronics', 'Media & Entertainment', 'Wholesale Distribution & Services') ~ 'BIN_C',
                              INDUSTRY %in% c('Computer Services', 'Professional Services', 'Exclusions', 'Education') ~ 'BIN_D',
                              INDUSTRY %>% is.na ~ 'UNKNOWN',
                              T ~ "NULL"),
         JOB_FUNCTION = case_when(JOB_FUNCTION %in% c('Transport & Travel Professions', 'Information Tech Professions', 'Govt & Civil Svc Professions', 'Retail/Wholesale and Services') ~ 'BIN_A',
                                  JOB_FUNCTION %in% c('Finance/Insurance Professions', 'Distribution/Storage/Logistics', 'Business Admin/Operations/Svcs') ~ 'BIN_B',
                                  JOB_FUNCTION %in% c('Sales and Marketing Professions', 'Processing/Manufac/Engineering', 'Life/Physical/Social Sciences', 'Health Discipline Professions', 'Executive Professions', 'Unknown', 'Legal Professions') ~ 'BIN_C',
                                  JOB_FUNCTION %in% c('Other', 'Educational Professions') ~ 'BIN_D',
                                  JOB_FUNCTION %>% is.na ~ 'UNKNOWN',
                                  T ~ "NULL"),
         # MARKET_INTERACTION = case_when(MARKET_INTERACTION %in% c('EMSG') ~ 'EMSG',
         #                                MARKET_INTERACTION %in% c('DISPLAY', 'EARNED', 'FACE-TO-FACE EVENT', 'OTHER', 'OWNED SOCIAL', 'SEARCH', 'CONTENT SYNDICATION', 'PAID SOCIAL', 'PRE-EVENT REGISTRATION / CONFIRMATION', 'WEBCAST', 'AFFILIATE', 'DIGITAL OTHER', 'LANDING PAGE') ~ 'RESP',
         #                                MARKET_INTERACTION %>% is.na ~ "UNKNOWN", 
         #                                T ~ "NULL"),
         # MARKET_INTERACTION = ifelse(MARKET_INTERACTION == 'RESP', 1, 0),
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
         CHANNEL_NAME = case_when(CHANNEL_NAME %in% c('Digital Inbound') ~ 'DIGITAL INBOUND',
                                  CHANNEL_NAME %in% c('Event') ~ 'EVENT',
                                  CHANNEL_NAME %>% is.na() ~ 'NULL',
                                  T ~ 'OTHER'),
         CHANNEL_TYPE = case_when(CHANNEL_TYPE %in% c('Content Syndication') ~ 'CONT SYND',
                                  CHANNEL_TYPE %in% c('Email') ~ 'EMAIL',
                                  CHANNEL_TYPE %in% c('External-OB Telemarketing', 'External-Purchased') | is.na(CHANNEL_TYPE)  ~ 'EPEOS',
                                  CHANNEL_TYPE %in% c('Other', 'Face to Face') ~ 'F2F OTH',
                                  CHANNEL_TYPE %in% c('Hybrid', 'Virtual', 'Landing Page') ~ 'VR LP HYB',
                                  T ~ 'F2F OTH'),
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
                                                 T ~ PARENT_ASSET_BUYERS_JOURNEY)) 


# 1.2) Clean up other categoricals ------------------------------------------------------
# let's grab the other categorical variables that we didn't have to reclassify
# we still need to one hot encode / treat NULLs so we will do that here
# then merge with the cat_box table back into the main modeling df
cat_keep <- data %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, where(is.character)) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, !any_of(bin_ref %>% pull(VAR))) %>%
  select(-UT_LVL_10_CD, -UT_LVL_10_DSCR) %>%
  mutate(across(where(is.character), ~ coalesce(.x, 'NULL')))
  

# 1.3) Create categorical encoded tbl -------------------------------------
# join by INBOUND MARKETING KEY
# this now has all the categorical features set as binary flags
# so the next step is to merge back into main input table

cat_box_wide <- cat_box %>%
  gather(var, val, -INBOUND_MARKETING_INTERACTION_KEY) %>%
  unite("var", var, val, sep = "_") %>%
  mutate(var = str_replace_all(var, ' ', '_'),
         var = toupper(var),
         dummy = 1) %>%
  spread(var, dummy, fill = 0)

cat_keep_wide <- cat_keep %>%
  gather(var, val, -INBOUND_MARKETING_INTERACTION_KEY) %>%
  unite("var", var, val, sep = "_") %>%
  mutate(var = str_replace_all(var, ' ', '_'),
         var = toupper(var),
         dummy = 1) %>%
  spread(var, dummy, fill = 0)

cat_final <- cat_box_wide %>%
  inner_join(cat_keep_wide)

# vector of names
cat_names <- cat_final %>%
  select(-INBOUND_MARKETING_INTERACTION_KEY) %>%
  names()


# junk --------------------------------------------------------------------


numInput <- data %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, 
         any_of(selector_Final %>%
                  filter(FINAL == 'KEEP') %>%
                  pull(FIELD)))

catInput <- data %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, UT_LVL_10_CD, BUS_AREA_NAME, COMP_REV_SEG, EMP_CNT_SEG, INDUSTRY, 
         JOB_FUNCTION, MARKET_INTERACTION, PARENT_ASSET_BUYERS_JOURNEY,
         UT_LVL_30_CD_INTENT_NEW, UT_LVL_20_CD_INTENT_NEW, UT_LVL_15_CD_INTENT_NEW,
         CHANNEL_NAME, CHANNEL_TYPE, IBM_GBL_IMT_DSCR, PROGRAM_NAME)

