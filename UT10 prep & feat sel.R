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


# workspace
load("~/Documents/Projects/MQL Modeling/UT10 Model/UT10 prep and feature select.RData")

# load("~/R Files/MQL Feature Selection/UT10 prep and feature select.RData")
# load("~/R Files/MQL Feature Selection/UT10 prep and feature select - PRE RFE.RData")
# load("~/R Files/MQL Feature Selection/CORR DROP.RData")
# load("~/R Files/MQL Feature Selection/chonker.RData")

# LOAD MODELING DATA ------------------------------------------------------

data <- read_csv("full ut10 modeling data.csv")


# EMPLOYEE DATA -----------------------------------------------------------

jobs <- read_csv('MQL_MODELING_MASTER_GATED_EMPL_FIELDS 20210713.csv') %>%
  mutate(JOB_CTGY_CD = coalesce(JOB_CTGY_CD, 'NULL'))


# SET UT10 TARGET AS BINARY -----------------------------------------------
radar <- data %>%
  select(UT_LVL_10_CD, INBOUND_MARKETING_INTERACTION_KEY) %>% 
  mutate(UT_LVL_10_CD = case_when(UT_LVL_10_CD %in% c('10G00', '10J00') ~ '10G00_10J00',
                                  T ~ UT_LVL_10_CD),
         dummy = 1,
         UT_LVL_10_CD = paste0("TARGET_",UT_LVL_10_CD)) %>% 
  spread(UT_LVL_10_CD, dummy,  fill = 0)

# DROP FIELDS WE WON'T NEED FOR MODELING ----------------------------------
# FROM EXCEL WORKBOOK WITH BINS...
dropper <- read_excel("~/Documents/Projects/MQL Routing/MQL MODEL - category binning summaries.xlsx", 
                      sheet = "Modeling Base") %>%
  filter(Include == 'F') %>%
  pull(VARIABLES)


# MAP BINARY TARGETS BACK TO RAW DATA & DROP FIELDS -------------------------------------
# EDITS: need to clean up NULLs in S1/S2 field and then set a combined tag for Services UT10 CD
df <- radar %>%
  inner_join(data) %>%
  inner_join(jobs) %>%
  select(!all_of(dropper)) %>%
  mutate(S1_CLIENT_TEAMS = coalesce(S1_CLIENT_TEAMS, "S2"),
         UT_LVL_10_CD = case_when(UT_LVL_10_CD %in% c('10G00', '10J00') ~ '10G00_10J00',
                                  T ~ UT_LVL_10_CD),
         UT_LVL_10_CD_INTENT_NEW = case_when(UT_LVL_10_CD_INTENT_NEW %in% c('10G00', '10J00') ~ '10G00_10J00',
                                             T ~ UT_LVL_10_CD_INTENT_NEW))


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

# spit out one chunk of dummy code for CASE WHENs
bin_ref %>%
  filter(VAR == 'PARENT_ASSET_BUYERS_JOURNEY') %>%
  pull(VAL) %>%
  cat


# 1.1) BINNING: CATEGORICALS ----------------------------------------------
# use bin_ref to bin certain categorical fields to simplify one-hot encoding
# we will append this later to the main data set
cat_box <- df %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, BUS_AREA_NAME, COMP_REV_SEG, EMP_CNT_SEG, INDUSTRY, 
         JOB_FUNCTION, MARKET_INTERACTION, PARENT_ASSET_BUYERS_JOURNEY,
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
         MARKET_INTERACTION = case_when(MARKET_INTERACTION %in% c('EMSG') ~ 'EMSG',
                                         MARKET_INTERACTION %in% c('DISPLAY', 'EARNED', 'FACE-TO-FACE EVENT', 'OTHER', 'OWNED SOCIAL', 'SEARCH', 'CONTENT SYNDICATION', 'PAID SOCIAL', 'PRE-EVENT REGISTRATION / CONFIRMATION', 'WEBCAST', 'AFFILIATE', 'DIGITAL OTHER', 'LANDING PAGE') ~ 'RESP',
                                         MARKET_INTERACTION %>% is.na ~ "UNKNOWN", 
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
                                                 T ~ PARENT_ASSET_BUYERS_JOURNEY),
         MARKET_INTERACTION = ifelse(MARKET_INTERACTION == 'RESP', 1, 0)) 





# 1.2) Clean up other categoricals ------------------------------------------------------
# let's grab the other categorical variables that we didn't have to reclassify
# we still need to one hot encode / treat NULLs so we will do that here
# then merge with the cat_box table back into the main modeling df
cat_keep <- df %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, where(is.character)) %>%
  select(INBOUND_MARKETING_INTERACTION_KEY, !all_of(bin_ref %>% pull(VAR))) %>%
  select(-UT_LVL_10_CD, -UT_LVL_10_DSCR) %>%
  mutate(across(where(is.character), ~ coalesce(.x, 'NULL')))


# 1.3) Create categorical encoded tbl -------------------------------------
# join by INBOUND MARKETING KEY
# this now has all the categorical features set as binary flags
# so the next step is to merge back into main input table

cat_box_wide <- cat_box %>%
  gather(var, val, -INBOUND_MARKETING_INTERACTION_KEY, - MARKET_INTERACTION) %>%
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

# 1.4) create input table (version 1) -------------------------------------
# vector of fields we want to replace with our new bins
cat_cleaner <- tibble(FIELD = c(cat_keep %>% names, cat_box %>% names)) %>%
  filter(FIELD != "INBOUND_MARKETING_INTERACTION_KEY") %>%
  distinct() %>%
  pull(FIELD) 

# drop raw binned fields 
# map in new bins!
# use this for feature selection
input_master <- df %>%
  select(!all_of(cat_cleaner)) %>%
  inner_join(cat_final) %>%
  mutate(across(where(is.numeric), coalesce, -1)) 

# rm(cat_box, cat_keep, cat_box_wide, cat_keep_wide)

# 1.5) Last Touch Baseline ------------------------------------------------
# for reference!
lastTouch <- df %>%
  select(UT_LVL_10_CD_INTENT_NEW, UT_LVL_10_CD) %>%
  mutate(HIT = coalesce(UT_LVL_10_CD_INTENT_NEW == UT_LVL_10_CD, NA)) %>%
  # filter(!is.na(UT_LVL_10_CD_INTENT_NEW)) %>%
  group_by(UT_LVL_10_CD) %>%
  count(HIT) %>%
  mutate(TOTAL = sum(n)) %>%
  spread(HIT, n) %>%
  mutate(prct = `TRUE`/TOTAL,
         prct_pos = `TRUE`/(`TRUE` + `FALSE`)) %>%
  arrange(-prct)
  

# 2.1) FEATURE SELECTION PREP ---------------------------------------------
# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      verbose = T,
                      repeats = 5, # number of repeats
                      number = 5) # number of folds

# Features
input <- input_master %>%
  # SELECT SINGLE BINARY TARGET (START WITH CNDP 10A00)
  select(#-TARGET_10A00,
         -TARGET_10N00,
         -TARGET_10C00,
         -TARGET_10B00,
         -TARGET_10L00,
         -TARGET_10G00_10J00,
         -TARGET_10H00,
         -UT_LVL_10_CD, -UT_LVL_10_DSCR)


# Training: 80%; Test: 20%
set.seed(12345)
train <- input %>%
  sample_frac(.8)

test <- anti_join(input, train, by = 'INBOUND_MARKETING_INTERACTION_KEY')

# rm(input_master, bin_ref, cat_box, jobs, cat_box_wide, cat_keep, cat_keep_wide, df, cat_final, radar,cat_cleaner, dropper, cat_cleaner, dropper, lastTouch, data)

# 2.2) RFE: CNDP binary target - CATS ONLY  --------------------------------------------
# TOO MANY FEATURES SO LETS DO IT IN CHUNKS...
# first chunk: Categorical variables 

x_train <- train %>%
  select(all_of(cat_names))

y_train <- train %>%
  pull(TARGET_10A00)

rfe_CNDP_cats <- rfe(x = x_train, 
                     y = y_train, 
                     sizes = c(1,5,25),
                     rfeControl = control)



# 3.2) BORUTA - CNDP NUMERIC? ---------------------------------------------
# Let's put aside binary flag features (there's quite a bit)
# looks like there is still some junk we need to drop...
# then prep train set and throw into BORUTA
x_train <- train %>%
  select(!any_of(corr_DROP)) %>%
  select(!any_of(cat_names)) %>%
  select(!ends_with('_FLG')) %>%
  select(-DV_IS_OPEN_x, -DV_IS_OPEN_y, -BUS_AREA_ID, -RS_MATCH_FLAG, -EXT_DTL_REVN_USD,
         -INDEX, -P_SCORE_NEW_FLAG, -NEWCO_FLG_BASE) 


boruta_CNDP_num <- Boruta(TARGET_10A00 ~ ., 
                          data = x_train,
                          doTrace = 1)



# 3.3 BORUTA - CNDP CAT ** ------------------------------------------------
# Let's start with the simplest chunk first
# looks like there is still some junk we need to drop...
# then prep train set and throw into BORUTA
x_train <- train %>%
  select(!any_of(corr_DROP)) %>%
  select(TARGET_10A00, any_of(cat_names)) 

# dummy <- x_train %>%
#   select(TARGET_10A00, 
#          BUS_AREA_NAME_BIN_D, 
#          PROGRAM_NAME_CONTACT_MODULE, BUS_AREA_NAME_BIN_B,
#          PROGRAM_NAME_BIN_MED, INDUSTRY_BIN_A,
#          JOB_FUNCTION_BIN_A, EMP_CNT_SEG_BIN_A) %>%
#   mutate(junk = runif(nrow(.)))

boruta_CNDP_cat <- Boruta(TARGET_10A00 ~ ., 
                          data = x_train, 
                          maxRuns = 11,
                          doTrace = 1)

# save(boruta_CNDP_cat, file = 'boruta_CNDP_cat.RData')

attStats(boruta_CNDP_cat) %>%
  arrange(-medianImp)


plot(boruta_CNDP_cat)


# chunk up features **** -------------------------------------------------------
# already running cats + FLG fields

chonker <- tibble(FIELD = input %>%
                    select(!any_of(corr_DROP), corr_DROP[str_detect(corr_DROP, 'COMP_INST')]) %>%
                    select(!any_of(cat_names)) %>%
                    select(!ends_with('_FLG')) %>%
                    select(-DV_IS_OPEN_x, -DV_IS_OPEN_y, -BUS_AREA_ID, -RS_MATCH_FLAG, -EXT_DTL_REVN_USD,
                           -INDEX, -P_SCORE_NEW_FLAG, -NEWCO_FLG_BASE) %>%
                    names) %>%
  filter(!FIELD %in% c('TARGET_10A00', 'INBOUND_MARKETING_INTERACTION_KEY')) %>%
  mutate(type = case_when(FIELD %>% str_detect('INDIV_SUM_I') ~ 'PIPE INDIV',
                          FIELD %>% str_detect('INDIV_YIELD_I') ~ 'PIPE INDIV',
                          FIELD %>% str_detect('COMP_SUM_C') ~ 'PIPE COMP',
                          FIELD %>% str_detect('COMP_YIELD_C') ~ 'PIPE COMP',
                          FIELD %>% str_starts('INTR_') ~ 'INTR',
                          FIELD %>% str_starts('PAG_') ~ 'WEB',
                          FIELD %>% str_starts('PG') ~ 'WEB',
                          FIELD %>% str_detect('_OFFER_') ~ 'OFFER',
                          FIELD %>% str_starts('CHN_') ~ 'CHANNEL',
                          FIELD %>% str_starts('BJ_') ~ 'BJ',
                          FIELD %>% str_starts('UT10') ~ 'LAST TOUCH',
                          FIELD %>% str_starts('COMP_INST') ~ 'COMP INST',
                          T ~ 'OTHER')) %>%
  arrange(type) %>%
  group_by(index = (row_number()-1) %/% 100)

save(chonker, file = 'chonker.RData')



# BORUTA - CNDP NUM CHUNK 0 **** -----------------------------------------------
x_train <- train %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 0) %>%
                  pull(FIELD)))

boruta_CNDP_chunk0 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

# save(boruta_CNDP_chunk0, file = 'boruta chunk 0.RData')

# BORUTA - CNDP NUM CHUNK 1 **** -----------------------------------------------
x_train <- train %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 1) %>%
                  pull(FIELD)))

boruta_CNDP_chunk1 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_CNDP_chunk1, file = 'boruta chunk 1.RData')

# BORUTA - CNDP NUM CHUNK 2 **** -----------------------------------------------
x_train <- train %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 2) %>%
                  pull(FIELD)))

boruta_CNDP_chunk2 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_CNDP_chunk2, file = 'boruta chunk 2.RData')

# BORUTA - CNDP NUM CHUNK 3 **** -----------------------------------------------
x_train <- train %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 3) %>%
                  pull(FIELD)))

boruta_CNDP_chunk3 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 3)

save(boruta_CNDP_chunk3, file = 'boruta chunk 3.RData')

# BORUTA - CNDP NUM CHUNK 3 **** -----------------------------------------------
x_train <- train %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 4) %>%
                  pull(FIELD)))

boruta_CNDP_chunk4 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_CNDP_chunk4, file = 'boruta chunk 4.RData')


# Combined Table ----------------------------------------------------------

chunk_stats_CNDP <-  bind_rows(attStats(boruta_CNDP_chunk0) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_CNDP_chunk1) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_CNDP_chunk2) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_CNDP_chunk3) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_CNDP_chunk4) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble()) %>% 
  left_join(chonker) %>%
  arrange(-medianImp)

save(chunk_stats_CNDP, file = 'chunk stats.RData')
# TEMPLATE ----------------------------------------------------------------


# Run RFE
x_train <- train %>%
  select(-INBOUND_MARKETING_INTERACTION_KEY, -TARGET_10A00) 

y_train <- train %>%
  pull(TARGET_10A00)

rfe_CNDP <- rfe(x = x_train, 
                y = y_train, 
                sizes = c(1,5,10,25,50),
                rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()


