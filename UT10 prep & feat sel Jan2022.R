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
load("prep ws jan2022.RData")
# load("~/Documents/Projects/MQL Modeling/UT10 Model/UT10 prep and feature select.RData")
# load("~/R Files/MQL Feature Selection/UT10 prep and feature select.RData")
# load("~/R Files/MQL Feature Selection/UT10 prep and feature select - PRE RFE.RData")
# load("~/R Files/MQL Feature Selection/CORR DROP.RData")
# load("~/R Files/MQL Feature Selection/chonker.RData")


# LOAD MODELING DATA ------------------------------------------------------

# data <- read_csv("full ut10 modeling data.csv")

input <- input %>%
  rename(IBM_GBL_IOT_DSCR_NULL = IBM_GBL_IOT_DSCR_.1,
         ASSET_MKTG_CHNL_NAME_NULL = ASSET_MKTG_CHNL_NAME_.1,
         ASSET_UT_LVL_10_CD_NULL = ASSET_UT_LVL_10_CD_.1,
         OFFERING_SOURCE_NULL = OFFERING_SOURCE_.1,
         RLM_FLG = RLM_FLG_1PLUS) %>%
  mutate(BUYR_STG_NULL = case_when(BUYR_STG_NULL == 0 & BUYR_STG_.1 == 1 ~ 1,
                                   T ~ 0)) %>%
  select(-RLM_FLG_ZERO)

# SET UT10 TARGET AS BINARY -----------------------------------------------
radar <- data %>%
  select(UT_LVL_10_CD, MIP_INBOUND_MKTG_INTRCTN_ID) %>% 
  mutate(dummy = 1,
         UT_LVL_10_CD = paste0("TARGET_",UT_LVL_10_CD)) %>% 
  spread(UT_LVL_10_CD, dummy,  fill = 0)

# DROP FIELDS WE WON'T NEED FOR MODELING ----------------------------------
# FROM EXCEL WORKBOOK WITH BINS...
# dropper <- read_excel("~/Documents/Projects/MQL Routing/MQL MODEL - category binning summaries.xlsx", 
#                       sheet = "Modeling Base") %>%
#   filter(Include == 'F') %>%
#   pull(VARIABLES)


# MAP BINARY TARGETS BACK TO RAW DATA & DROP FIELDS -------------------------------------
# EDITS: need to clean up NULLs in S1/S2 field and then set a combined tag for Services UT10 CD
df <- radar %>%
  inner_join(input %>%
               filter(RLM_FLG == 1)) %>%
  select(-RLM_FLG, -EXT_DTL_REVN_USD) %>%
  select(!starts_with('ASSET_UT_LVL_10_DSCR')) %>%
  select(!starts_with('ASSET_BUYR_STG')) %>%
  select(!starts_with('ASSET_MKTG_CHNL_TYPE')) %>%
  select(!starts_with('ASSET_MKTG_INTRCTN_TYPE_CD')) %>%
  select(!starts_with('EMP_CNT_SEG_2')) %>%
  select(!starts_with('OWNGORG')) %>%
  select(!starts_with('OWNG_ORG')) %>%
  select(!starts_with('MODEL_UT')) %>%
  select(!starts_with('DV_')) %>%
  select(!contains('.1')) %>%
  select(!contains('OPTY')) %>%
  select(!contains('SALES_STAGE_DSCR'))


# 2.1) CNDP FEATURE SELECTION PREP ---------------------------------------------
df %>% select(starts_with('TARGET')) %>% names

# Features
model <- df %>%
  select(!starts_with('TARGET_'), TARGET_10A00) %>%
  select(-UT_LVL_10_CD)

rm(data, input, pipe_IOT, pipe_TREND, radar, resp_date)
gc()

save.image("boruta input ut10 jan2022.RData")

# 2.2) chunk up features **** -------------------------------------------------------
# already running cats + FLG fields

chonker <- tibble(FIELD = model %>%
                    select(-MIP_INBOUND_MKTG_INTRCTN_ID, -TARGET_10A00) %>%
                    names) %>%
  mutate(type = case_when(FIELD %>% str_detect('INDIV_SUM_I') ~ 'PIPE INDIV',
                          FIELD %>% str_detect('COMP_SUM_C') ~ 'PIPE COMP',
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
  group_by(index = (row_number()-1) %/% 200)

# save(chonker, file = 'chonker.RData')



# 2.3) BORUTA - CNDP NUM CHUNK 0 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 0) %>%
                  pull(FIELD)))

boruta_CNDP_chunk0 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

# save(boruta_CNDP_chunk0, file = 'boruta chunk 0.RData')

# 2.3) BORUTA - CNDP NUM CHUNK 1 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 1) %>%
                  pull(FIELD)))

boruta_CNDP_chunk1 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_CNDP_chunk1, file = 'boruta chunk 1.RData')

# 2.4) BORUTA - CNDP NUM CHUNK 2 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 2) %>%
                  pull(FIELD)))

boruta_CNDP_chunk2 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_CNDP_chunk2, file = 'boruta chunk 2.RData')

# 2.5) BORUTA - CNDP NUM CHUNK 3 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10A00, 
         any_of(chonker %>% 
                  filter(index == 3) %>%
                  pull(FIELD)))

boruta_CNDP_chunk3 <- Boruta(TARGET_10A00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 3)

save(boruta_CNDP_chunk3, file = 'boruta chunk 3.RData')



# 2.6) CNDP Combined Table ----------------------------------------------------------

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
                                 as_tibble()) %>% 
  left_join(chonker) %>%
  arrange(-medianImp)

save(chunk_stats_CNDP, file = 'CNDP chunk boruta stats.RData')

# 3.1) INFRASTRUCTURE FEATURE SELECTION PREP ---------------------------------------------
df %>% select(starts_with('TARGET')) %>% names

# Features
model <- df %>%
  select(!starts_with('TARGET_'), TARGET_10C00) %>%
  select(-UT_LVL_10_CD)

# rm(data, input, pipe_IOT, pipe_TREND, radar, resp_date)
# gc()

# save.image("boruta input ut10 jan2022.RData")

# 3.2) chunk up features **** -------------------------------------------------------
# use existing chonks



# 3.3) BORUTA - 10C00 NUM CHUNK 0 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10C00, 
         any_of(chonker %>% 
                  filter(index == 0) %>%
                  pull(FIELD)))

boruta_10C00_chunk0 <- Boruta(TARGET_10C00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_10C00_chunk0, file = 'boruta chunk 0.RData')

# 3.3) BORUTA - 10C00 NUM CHUNK 1 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10C00, 
         any_of(chonker %>% 
                  filter(index == 1) %>%
                  pull(FIELD)))

boruta_10C00_chunk1 <- Boruta(TARGET_10C00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_10C00_chunk1, file = 'boruta chunk 1.RData')

# 3.4) BORUTA - 10C00 NUM CHUNK 2 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10C00, 
         any_of(chonker %>% 
                  filter(index == 2) %>%
                  pull(FIELD)))

boruta_10C00_chunk2 <- Boruta(TARGET_10C00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 2)

save(boruta_10C00_chunk2, file = 'boruta chunk 2.RData')

# 3.5) BORUTA - 10C00 NUM CHUNK 3 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10C00, 
         any_of(chonker %>% 
                  filter(index == 3) %>%
                  pull(FIELD)))

boruta_10C00_chunk3 <- Boruta(TARGET_10C00 ~ ., 
                             data = x_train, 
                             maxRuns = 11,
                             doTrace = 3)

save(boruta_10C00_chunk3, file = 'boruta chunk 3.RData')



# 3.6) 10C00 Combined Table ----------------------------------------------------------

chunk_stats_10C00 <-  bind_rows(attStats(boruta_10C00_chunk0) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_10C00_chunk1) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_10C00_chunk2) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble(),
                               attStats(boruta_10C00_chunk3) %>%
                                 mutate(FIELD = row.names(.)) %>%
                                 as_tibble()) %>% 
  left_join(chonker) %>%
  arrange(-medianImp)

save(chunk_stats_10C00, file = '10C00 chunk boruta stats.RData')


# 4.1) CONSULT FEATURE SELECTION PREP ---------------------------------------------
df %>% select(starts_with('TARGET')) %>% names

# Features
model <- df %>%
  select(!starts_with('TARGET_'), TARGET_10J00) %>%
  select(-UT_LVL_10_CD)

# rm(data, input, pipe_IOT, pipe_TREND, radar, resp_date)
# gc()

# save.image("boruta input ut10 jan2022.RData")

# 3.2) chunk up features **** -------------------------------------------------------
# use existing chonks



# 4.3) BORUTA - 10J00 NUM CHUNK 0 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10J00, 
         any_of(chonker %>% 
                  filter(index == 0) %>%
                  pull(FIELD)))

boruta_10J00_chunk0 <- Boruta(TARGET_10J00 ~ ., 
                              data = x_train, 
                              maxRuns = 11,
                              doTrace = 2)

save(boruta_10J00_chunk0, file = 'boruta chunk 0.RData')

# 4.4) BORUTA - 10J00 NUM CHUNK 1 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10J00, 
         any_of(chonker %>% 
                  filter(index == 1) %>%
                  pull(FIELD)))

boruta_10J00_chunk1 <- Boruta(TARGET_10J00 ~ ., 
                              data = x_train, 
                              maxRuns = 11,
                              doTrace = 2)

save(boruta_10J00_chunk1, file = 'boruta chunk 1.RData')

# 4.5) BORUTA - 10J00 NUM CHUNK 2 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10J00, 
         any_of(chonker %>% 
                  filter(index == 2) %>%
                  pull(FIELD)))

boruta_10J00_chunk2 <- Boruta(TARGET_10J00 ~ ., 
                              data = x_train, 
                              maxRuns = 11,
                              doTrace = 2)

save(boruta_10J00_chunk2, file = 'boruta chunk 2.RData')

# 4.6) BORUTA - 10J00 NUM CHUNK 3 **** -----------------------------------------------
x_train <- model %>%
  select(TARGET_10J00, 
         any_of(chonker %>% 
                  filter(index == 3) %>%
                  pull(FIELD)))

boruta_10J00_chunk3 <- Boruta(TARGET_10J00 ~ ., 
                              data = x_train, 
                              maxRuns = 11,
                              doTrace = 3)

save(boruta_10J00_chunk3, file = 'boruta chunk 3.RData')



# 4.7) 10J00 Combined Table ----------------------------------------------------------

chunk_stats_10J00 <-  bind_rows(attStats(boruta_10J00_chunk0) %>%
                                  mutate(FIELD = row.names(.)) %>%
                                  as_tibble(),
                                attStats(boruta_10J00_chunk1) %>%
                                  mutate(FIELD = row.names(.)) %>%
                                  as_tibble(),
                                attStats(boruta_10J00_chunk2) %>%
                                  mutate(FIELD = row.names(.)) %>%
                                  as_tibble(),
                                attStats(boruta_10J00_chunk3) %>%
                                  mutate(FIELD = row.names(.)) %>%
                                  as_tibble()) %>% 
  left_join(chonker) %>%
  arrange(-medianImp)

save(chunk_stats_10J00, file = '10J00 chunk boruta stats.RData')


# 5.0) Combine & Final selection ------------------------------------------
combo <- bind_rows(chunk_stats_CNDP %>%
                     filter(row_number() <= 150) %>%
                     select(FIELD, type) %>%
                     mutate(TARGET = '10A00',
                            INDEX = row_number()),
                   chunk_stats_10C00 %>%
                     filter(row_number() <= 150) %>%
                     select(FIELD, type) %>%
                     mutate(TARGET = '10C00',
                            INDEX = row_number()),
                   chunk_stats_10J00 %>%
                     filter(row_number() <= 150) %>%
                     select(FIELD, type) %>%
                     mutate(TARGET = '10J00',
                            INDEX = row_number())) %>%
  distinct(FIELD, .keep_all = T) 



