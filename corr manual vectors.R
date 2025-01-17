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