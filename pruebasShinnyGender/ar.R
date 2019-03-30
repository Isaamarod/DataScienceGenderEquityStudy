library(tidyverse)
library("Matrix")
library(data.table)
library(fastDummies)
library(plyr)
library(arules)
library(arulesViz)

remove(list = ls())

women <- read.csv("C:/Users/quivi/OneDrive/Escritorio/DataScienceGenderEquityStudy-master/pruebasShinnyGender/wbldata.csv")
datos_na <- women[is.na(women$global_index),]
datos_no_na <- setdiff(women, datos_na)


year_2018 <- datos_no_na %>% filter(year %in% c(2018))
year_2009 <- datos_no_na %>% filter(year %in% c(2009))


year_2018 <- year_2018 %>% filter(!(country_code=="SSD"))
year_2018$improvement<- year_2018$global_index - year_2009$global_index
year_2018$global_index_2009 <- year_2009$global_index

involutionaring_countries <- women %>% filter(country_code %in% c("BHR","UZB","SVN"))
highest_evolution_countries <-year_2018 %>% select(one_of("country_code","country","improvement")) %>% arrange(improvement,desc(country)) %>% top_n(5)
datos_highest_evolution_countries <- women %>% filter(country_code %in% highest_evolution_countries$country_code)


#Association rules involutioning countries

rem_cols <- c("country", "country_code")
data_2018_for_ar <- involutionaring_countries %>% select_if(negate(is.numeric)) %>% select(-one_of(rem_cols))
cols_dummies <- data_2018_for_ar %>% select(region,income_group) %>% dummy_cols() %>% select(-one_of(c("region","income_group")))
data_2018_for_ar <- data_2018_for_ar %>% select(-one_of(c("region","income_group")))
data_2018_for_ar_01 <- copy(data_2018_for_ar)
for (c in names(data_2018_for_ar)){
  data_2018_for_ar_01[c] <- ifelse(data_2018_for_ar[c] == "Yes", 1, 0)
}
data_2018_for_ar <- data_2018_for_ar_01
data_2018_for_ar <- bind_cols(cols_dummies, data_2018_for_ar)
data_2018_for_ar_matrix <- data.matrix(data_2018_for_ar)
trans <-  as(data_2018_for_ar_matrix, "transactions")
gc()
association.rules <- apriori(trans, parameter = list(supp=0.7, conf=0.7, maxlen=11, minlen=5), control = list(memopt = TRUE,load = FALSE))
#appearance = list(default="lhs",rhs=c("domestic_violence_legislation"))
#inspect(association.rules[0:100])
#inspect(head(sort(association.rules, by = c("confidence","support")), 10))
rules <- association.rules[!is.redundant(association.rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]



rules_travelabroadasman_involutioning <- subset(rules, subset= rhs %in% "travel_abroad_as_man")
rules_travelabroadasman_involutioning_head <- head(rules_travelabroadasman_involutioning, n = 5, by = "lift")
rules_travelabroadasman_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_travelabroadasman_involutioning_head) ), 
                                                   rhs = labels( rhs(rules_travelabroadasman_involutioning_head) ), 
                                                   quality(rules_travelabroadasman_involutioning_head) )[ order(-lift), ]


rules_traveloutsidehomeasman_involutioning <- subset(rules, subset= rhs %in% "travel_outside_home_as_man")
rules_traveloutsidehomeasman_involutioning_head <- head(rules_traveloutsidehomeasman_involutioning, n = 5, by = "lift")
rules_travelabroadasman_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_traveloutsidehomeasman_involutioning_head) ), 
                                                             rhs = labels( rhs(rules_traveloutsidehomeasman_involutioning_head) ), 
                                                             quality(rules_traveloutsidehomeasman_involutioning_head) )[ order(-lift), ]


rules_chooseaplacetolive_involutioning <- subset(rules, subset= rhs %in% "choose_place_to_live_as_man")
rules_chooseaplacetolive_involutioning_head <- head(rules_chooseaplacetolive_involutioning, n = 5, by = "lift")
rules_chooseaplacetolive_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_chooseaplacetolive_involutioning_head) ), 
                                                             rhs = labels( rhs(rules_chooseaplacetolive_involutioning_head) ), 
                                                             quality(rules_chooseaplacetolive_involutioning_head) )[ order(-lift), ]


rules_getjob_involutioning <- subset(rules, subset= rhs %in% "get_job_as_man")
rules_getjob_involutioning_head <- head(rules_getjob_involutioning, n = 5, by = "lift")
rules_getjob_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_getjob_involutioning_head) ), 
                                                              rhs = labels( rhs(rules_getjob_involutioning_head) ), 
                                                              quality(rules_getjob_involutioning_head) )[ order(-lift), ]


rules_nloh_involutioning <- subset(rules, subset= rhs %in% "no_law_obedience_to_husband")
rules_nloh_involutioning_head <- head(rules_nloh_involutioning, n = 5, by = "lift")
rules_nloh_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_nloh_involutioning_head) ), 
                                                  rhs = labels( rhs(rules_nloh_involutioning_head) ), 
                                                  quality(rules_nloh_involutioning_head) )[ order(-lift), ]


rules_hhm_involutioning <- subset(rules, subset= rhs %in% "head_of_household_as_man")
rules_hhm_involutioning_head <- head(rules_hhm_involutioning, n = 5, by = "lift")
rules_hhm_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_hhm_involutioning_head) ), 
                                                rhs = labels( rhs(rules_hhm_involutioning_head) ), 
                                                quality(rules_hhm_involutioning_head) )[ order(-lift), ]


rules_fdp_involutioning <- subset(rules, subset= rhs %in% "forbidden_dismiss_pregnant")
rules_fdp_involutioning_head <- head(rules_fdp_involutioning, n = 5, by = "lift")
rules_fdp_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_fdp_involutioning_head) ), 
                                               rhs = labels( rhs(rules_fdp_involutioning_head) ), 
                                               quality(rules_fdp_involutioning_head) )[ order(-lift), ]


rules_sc_involutioning <- subset(rules, subset= rhs %in% "sign_contract_as_man")
rules_sc_involutioning_head <- head(rules_sc_involutioning, n = 5, by = "lift")
rules_sc_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_sc_involutioning_head) ), 
                                               rhs = labels( rhs(rules_sc_involutioning_head) ), 
                                               quality(rules_sc_involutioning_head) )[ order(-lift), ]

rules_rb_involutioning <- subset(rules, subset= rhs %in% "register_business_as_man")
rules_rb_involutioning_head <- head(rules_rb_involutioning, n = 5, by = "lift")
rules_rb_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_rb_involutioning_head) ), 
                                              rhs = labels( rhs(rules_rb_involutioning_head) ), 
                                              quality(rules_rb_involutioning_head) )[ order(-lift), ]

rules_oba_involutioning <- subset(rules, subset= rhs %in% "open_bankaccount_as_man")
rules_oba_involutioning_head <- head(rules_oba_involutioning, n = 5, by = "lift")
rules_oba_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_oba_involutioning_head) ), 
                                              rhs = labels( rhs(rules_oba_involutioning_head) ), 
                                              quality(rules_oba_involutioning_head) )[ order(-lift), ]

rules_mcseo_involutioning <- subset(rules, subset= rhs %in% "married_couple_same_equal_ownership")
rules_mcseo_involutioning_head <- head(rules_mcseo_involutioning, n = 5, by = "lift")
rules_mcseo_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_mcseo_involutioning_head) ), 
                                               rhs = labels( rhs(rules_mcseo_involutioning_head) ), 
                                               quality(rules_mcseo_involutioning_head) )[ order(-lift), ]


rules_mceaa_involutioning <- subset(rules, subset= rhs %in% "married_couple_equal_administrative_authority")
rules_mceaa_involutioning_head <- head(rules_mceaa_involutioning, n = 5, by = "lift")
rules_mceaa_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_mceaa_involutioning_head) ), 
                                                 rhs = labels( rhs(rules_mceaa_involutioning_head) ), 
                                                 quality(rules_mceaa_involutioning_head) )[ order(-lift), ]


rules_pprsa_involutioning <- subset(rules, subset= rhs %in% "partialpension_retire_same_ages_as_man")
rules_pprsa_involutioning_head <- head(rules_pprsa_involutioning, n = 5, by = "lift")
rules_pprsa_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_pprsa_involutioning_head) ), 
                                                 rhs = labels( rhs(rules_pprsa_involutioning_head) ), 
                                                 quality(rules_pprsa_involutioning_head) )[ order(-lift), ]


rules_mrae_involutioning <- subset(rules, subset= rhs %in% "mandatory_retirement_age_equal")
rules_mrae_involutioning_head <- head(rules_mrae_involutioning, n = 5, by = "lift")
rules_mrae_involutioning_head_dt <- data.table( lhs = labels( lhs(rules_mrae_involutioning_head) ), 
                                                 rhs = labels( rhs(rules_mrae_involutioning_head) ), 
                                                 quality(rules_mrae_involutioning_head) )[ order(-lift), ]

#Association rules best evolution countries
rem_cols <- c("country", "country_code")
data_2018_for_ar <- datos_highest_evolution_countries %>% select_if(negate(is.numeric)) %>% select(-one_of(rem_cols))
cols_dummies <- data_2018_for_ar %>% select(region,income_group) %>% dummy_cols() %>% select(-one_of(c("region","income_group")))
data_2018_for_ar <- data_2018_for_ar %>% select(-one_of(c("region","income_group")))
data_2018_for_ar_01 <- copy(data_2018_for_ar)
for (c in names(data_2018_for_ar)){
  data_2018_for_ar_01[c] <- ifelse(data_2018_for_ar[c] == "Yes", 1, 0)
}
data_2018_for_ar <- data_2018_for_ar_01
data_2018_for_ar <- bind_cols(cols_dummies, data_2018_for_ar)
data_2018_for_ar_matrix <- data.matrix(data_2018_for_ar)
trans <-  as(data_2018_for_ar_matrix, "transactions")
gc()
association.rules <- apriori(trans, parameter = list(supp=0.7, conf=0.7, maxlen=11, minlen=5), control = list(memopt = TRUE,load = FALSE))
#appearance = list(default="lhs",rhs=c("domestic_violence_legislation"))
#inspect(association.rules[0:100])
#inspect(head(sort(association.rules, by = c("confidence","support")), 10))
rules <- association.rules[!is.redundant(association.rules)]
rules_dt <- data.table( lhs = labels( lhs(rules) ), 
                        rhs = labels( rhs(rules) ), 
                        quality(rules) )[ order(-lift), ]





rules_travelabroadasman_evol <- subset(rules, subset= rhs %in% "travel_abroad_as_man")
rules_travelabroadasman_evol_head <- head(rules_travelabroadasman_evol, n = 5, by = "lift")
rules_travelabroadasman_evol_head_dt <- data.table( lhs = labels( lhs(rules_travelabroadasman_evol_head) ), 
                                                             rhs = labels( rhs(rules_travelabroadasman_evol_head) ), 
                                                             quality(rules_travelabroadasman_evol_head) )[ order(-lift), ]


rules_traveloutsidehomeasman_evol <- subset(rules, subset= rhs %in% "travel_outside_home_as_man")
rules_traveloutsidehomeasman_evol_head <- head(rules_traveloutsidehomeasman_evol, n = 5, by = "lift")
rules_travelabroadasman_evol_head_dt <- data.table( lhs = labels( lhs(rules_traveloutsidehomeasman_evol_head) ), 
                                                             rhs = labels( rhs(rules_traveloutsidehomeasman_evol_head) ), 
                                                             quality(rules_traveloutsidehomeasman_evol_head) )[ order(-lift), ]


rules_chooseaplacetolive_evol <- subset(rules, subset= rhs %in% "choose_place_to_live_as_man") #0 rules



rules_getjob_evol <- subset(rules, subset= rhs %in% "get_job_as_man") #0 rules



rules_nloh_evol <- subset(rules, subset= rhs %in% "no_law_obedience_to_husband")
rules_nloh_evol_head <- head(rules_nloh_evol, n = 5, by = "lift")
rules_nloh_evol_head_dt <- data.table( lhs = labels( lhs(rules_nloh_evol_head) ), 
                                                rhs = labels( rhs(rules_nloh_evol_head) ), 
                                                quality(rules_nloh_evol_head) )[ order(-lift), ]


rules_hhm_evol <- subset(rules, subset= rhs %in% "head_of_household_as_man") # 0 rules



rules_fdp_evol <- subset(rules, subset= rhs %in% "forbidden_dismiss_pregnant")
rules_fdp_evol_head <- head(rules_fdp_evol, n = 5, by = "lift")
rules_fdp_evol_head_dt <- data.table( lhs = labels( lhs(rules_fdp_evol_head) ), 
                                               rhs = labels( rhs(rules_fdp_evol_head) ), 
                                               quality(rules_fdp_evol_head) )[ order(-lift), ]


rules_sc_evol <- subset(rules, subset= rhs %in% "sign_contract_as_man")
rules_sc_evol_head <- head(rules_sc_evol, n = 5, by = "lift")
rules_sc_evol_head_dt <- data.table( lhs = labels( lhs(rules_sc_evol_head) ), 
                                              rhs = labels( rhs(rules_sc_evol_head) ), 
                                              quality(rules_sc_evol_head) )[ order(-lift), ]

rules_rb_evol <- subset(rules, subset= rhs %in% "register_business_as_man")
rules_rb_evol_head <- head(rules_rb_evol, n = 5, by = "lift")
rules_rb_evol_head_dt <- data.table( lhs = labels( lhs(rules_rb_evol_head) ), 
                                              rhs = labels( rhs(rules_rb_evol_head) ), 
                                              quality(rules_rb_evol_head) )[ order(-lift), ]

rules_oba_evol <- subset(rules, subset= rhs %in% "open_bankaccount_as_man")
rules_oba_evol_head <- head(rules_oba_evol, n = 5, by = "lift")
rules_oba_evol_head_dt <- data.table( lhs = labels( lhs(rules_oba_evol_head) ), 
                                               rhs = labels( rhs(rules_oba_evol_head) ), 
                                               quality(rules_oba_evol_head) )[ order(-lift), ]

rules_mcseo_evol <- subset(rules, subset= rhs %in% "married_couple_same_equal_ownership")
rules_mcseo_evol_head <- head(rules_mcseo_evol, n = 5, by = "lift")
rules_mcseo_evol_head_dt <- data.table( lhs = labels( lhs(rules_mcseo_evol_head) ), 
                                                 rhs = labels( rhs(rules_mcseo_evol_head) ), 
                                                 quality(rules_mcseo_evol_head) )[ order(-lift), ]


rules_mceaa_evol <- subset(rules, subset= rhs %in% "married_couple_equal_administrative_authority")
rules_mceaa_evol_head <- head(rules_mceaa_evol, n = 5, by = "lift")
rules_mceaa_evol_head_dt <- data.table( lhs = labels( lhs(rules_mceaa_evol_head) ), 
                                                 rhs = labels( rhs(rules_mceaa_evol_head) ), 
                                                 quality(rules_mceaa_evol_head) )[ order(-lift), ]


rules_pprsa_evol <- subset(rules, subset= rhs %in% "partialpension_retire_same_ages_as_man")
rules_pprsa_evol_head <- head(rules_pprsa_evol, n = 5, by = "lift")
rules_pprsa_evol_head_dt <- data.table( lhs = labels( lhs(rules_pprsa_evol_head) ), 
                                                 rhs = labels( rhs(rules_pprsa_evol_head) ), 
                                                 quality(rules_pprsa_evol_head) )[ order(-lift), ]


rules_mrae_evol <- subset(rules, subset= rhs %in% "mandatory_retirement_age_equal")
rules_mrae_evol_head <- head(rules_mrae_evol, n = 5, by = "lift")
rules_mrae_evol_head_dt <- data.table( lhs = labels( lhs(rules_mrae_evol_head) ), 
                                                rhs = labels( rhs(rules_mrae_evol_head) ), 
                                                quality(rules_mrae_evol_head) )[ order(-lift), ]





