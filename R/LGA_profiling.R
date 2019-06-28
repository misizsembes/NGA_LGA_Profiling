#SET WORKING DIRECTORY
setwd("~/Desktop/Nigeria/LGA_Profiling")
#####LOAD DATA#####
excel_file_name <- "nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2.xlsx"
excel_sheet_name <- "nga_lga_profiling_phase2_hh_yn"

profile <- read_excel(excel_file_name,sheet = excel_sheet_name)

#####LOAD PACKAGES#####
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(randomForest)) install.packages('randomForest')
library(randomForest)

if (!require(weights)) install.packages('weights')
library(weights)

if (!require(agricolae)) install.packages('agricolae')
library(agricolae)

if (!require(pls)) install.packages('pls')
library(pls)

if (!require(gmodels)) install.packages('gmodels')
library(gmodels)

if (!require(splitstackshape)) install.packages('splitstackshape')
library(splitstackshape)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(data.table)) install.packages('data.table')
library(data.table)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(scales)) install.packages('scales')
library(scales)

if (!require(tm)) install.packages('tm')
library(tm)

if (!require(SDMTools)) install.packages('SDMTools')
library(SDMTools)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(tibble)) install.packages('tibble')
library(tibble)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(stringr)) install.packages('stringr')
library(stringr)

if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

if (!require(MASS)) install.packages('MASS')
library(MASS)

if (!require(magrittr)) install.packages('magrittr')
library(magrittr)

if (!require(foreign)) install.packages('foreign')
library(foreign)

if (!require(sandwich)) install.packages('sandwich')
library(sandwich)

if (!require(lmtest)) install.packages('lmtest')
library(lmtest)

if (!require(corrplot)) install.packages('corrplot')
library(corrplot)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!require(xtable)) install.packages('xtable')
library(xtable)

if (!require(Hmisc)) install.packages('Hmisc')
library(Hmisc)

if (!require(car)) install.packages('car')
library(car)

if (!require(readr)) install.packages('readr')
library(readr)

######################FUNCTIONS######################

############Calculate proportions with or without "dont know"
#data == dataframe
#agg_var == Name of the geographic aggregation unit(e.g., lga)
#indicator_index == Column INDEX of the first column to be aggregated
#dont_denom = TRUE if "dont know" is included in the calculations/denominator; FALSE if not
#dontknow_format == IN QUOTATIONS: How "dont know" is spelled in the data
make_proportions <- function(data,agg_var,indicator_index, dont_denom, dontknow_format){
  var_name <- colnames(data)[indicator_index]
  locationz <- as.vector(unique(data[grep(paste0("^",agg_var,"$"),colnames(data))]))
  #locationz <- sort(locationz, decreasing=TRUE)
  data<- add_column(data, onesz = 1)
  dontknow_format <- paste0("'",dontknow_format,"'")
  if(dont_denom == FALSE){
    data  <- data %>%
      dplyr ::filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))])) 
    data  <- dplyr:: filter_(data, paste(var_name,"!=", dontknow_format,sep=" "))
  } else {
    data  <- data %>%
      dplyr ::  filter(!is.na(data[grep(paste0("^",var_name,"$"), colnames(data))]))
  }
  agg_var_indicator <- as.formula(paste0(agg_var,"~",var_name))  
  result <- data %>% dcast(agg_var_indicator, fun.aggregate = sum,value.var="onesz", na.rm=TRUE)
  namez <- paste0(names(result)[2:ncol(result)],"_",var_name)
  names(result)[2:ncol(result)] <- namez
  result<- add_column(result, total_respondents = rowSums(result[2:ncol(result)]))
  denom_column <- ncol(result)
  props <- list()
  for(i in 2:(ncol(result)-1)){
    props[[i]]<-  result[i]/result[denom_column]
  }
  props[sapply(props, is.null)] <- NULL
  props <- as.data.frame(props)
  names(props) <- paste0("pr_",names(props))
  result <- data.frame(result, props )
  result<-merge(locationz, result, by=agg_var , all.x=TRUE)
  return(result)
}
#aa<- make_proportions(nonnumeric,lga, 3,TRUE)


############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5; -5 produces bottom 5)
rank_money2 <- function(df, aggunit, toprank) {
  callag <- melt(df, id.vars = c(aggunit))
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag))
  unique_units <- unique(callag[id_index])
  unique_units<-as.data.frame(unique_units)
  if(toprank >= 1){
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  toprank <- abs(toprank)
  for (i in 1:nrow(unique_units)){
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ])
  sorted_dataframes_list <- lapply(snowflakes, function(df){
    df[order(df$value,decreasing = direction),]
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank)
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))
  })
  for (k in 1: nrow(unique_units)){
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed)
  castedd <- lapply(castedd, setNames, titles)
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))]
  locations <- unique(locations)
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank])
  }
  return(ordername)
}
#resulttorank <- subset(result, select=c(lga_group,pr_TRUE_b_movement_intentions_b_push_firstreason_push_security,pr_TRUE_b_movement_intentions_b_pull_firstreason_pull_education,pr_TRUE_b_movement_intentions_b_push_firstreason_push_food,pr_TRUE_b_movement_intentions_b_push_firstreason_push_wash,pr_TRUE_b_movement_intentions_b_push_firstreason_push_shelter,pr_TRUE_b_movement_intentions_b_push_firstreason_push_land,pr_TRUE_b_movement_intentions_b_push_firstreason_push_employment_cash))
#aaa <- rank_money2(resulttorank, "lga_group", 3, "highest")


######################BEGIN AGGREGATION######################
##REMOVE SPECIAL CHARACTERS AND "consent_yes" FROM HEADERS
names(profile) <- names(profile) %<>%
  gsub("/", "_", .) %>%
  gsub("-", "_", .)%>%
  gsub("/", "_", .) %>%
  gsub("'", "", .) %>%
  gsub("consent_ok_hoh_ok_", "", .) 
profile<-as.data.frame(profile)
profile[449:456] <- lapply(profile[449:456], gsub, pattern = "98", replacement = NA)
profile[449:456] <- lapply(profile[449:456], gsub, pattern = "99", replacement = NA)
#CREATE COMBINED LGA AND GROUP VARIABLE TO AGGREGATE BY
profile$lga_group <- paste0(profile$lga,".",profile$group)
#GEOGRAPHIC AGGREGATION LEVEL
agg_varz <- "lga"  
#CHANGE "1" TO "YES" & "2" TO "NO"
change_yes_no <- profile %>% 
  dplyr:: select(lga,a_demographics_a_total_hh_confirm,
 a_demographics_a_vulnerability_hh_a_vulnerability_hh_plw,
a_demographics_a_vulnerability_hh_a_vulnerability_hh_minor,
a_demographics_a_vulnerability_hh_a_vulnerability_hh_chronic,
a_demographics_a_vulnerability_hh_a_vulnerability_hh_disabled,
c_wash_c_water_enough,
d_health_d_sick_2weeks,
 d_health_d_birth_lastyear,
e_foodsec_e_market_access,
g_shelter_g_rent_contract_yn,
g_shelter_g_dmg_status,
i_protection_i_security_yn,
i_protection_i_security_uxo,
j_assistance_j_aid_receive,
 j_assistance_j_aid_satis_yndk,
 j_assistance_j_aid_satis_workers,
j_assistance_j_access_radio,
j_assistance_j_aid_asked) 

#SELECT ALL NON-NUMERIC INDICATORS TO BE AGGREGATED
nonnumeric <- profile %>%
  dplyr:: select(lga, lga_group,status_group,
                 a_demographics_a_sex_resp,
                 a_demographics_a_sex_hoh,
                 a_demographics_a_marital_hoh,
                 a_demographics_a_language_hh_bura,
                 a_demographics_a_language_hh_english,
                 a_demographics_a_language_hh_fulfulde,
                 a_demographics_a_language_hh_glavda,
                 a_demographics_a_language_hh_guduf,
                 a_demographics_a_language_hh_gwoza,
                 a_demographics_a_language_hh_hausa,
                 a_demographics_a_language_hh_higi,
                 a_demographics_a_language_hh_jukun,
                 a_demographics_a_language_hh_kanuri,
                 a_demographics_a_language_hh_karekare,
                 a_demographics_a_language_hh_kibaku,
                 a_demographics_a_language_hh_marghi,
                 a_demographics_a_language_hh_shuwa_arabic,
                 a_demographics_a_language_hh_tiv,
                 a_demographics_a_language_hh_waha,
                 a_demographics_a_language_hh_zalidiva,
                 a_demographics_a_language_hh_97,
                 a_demographics_a_vulnerability_hh_a_vulnerability_hh_plw,
                 a_demographics_a_vulnerability_hh_a_vulnerability_hh_minor,
                 a_demographics_a_vulnerability_hh_a_vulnerability_hh_chronic,
                 a_demographics_a_vulnerability_hh_a_vulnerability_hh_disabled,
                 b_movement_intentions_b_move_plans,
                    b_movement_intentions_b_move_plans_when,
                  b_movement_intentions_b_push_firstreason_push_security,
                 b_movement_intentions_b_push_firstreason_push_health,
                 b_movement_intentions_b_push_firstreason_push_education,
                 b_movement_intentions_b_push_firstreason_push_food,
                 b_movement_intentions_b_push_firstreason_push_wash,
                 b_movement_intentions_b_push_firstreason_push_shelter,
                 b_movement_intentions_b_push_firstreason_push_land,
                 b_movement_intentions_b_push_firstreason_push_employment_cash,
                 b_movement_intentions_b_push_firstreason_push_family,
                 b_movement_intentions_b_push_firstreason_push_gov_recommended,
                 b_movement_intentions_b_push_firstreason_push_ngo_recommended,
                 b_movement_intentions_b_push_firstreason_push_forced_leave,
                 b_movement_intentions_b_push_firstreason_push_lack_means,
                 b_movement_intentions_b_push_firstreason_97,
             b_movement_intentions_b_pull_firstreason_pull_security,
             b_movement_intentions_b_pull_firstreason_pull_health,
             b_movement_intentions_b_pull_firstreason_pull_education,
             b_movement_intentions_b_pull_firstreason_pull_food,
             b_movement_intentions_b_pull_firstreason_pull_wash,
             b_movement_intentions_b_pull_firstreason_pull_shelter,
             b_movement_intentions_b_pull_firstreason_pull_land,
             b_movement_intentions_b_pull_firstreason_pull_employment_cash,
             b_movement_intentions_b_pull_firstreason_pull_family,
             b_movement_intentions_b_pull_firstreason_pull_gov_recommended,
             b_movement_intentions_b_pull_firstreason_pull_ngo_recommended,
             b_movement_intentions_b_pull_firstreason_pull_forced_gov,
             b_movement_intentions_b_pull_firstreason_pull_lack_means,
             b_movement_intentions_b_pull_firstreason_pull_here_temporarily,
             b_movement_intentions_b_pull_firstreason_pull_nowhere_go,
             b_movement_intentions_b_pull_firstreason_pull_fatherland,
             b_movement_intentions_b_pull_firstreason_97,
             c_wash_c_water_enough,
             c_wash_c_water_source_main_borehole,
             c_wash_c_water_source_main_public_tap,
             c_wash_c_water_source_main_piped_dwelling,
             c_wash_c_water_source_main_handpump,
             c_wash_c_water_source_main_protected_well,
             c_wash_c_water_source_main_protected_spring,
             c_wash_c_water_source_main_water_truck,
             c_wash_c_water_source_main_sachet_water,
             c_wash_c_water_source_main_surface_water,
             c_wash_c_water_source_main_unprotected_well,
             c_wash_c_water_source_main_unprotected_spring,
             c_wash_c_water_source_main_unprotected_tank,
             c_wash_c_water_source_main_mai_moya,
             c_wash_c_water_source_main_97,
             c_wash_c_quality_water,
             c_wash_c_quality_water_type_dirty,
             c_wash_c_quality_water_type_smells,
             c_wash_c_quality_water_type_bad,
             c_wash_c_quality_water_type_97,
             c_wash_c_fetch_prob_main_no_problem,
             c_wash_c_fetch_prob_main_long_distance,
             c_wash_c_fetch_prob_main_long_queue,
             c_wash_c_fetch_prob_main_too_expensive,
             c_wash_c_fetch_prob_main_not_safe_general,
             c_wash_c_fetch_prob_main_not_safe_gbv,
             c_wash_c_fetch_prob_main_not_safe_children,
             c_wash_c_fetch_prob_main_97,
             c_wash_c_total_time,
             c_wash_c_has_soap,
             c_wash_c_has_not_soap,
             c_wash_c_treat_water_yn,
             c_wash_c_treatment_water,
             c_wash_c_access_latrine,
             c_wash_c_type_latrine,
             c_wash_c_latrine_use_asn,
             c_wash_c_who_latrine_use_infant,
             c_wash_c_who_latrine_use_male_child,
             c_wash_c_who_latrine_use_female_child,
             c_wash_c_who_latrine_use_adult_men,
             c_wash_c_who_latrine_use_adult_female,
             c_wash_c_who_latrine_use_elderly,
             c_wash_c_who_latrine_use_chronic_illness,
             c_wash_c_who_latrine_use_all,
             c_wash_c_who_latrine_use_97,
             c_wash_c_latrine_nouse_reason_damaged,
             c_wash_c_latrine_nouse_reason_not_sayfe_children,
             c_wash_c_latrine_nouse_reason_not_safe_night,
             c_wash_c_latrine_nouse_reason_latrine_filled,
             c_wash_c_latrine_nouse_reason_latrine_dirty,
             c_wash_c_latrine_nouse_reason_latrine_not_disabled,
             c_wash_c_latrine_nouse_reason_97,
             c_wash_c_trash_disposal,
             c_wash_c_trash_freq,
             d_health_d_sick_2weeks,
             d_health_d_sick_illness_fever,
             d_health_d_sick_illness_cough,
             d_health_d_sick_illness_diarrhea,
             d_health_d_sick_illness_skin_infection,
             d_health_d_sick_illness_eye_infection,
             d_health_d_sick_illness_dizzy,
             d_health_d_sick_illness_vomiting,
             d_health_d_sick_illness_injury,
             d_health_d_sick_illness_abdominal_pain_NC,
             d_health_d_sick_illness_headache_NC,
             d_health_d_sick_illness_ulcer_NC,
             d_health_d_sick_illness_pregnancy_related_NC,
             d_health_d_sick_illness_other_symptom_NC,
             d_health_d_sick_illness_97,
             d_health_d_distance_hf,
             d_health_d_type_hf,
             d_health_d_main_barrier_health_no_barrier,
             d_health_d_main_barrier_health_facility_occupied,
             d_health_d_main_barrier_health_not_safe_travel,
             d_health_d_main_barrier_health_services_expensive,
             d_health_d_main_barrier_health_medicine_expensive,
             d_health_d_main_barrier_health_no_qualified_staff,
             d_health_d_main_barrier_health_no_medicine_available,
             d_health_d_main_barrier_health_language_barrier,
             d_health_d_main_barrier_health_refused_treatment,
             d_health_d_main_barrier_health_gender_discrimination,
             d_health_d_main_barrier_health_no_treatment_closest,
             d_health_d_main_barrier_health_phc_no_referral,
             d_health_d_main_barrier_health_facility_too_far,
             d_health_d_main_barrier_health_no_transportation,
             d_health_d_main_barrier_health_problems_civildoc,
             d_health_d_main_barrier_health_phc_not_open,
             d_health_d_main_barrier_health_nosupport_family,
             d_health_d_main_barrier_health_long_waitingtime_NC,
             d_health_d_main_barrier_health_didnt_need_to_use_NC,
             d_health_d_main_barrier_health_97,
             d_health_d_birth_lastyear,
             d_health_d_birth_where,
             d_health_d_birth_who,
             d_health_d_birth_with_whom,
             e_foodsec_e_food_source_main_purchased_market,
             e_foodsec_e_food_source_main_market_outside,
             e_foodsec_e_food_source_main_own_cultivation,
             e_foodsec_e_food_source_main_food_assistance,
             e_foodsec_e_food_source_main_assistance_public,
             e_foodsec_e_food_source_main_assistance_community,
             e_foodsec_e_food_source_main_foraged_wild,
             e_foodsec_e_food_source_main_consumed_stocks,
             e_foodsec_e_food_source_main_97,
             e_foodsec_e_market_access,
             e_foodsec_e_food_barrier_main_none,
             e_foodsec_e_food_barrier_main_far_away,
             e_foodsec_e_food_barrier_main_transportation_expensive,
             e_foodsec_e_food_barrier_main_prices_higher,
             e_foodsec_e_food_barrier_main_no_resources,
             e_foodsec_e_food_barrier_main_not_available,
             e_foodsec_e_food_barrier_main_not_distributed,
             e_foodsec_e_food_barrier_main_perceived_innsecurity,
             e_foodsec_e_food_barrier_main_movement_restriction,
             e_foodsec_e_food_barrier_main_no_access_land_owners,
             e_foodsec_e_food_barrier_main_no_access_land_uxos,
             e_foodsec_e_food_barrier_main_no_access_land_insecurity,
             e_foodsec_e_food_barrier_main_quant_distr_not_enough_NC,
             e_foodsec_e_food_barrier_main_97,
             e_foodsec_e_land_access_need,
             e_foodsec_e_land_access,
             e_foodsec_e_land_barrier_main,
             e_foodsec_e_land_barrier_main_no_barrier,
             e_foodsec_e_land_barrier_main_presence_uxo,
             e_foodsec_e_land_barrier_main_land_taken,
             e_foodsec_e_land_barrier_main_land_insecurity,
             e_foodsec_e_land_barrier_main_charges_owner_expensive,
             e_foodsec_e_land_barrier_main_no_need_to_access_nc,
             e_foodsec_e_land_barrier_main_97,
             f_livelihoods_f_main_income_no_income,
             f_livelihoods_f_main_income_agriculture,
             f_livelihoods_f_main_income_livestock,
             f_livelihoods_f_main_income_fishery,
             f_livelihoods_f_main_income_trade,
             f_livelihoods_f_main_income_remittance,
             f_livelihoods_f_main_income_salaried_employment,
             f_livelihoods_f_main_income_small_business,
             f_livelihoods_f_main_income_casual_labour,
             f_livelihoods_f_main_income_natural_resources,
             f_livelihoods_f_main_income_sewing,
             f_livelihoods_f_main_income_transportation,
             f_livelihoods_f_main_income_97,
             f_livelihoods_f_cash_access,
             f_livelihoods_f_main_income_no_income,
             f_livelihoods_f_main_income_agriculture,
             f_livelihoods_f_main_income_livestock,
             f_livelihoods_f_main_income_fishery,
             f_livelihoods_f_main_income_trade,
             f_livelihoods_f_main_income_remittance,
             f_livelihoods_f_main_income_salaried_employment,
             f_livelihoods_f_main_income_small_business,
             f_livelihoods_f_main_income_casual_labour,
             f_livelihoods_f_main_income_natural_resources,
             f_livelihoods_f_main_income_sewing,
             f_livelihoods_f_main_income_transportation,
             f_livelihoods_f_cash_access,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_sell_assets,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_savings,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_nonrepro_animals,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_hhmember_eat_elsewhere,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_purchasefood_credit,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_borrow_money,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_depend_support,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_sell_productive_assets,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_withdraw,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_reduceexpense,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_harvest_immature,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_consume_seeds,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_decreaseexpense_animalcare_fertilizer,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_sell_land_property,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_beg,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_dangerous,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_sell_female,
             f_livelihoods_f_livelihood_coping_f_livelihood_coping_child_marriage,
             g_shelter_g_type_shelter,
             g_shelter_g_occup_status,
             g_shelter_g_rent_contract_yn,
             g_shelter_g_dmg_status,
            g_shelter_g_dmg_severity,
             g_shelter_g_dmg_cause_fire,
             g_shelter_g_dmg_cause_flood,
             g_shelter_g_dmg_cause_storm_wind,
             g_shelter_g_dmg_cause_bullet_conflict,
             g_shelter_g_dmg_cause_termites_NC,
             g_shelter_g_dmg_cause_97,
             g_shelter_g_items_own_none,
             g_shelter_g_items_own_blanket,
             g_shelter_g_items_own_sleepingmat,
             g_shelter_g_items_own_mosquito,
             g_shelter_g_items_own_jerry,
             g_shelter_g_items_own_laundry,
             g_shelter_g_items_own_bathsoap,
             g_shelter_g_items_own_sanitarypad,
             g_shelter_g_items_own_solarlamp,
             g_shelter_g_items_own_mattress,
             g_shelter_g_items_own_kettle,
             g_shelter_g_items_own_10lbasin,
             g_shelter_g_items_own_rope,
             g_shelter_g_items_own_cookingpot,
             g_shelter_g_items_own_stainlesstray,
             g_shelter_g_items_own_stainlesscup,
             g_shelter_g_items_own_servingspoon,
             g_shelter_g_items_own_kitchenknife,
             g_shelter_g_items_own_10lbucket,
             g_shelter_g_items_own_aquatab,
             g_shelter_g_items_own_schoolbag,
             g_shelter_g_items_own_schoolnotebook,
             g_shelter_g_items_own_schooltextbook,
             h_education_h_education_formal,
             h_education_h_education_informal,
             h_education_h_accesscfs,
             h_education_h_education_barrier_no_barrier,
             h_education_h_education_barrier_children_hh,
             h_education_h_education_barrier_children_beg,
             h_education_h_education_barrier_lack_finance,
             h_education_h_education_barrier_no_western,
             h_education_h_education_barrier_not_functional,
             h_education_h_education_barrier_too_far,
             h_education_h_education_barrier_school_full,
             h_education_h_education_barrier_poor_education,
             h_education_h_education_barrier_enough_teachers,
             h_education_h_education_barrier_insecurity_at,
             h_education_h_education_barrier_insecurity_way,
             h_education_h_education_barrier_insecurity_around,
             h_education_h_education_barrier_armed_group_occupy,
             h_education_h_education_barrier_dropped__out,
             h_education_h_education_barrier_child_labour,
             h_education_h_education_barrier_97,
             i_protection_i_security_yn,
             i_protection_i_security_where,
             i_protection_i_security_type_physical_violence,
             i_protection_i_security_type_abduction,
             i_protection_i_security_type_armed_attack,
             i_protection_i_security_type_fire,
             i_protection_i_security_type_destruction_property,
             i_protection_i_security_type_uxo,
             i_protection_i_security_type_gbv,
             i_protection_i_security_type_97,
             i_protection_i_num_havedocs,
             i_protection_i_birth_cert,
             i_protection_i_move_restriction_yn,
             i_protection_i_reason_restriction,
             i_protection_i_security_uxo,
             i_protection_i_security_uxo_where_agricultural_land,
             i_protection_i_security_uxo_where_hospital,
             i_protection_i_security_uxo_where_school,
             i_protection_i_security_uxo_where_other_public,
             i_protection_i_security_uxo_where_residential_area,
             i_protection_i_security_uxo_where_97,
             j_assistance_j_aid_receive,
             j_assistance_j_aid_sources_ingo,
             j_assistance_j_aid_sources_nngo,
             j_assistance_j_aid_sources_government,
             j_assistance_j_aid_sources_community,
             j_assistance_j_aid_sources_97,
             j_assistance_j_aid_types_wash,
             j_assistance_j_aid_types_food,
             j_assistance_j_aid_types_livelihoods,
             j_assistance_j_aid_types_health_nutrition,
             j_assistance_j_aid_types_protection,
             j_assistance_j_aid_types_shelter_nfi,
             j_assistance_j_aid_types_education,
             j_assistance_j_aid_types_97,
             j_assistance_j_aid_modality_received,
             j_assistance_j_aid_modality_future_j_aid_modality_future_food,
             j_assistance_j_aid_modality_future_j_aid_modality_future_water,
             j_assistance_j_aid_modality_future_j_aid_modality_future_health,
             j_assistance_j_aid_modality_future_j_aid_modality_future_education,
             j_assistance_j_aid_modality_future_j_aid_modality_future_livelihoods,
             j_assistance_j_aid_modality_future_j_aid_modality_future_agriculture,
             j_assistance_j_aid_modality_future_j_aid_modality_future_shelter,
             j_assistance_j_aid_modality_future_j_aid_modality_future_nfi,
             j_assistance_j_aid_satis_yndk,
             j_assistance_j_aid_satis_not_quantity,
             j_assistance_j_aid_satis_not_quality,
             j_assistance_j_aid_satis_not_not_timely,
             j_assistance_j_aid_satis_not_not_appropriate,
             j_assistance_j_aid_satis_not_97,
             j_assistance_j_aid_satis_workers,
             j_assistance_j_aid_satis_workers_not_rude,
             j_assistance_j_aid_satis_workers_not_asked_goodsmoney,
             j_assistance_j_aid_satis_workers_not_serious_abuse,
             j_assistance_j_aid_satis_workers_not_97,
             j_assistance_j_access_comms_mobile_phone,
             j_assistance_j_access_comms_internet,
             j_assistance_j_access_comms_tv,
             j_assistance_j_access_comms_functioning_radio,
             j_assistance_j_access_comms_none,
             j_assistance_j_access_phone_network_which_none,
             j_assistance_j_access_phone_network_which_mtn,
             j_assistance_j_access_phone_network_which_airtel,
             j_assistance_j_access_phone_network_which_glo,
             j_assistance_j_access_phone_network_which_etisalat,
             j_assistance_j_access_phone_network_which_97,
             j_assistance_j_access_internet,
             j_assistance_j_access_internet_which_none,
             j_assistance_j_access_internet_which_mtn,
             j_assistance_j_access_internet_which_airtel,
             j_assistance_j_access_internet_which_glo,
             j_assistance_j_access_internet_which_etisalat,
             j_assistance_j_access_internet_which_97,
             j_assistance_j_access_radio,
             j_assistance_j_access_radio_which_abc,
             j_assistance_j_access_radio_which_bbc,
             j_assistance_j_access_radio_which_radio_borno,
             j_assistance_j_access_radio_which_dandal,
             j_assistance_j_access_radio_which_dw,
             j_assistance_j_access_radio_which_fombina,
             j_assistance_j_access_radio_which_peacefm,
             j_assistance_j_access_radio_which_pulaaku,
             j_assistance_j_access_radio_which_rfi,
             j_assistance_j_access_radio_which_gotel,
             j_assistance_j_access_radio_which_unimaid,
             j_assistance_j_access_radio_which_voice_america,
             j_assistance_j_access_radio_which_voice_nigeria,
             j_assistance_j_access_radio_which_97,
             j_assistance_j_access_radio_time,
             j_assistance_j_aid_info_receiving_comm_leader,
             j_assistance_j_aid_info_receiving_religious_leader,
             j_assistance_j_aid_info_receiving_gov_officials,
             j_assistance_j_aid_info_receiving_local_officials,
             j_assistance_j_aid_info_receiving_cjtf,
             j_assistance_j_aid_info_receiving_military,
             j_assistance_j_aid_info_receiving_relatives,
             j_assistance_j_aid_info_receiving_aid_un,
             j_assistance_j_aid_info_receiving_aid_nngo,
             j_assistance_j_aid_info_receiving_aid_ingo,
             j_assistance_j_aid_info_receiving_camp_leader_chairman_NC,
             j_assistance_j_aid_info_receiving_not_received_info_NC,
             j_assistance_j_aid_info_receiving_97,
             j_assistance_j_aid_info_types_no_need,
             j_assistance_j_aid_info_types_news_home,
             j_assistance_j_aid_info_types_news_current,
             j_assistance_j_aid_info_types_missing_people,
             j_assistance_j_aid_info_types_security_situation,
             j_assistance_j_aid_info_types_register_aid,
             j_assistance_j_aid_info_types_access_health,
             j_assistance_j_aid_info_types_access_educ,
             j_assistance_j_aid_info_types_prices_food,
             j_assistance_j_aid_info_types_help_survivor,
             j_assistance_j_aid_info_types_prevent_attack,
             j_assistance_j_aid_info_types_replace_id,
             j_assistance_j_aid_info_types_find_work,
             j_assistance_j_aid_info_types_get_transport,
             j_assistance_j_aid_info_types_return_aoo,
             j_assistance_j_aid_info_types_info_relocation,
             j_assistance_j_aid_info_types_info_aid_agencies,
             j_assistance_j_aid_info_types_complain_aid,
             j_assistance_j_aid_info_types_complain_workers,
             j_assistance_j_aid_info_types_aidworker_behaviour,
             j_assistance_j_aid_info_types_97,
             j_assistance_j_aid_prefwho_comm_leader,
             j_assistance_j_aid_prefwho_religious_leader,
             j_assistance_j_aid_prefwho_gov_officials,
             j_assistance_j_aid_prefwho_local_officials,
             j_assistance_j_aid_prefwho_cjtf,
             j_assistance_j_aid_prefwho_military,
             j_assistance_j_aid_prefwho_relatives,
             j_assistance_j_aid_prefwho_aid_un,
             j_assistance_j_aid_prefwho_aid_nngo,
             j_assistance_j_aid_prefwho_aid_ingo,
             j_assistance_j_aid_prefwho_camp_leader_NC,
             j_assistance_j_aid_prefwho_97,
             j_assistance_j_aid_prefmode_call_phone,
             j_assistance_j_aid_prefmode_text_phone,
             j_assistance_j_aid_prefmode_radio,
             j_assistance_j_aid_prefmode_face_face,
             j_assistance_j_aid_prefmode_facebook_,
             j_assistance_j_aid_prefmode_twitter,
             j_assistance_j_aid_prefmode_whatsapp,
             j_assistance_j_aid_prefmode_posters,
             j_assistance_j_aid_prefmode_info_desk,
             j_assistance_j_aid_prefmode_comm_event,
             j_assistance_j_aid_prefmode_97,
             j_assistance_j_aid_feedbacks_no_feedback,
             j_assistance_j_aid_feedbacks_face_face_home,
             j_assistance_j_aid_feedbacks_face_face_office,
             j_assistance_j_aid_feedbacks_face_face_community,
             j_assistance_j_aid_feedbacks_phone_call,
             j_assistance_j_aid_feedbacks_text_message,
             j_assistance_j_aid_feedbacks_complaints_box,
             j_assistance_j_aid_feedbacks_97,
             j_assistance_j_aid_asked,
             j_needs_j_need1,
             j_needs_j_need2,
             j_needs_j_need3)
write.csv(nonnumeric,"head_look.csv")
#PASTE NAME OF FIRST INDICATOR TO BE AGGREGATED IN THE DATAFRAME (FOR THE "make_proportions" FUNCTION)
agg_var_start <- grep("status_group",colnames(nonnumeric))
#AGGREGATE ALL NON-NUMERIC INDICATORS
result<-list()
for(i in agg_var_start:length(nonnumeric)){
  result[[i]] <- make_proportions(nonnumeric,agg_varz, i,TRUE,"dont_know")
}
result[sapply(result, is.null)] <- NULL
result<- as.data.frame(result)

#SEPARATE THE COMBINED LGA AND GROUP COLUMN
#two_groups <- data.frame(do.call('rbind', strsplit(as.character(result$lga_group),'.',fixed=TRUE)))
#names(two_groups)<- c("lga","group")
#ADD GROUP COLUMN TO RESULTS
#result<- data.frame(two_groups, result)

#RENAME FIRST COLUMN
colnames(result)[1] <- "grouping"
group_idx <- result[1]
#REMOVE UNWANTED COLUMNS--"lga_group" and NEGATIVE RESPONSE COLUMNS
result_cnt <- dplyr:: select(result, -contains('lga')) %>%
  dplyr:: select(-contains('FALSE'))%>%
  dplyr:: select(contains('pr_'))
result_cnt <- data.frame(group_idx,result_cnt)

#####################NUMERIC INDICATORS######################
#NUMERIC INDICATORS
#DEFINE AGES TO AVERAGE
numberz <- profile[,c(5,8,13,17)]
idx<-numberz[1] 
numberz[numberz=="n/a"]<-0
#CHANGE TO NUMERIC
numberz<- sapply( numberz[3:ncol(numberz)], as.numeric )
numberz<- as.data.frame(numberz)
#AGES TO AVERAGE
numberz<-data.frame(idx,numberz)
#SELECT ALL NUMERIC INDCIATORS AND CALCULATE MEAN
agg_varz <- "lga" #c( "lga","group")
avg_numeric <- numberz %>% 
  dplyr:: select_all() %>%
  group_by( lga) %>%
  #group_by_(.dots = agg_varz) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  summarise_all(funs(mean), na.rm = TRUE)
#NOTE THEY ARE AVERAGES
names(avg_numeric)[2:ncol(avg_numeric)] <- paste0("avg_",names(avg_numeric)[2:ncol(avg_numeric)])
#COMBINE WITH EXISTING AGGREGATIONS
#avg_numeric$lga_group <- paste0(avg_numeric$lga,avg_numeric$group)
result <- data.frame(result_cnt, avg_numeric[2:ncol(avg_numeric)])
#EXPORT HEADERS
#result <- result[!grepl("lga.", colnames(result),fixed = TRUE)]
#head1 <- write.csv(result,"head1.csv")

#USE NEW HEADERS TO ORGANIZE COLUMNS
#head1 <- read_excel("nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2.xlsx",sheet = "head1")
#head1<-names(head1)
#result<-result[, head1]

#####SUMMARIZE DEMOGRAPHICS
#SUBSET DEMOGRAPHIC QUESTIONS
numberz <- profile %>%
  dplyr::select(lga, 
                a_demographics_a_hh_members_a_hh_members_maleinfant,
                a_demographics_a_hh_members_a_hh_members_femaleinfant,
                a_demographics_a_hh_members_a_hh_members_malechild,
                a_demographics_a_hh_members_a_hh_members_femalechild,
                a_demographics_a_hh_members_a_hh_members_maleyouth,
                a_demographics_a_hh_members_a_hh_members_femaleyouth,
                a_demographics_a_hh_members_a_hh_members_maleadult,
                a_demographics_a_hh_members_a_hh_members_femaleadult,
                a_demographics_a_hh_members_a_hh_members_maleelder,
                a_demographics_a_hh_members_a_hh_members_femaleelder,
                a_demographics_a_total_children,
                a_demographics_a_total_male,
                a_demographics_a_total_female,
                a_demographics_a_total_elderly,
                a_demographics_a_total_dependent,
                a_demographics_a_total_independent)
idx<-numberz[1] 
numberz[numberz=="n/a"]<-0
#CHANGE TO NUMERIC
numberz<- sapply( numberz[2:ncol(numberz)], as.numeric )
numberz<- as.data.frame(numberz)
#DEMOGRAPHIC QUESTIONS
numberz<-data.frame(idx,numberz)
#SELECT ALL NUMERIC INDCIATORS AND CALCULATE MEAN
agg_varz <- c( "lga","group")
sum_numeric <- numberz %>% 
  dplyr:: select_all() %>%
 group_by(lga) %>%
  #group_by_(.dots = agg_varz) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  summarise_all(funs(sum), na.rm = TRUE)
#NOTE SUMS
names(sum_numeric)[2:ncol(sum_numeric)] <- paste0("sum_",names(sum_numeric)[2:ncol(sum_numeric)])
sum_numeric$dependency_ratio<-sum_numeric$sum_a_demographics_a_total_dependent/sum_numeric$sum_a_demographics_a_total_independent
#TOTAL PEOPLE
sum_numeric$total_people <- rowSums(sum_numeric[2:grep("sum_a_demographics_a_hh_members_a_hh_members_femaleelder",colnames(sum_numeric))])
#CREATE DEMOGRAPHIC PROPORTION COMPOSITION
last_to_be_proportion <- grep("sum_a_demographics_a_total_independent",colnames(sum_numeric))
for(i in 2: grep("sum_a_demographics_a_total_independent",colnames(sum_numeric))){
  sum_numeric[,(i+last_to_be_proportion+1)] <- sum_numeric[i]/sum_numeric[grep("^total_people$",colnames(sum_numeric))]
}
#RENAME DEMOGRAPHIC PROPORTION INDICATORS
names(sum_numeric)[(grep("^total_people$",colnames(sum_numeric))+1):ncol(sum_numeric)]<- c("pr_maleinfant",	"pr_femaleinfant",	"pr_malechild",	"pr_femalechild",	"pr_maleyouth",	"pr_femaleyouth",	"pr_maleadult",	"pr_femaleadult",	"pr_maleelder",	"pr_femaleelder","pr_children",	"pr_male",	"pr_female",	"pr_elderly",	"pr_dependent",	"pr_independent")
colnames(sum_numeric)[1]<- "grouping"
sum_numeric <- sum_numeric %>% dplyr::select(matches('pr_|grouping'))
#COMBINE RESULTS
result <- merge(result, sum_numeric, by="grouping", all.x=TRUE)

#EXPORT HEADERS
#head2 <- write.csv(result,"head2.csv")

#head2 <- read_excel("nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2.xlsx",sheet = "head2")
#head2<-names(head2)
#result<-result[, head2]


######OTHER AVERAGES######
#REMOVE "n/a" and average
other_numeric <- profile %>%
  dplyr:: select(lga,
                 g_shelter_g_contract_months,
                 g_shelter_g_rent_cost,
                 g_shelter_g_num_families_share)
other_numeric <- as.data.frame(other_numeric)
other_numeric[other_numeric=="n/a"]<-NA
other_numeric[,2:4] <- as.numeric(unlist(other_numeric[,2:4]))
agg_varz <- "lga_group"
other_numeric<- other_numeric %>%
  dplyr:: select_all() %>%
  #group_by_(.dots = agg_varz) %>% ## .dots basically accepts the column values for the function. In this function, we are saying group by the three columns listed!
  group_by(lga) %>%
  summarise_all(funs(mean), na.rm = TRUE)
names(other_numeric)[2:ncol(other_numeric)] <- paste0("avg_",names(other_numeric)[2:ncol(other_numeric)])
other_numeric[sapply(other_numeric, is.nan)] <- NA
colnames(other_numeric)[1] <- "grouping"
result <- merge(result, other_numeric, by= "grouping", all.x=TRUE)

#EXPORT HEADERS
#head3 <- write.csv(result,"head3.csv")

#head3 <- read_excel("nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2.xlsx",sheet = "head3")
#head3<-names(head3)
#result<-result[, head3]

#result <- result[!grepl("lga_group.", colnames(result),fixed = TRUE)]
result <- result[ !grepl("X98", colnames(result))]
result <- result[ !grepl("X99", colnames(result))]
#REMOVE UNWANTED COLUMNS--"lga_group" and NEGATIVE RESPONSE COLUMNS
#result <- dplyr:: select(result, -contains('FALSE')) 
#EXPORT TO CSV
write.csv(result, "LGA_profile_just_LGA_percents.csv")






#######CREATE DATA MERGE FILE##########
#SUBSET FORMAL
formal <- subset(result, group == "formal_site")
names(formal) <- paste0("formal_",names(formal))
#SUBSET HOST
host <- subset(result, group == "host_community" )
names(host) <- paste0("host_",names(host))
#JOIN
data_merge <- data.frame(formal, host)
data_merge[2]<- NULL
names(data_merge)[1] <- c("lga")
#EXPORT
write.csv(data_merge, "LGA_profile_data_merge.csv")


###REMOVE PROBLEMATIC INDICATORS FOR RANKING
result <- result[ !grepl("health_no_barrier", colnames(result))]
result <- result[ !grepl("land_barrier_main_no_barrier", colnames(result))]
result <- result[ !grepl("education_barrier_no_barrier", colnames(result))]
result <- result[ !grepl("food_barrier_main_none", colnames(result))]
result <- result[ !grepl("no_barrier_e_foodsec_e_land_barrier", colnames(result))]
result <- result[ !grepl("_NC", colnames(result))]
result <- result[ !grepl("_nc", colnames(result))]
result <- result[ !grepl("_no_income", colnames(result))]
result <- result[ !grepl("98", colnames(result))]
result <- result[ !grepl("99", colnames(result))]
result <- result[ !grepl("g_items_own_none", colnames(result))]
result$no_need_j_needs_j_need1 <- NULL
result$X98_j_needs_j_need1 <- NULL
result$X98_i_protection_i_security_where <-NULL
result$yes_f_livelihoods_f_livelihood_coping_f_livelihood_coping_withdraw

###RANK INDICATORS
##############################RANKED DELUX###################################
#indicator_to_rank == IN QUOTATIONS: name of the last portion of the indicator to rank
#rank_type == IN QUOTATIONS: "count" or "percent"
#top_howmany == Top N (3 == top 3; -3 == bottom 3)
#ranklabels == Label of these ranked indicators
#geovary == IN QUOTATIONS: column of aggregate units
rank_delux <- function(dataa,indicator_to_rank,rank_type,top_howmany,ranklabels, geovary){
  ###SUBSET RANKED INDICATORS
  #RANKED: PUSH REASONS
  first_sub <- min(grep(indicator_to_rank ,colnames(dataa))) 
  last_sub <- max(grep(indicator_to_rank ,colnames(dataa))) 
  #AGGREGATION UNIT
  geounit <- grep(geovary,colnames(dataa))
  to_rank <- dataa[,c(geounit,first_sub:last_sub)]
  to_rank <- dplyr:: select(to_rank,contains(indicator_to_rank))
   geoidd <- dataa[,c(geounit)]
  to_rank <- data.frame(geoidd, to_rank)
  ######PERCENT OR COUNT#########
  type <- rank_type
  if(type == "count"){
    iddd <- to_rank[1]
    to_rank <- to_rank[ ,!grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  } else if(type=="percent"){
    iddd <- to_rank[1]
    to_rank <- to_rank[ ,grepl("pr_", names( to_rank ))]
    to_rank <- to_rank[ ,!grepl("total_respondents", names( to_rank ))]
  }
   colnames(to_rank)[1] <- geovary
  to_rank[sapply(to_rank, is.na)] <- 0
  #RANK TOP 3 INDICATORS
  to_rank <- rank_money2(to_rank, geovary, top_howmany)
  #LABEL COLUMNS
  names(to_rank)[2:ncol(to_rank)] <- paste0(ranklabels,"_",names(to_rank)[2:ncol(to_rank)])
  return(to_rank)
}

push_ranked <- rank_delux(result,"b_push_firstreason","count",3,"b_push_firstreason",agg_varz)
pull_ranked <- rank_delux(result,"b_pull_firstreason","count",3,"b_pull_firstreason",agg_varz)
sick_illness_ranked <- rank_delux(result,"d_sick_illness","count",3,"d_sick_illness",agg_varz)
barrier_health_ranked <- rank_delux(result,"d_main_barrier_health","count",3,"d_main_barrier_health",agg_varz)
food_source <- rank_delux(result,"e_food_source_main","count",3,"e_food_source_main",agg_varz)
food_barrier <- rank_delux(result,"e_food_barrier_main","count",3,"e_food_barrier_main",agg_varz)
land_barrier <- rank_delux(result,"e_land_barrier_main","count",3,"e_land_barrier_main",agg_varz)
main_income <- rank_delux(result,"f_main_income","count",3,"f_main_income",agg_varz)
aid_sources <- rank_delux(result,"j_aid_sources","count",3,"j_aid_sources",agg_varz)
aid_types <- rank_delux(result,"j_aid_types","count",3,"j_aid_types",agg_varz)
need1 <- rank_delux(result,"j_need1","count",3,"j_need1",agg_varz)
aid_prefmode <- rank_delux(result,"j_aid_prefmode","count",3,"j_aid_prefmode",agg_varz)
dmg_cause <- rank_delux(result,"g_dmg_cause","count",3,"g_dmg_cause",agg_varz)
dmg_severity <- rank_delux(result,"g_shelter_g_dmg_severity","count",3,"g_dmg_severity",agg_varz)
security_where <- rank_delux(result,"i_security_where","count",3,"i_security_where",agg_varz)
security_type <- rank_delux(result,"i_security_type","count",3,"i_security_type",agg_varz)
uxo_where <- rank_delux(result,"i_security_uxo_where","count",3,"i_security_uxo_where",agg_varz)

#TOP 5
exhaust_livelihood_coping <- rank_delux(result,"exhausted_f_livelihoods_f_livelihood_coping","count",5,"exhausted_f_livelihoods_f_livelihood_coping",agg_varz)
yes_livelihood_coping <- rank_delux(result,"yes_f_livelihoods_f_livelihood_coping_f_livelihood","count",5,"yes_f_livelihoods_f_livelihood_coping",agg_varz)
edu_barrier <- rank_delux(result,"h_education_barrier","count",5,"h_education_barrier",agg_varz)

#TOP 1
future_food <- rank_delux(result,"j_aid_modality_future_food","count",1,"j_aid_modality_future_food",agg_varz)
future_water <- rank_delux(result,"j_aid_modality_future_water","count",1,"j_aid_modality_future_water",agg_varz)
future_health <- rank_delux(result,"j_aid_modality_future_health","count",1,"j_aid_modality_future_health",agg_varz)
future_edu <- rank_delux(result,"j_aid_modality_future_education","count",1,"j_aid_modality_future_education",agg_varz)
future_livelihood <- rank_delux(result,"j_aid_modality_future_livelihoods","count",1,"j_aid_modality_future_livelihoods",agg_varz)
future_agriculture <- rank_delux(result,"j_aid_modality_future_agriculture","count",1,"j_aid_modality_future_agriculture",agg_varz)
future_shelter <- rank_delux(result,"j_aid_modality_future_shelter","count",1,"j_aid_modality_future_shelter",agg_varz)
future_nfi <- rank_delux(result,"j_aid_modality_future_nfi","count",1,"j_aid_modality_future_nfi",agg_varz)
aid_not_satis <- rank_delux(result,"j_aid_satis_not","count",1,"j_aid_satis_not",agg_varz)
aidworkers_not_satis <- rank_delux(result,"j_aid_satis_workers_not","count",1,"j_aid_satis_workers_not",agg_varz)

#LOWEST 3
g_shelter_g_items_own <- rank_delux(result,"g_shelter_g_items_own","count",-3,"g_shelter_g_items_own",agg_varz)

#TOGETHER
to_rank <- data.frame(push_ranked,pull_ranked,sick_illness_ranked,barrier_health_ranked,food_source,food_barrier,land_barrier,main_income,aid_sources,aid_types,need1,aid_prefmode,dmg_cause,security_where,security_type,uxo_where,exhaust_livelihood_coping,yes_livelihood_coping,edu_barrier,future_food,future_water,future_health,future_edu,future_livelihood,future_agriculture,future_shelter,future_nfi,aid_not_satis,aidworkers_not_satis,g_shelter_g_items_own)
geoidx <- to_rank[1]
to_rank <- to_rank[!grepl(agg_varz, colnames(to_rank))]
to_rank <- data.frame(geoidx, to_rank)

#CHANGE TO DELIVERABLE NAMES
old_new_names <- read_excel("nga_lga_profiling_phase2_hh_final.xlsx",sheet = "list_selection")
for(i in 1:nrow(old_new_names)){
to_rank <- as.data.frame(lapply(to_rank, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}

write.csv(to_rank,"ranked_indicators.csv")

#COMBINE WITH EXISTING AGGREGATIONS
#result<- data.frame(result,push_reasons)
#result<-result[!duplicated(as.list(result))]






###########GRAPHS############
choose3 <- subset(result, select=c(lga, group,pr_cash_voucher_j_assistance_j_aid_modality_future_j_aid_modality_future_water,pr_average_c_wash_c_quality_water,pr_afternoon_j_assistance_j_access_radio_time, pr_cash_voucher_j_assistance_j_aid_modality_future_j_aid_modality_future_nfi))
choose3$onesz <-1

#GROUPED COMPARISONS--GEOGRAPHIC
choose3 %>%
gather(onesz, value,  starts_with("pr_")) %>%
  ggplot( aes(x = lga, y = (value*100), fill = group)) + 
  coord_cartesian(ylim = c(0, 100)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  geom_text(aes(label = paste0(round(value*100, digits = 2),"%")), position = position_dodge(width = 0.9), 
            vjust = -0.25, color = "black", size = 3) + ggtitle("Indicator Comparison")+
  theme_bw()+
  facet_wrap(~ onesz, scales = "free", ncol = 1, nrow = 4) 
 

#####GROUPED COMPARISONS--ORSI STYLE
###PICK
#SELECT VARIABLES
choose3 <- subset(result, select=c(lga, group,pr_cash_voucher_j_assistance_j_aid_modality_future_j_aid_modality_future_water,pr_average_c_wash_c_quality_water,pr_afternoon_j_assistance_j_access_radio_time, pr_cash_voucher_j_assistance_j_aid_modality_future_j_aid_modality_future_nfi))
#CHOOSE GEOGRAPHY
lga_select <- "Bama"

####START
 choose3$onesz <-1 
 choose3 %>%
  dplyr:: filter(lga == lga_select) %>%
  gather(onesz, value,  starts_with("pr_")) %>%
  ggplot( aes(x = reorder(onesz, -value), y = value, fill = group)) + 
  coord_cartesian(ylim = c(0, 1)) + 
  geom_bar(position = "dodge", stat = "identity", width = 0.9) +
  geom_text(aes(label = paste0(round(value*100, digits = 2),"%")), position = position_dodge(width = 0.9), 
            vjust = -0.25, color = "black", size = 3) + ggtitle("Indicator Comparison")+
  facet_grid(~ onesz, scales = "free") +
theme_bw()+
   theme(axis.text.x = element_blank(),axis.title.x = element_blank())+
    labs(y="Percent of Households")+
   scale_y_continuous(labels = percent) 

 

 
 
 
 
 ##############ORDERED--GEOGRAPHIC
 group_selection <-  "host_community"   #formal_site  host_community
 choose3$onesz <-1
choose3 %>%
   dplyr::  filter(group == group_selection ) %>%  
   gather(onesz, value,  starts_with("pr_")) %>%
   ungroup() %>%
   arrange(onesz, value) %>%
   mutate(order = row_number(onesz))  %>%
 ggplot( aes(x = order, y = (value*100), fill = group)) +
   # geom_col() is replacement for geom_bar(stat = "identity")
   geom_col() +
   # independent x-axis scale in each facet, 
   # drop absent factor levels (not the case here)
   facet_wrap(~ onesz, scales = "free_x", drop = TRUE) +
   # use named character vector to replace x-axis labels
   scale_x_discrete(labels = aaa[, setNames(lga, lga)]) + 
   # replace x-axis title
  geom_text(aes(label = paste0(round(value*100, digits = 2),"%")), position = position_dodge(width = 0.9), 
            vjust = -0.25, color = "black", size = 3) + ggtitle("Indicator Comparison")+
   xlab("id")
 
   