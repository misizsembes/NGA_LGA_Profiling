#SET WORKING DIRECTORY
setwd("~/Desktop/Nigeria/LGA_Profiling/REVISED_PROCESS/LGA_Profiling")  #DO NOT CHANGE
options(scipen = 999)
#####LOAD DATA#####
data_path <- "Raw_to_Cleaning_Process/Raw_Data/updated_data" #DO NOT CHANGE
file_type <- "CSV"  #  "CSV" OR "EXCEL"
file_name <- "UPDATED_CLEANED_DATA_2019-07-05_nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2_nga_lga_profiling_phase2_hh"
excel_sheet_name <- "nga_lga_profiling_phase2_hh_yn"  # ONLY IF "file_type" IS EXCEL--OTHERWISE IGNORE

#EXPORT FOLDER
output_folder <- "Analysis"

######################BEGIN AGGREGATION: DO NOT TOUCH######################
if(file_type == "CSV"){
  profile <-  read_csv(paste0(data_path,"/",file_name,".csv"))
  profile[1]<-NULL   
} else if(file_type == "EXCEL"){
  profile <- read_excel(paste0(data_path,"/",file_name,".xlsx"),sheet = excel_sheet_name)
}
profile <- remove_special_char_colnames(profile)  #REMOVE SPECIAL CHARACTERS FROM COLUMN HEADERS

#DEFINE AGGREGATION CATEGORIES
geo_aggregation_indicator <- "lga"
grouping_indicator <- "group"
#DEFINE FIRST AND LAST INDICATORS TO AGGREGATE
first_var_to_analyze <- "b_movement_intentions_b_move_plans"
last_var_to_analyze <-  "j_needs_j_needs3_other"

###BEGIN ANALYSIS###
lga_profile_aggregated <- loop_over_dataset(profile,first_var_to_analyze,
                                            last_var_to_analyze,
                                            geo_aggregation_indicator, 
                                            grouping_indicator)
write.csv(lga_profile_aggregated,paste0(output_folder,"/","lga_profile_aggregated.csv"))

###BEGIN DEMOGRAPHICS
agged_demos <- agg_demographics(profile, geo_aggregation_indicator ,grouping_indicator, 
                                "a_demographics_a_hh_members_a_hh_members_maleinfant" , 
                                "a_demographics_a_total_independent", 
                                "a_demographics_a_hh_members_a_hh_members_maleinfant",
                                "a_demographics_a_hh_members_a_hh_members_femaleelder",
                                "a_demographics_a_total_independent" ,
                                "a_demographics_a_total_dependent")
write.csv(agged_demos,paste0(output_folder,"/","agged_demos.csv"))

############STATISTICAL TESTS###############
#####PREPARE DATA FOR STATISTICAL TESTS
stat_test_preps <- indicators_for_stat_tests(profile,first_var_to_analyze,last_var_to_analyze,geo_aggregation_indicator,grouping_indicator)

#SEPARATED CATE & NUMERIC DATASETS
cateez <- select_var_type(stat_test_preps, "categorical", geo_aggregation_indicator,grouping_indicator)
numerik <- select_var_type(stat_test_preps, "numeric",geo_aggregation_indicator,grouping_indicator)

###CHI2 LOOP###
chi2_result <- chi2_tests(cateez,grouping_indicator)
write.csv(chi2_result,paste0(output_folder,"/","chi2_result.csv"))

###T-TEST LOOP###
ttest_results <- ttest_loop(numerik,grouping_indicator)
write.csv(ttest_results,paste0(output_folder,"/","ttest_results.csv"))




