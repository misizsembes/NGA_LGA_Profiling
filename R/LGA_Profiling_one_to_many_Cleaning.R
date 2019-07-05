#THE FOLDER WHERE UNCLEANED/RAW DATA IS STORED
setwd("~/Desktop/Nigeria/LGA_Profiling/REVISED_PROCESS/LGA_Profiling/Raw_to_Cleaning_Process")

#UNLCEANED DATA == "CSV" OR "EXCEL"
uncleaned_data_file_type <- "EXCEL"

#IMPORT CLEANING SHEET
cleaning_name <- "nga_lga_profiling_phase2_household_survey_final_ANONYMISED_v2"  #NAME OF EXCEL/CSV FILE WHERE CLEANING LOG IS STORED
cleaning_log_org_excel_sheet <- "cleaning_log"  #CLEANING LOG EXCEL SHEET NAME (IF EXCEL)

#HEADER DELIMITER OF COLUMN NAMES
header_delimiter <- "/"
alt_id <-  "_index"  #IF NOT ALL ID COLUMNS ARE UUID, DEFINE THE ALTERNATIVE (FOR KOBO LOOPS)

#IMPORT FOLDER--DO NOT CHANGE
cleaning_log_import_folder <- "Raw_Data"  #FOLDER WHERE THE CLEANING LOG IS STORED 
#OUTPUT FOLDERS--DO NOT CHANGE
cleaning_output_folder <- "updated_cleaning_log"  # UPDATED CLEANING LOG OUTPUT FOLDER
updated_data_folder <- "updated_data" #FOLDER WHERE UPDATED DATA GOES

#############################LOAD PACKAGES######################################
if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(weights)) install.packages('weights')
library(weights)

if (!require(pls)) install.packages('pls')
library(pls)

if (!require(gmodels)) install.packages('gmodels')
library(gmodels)

if (!require(splitstackshape)) install.packages('splitstackshape')
library(splitstackshape)

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

if (!require(taRifx)) install.packages('taRifx')
library(taRifx)

if (!require(car)) install.packages('car')
library(car)

if (!require(readr)) install.packages('readr')
library(readr)

#if (!require(tidyverse)) install.packages('tidyverse')
#library(tidyverse)

############################################################################################
############################################################################################
#LOAD CLEANING LOG
cleaning_log_org_excel<- paste0(cleaning_log_import_folder,"/",cleaning_name,".xlsx") 
cleaning_log_org<- read_excel(cleaning_log_org_excel, sheet= cleaning_log_org_excel_sheet)
cleaning_log_real_org <-cleaning_log_org
#ENSURE CLEANING LOG NAMES ARE THE SAME AS THE ORIGINAL
names(cleaning_log_org)<- c("folder", "dataset","sheet", "question", "old", "new", "reason", "_uuid", "modified", "cleaner_name")
number_cols_cleaning_log <- ncol(cleaning_log_org)
#CREATE NICE "uuid" COLUMN
cleaning_log_org$uuid <-cleaning_log_org$`_uuid`
#CREATE INDEX COLUMN
cleaning_log_org$index_number <- 1:nrow(cleaning_log_org) 
#GRAB NEWEST CORRECTIONS (HIGHEST INDEX NUMBER)
cleaning_log_org <- cleaning_log_org %>% group_by(uuid, question) %>% top_n(n=1) %>% ungroup()
cleaning_log_first_setup <- cleaning_log_org
#FOR QUESTIONS: REPLACE "/" WITH "_"
cleaning_log_org$question <- cleaning_log_org$question %>%
  gsub("/", "_", .) 
#UNIQUE FOLDERS
folders <- unique(cleaning_log_first_setup$folder)
cleaning_folder_list_intofolder <- list()
cleaning_folder_list <- list()
cleaning_sheet_list <- list()
cleaning_dataset_list <- list()
###FIRST SET OF LOOPS:  FOR EVERY FOLDER NAME
for(f in 1:length(folders)){  #FOLDER BEGIN
  print(f)  
  folder <- folders[f]
  #UNIQUE SURVEYS
  surveys_full <- cleaning_log_first_setup %>% dplyr:: filter(folder==folders[f]) 
  surveys <- unique(surveys_full$dataset)
  #FOR EVERY EXCEL FILE (SURVEY)
  for(j in 1: length(surveys)){  #SURVEYS IN FOLDER
    survey_name <- surveys[j]
    #SUBSET CLEANING LOG
    cleaning_log_org_for_each_survey <- cleaning_log_first_setup %>% 
      dplyr:: filter(folder == folders[f] )%>% dplyr:: filter(dataset == surveys[j] )
    sheets <- unique(cleaning_log_org_for_each_survey$sheet)
      for(s in 1: length(sheets)){   #SHEETS IN DATASET
        print("sheet")
        print(s)
      cleaning_log_org_for_each_sheet <- cleaning_log_org_for_each_survey %>% 
     dplyr:: filter(sheet == sheets[s] )
    
     #IF "UUID" LETTER IS TRUE
   any_alpha <-  grepl("[[:alpha:]]", cleaning_log_org_for_each_sheet$uuid) #NO ALPHABET NUMBERS IF FALSE
     any_alpha <- any(any_alpha)
       
    #ALL QUESTIONS TO BE CLEANED
    allquestions <- unique(cleaning_log_org_for_each_sheet$question)
    if (uncleaned_data_file_type == "EXCEL"){
      #UNCLEANED EXCEL FILE NAME
      uncleaned_excel<- paste0(folder,"/", survey_name ,".xlsx")
      #UNCLEANED EXCEL SHEET NAME
      uncleaned <- read_excel(uncleaned_excel, sheet= sheets[s])
    } else if (uncleaned_data_file_type == "CSV"){
      uncleaned <- read_csv(paste0(folder,"/",survey_name, ".csv"))
    } else {
      print("UNCLEANED FILE TYPE ERROR")
    }
    
    ###ADJUST INDEX TO BE UUID COLUMN
    if( length(grep("^_uuid$",colnames(uncleaned)))>0 ){
      uuid_index <- grep("^_uuid$",colnames(uncleaned))
    } else  {
      uuid_index <- grep("uuid",colnames(uncleaned)) 
    }
    
    if( nrow(unique(uncleaned[uuid_index]))==1 ){
      grep("uuid",colnames(uncleaned))
      uncleaned$`_uuid` <- NULL
      colnames(uncleaned)[grep("^_index$",colnames(uncleaned))] <- "index"
      uncleaned <- uncleaned  %>% dplyr:: rename(uuid = index) 
      print("INDEX NOT UUID")
    } else {
      colnames(uncleaned)[uuid_index] <- "uuid"  #ADD UUID COLUMN
      #uncleaned <- uncleaned
      print("UUID GOOD")
    }

##############BEGIN CHANGE HEADERS##############
    #CHECK IF ANY COLNAMES HAVE "/" IN THEM  
    special_chars <- grepl(header_delimiter, colnames(uncleaned))
    if(any(special_chars ==TRUE , na.rm=FALSE)==TRUE){
      #REMOVE UNWANTED STUFF BEFORE
      for(k in min(grep(header_delimiter,colnames(uncleaned))) : max(grep(header_delimiter,colnames(uncleaned)))){
        tmp_str  <- unlist(strsplit(colnames(uncleaned)[k] , ""))
        ind <- min(grep("[a-z]", tmp_str)) #CHARACTER OF FIRST INSTANCE OF LOWER CASE
        that <- paste0(c(tmp_str[1:(ind-1)], ";", tmp_str[ind:sum(nchar(tmp_str))]), collapse="")
        other<- regmatches(that,gregexpr("(?<=;).*",that,perl=TRUE))
        colnames(uncleaned)[k] <- other[[1]] 
      }
      #CHANGE "/" TO "_"
      colnames(uncleaned) <- gsub(header_delimiter, "_", colnames(uncleaned))
    } else{
      print("NO / IN COLNAMES")
    }

    #REMOVE DUPLICATE COLUMNS
    uncleaned <- uncleaned[, !duplicated(colnames(uncleaned))]
    ##############END CHANGE HEADERS##############
  ##CHANGE FROM UUID TO "_index" 
    if(any_alpha == FALSE){
   print("INDEX NOT UUID")
      uuid_columns <-  grep("uuid",colnames(uncleaned))
   uncleaned <- uncleaned[, -c(uuid_columns)]
   index_column <-  grep("^",alt_id,"$",colnames(uncleaned))
   colnames(uncleaned)[index_column] <- "uuid"
    } else{
      uncleaned <- uncleaned
          }
    #FILTER OUT ON THIS MONTH'S CORRECTIONS
    cleaning_log<- cleaning_log_org_for_each_sheet
    cleaning_log <- data.frame(lapply(cleaning_log, function(x) {gsub(header_delimiter, "_", x)}))
    #LIST OF QUESTIONS IN THE CLEANING LOG SUBSET
    colquestion <-   as.character(unique(cleaning_log$question))
    #REMOVE IF NOT A HEADER IN THE DATA
    colquestion <- colquestion[(colquestion %in% colnames(uncleaned))]
    #WARNING: CLEANING QUESTION NAME DOES NOT EXIST IN THE UNCLEANED DATASET
    ###LOOP BEGIN
    #EMPTY LIST FOR UPDATED QUESTIONS
    keep_q<-list()
    #CREATE COPIES OF DATA AND CLEANING LOG
    uncleaned_added_correction <- uncleaned
    cleaning_log_all_updated <-cleaning_log
    ###UPDATE DATA SHEET LOOP
    for(i in seq_along(colquestion)){ #START QUESTION
      print("questions")
      print(i)
      corrected_question_in_df <- data.frame()
      #SUBSET EACH CLEANING QUESTION FROM CLEANING LOG
      clean_log_one_question<- cleaning_log %>% 
        dplyr:: filter(question == colquestion[i])
      clean_log_one_question <- remove.factors(clean_log_one_question)
      if (i >= 2){ 
        colnames(uncleaned_added_correction)[grep("uuid.x" , colnames(uncleaned_added_correction))] <- "idd"
        uncleaned_added_correction  <- uncleaned_added_correction %>% dplyr:: mutate() %>% dplyr:: select (-contains("uuid")) %>% dplyr:: rename(uuid = idd)
      } else {
        print("FIRST LOOP -- NO DUPLICATE DEALINGS")
      }
      #DATA UNIQUE ID
      uncleaned_added_correction$unique_idx <- paste0(unlist(uncleaned_added_correction$uuid)  , "_",  as.vector(unlist(uncleaned_added_correction[grep(paste0("^",colquestion[i],"$"),colnames(uncleaned_added_correction))])) ) 
      uncleaned_added_correction$unique_idx <- paste0(uncleaned_added_correction$unique_idx, "_",  ave(uncleaned_added_correction$unique_idx ,uncleaned_added_correction$unique_idx , FUN=seq_along) )
       #CLEANING LOG UNIQUE ID
      clean_log_one_question$unique_idx <- paste0(clean_log_one_question$uuid , "_" ,clean_log_one_question$old)
      clean_log_one_question$unique_idx <- paste0(clean_log_one_question$unique_idx , "_" , ave(clean_log_one_question$unique_idx ,clean_log_one_question$unique_idx , FUN=seq_along))
      #ADD CLEANING QUESTION TO DATASET 
      newgo <- merge(uncleaned_added_correction, clean_log_one_question ,by="unique_idx",all.x=TRUE)
      unique_idx <- newgo$unique_idx
      #FIND INDEX OF QUESTION TO REPLACE IN THE DATASET
      org_index_question <-grep( paste0("^",colquestion[i],"$"),colnames(newgo))
      #FIND INDEX OF CLEANING QUESTION COLUMN
      question_to_replace_index <- grep( "^question$",colnames(newgo))
      #FIND INDEX OF REPLACEMENT INFO (CLEANING) COLUMN
      new_answer_index <- grep( "^new$",colnames(newgo))
      #COPY ORIGINAL UNCLEANED RESPONSE COLUMN
      newgo$question_clone <- newgo[,org_index_question]
      question_clone_index <- grep( "^question_clone$",colnames(newgo))
      #SUBSET RELEVANT COLUMNS
      trim_newgo <-  newgo[,c(org_index_question,question_to_replace_index,new_answer_index,question_clone_index)]
      #UPDATE ONE QUESTION AT A TIME
      trim_newgo[,5] <- ifelse(!is.na(trim_newgo[,2]), as.vector(trim_newgo[,3]),trim_newgo[,4])
      keep_q[[i]] <- trim_newgo[,5]
      
      #SAVE "uuid" FROM "newgo" TO JOIN TO "uncleaned"
      #REMOVE MULTIPLE UUID COLUMNS
      newgo$idd <- as.vector(newgo[,min(grep("uuid",colnames(newgo)))])
      newgo <- newgo %>% dplyr:: select(-contains("uuid")) %>% dplyr:: rename(uuid=idd)  
      newgo <- newgo[, !duplicated(colnames(newgo))]
      corrected_question_uuid<- newgo[,grep("^uuid$", colnames(newgo))]
      #ADD CORRECTED QUESTION WITH "uuid"
      corrected_question_with_uuid_df <- as.data.frame(cbind(corrected_question_uuid,keep_q[[i]],unique_idx))
      #RENAME CORRECTED QUESTION SUBSET
      colnames(corrected_question_with_uuid_df)[1] <- "uuid" 
      colnames(corrected_question_with_uuid_df)[2] <- "new_question"
      #UPDATE "uncleaned"
      uncleaned_added_correction <- merge(uncleaned_added_correction, corrected_question_with_uuid_df,by="unique_idx", all.x=TRUE)
      question_needed <- grep(paste0("^",colquestion[i],"$"),colnames(uncleaned_added_correction))
      uncleaned_added_correction[,question_needed] <- uncleaned_added_correction$new_question
      uncleaned_added_correction$new_question <- NULL
      trim_newgo<-NULL
      clean_log_one_question$unique_idx<-NULL
      #UPDATE CLEANING LOG
      clean_log_one_question$modified<-"yes"
      clean_log_update  <- subset(cleaning_log_all_updated, !(index_number %in% clean_log_one_question$index_number))
      cleaning_log_all_updated<-rbind(clean_log_update,clean_log_one_question)
      }  #END QUESTION
    cleaning_sheet_list[[s]] <- cleaning_log_all_updated
    #RENAME ADJUSTED DATA
    cleaned_up <- uncleaned_added_correction
    #EXPORT TO CSV
    if( length(grep("^uuid.x$",colnames(cleaned_up)))>0  ) {
      cleaned_up <- cleaned_up %>% dplyr:: mutate(idx = uuid.x) %>% dplyr:: select(-c(unique_idx,	uuid.x,	uuid.y))%>% dplyr:: rename(uuid = idx) 
    } else {
      cleaned_up <- cleaned_up 
    }
    #REMOVE NULL QUESTIONS
   cleaning_log_null <- cleaning_log_org %>% dplyr:: filter(new=="NULL") %>% dplyr:: filter(sheet == sheets[s])  %>% dplyr:: select(uuid) %>% dplyr:: distinct(uuid)
   if(nrow(cleaning_log_null)>0){
   cleaned_up <- cleaned_up[ ! as.character(cleaned_up$uuid) %in% unique(cleaning_log_null$uuid), ]
   } else{
     cleaned_up <- cleaned_up 
   }
    write.csv(cleaned_up,paste0(folders[f],"/",updated_data_folder,"/","UPDATED_CLEANED_DATA_",Sys.Date(),"_", survey_name,"_", sheets[s], ".csv") ,na = "")
    } #SHEET END
    cleaning_dataset_list[[j]] <- cleaning_sheet_list
    } #SURVEY END
  cleaning_folder_list_intofolder[[f]] <- cleaning_dataset_list
} #END FOLDER
cleaning_folder_list_intofolder[sapply(cleaning_folder_list_intofolder, is.null)] <- NULL

#ALT CONVERT LIST OF LISTS TO DF
flat = unlist(cleaning_folder_list_intofolder, recursive = FALSE)
flat = unlist(flat, recursive = FALSE)
cleaning_log_all_updated <-ldply(flat, data.frame)

####SECOND SET OF LOOPS: JOIN WITH ORGINAL CLEANING SHEET
clean_log_update  <- subset(cleaning_log_org, !(index_number %in% cleaning_log_all_updated$index_number))
names(clean_log_update) <- names(cleaning_log_all_updated)
#ROWBIND UPDATED WITH NON-UPDATED
cleaning_log_all_updated <- rbind(clean_log_update,cleaning_log_all_updated)
#DELETE REPEATED CLEANS PER-QUESTION
recent_cleans_quest <- list()
recent_cleans_survey <- list()
recent_cleans_folder <- list()
for(j in seq_along(folders)){
  folder_sub <-  subset(cleaning_log_all_updated, folder == folders[j])
  all_surveys <- as.character(unique(folder_sub$dataset))
  for(k in seq_along(all_surveys)){
    survey_sub <-  subset(folder_sub, dataset == all_surveys[k])
    questions_per_survey <- as.character(unique(survey_sub$question))
    for(i in seq_along(questions_per_survey)){
      one_question <- subset(survey_sub, question == questions_per_survey[i])
      one_question$index_number <- as.numeric(as.character( one_question$index_number))
      latest_clean_perquestion <- one_question %>% 
        group_by(uuid) %>%
        filter(index_number == max(index_number)) 
      latest_clean_perquestion <- data.frame(lapply(latest_clean_perquestion, as.character), stringsAsFactors=FALSE)
      recent_cleans_quest[[i]] <- latest_clean_perquestion
    }
    recent_cleans_survey[[k]] <- recent_cleans_quest
  }
  recent_cleans_folder[[j]] <- recent_cleans_survey
}

#UNLIST LIST OF LISTS
flat = unlist(recent_cleans_folder, recursive = FALSE)
flat2 = unlist(flat, recursive = FALSE)
cleaning_log_all_updated <- do.call(rbind.data.frame, flat2)  
cleaning_log_all_updated <- cleaning_log_all_updated[!duplicated(cleaning_log_all_updated$index_number),]

#CLEAN FILE
cleaning_log_all_updated$index_number <- NULL
cleaning_log_all_updated$uuid <- NULL
names(cleaning_log_all_updated) <- names(cleaning_log_real_org)

write.csv(cleaning_log_all_updated,paste0(cleaning_log_import_folder,"/",cleaning_output_folder,"/","UPDATED_CLEANING_LOG_",Sys.Date(),"_",cleaning_name,".csv"), na = "")
