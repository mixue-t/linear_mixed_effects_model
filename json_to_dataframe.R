###################################################################
# Author: Mi Xue Tan
# Script: Read in data and create main dataframe
#
###################################################################

# Import packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)

rootdir = "D:/example/path/to/data/folder" 


setwd(rootdir)

## inputs
# type_analysis: emotion/appraisal/game
# folder_name_type: name of folder to the log files in question 
# choose between "/emotion_questionnaire_log", "/appraisal_questionnaire_log", "/game_log"

type_analysis = "appraisal"
folder_name_type = "/appraisal_questionnaire_log"  #"/emotion_questionnaire_log"  

if (!file.exists(paste0("analysed_data/", type_analysis))){
  dir.create(file.path("analysed_data"))
  dir.create(paste0("analysed_data/", type_analysis))}

# Load files --------------------------------------------------------------
primary_dirs = list.files(path=paste(rootdir, "_Results/", sep=''))
subject_dirs = lapply(primary_dirs, function(second_dirs) paste(rootdir, "_Results/", second_dirs, sep = ""))
# remove last index, which is the zip folder
subject_dirs = subject_dirs[-length(subject_dirs)]

# load data per subject if analysing game data, otherwise loop through once
# for questionnaires to get the whole data frame of all participants
if (type_analysis == "game"){
  subs = NULL
  sub_list = unlist(lapply(subject_dirs, function(subdir){
    sub = basename(subdir)
    subs = c(subs, as.numeric(sub("sub(\\d+).*", "\\1", sub)))}))
} else {
  sub_list = 1 # only run once for questionnaires
}


for (sub_idx in 1:length(sub_list)){
  print(sub_list[sub_idx])
  
  
  if (type_analysis == 'game'){
    path_base = sprintf("analysed_data/%s/sub-%02d/", type_analysis, sub_list[sub_idx])
  } else {
    path_base = sprintf("analysed_data/%s/", type_analysis)
  }
  
  if (!file.exists(path_base)){
    dir.create(file.path("analysed_data"))
    dir.create(path_base)
  }
  
  if (type_analysis == 'game'){
    primary_dirs = list.files(path=sprintf("%s_Results/sub%02d/", rootdir, sub_list[sub_idx]), pattern = "S")
    subject_dirs = lapply(primary_dirs, function(second_dirs) sprintf("%s_Results/sub%02d/%s", rootdir, sub_list[sub_idx], second_dirs))
    subject_dirs = subject_dirs[1]
  } 
  
  app_files = unlist(lapply(subject_dirs, function(subdir){
    list.files(paste0(subdir,folder_name_type), pattern = "*.json", recursive = TRUE, full.names = TRUE) }))
  
  ## add index to list!
  list_app = lapply(app_files, function(file) {fromJSON(file, flatten=FALSE)})
  
  for (ind in 1:length(list_app)){
    list_app[[ind]][[1]][[1]][['index']] = ind  
  }
  
  
  if (type_analysis == 'game') {
    path_df = sprintf("%sgame_listapp_sub-%02d.Rda", path_base, sub_list[sub_idx])
    
    if (!file.exists(path_df)){
      saveRDS(list_app, file=path_df)
    } else {
      list_app_old = readRDS(path_df)
      list_app = rbind(list_app, list_app_old)
      saveRDS(list_app, file=path_df)
    } 
  } else {
    path_df = sprintf("analysed_data/%s/listapp_%s.Rda", type_analysis, type_analysis)
    saveRDS(list_app, file=path_df)
  }
  
  print("saved list_app")
  

  
  
  # Trials and runs ---------------------------------------------------------
  
  for (i in 1:2){
    
    if (i == 1){
      fname = "game_log"
    } else fname = "emotion_questionnaire_log"
    
    
    temp = app_files = unlist(lapply(subject_dirs, function(subdir){
      list.files(paste(subdir,"/", fname, sep = ""), pattern = "*.json", recursive = TRUE)}))
    
    
    int_run = as.numeric(sub(".*_R(\\d+).*", "\\1", temp))
    int_trial = as.numeric(sub(".*_T(\\d+).*", "\\1", temp))
    int_subject = as.numeric(sub(".*_S(\\d+).*", "\\1", temp))
    
    if (fname == 'game_log'){
      int_layout = as.numeric(sub(".*_(\\d+)_.*", "\\1", temp))
      int_condition = as.numeric(sub(".*_(\\d+).*", "\\1", temp))
      df_levelinfo = data.frame(int_subject, int_run, int_trial, int_condition, int_layout)
      colnames(df_levelinfo) = c("subject", "run", "trial", "condition", "layout")
    } else {
      df_levelinfo = data.frame(int_subject, int_run, int_trial)
      colnames(df_levelinfo) = c("subject", "run", "trial")
    }
    
    
    df_levelinfo = df_levelinfo %>%  mutate(index = 1:n()) %>% 
      mutate_if(is.double, as.integer) %>%   mutate(run = run + 1)
    
    saveRDS(df_levelinfo, sprintf("%sdf_levelinfo_%s.Rda", path_base, fname))
  }
  
  
  
  # Merge info from questionnaire trials with game trials
  df_levelinfo= readRDS(paste( path_base, "df_levelinfo_game_log.Rda", sep =""))
  df_levelinfo_emo= readRDS(sprintf("%sdf_levelinfo_emotion_questionnaire_log.Rda", path_base))
  
  df_levelinfo_emo = inner_join(df_levelinfo_emo, df_levelinfo, by = c("subject", "run", "trial")) %>% 
    rename(index = index.x, index.game = index.y) 
  
  saveRDS(df_levelinfo_emo,sprintf("%sdf_levelinfo_emotion_questionnaire_log.Rda", path_base))
  saveRDS(df_levelinfo, sprintf("%sdf_levelinfo_game_log.Rda", path_base))
  
  print("creating and saving ... ")
  
  
  
  # Merging file info with data ----------------------------------------------
  # add file index 
  
  # all json files for all participants
  alljson = data.frame()
  for (i in 1:length(list_app)){
    # flatten JSON file
    app <- enframe(unlist(list_app[i]))
    # add index of file
    app$index = i
    # concatenate files
    alljson = rbind(alljson, app)
  }
  
  rm(list_app)
  
  df_levelinfo =  readRDS(sprintf("%sdf_levelinfo_game_log.Rda", path_base))
  df_level_info_sub = df_levelinfo %>%  filter(subject == sub_list[sub_idx])
  idx_minus = df_level_info_sub[1,]$index
  df_level_info_sub = df_level_info_sub %>%  mutate(index = index - idx_minus +1)
  
  if (type_analysis == "game"){
    df = inner_join(alljson, df_level_info_sub, by = "index")
    path_df = sprintf("analysed_data/%s/sub-%02d/df_%s_raw_%02d.Rda", type_analysis, sub_list[sub_idx], type_analysis, sub_list[sub_idx])
  } else {
    df = inner_join(alljson, df_levelinfo_emo, by = "index")
    path_df = sprintf("analysed_data/%s/df_%s_raw.Rda", type_analysis, type_analysis)
  }
  
  
  saveRDS(df, path_df)

  rm(alljson)
  
  print('saved df, deleted list app and all json')

  
  # find number of columns needed for split
  rgx_split <- "\\."
  # n_cols_max <-
  #   df %>%
  #   pull(name) %>% 
  #   str_split(rgx_split) %>% 
  #   map_dbl(~length(.)) %>% 
  #   max()
  # n_cols_max = 5 for game; 3
  
  if (type_analysis == 'game'){
    n_cols_max = 5
  } else {
    n_cols_max = 3
  }
  
  df <- df %>%  
    separate(name, into = c(paste0("x", 1:n_cols_max)), fill = "right", sep = rgx_split)
  
  print("separated df into columns")
  
  if (type_analysis != "game"){
    df<- df %>%  
      filter(x3 == c("s_emotion_value")) %>% 
      select(-x1, -x3,  -index) %>% 
      rename(var = x2) %>% 
      mutate(value = as.numeric(gsub(",", ".", value))) %>%  # change value to numeric, and change commas to fullstops (12,3 -> 12.3)
      mutate(layout_merge = as.numeric(lapply(layout, function(x) ifelse(x >= 5, x-4, x))))
    
    # function to rescale ratings from [0,1] to [-1, 1], do only with emo questionnaire
    if (type_analysis == "emotion")(
      to_rescale = T
    ) else if (type_analysis == "appraisal") {
      to_rescale = F
    }
    if (to_rescale) {
      f_rescale_val <- function(x, newMax, newMin){(x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin }
      value_rescaled = f_rescale_val(df$value, 1, -1)
      value_rescaled = as.data.frame(value_rescaled)
      df = bind_cols(df, value_rescaled)
    }
    
    if (type_analysis == "appraisal"){
      df= df %>% mutate(var = recode(var, "Allowed me to predict what was going to happen" = "Predictability",
                                     # "Adapt to challenges in the game" = "Coping potential",
                                     "Comprised of uncertain events" = "U_events",
                                     "Prevented me from reaching the goals of the game" = "G_Obstructiveness",
                                     "Made me uncertain of my next moves" = "U_moves",
                                     "Was easy" = "Ease"))
      df = rename(df, value_rescaled = value)
    }
    
    df = df %>% 
      group_by(subject, condition, var) %>% # get mean per participant
      mutate(mean_value = mean(value_rescaled))  %>% 
      mutate(U = ifelse(condition %in% c(1, 3), "low", "high")) %>% 
      mutate(GO = ifelse(condition %in% c(1, 2), "low", "high"))
    df$U = as.factor(df$U)
    df$U <- factor(df$U,levels = c("low", "high"))
    df$GO = as.factor(df$GO)
    df$GO <- factor(df$GO,levels = c("low", "high"))
    
    ## df_emo 
    # var (emotion): 7 emotions 
    # value: value of emotion rating
    # subject: sub no.
    # run: run no. 
    # trial: trial no. 
    # condition: 1 - 4
    # layout: 1-8
    # layout_merge: pool wall type variations per layout 1-4
    # value_rescaled: value rescaled to [-1, 1] from [0,1]
    # mean_value: mean ratings of an emotion per condition (based on value_rescaled)
    # U: uncertainty (high/ low)
    # GO: goal obstructiveness (high/low)
    if (!file.exists(paste0("analysed_data/", type_analysis))){
      dir.create(file.path("analysed_data"))
      dir.create(paste0("analysed_data/", type_analysis))}
    
    saveRDS(df, file=paste0("analysed_data/", type_analysis, "/df_", type_analysis, ".Rda"))
    # df = readRDS(file=paste0("analysed_data/", type_analysis, "/df_", type_analysis, ".Rda"))
    
  } else{
    saveRDS(df, file= sprintf("%sdf_%s_sub-%02d.Rda", path_base, type_analysis, sub_list[sub_idx]))
    print("go to general count file!")
    rm(df)
  }
  
}

