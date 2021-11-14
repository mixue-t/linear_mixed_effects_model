################################################################################################################################
# Written by: Mi Xue Tan                                                                                                       #
# Linear Mixed models                                                                                                          #
# Model selection of random effects and fixed effects                                                                          #
#                                                                                                                              #
# Updates:                                                                                                                     #
# [021120] - factors present in random effects should be present in fixed effects part.                                        #
# If simpler model were to be tested, used a simpler RE model                                                                  #
################################################################################################################################

library(tidyverse)
library(lme4)
library(lmerTest)  

rootdir = "D:/example/path/to/data/folder" 
setwd(rootdir)

load("analysed_data/appraisal/df_appraisal.Rda")
load("analysed_data/emotion/df_emotion.Rda")
df = readRDS("analysed_data/game/df_counts_summary.rds")
type_analysis = "emotion"
load(paste0("analysed_data/", type_analysis, "/df_", type_analysis, ".Rda"))


# create save path for plots
if (!file.exists("plots/linear_models")){
  dir.create(file.path("plots/linear_models"))}

df = df %>% group_by(var) %>% 
  nest()

if ("Goal Conduciveness" %in% df$var){
  path_save = "analysed_data/appraisal/df_app_nested_all.Rda"
} else {
  path_save = "analysed_data/emotion/df_emo_nested_all.Rda"
}
save(df, file=path_save) 
load(path_save)

analysis_type = "game"
if (analysis_type == "game"){
  dependent_var = "mean_value"
} else {
  dependent_var = "value_rescaled"
}



# choose RE - aic ---------------------------------------------------------
df_RE = df %>% 
  mutate(lme_1a=map2(data, var, ~lmer(!!sym(dependent_var) ~ pred * GC +
                                        (1|subject),  data = .x, REML = T)),
         lme_1b=map2(data, var, ~lmer(!!sym(dependent_var) ~ pred * GC +
                                        (1 + pred|subject),  data = .x, REML = T)),
         lme_1c=map2(data, var, ~lmer(!!sym(dependent_var) ~ pred * GC +
                                        (1 + GC |subject),  data = .x, REML = T)),
         lme_2a=map2(data, var, ~lmer(!!sym(dependent_var) ~ pred * GC +
                                        (1 + pred * GC |subject),  data = .x, REML = T)),
         
         aov_1a = map(lme_1a, ~anova(.x, type =2)),
         aov_1b = map(lme_1b, ~anova(.x, type =2)),
         aov_1c = map(lme_1c, ~anova(.x, type =2)),
         aov_2a = map(lme_2a, ~anova(.x, type =2)),
         
         summary_1a = map(lme_1a, summary),
         summary_1b = map(lme_1b, summary),
         summary_1c = map(lme_1c, summary),
         summary_2a = map(lme_2a, summary),
         
         coef_1a = map(summary_1a, coefficients),
         coef_1b = map(summary_1b, coefficients),
         coef_1c = map(summary_1c, coefficients),
         coef_2a = map(summary_2a, coefficients),
         
         compare_1a = map(summary_1a, "AICtab"),
         compare_1b = map(summary_1b, "AICtab"),
         compare_1c = map(summary_1c, "AICtab"),
         compare_2a = map(summary_2a, "AICtab"))

if ("Goal Conduciveness" %in% df$var){
  path_save = "analysed_data/appraisal/df_app_RE.Rda"
} else{
  path_save = "analysed_data/emotion/df_emo_RE.Rda"
}
save(df_RE, file=path_save)
load(path_save)


# AIC/BIC RE model selection ----------------------------------------------

RE_compare = df_RE %>% 
  select(var,compare_1a, compare_1b, compare_1c, compare_2a) 

index_names = rep("AIC BIC logLik deviance", 4) %>%  strsplit(., " ") %>%  unlist
index_names=c("AIC", "BIC", "logLik", "deviance")
RE_compare_long = RE_compare %>%  pivot_longer(cols = starts_with("compare"), 
                                               names_to = "model", values_to = "value") %>%  unnest %>% 
  mutate(index = index_names)%>% 
  filter(., index == "AIC")  # or BIC

model_best = RE_compare_long %>% 
  group_by(var) %>% 
  mutate(best_idx = which.min(value)) %>% 
  slice(best_idx) %>% 
  unique() %>% 
  select(-c(index, best_idx)) %>% 
  rename(Emotion = var, AIC_score = value)

path_base = "analysed_data/linear_models/"
if (!file.exists(path_base)){
  dir.create(file.path(path_base))}
if (!file.exists(paste0(path_base, type_analysis))){
  dir.create(file.path(paste0(path_base, type_analysis)))}

path_save = paste0(path_base, type_analysis, "/best_RE_model_AIC.csv")
write.table(model_best, path_save , append= T, sep=',', na="", col.names= T, row.names = F)


# choose FE ---------------------------------------------------------------
df_FE_1a = df %>% 
  mutate(lme_1=map2(data, var, ~lmer(value_rescaled ~  pred * GC + 
                                       (1|subject), data = .x, REML = F)),
         aov_1 = map(lme_1, ~anova(.x, type =2)),
         summary_1 = map(lme_1, summary),
         coef_1= map(summary_1, coefficients),
         compare_1 = map(summary_1, "AICtab")) 


if ("Goal Conduciveness" %in% df$var){
  path_save = "analysed_data/appraisal/df_app_FE1a.Rda"  
} else {
  path_save = "analysed_data/emotion/df_emo_FE1a.Rda" # was df_emo_plots_m5.Rda
}
save(df_FE_1a, file=path_save)
load(path_save)

# check aov
df = df_FE_1a
path_save = paste0(path_base, type_analysis, "/anova_diff_RE.csv")
app_list = df$var
for (i in 1:length(df$var)){
  # save header
  write.table(t(names(df$aov_1[[i]])), path_save  , append= T, sep=',', col.names= F, row.names = T)
  
  aovs = df$aov_1[[i]]
  aovs = data.frame(`Fixed effects` =row.names(aovs), aovs) %>% 
    mutate(sig = case_when(Pr..F. < 0.001 ~ "***",
                           Pr..F. >= 0.001  & Pr..F. < 0.01 ~ "**", 
                           Pr..F. >= 0.01 & Pr..F. < 0.05 ~ "*", 
                           Pr..F. >= 0.05 & Pr..F. < 0.1 ~ ".",
                           Pr..F. >= 0.1 ~ ""), 
           Appraisal = df$var[[i]])
  
  write.table(aovs, path_save , append= T, sep=',', na="", col.names= F, row.names = T)
}





## model 4a
choose_var = "Goal Conduciveness"
choose_var = c("Relief", "Regret")
df_FE_m4a = df %>% 
  filter(., var  %in%  choose_var) %>% 
  mutate(lme_0=map2(data, var, ~lmer(value_rescaled ~  pred * GC + 
                                       (1 +pred * GC | subject),
                                     data = .x, REML = F)),
         lme_1a=map2(data, var, ~lmer(value_rescaled ~  pred * GC + score_level + 
                                        (1 +pred * GC + score_level |subject),
                                      data = .x, REML = F)),
         lme_1b=map2(data, var, ~lmer(value_rescaled ~  pred * GC + count_enemy +
                                        (1 +pred * GC + count_enemy |subject),
                                      data = .x, REML = F)),
         lme_1c=map2(data, var, ~lmer(value_rescaled ~  pred * GC + total_collide_times +
                                        (1 +pred * GC  |subject),
                                      data = .x, REML = F)),
         
         lme_2a=map2(data, var, ~lmer(value_rescaled ~  pred * GC + score_level + count_enemy +
                                        (1 +pred * GC + score_level + count_enemy |subject),
                                      data = .x, REML = F)),
         lme_2b=map2(data, var, ~lmer(value_rescaled ~  pred * GC + score_level + total_collide_times +
                                        (1 +pred * GC + score_level  |subject),
                                      data = .x, REML = F)),
         lme_2c=map2(data, var, ~lmer(value_rescaled ~  pred * GC + count_enemy + total_collide_times +
                                        (1 +pred * GC + count_enemy |subject),
                                      data = .x, REML = F)),
         
         lme_3=map2(data, var, ~lmer(value_rescaled ~ pred * GC + score_level +  count_enemy + total_collide_times +
                                       (1 +pred * GC + score_level + count_enemy |subject),
                                     data = .x, REML = F)),
         
         aov_0 = map(lme_0, ~anova(.x, type =2)),
         aov_1a = map(lme_1a, ~anova(.x, type =2)),
         aov_1b = map(lme_1b, ~anova(.x, type =2)),
         aov_1c = map(lme_1c, ~anova(.x, type =2)),
         aov_2a = map(lme_2a, ~anova(.x, type =2)),
         aov_2b = map(lme_2b, ~anova(.x, type =2)),
         aov_2c = map(lme_2c, ~anova(.x, type =2)),
         aov_3 = map(lme_3, ~anova(.x, type =2)),
         
         summary_0 = map(lme_0, summary),
         summary_1a = map(lme_1a, summary),
         summary_1b = map(lme_1b, summary),
         summary_1c = map(lme_1c, summary),
         summary_2a = map(lme_2a, summary),
         summary_2b = map(lme_2b, summary),
         summary_2c = map(lme_2c, summary),
         summary_3 = map(lme_3, summary),
         
         coef_0= map(summary_0, coefficients),
         coef_1a = map(summary_1a, coefficients),
         coef_1b = map(summary_1b, coefficients),
         coef_1c = map(summary_1c, coefficients),
         coef_2a = map(summary_2a, coefficients),
         coef_2b = map(summary_2b, coefficients),
         coef_2c = map(summary_2c, coefficients),
         coef_3 = map(summary_3, coefficients),
         
         compare_0 = map(summary_0, "AICtab"),
         compare_1a = map(summary_1a, "AICtab"),
         compare_1b = map(summary_1b, "AICtab"),
         compare_1c = map(summary_1c, "AICtab"),
         compare_2a = map(summary_2a, "AICtab"),
         compare_2b = map(summary_2b, "AICtab"),
         compare_2c = map(summary_2c, "AICtab"),
         compare_3 = map(summary_3, "AICtab")) %>% 
  mutate(plot = map2(data, var, ~ ggplot (data = .x, aes(x = score_level, y = value_rescaled, color = as.factor(subject))) +
                       geom_point() + 
                       # geom_smooth(method=lm,se=FALSE) +
                       # geom_smooth(aes(y=predict(lme_out[[1]]), group=subject), method=lm, se=FALSE) + 
                       labs(title = glue("{.y} vs. level score"), 
                            x = "Score per level",
                            y = glue("Rating of {.y}")) + theme_light() +
                       scale_fill_brewer(palette="RdYlGn", direction=-1)  +
                       labs(colour = "Subject") +
                       theme(strip.background =element_rect(fill='lightblue')) +
                       theme(strip.text = element_text(colour = 'black')))) 


if (df$var[[1]] == "Goal Conduciveness"){
  path_save = "analysed_data/appraisal/df_app_plots_m4a.Rda" 
} else {
  path_save = "analysed_data/emotion/df_emo_plots_m4a.Rda"
}
save(df_FE_m4a, file=path_save)

load(path_save)


