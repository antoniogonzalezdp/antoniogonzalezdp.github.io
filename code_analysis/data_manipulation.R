#### LOAD LIBRARIES, FUNCTIONS AND GRAPHICAL PARAMETERS ####

#packages
require(car)  
library(rlang)
library(lme4)
library(lmerTest)
library(afex)
library(cowplot)
library(quickpsy)
library(scales)
library(zoo)
library(emmeans)
library(glue)
library(ggtext)
library(tidyverse)
library(data.table)
library(dtplyr)
library(effectsize)
library(r2glmm)
library(gghalves)

#functions
source("define_functions.R")

#graphical parameters
source("define_graphical_parameters.R")

#### LOAD DATASETS MAIN EXPERIMENT ####
load("disc_df.Rda") #discrete data
load("cont_conf_df.Rda") #continuous data, confidence condition
load("cont_risk_df.Rda") #continuous data, risk condition
load("cont_cross_df.Rda") #continuous data, cross condition

#### LOAD DATASETS FOLLOW-UP EXPERIMENT ####
load("disc_df_difttc.Rda") #discrete data
load("cont_conf_df_difttc.Rda") #continuous data, confidence condition
load("cont_cross_df_difttc.Rda") #continuous data, cross condition

#### CREATE NEW VARIABLES ####
#car velocities for each set and variability level
vels_set1_no <- c(10,10,10)
vels_set1_medium <- c(7.16,10,12.84)
vels_set1_high <- c(3.51,10,16.49)
vels_set2_no <- c(12,12,12)
vels_set2_medium <- c(8.59,12,15.41)
vels_set2_high <- c(4.22,12,19.78)

#cars velocities SDs for each set and variability level
sd_vels_set1_no <- 0
sd_vels_set1_medium <- round(sd(c(vels_set1_medium,vels_set1_medium)),2)
sd_vels_set1_high <- round(sd(c(vels_set1_high,vels_set1_high)),2)
sd_vels_set2_no <- 0
sd_vels_set2_medium <- round(sd(c(vels_set2_medium,vels_set2_medium)),2)
sd_vels_set2_high <- round(sd(c(vels_set2_high,vels_set2_high)),2)

#manually define whether in cross condition trials a crossing was attempted or successful
crossings_attsuc_trials <- cont_cross_df %>%
  group_by(participant,cond,session,trial_within_session) %>%
  summarise(
    helmet_posz_max = max(helmet_posz),
    helmet_posz_last = last(helmet_posz)) %>%
  mutate(
    cross_att = ifelse(helmet_posz_max > -2, 1,0),
    cross_suc = ifelse(cross_att == 1 & helmet_posz_last >= 2.8, 1,0)) %>%
  ungroup() %>%
  select(participant,session,trial_within_session,cross_att, cross_suc)

disc_df <- left_join(disc_df,crossings_attsuc_trials)

disc_df <- disc_df %>%
  mutate(
    t_cross = ifelse(cross_att == 1,1,0), 
    cross = ifelse(cross_suc == 1,1,0))

#add new variables to disc_df
disc_df <- disc_df %>%
  mutate(
    resp = ifelse(cond == "risk", 1 - resp, resp),
    go = ifelse(cond == "cross",t_cross,resp),
    var_rank = case_when(
      vel1 == vel2 & vel2 == vel3 ~ 1,
      if_any(c(vel1, vel2, vel3), ~ . %in% c(7.16, 8.59)) ~ 2,
      if_any(c(vel1, vel2, vel3), ~ . %in% c(3.51, 4.22)) ~ 3),
    
    var_name = case_when(
      vel1 == vel2 & vel2 == vel3 ~ "no",
      if_any(c(vel1, vel2, vel3), ~ . %in% c(7.16, 8.59)) ~ "medium",
      if_any(c(vel1, vel2, vel3), ~ . %in% c(3.51, 4.22)) ~ "high"),
    
    var_sd = case_when(
      vel1 == vel2 & vel2 == vel3 ~ 0,
      if_any(c(vel1, vel2, vel3), ~ . == 7.16) ~ sd_vels_set1_medium,
      if_any(c(vel1, vel2, vel3), ~ . == 3.51) ~ sd_vels_set1_high,
      if_any(c(vel1, vel2, vel3), ~ . == 8.59) ~ sd_vels_set2_medium,
      if_any(c(vel1, vel2, vel3), ~ . == 4.22) ~ sd_vels_set2_high)
  )

#turning some variables into factors
disc_df$ttc_factor <- factor(disc_df$ttc)
disc_df$var_sd_factor <- factor(disc_df$var_sd)
disc_df$var_rank_factor <- factor(disc_df$var_rank)
disc_df$session_factor <- factor(disc_df$session)
disc_df$var_name <- factor(disc_df$var_name, levels = c("no", "medium", "high"))

#add variables from discrete df to continuous dfs
disc_df_cross_tojoin <- disc_df %>%
  filter(cond == "cross") %>%
  select(participant, cond, session, trial_within_session,ttc,var_sd,ttc_factor,var_sd_factor,var_name,t_cross,cross)

cont_cross_df <- left_join(cont_cross_df, disc_df_cross_tojoin, by= c("participant", "cond", "session", "trial_within_session","ttc"))

disc_df_conf_tojoin <- disc_df %>%
  filter(cond == "conf") %>%
  select(participant, cond, session, trial_within_session,ttc,var_sd,ttc_factor,var_sd_factor,var_name)

cont_conf_df <- left_join(cont_conf_df, disc_df_conf_tojoin, by= c("participant", "cond", "session", "trial_within_session","ttc"))

disc_df_risk_tojoin <- disc_df %>%
  filter(cond == "risk") %>%
  select(participant, cond, session, trial_within_session,ttc,var_sd,ttc_factor,var_sd_factor,var_name)

cont_risk_df <- left_join(cont_risk_df, disc_df_risk_tojoin, by= c("participant", "cond", "session", "trial_within_session","ttc"))

#unique ttcs and var_sd
unique_ttc <- sort(unique(disc_df$ttc))
unique_var_sd <- sort(unique(disc_df$var_sd))

#define plotting scales with unique ttc and var_sd
scale_x_ttc <- scale_x_continuous(breaks = unique_ttc, limits = c(3,8))
scale_x_var_sd <- scale_x_continuous(breaks = unique_var_sd, limits = c(0,6.96))
scale_x_ttc_discrete <- scale_x_discrete(labels = c("3.35", "4.09", "5", "6.11", "7.46"))
scale_x_var_sd_discrete <- scale_x_discrete(labels = c("0", "2.54", "3.05", "5.8", "6.96"))

#### EXCLUDE PARTICIPANTS BASED ON PSYCHOMETRIC FIT ####
participants_to_exclude <- c("AAAA") #if not excluding participants, just give a dummy participant name
unique_participants <- sort(unique(disc_df$participant)) #unique participants before excluding outliers

outlier_subjs <- c("clr","jpc")
outlier_subjs_withrisk <- c(outlier_subjs,"pmg")

disc_df_nofilt <- disc_df #keep copy of disc_df before participant exclusion
disc_df <- disc_df %>% filter(! participant %in% outlier_subjs)

cont_conf_df <- cont_conf_df %>% filter(! participant %in% outlier_subjs)
cont_risk_df <- cont_risk_df %>% filter(! participant %in% outlier_subjs)
cont_cross_df <- cont_cross_df %>% filter(! participant %in% outlier_subjs)

#### PSYCHOMETRIC FUNCTIONS SEPARATE PER VARIABILITY LEVEL ####

#fit
psyfit_results <- process_participants(
  df = disc_df_nofilt, 
  unique_participants = unique_participants, 
  df_name = "psyfit"
)

#store different fit results in individual dfs
psyfit_conf_averages <- psyfit_results$averages %>% filter(cond == "conf")
psyfit_risk_averages <- psyfit_results$averages %>% filter(cond == "risk")
psyfit_cross_averages <- psyfit_results$averages %>% filter(cond == "cross")

psyfit_conf_curves <- psyfit_results$curves %>% filter(cond == "conf")
psyfit_risk_curves <- psyfit_results$curves %>% filter(cond == "risk")
psyfit_cross_curves <- psyfit_results$curves %>% filter(cond == "cross")

psyfit_conf_thresholds <- psyfit_results$thresholds %>% filter(cond == "conf")
psyfit_risk_thresholds <- psyfit_results$thresholds %>% filter(cond == "risk")
psyfit_cross_thresholds <- psyfit_results$thresholds %>% filter(cond == "cross")

#reorder factor levels for var_name
psyfit_conf_averages$var_name <- factor(psyfit_conf_averages$var_name, levels = c("no", "medium", "high"))
psyfit_conf_curves$var_name <- factor(psyfit_conf_curves$var_name, levels = c("no", "medium", "high"))
psyfit_conf_thresholds$var_name <- factor(psyfit_conf_thresholds$var_name, levels = c("no", "medium", "high"))
psyfit_risk_averages$var_name <- factor(psyfit_risk_averages$var_name, levels = c("no", "medium", "high"))
psyfit_risk_curves$var_name <- factor(psyfit_risk_curves$var_name, levels = c("no", "medium", "high"))
psyfit_risk_thresholds$var_name <- factor(psyfit_risk_thresholds$var_name, levels = c("no", "medium", "high"))
psyfit_cross_averages$var_name <- factor(psyfit_cross_averages$var_name, levels = c("no", "medium", "high"))
psyfit_cross_curves$var_name <- factor(psyfit_cross_curves$var_name, levels = c("no", "medium", "high"))
psyfit_cross_thresholds$var_name <- factor(psyfit_cross_thresholds$var_name, levels = c("no", "medium", "high"))

#extract threshold dfs from each fit object, append them and create across-participant summaries
psyfit_conf_thresholds <- psyfit_conf_thresholds %>%
  mutate(cond = "conf")
psyfit_risk_thresholds <- psyfit_risk_thresholds %>%
  mutate(cond = "risk")
psyfit_cross_thresholds <- psyfit_cross_thresholds %>%
  mutate(cond = "cross")

psyfit_thresholds_sin <- bind_rows(psyfit_conf_thresholds, psyfit_risk_thresholds, psyfit_cross_thresholds)

psyfit_thresholds_sin <- tibble(psyfit_thresholds_sin)
psyfit_thresholds_sin$var_name <- factor(psyfit_thresholds_sin$var_name)
psyfit_thresholds_sin <- psyfit_thresholds_sin %>%
  filter(! participant %in% participants_to_exclude) #individual participants

psyfit_thresholds_sav <- psyfit_thresholds_sin %>%
  filter(! participant %in% outlier_subjs) %>%
  group_by(cond,var_name) %>%
  summarise_with_ci(thre) #across-participants summary

#### RESPONSES ACROSS TTC ####
go_ttc_sin <- disc_df %>%
  group_by(cond,participant,ttc_factor) %>%
  summarise(go = mean(go))

go_ttc_sav <- go_ttc_sin %>%
  group_by(cond,ttc_factor) %>%
  summarise_with_ci(go)

#### RESPONSES ACROSS VARIABILITY ####
go_var_sin <- disc_df %>%
  group_by(cond,participant,var_rank_factor) %>%
  summarise(go = mean(go))

go_var_sav <- go_var_sin %>%
  group_by(cond,var_rank_factor) %>%
  summarise_with_ci(go)

#### CORRELATIONS ####

#variability, 3 levels (factor) - logit transformed
dotsforcors_3lev_var_sd <- disc_df %>%
  group_by(participant, cond,var_name) %>%
  summarise(go = mean(go, na.rm = TRUE)) %>%
  pivot_wider(names_from = cond, values_from = go)

succcross_3lev_var_sd_summary <- disc_df %>%
  filter(cond == "cross" & t_cross == 1) %>%
  group_by(participant,var_name) %>%
  summarise(crossperf = mean(cross, na.rm = TRUE)) %>%
  ungroup

dotsforcors_3lev_var_sd <- left_join(dotsforcors_3lev_var_sd,succcross_3lev_var_sd_summary)

dotsforcors_3lev_var_sd <- dotsforcors_3lev_var_sd %>%
  mutate(crossperf = ifelse(is.na(crossperf),0,crossperf)) %>%
  rename(var_sd = var_name)

dotsforcors_3lev_var_sd <- dotsforcors_3lev_var_sd %>%
  mutate(var_sd_forfacet = paste0(var_sd," variability"))
dotsforcors_3lev_var_sd$var_sd_forfacet = factor(dotsforcors_3lev_var_sd$var_sd_forfacet, levels=c('no variability','medium variability','high variability'))

#### CORRELATIONS (LOG ODDS) ####
dotsforcors_3lev_logit_var_sd <- dotsforcors_3lev_var_sd %>%
  mutate(
    conf = log_odds(conf),
    cross = log_odds(cross),
    risk = log_odds(risk),
    crossperf = log_odds(crossperf))

paircors_3lev_logit_var_sd <- calculate_pairwise_correlations_fromcorpoints(dotsforcors_3lev_logit_var_sd, "var_sd", c("conf", "cross", "risk","crossperf"))

corstrings_3lev_logit_var_sd <- paircors_3lev_logit_var_sd %>%
  mutate(
    corrstring_conf_cross = create_correlation_string(cor_conf_cross, p_conf_cross)) %>%
  select(var_sd, corrstring_conf_cross)

#### MOVEMENT VELOCITY ####

#calculate velocity
time_between_timeruns <- 0.011
velocity_threshold <- 0.5
threshold_cont_timepoints <- round(1 /time_between_timeruns)

cont_cross_df <- cont_cross_df %>%
  arrange(participant,session,trial_within_session,timerun) %>%
  group_by(participant, session, trial_within_session) %>%
  mutate(
    velocity_unfilt = velocity,
    velocity = ifelse(velocity > -1 & velocity < 3, velocity, NA),
    ismoving = velocity > velocity_threshold,
    ismoving = replace_na(ismoving, FALSE),
    movement_segment = cumsum(c(TRUE, diff(ismoving) != 0))
  )

cont_cross_df_movementsegments <- cont_cross_df %>%
  group_by(participant, session, trial_within_session, movement_segment) %>%
  filter(all(ismoving) & n() >= threshold_cont_timepoints)

#### TIME AT WHICH SIDEWALK IS LEFT (RT) FOR ATTEMPTED CROSSINGS, AND VELOCITY VARIABLES ####
movinitrt_df <- cont_cross_df %>%
  filter(t_cross == 1 & helmet_posz >= -3) %>%
  group_by(participant,session,trial_within_session,ttc,var_name) %>%
  summarise(
    leaving_time = first(timerun)) %>%
  mutate(
    leaving_time_norm = leaving_time / ttc,
    leaving_time_tottc = ttc - leaving_time)

movementsegments_trialsum <- cont_cross_df_movementsegments %>% 
  filter(t_cross == 1) %>%
  group_by(participant,session,trial_within_session,ttc,var_name) %>%
  summarise(
    peakvel = max(velocity, na.rm = TRUE),
    timetopeakvel = timerun[which.max(velocity)],
    meanvel = mean(velocity, na.rm = TRUE))

movementsegments_trialsum <- left_join(movementsegments_trialsum, movinitrt_df)

movend_df <- cont_cross_df %>%
  filter(t_cross == 1 & helmet_posz <= 3) %>%
  group_by(participant,session,trial_within_session,ttc,var_name) %>%
  summarise(last_mtime = last(timerun))

movementsegments_trialsum <- left_join(movementsegments_trialsum, movend_df)

movementsegments_trialsum <- movementsegments_trialsum %>%
  mutate(
    movement_time = last_mtime - leaving_time,
    movement_time_norm = movement_time / ttc)

#summaries leaving time
leaving_time_norm_var_sin <- movinitrt_df %>%   
  group_by(participant,var_name) %>%
  summarise(leaving_time_norm = mean(leaving_time_norm))
leaving_time_norm_var_sin$var_name <- factor(leaving_time_norm_var_sin$var_name, levels = c("no","medium","high"))

leaving_time_norm_var_sav <- leaving_time_norm_var_sin %>%
  group_by(var_name) %>%
  summarise_with_ci(leaving_time_norm)

#summaries peak vel
peakvel_varname_sin <- movementsegments_trialsum %>%
  group_by(participant, var_name) %>%
  summarise(peakvel = mean(peakvel, na.rm = TRUE))

peakvel_varname_sin$var_name <- factor(peakvel_varname_sin$var_name, levels = c("no","medium", "high"))

peakvel_varname_sav <- peakvel_varname_sin %>%
  group_by(var_name) %>%
  summarise_with_ci(peakvel)

#### EYE MOVEMENTS ####

#apply function to detect whether cars were looked at on each timerun
cont_conf_df <- detect_gaze_on_cars_vec(cont_conf_df)
cont_risk_df <- detect_gaze_on_cars_vec(cont_risk_df)
cont_cross_df <- detect_gaze_on_cars_vec(cont_cross_df)

#### CREATE DF WITH CONTINUOUS DATA BEFORE ATTEMPTING TO CROSS ####
cont_cross_befmov_df <- cont_cross_df %>%
  filter(t_cross == 1 & helmet_posz <= -3) %>%
  mutate(cond = "cross_befmov")

cross_matchedrts_ttcvar_sin <- cont_cross_befmov_df %>%
  group_by(participant,cond,session,trial_within_session,ttc,var_name) %>%
  summarise(matched_rt = last(timerun)) %>%
  group_by(participant,cond,ttc,var_name) %>%
  summarise(matched_rt = mean(matched_rt, na.rm = TRUE))

cont_cross_befmov_noattcross_df <- cont_cross_df %>%
  filter(t_cross == 0) %>%
  mutate(cond = "cross_befmov")

cont_cross_befmov_noattcross_df <- left_join(cont_cross_befmov_noattcross_df,cross_matchedrts_ttcvar_sin)

cont_cross_befmov_noattcross_df <- cont_cross_befmov_noattcross_df %>%
  filter(timerun <= matched_rt) %>%
  select(-matched_rt)

cont_cross_befmov_df <- rbind(cont_cross_befmov_df, cont_cross_befmov_noattcross_df)
cont_cross_befmov_df <- cont_cross_befmov_df %>% arrange(participant,cond,session,trial_within_session,timerun)

#### CREATE DF WITH EACH TRIAL'S GO RESPONSES TO ATTACH TO OTHER DFS####
goresps_df <- rbind(
  disc_df %>% 
    select(participant,cond,session,trial_within_session,ttc,var_name,go),
  disc_df %>% 
    filter(cond == "cross") %>% 
    mutate(cond = "cross_befmov") %>%
    select(participant,cond,session,trial_within_session,ttc,var_name,go)
)

#### PROPORTION OF TOTAL TRIAL TIME SPENT LOOKING AT ANY CAR ####
propcarlook_conf_df <- calculate_propcarlook(cont_conf_df) 
propcarlook_risk_df <- calculate_propcarlook(cont_risk_df)
propcarlook_cross_df <- calculate_propcarlook(cont_cross_df)
propcarlook_cross_befmov_df <- calculate_propcarlook(cont_cross_befmov_df)
propcarlook_df <- rbind(propcarlook_conf_df,propcarlook_risk_df,propcarlook_cross_df,propcarlook_cross_befmov_df)

propcarlook_df <- left_join(propcarlook_df,goresps_df)

#### PROPORTION OF CAR LOOKING TIME SPENT LOOKING AT 2 GROUPS OF CARS ACCORDING TO PROXIMITY (NEAR / FAR) ####
lanedif_conf_df <- create_lanedif_props(cont_conf_df,goresps_df)
lanedif_risk_df <- create_lanedif_props(cont_risk_df,goresps_df)
lanedif_cross_df <- create_lanedif_props(cont_cross_df,goresps_df)
lanedif_cross_befmov_df <- create_lanedif_props(cont_cross_befmov_df,goresps_df)

lanedif_df <- rbind(lanedif_conf_df,lanedif_risk_df,lanedif_cross_df,lanedif_cross_befmov_df)

#### EYE MOVEMENT DYNAMICS ####

#proportion of time looking at any car, confidence condition
cont_conf_df <- cont_conf_df %>%
  mutate(
    time_norm = timerun / ttc,
    carslooked = ifelse(car1_look | car2_look | car3_look | car4_look | car5_look | car6_look,1,0)
  )

cont_conf_df <- left_join(
  cont_conf_df,
  goresps_df %>% filter(cond == "conf")
)

carslooked_bin_conf_var <- cont_conf_df %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(carslooked = mean(carslooked), .groups = "drop") %>%
  mutate(time_bin_center = (bin_num - 0.5) / 100)

carslooked_bin_conf_var_sav <- carslooked_bin_conf_var %>%
  group_by(var_name, go, bin_num,time_bin_center) %>%
  summarise_with_ci(carslooked)

#proportion of time looking at any car, cross condition
cont_cross_befmov_df <- cont_cross_befmov_df %>%
  ungroup() %>%
  mutate( carslooked = ifelse(car1_look | car2_look | car3_look | car4_look | car5_look | car6_look, 1, 0)) %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(
    time_norm = timerun / max(timerun, na.rm = TRUE),
  )

cont_cross_befmov_df <- left_join(
  cont_cross_befmov_df,
  goresps_df %>% filter(cond == "cross_befmov")
)

carslooked_crossbefmov_bin_var <- cont_cross_befmov_df %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(carslooked = mean(carslooked), .groups = "drop") %>%
  mutate(time_bin_center = (bin_num - 0.5) / 100)

carslooked_crossbefmov_bin_var_sav <- carslooked_crossbefmov_bin_var %>%
  group_by(var_name, go, bin_num,time_bin_center) %>%
  summarise_with_ci(carslooked)

#difference between time looking at far minus near lane cars, confidence condition
carslanediflooked_bin_conf_var <- left_join(
  cont_conf_df %>%
    ungroup() %>%
    mutate(
      time_norm = timerun / ttc,
      carsnear_looked = as.integer(car1_look | car2_look | car3_look),
      carsfar_looked  = as.integer(car4_look | car5_look | car6_look))
  ,
  goresps_df %>% filter(cond == "conf")) %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(
    carsnear_looked = mean(carsnear_looked),
    carsfar_looked = mean(carsfar_looked)) %>%
  mutate(
    carslane_looked_dif = carsfar_looked - carsnear_looked,
    time_bin_center = (bin_num - 0.5) / 100)

carslanediflooked_bin_conf_var_sav <- carslanediflooked_bin_conf_var %>%
  group_by(var_name, go, bin_num,time_bin_center) %>%
  summarise_with_ci(carslane_looked_dif)

#difference between time looking at far minus near lane cars, cross condition
carslanediflooked_bin_cross_befmov_var <- left_join(
  cont_cross_befmov_df %>%
    ungroup() %>%
    mutate(
      carsnear_looked = as.integer(car1_look | car2_look | car3_look),
      carsfar_looked  = as.integer(car4_look | car5_look | car6_look)) %>% 
    group_by(participant, cond, session, trial_within_session) %>%
    mutate(
      time_norm = timerun / max(timerun, na.rm = TRUE),
    )
  ,
  goresps_df %>% filter(cond == "cross_befmov")
) %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(
    carsnear_looked = mean(carsnear_looked),
    carsfar_looked = mean(carsfar_looked)) %>%
  mutate(
    carslane_looked_dif = carsfar_looked - carsnear_looked,
    time_bin_center = (bin_num - 0.5) / 100)

carslanediflooked_bin_cross_befmov_var_sav <- carslanediflooked_bin_cross_befmov_var %>%
  group_by(var_name, go, bin_num,time_bin_center) %>%
  summarise_with_ci(carslane_looked_dif)
#### ANALYSES PEAKS AND TROUGHS EYE MOVEMENT DYNAMICS ####

#prop looking time, confidence condition
peaktroughs_propcarlook_conf <- cont_conf_df %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(carslooked = mean(carslooked), .groups = "drop") %>%
  mutate(time_bin_center = (bin_num - 0.5) / 100) %>%
  group_by(participant, var_name, go) %>%
  arrange(time_bin_center, .by_group = TRUE) %>%
  nest() %>% 
  mutate(
    fit = map(data, ~ loess(carslooked ~ time_bin_center, data = .x, span = 0.4)),
    smooth = map2(data, fit, ~ mutate(.x, cars_s = predict(.y, .x))),
    peaks = map(smooth, ~ {
      df <- .x
      prev <- lag(df$cars_s, default = -Inf)
      nxt  <- lead(df$cars_s, default = -Inf)
      is_pk <- df$cars_s >= prev & df$cars_s >= nxt
      which(is_pk)[1]
    }),
    peak_time   = map2_dbl(smooth, peaks, ~ .x$time_bin_center[.y]),
    peak_height = map2_dbl(smooth, peaks, ~ .x$cars_s[.y]),
    trough_idx = map2_int(smooth, peaks, ~{
      df <- .x
      pk <- .y
      if (pk < nrow(df) - 1) {
        prev <- lag(df$cars_s)
        nxt  <- lead(df$cars_s)
        is_min <- (df$cars_s <= prev) & (df$cars_s <= nxt)
        
        which(is_min & seq_along(is_min) > pk)[1]
      } else {
        NA_integer_
      }
    }),
    trough_time   = map2_dbl(smooth, trough_idx,   ~ if (!is.na(.y)) .x$time_bin_center[.y] else NA_real_),
    trough_height = map2_dbl(smooth, trough_idx,   ~ if (!is.na(.y)) .x$cars_s[.y] else NA_real_),
    dip_amount   = peak_height - trough_height,
    dip_duration = trough_time  - peak_time ) %>%
  mutate(go_factor = factor(go)) %>%
  select(participant, var_name, go_factor,
         peak_time,  peak_height,
         trough_time, trough_height,
         dip_amount, dip_duration)

#prop looking time, cross condition
peaktroughs_propcarlook_cross_befmov <-  cont_cross_befmov_df %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(carslooked = mean(carslooked), .groups = "drop") %>%
  mutate(time_bin_center = (bin_num - 0.5) / 100) %>%
  group_by(participant,var_name,go) %>%
  summarise(
    time_bin_center = time_bin_center[which.min(carslooked)],
    carslooked = min(carslooked)) %>%
  mutate(go_factor = factor(go))

#difference far minus near looking time, confidence condition
peaktroughs_diflanelook_conf <- left_join(
  cont_conf_df %>%
    ungroup() %>%
    mutate(
      time_norm = timerun / ttc,
      carsnear_looked = as.integer(car1_look | car2_look | car3_look),
      carsfar_looked  = as.integer(car4_look | car5_look | car6_look)
    )
  ,
  goresps_df %>% filter(cond == "conf")
) %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(
    carsnear_looked = mean(carsnear_looked),
    carsfar_looked = mean(carsfar_looked)) %>%
  mutate(
    carslane_looked_dif = carsfar_looked - carsnear_looked,
    time_bin_center = (bin_num - 0.5) / 100) %>%
  group_by(participant, var_name, go) %>%
  arrange(time_bin_center, .by_group = TRUE) %>%
  nest() %>%
  mutate(
    fit    = map(data, ~ loess(carslane_looked_dif ~ time_bin_center, data = .x, span = 0.4)),
    smooth = map2(data, fit, ~ mutate(.x, dif_s = predict(.y, .x)))
  ) %>%
  mutate(
    first_trough_idx = map_int(smooth, ~ {
      df <- .x
      prev <- lag(df$dif_s, default =  Inf)
      nxt  <- lead(df$dif_s, default =  Inf)
      is_tr  <- (df$dif_s <= prev) & (df$dif_s <= nxt)
      which(is_tr)[1]
    }),
    trough_time   = map2_dbl(smooth, first_trough_idx, ~ .x$time_bin_center[.y]),
    trough_height = map2_dbl(smooth, first_trough_idx, ~ .x$dif_s[.y])
  ) %>%
  mutate(
    rebound_peak_idx = map2_int(smooth, first_trough_idx, ~ {
      df <- .x
      pk_start <- .y
      if (is.na(pk_start) || pk_start >= nrow(df)) return(NA_integer_)
      prev <- lag(df$dif_s)
      nxt  <- lead(df$dif_s)
      is_pk <- (df$dif_s >= prev) & (df$dif_s >= nxt)
      which(is_pk & seq_along(is_pk) > pk_start)[1]
    }),
    peak_time   = map2_dbl(smooth, rebound_peak_idx, ~ if (!is.na(.y)) .x$time_bin_center[.y] else NA_real_),
    peak_height = map2_dbl(smooth, rebound_peak_idx, ~ if (!is.na(.y)) .x$dif_s[.y] else NA_real_)
  ) %>%
  mutate(
    dip_amount   = peak_height - trough_height,
    dip_duration = peak_time   - trough_time
  ) %>%
  ungroup() %>%
  mutate(go_factor = factor(go)) %>%
  select(
    participant, var_name, go_factor,
    trough_time, trough_height,
    peak_time,   peak_height,
    dip_amount,  dip_duration)

#difference far minus near looking time, cross condition
peaktroughs_diflanelook_cross_befmov <-  left_join(
  cont_cross_befmov_df %>%
    ungroup() %>%
    mutate(
      carsnear_looked = as.integer(car1_look | car2_look | car3_look),
      carsfar_looked  = as.integer(car4_look | car5_look | car6_look)) %>% 
    group_by(participant, cond, session, trial_within_session) %>%
    mutate(
      time_norm = timerun / max(timerun, na.rm = TRUE),
    )
  ,
  goresps_df %>% filter(cond == "cross_befmov")
) %>%
  group_by(participant, cond, session, trial_within_session) %>%
  mutate(bin_num = cut(time_norm, breaks = seq(0, 1, length.out = 101), labels = FALSE, include.lowest = TRUE)) %>%
  group_by(participant, var_name, go, bin_num) %>%
  summarise(
    carsnear_looked = mean(carsnear_looked),
    carsfar_looked = mean(carsfar_looked)) %>%
  mutate(
    carslane_looked_dif = carsfar_looked - carsnear_looked,
    time_bin_center = (bin_num - 0.5) / 100) %>%
  group_by(participant, var_name, go, time_bin_center) %>%
  summarise(carslane_looked_dif = mean(carslane_looked_dif), .groups = "drop") %>%
  group_by(participant,var_name,go) %>%
  summarise(
    time_bin_center = time_bin_center[which.max(carslane_looked_dif)],
    carslane_looked_dif = max(carslane_looked_dif)) %>%
  mutate(go_factor = factor(go))

#### CORRELATIONS AMONG THRESHOLDS, KINEMATICS AND EYE MOVEMENTS ####

psyfit_thresholds <- rbind(psyfit_conf_thresholds, psyfit_risk_thresholds, psyfit_cross_thresholds)

dotsforcors_propcarlook <- propcarlook_df %>%
  filter(cond != "cross") %>%
  mutate(cond = ifelse(cond == "cross_befmov","cross",cond)) %>%
  group_by(participant,cond,var_name) %>%
  summarise(propcarlook = mean(propcarlook,na.rm = TRUE))

dotsforcors_lanediflook <- lanedif_df %>%
  filter(cond != "cross") %>%
  mutate(cond = ifelse(cond == "cross_befmov","cross",cond)) %>%
  group_by(participant,cond,var_name) %>%
  summarise(lanediflook = mean(diff_far_minus_near,na.rm = TRUE))

dotsforcors_thres_kin_em <- 
  left_join(
    psyfit_thresholds %>% select(-prob), 
    leaving_time_norm_var_sin) %>%
  left_join(., peakvel_varname_sin) %>%
  left_join(., dotsforcors_propcarlook) %>%
  left_join(., dotsforcors_lanediflook)
  
dotsforcors_thres_kin_em_wide <- 
  dotsforcors_thres_kin_em %>%
  pivot_wider(
    id_cols = c(participant, var_name),
    names_from = cond,
    values_from = c(thre, leaving_time_norm, peakvel, propcarlook, lanediflook),
    names_sep = "_")
#### GAZE ACROSS VELOCITY RANKS ####

#create reference table for velocity sets
velocity_table <- tribble(
  ~var_name, ~set, ~vel1, ~vel2, ~vel3,
  "no",         1,    10.00, 10.00, 10.00,
  "no",         2,    12.00, 12.00, 12.00,
  "medium",     1,     7.16, 10.00, 12.84,
  "medium",     2,     8.59, 12.00, 15.41,
  "high",       1,     3.51, 10.00, 16.49,
  "high",       2,     4.22, 12.00, 19.78
)

#create function to infer set and velocity based on starting position and unique velocity pairings
infer_car_velocities <- function(df) {
  initial_positions <- df %>%
    group_by(participant, session, trial_within_session) %>%
    slice(1) %>%
    select(participant, session, trial_within_session, var_name, ttc,
           car1_posx, car2_posx, car3_posx, car4_posx, car5_posx, car6_posx)
  
  trial_velocities <- initial_positions %>%
    rowwise() %>%
    mutate(
      car1_vel_calc = abs(car1_posx) / ttc,
      car2_vel_calc = abs(car2_posx) / ttc,
      car3_vel_calc = abs(car3_posx) / ttc,
      car4_vel_calc = abs(car4_posx) / ttc,
      car5_vel_calc = abs(car5_posx) / ttc,
      car6_vel_calc = abs(car6_posx) / ttc,
      
      car1_rank_group1 = rank(c(car1_vel_calc, car2_vel_calc, car3_vel_calc))[1],
      car2_rank_group1 = rank(c(car1_vel_calc, car2_vel_calc, car3_vel_calc))[2],
      car3_rank_group1 = rank(c(car1_vel_calc, car2_vel_calc, car3_vel_calc))[3],
      
      car4_rank_group2 = rank(c(car4_vel_calc, car5_vel_calc, car6_vel_calc))[1],
      car5_rank_group2 = rank(c(car4_vel_calc, car5_vel_calc, car6_vel_calc))[2],
      car6_rank_group2 = rank(c(car4_vel_calc, car5_vel_calc, car6_vel_calc))[3]
    ) %>%
    ungroup()
  
  trial_velocities <- trial_velocities %>%
    rowwise() %>%
    mutate(
      sorted_vels_123 = list(sort(c(car1_vel_calc, car2_vel_calc, car3_vel_calc))),
      
      vel_set1 = list(velocity_table %>% filter(var_name == .data$var_name, set == 1) %>% select(vel1, vel2, vel3) %>% unlist()),
      vel_set2 = list(velocity_table %>% filter(var_name == .data$var_name, set == 2) %>% select(vel1, vel2, vel3) %>% unlist()),
      
      set1_dist = sum((sorted_vels_123[[1]] - vel_set1[[1]])^2),
      set2_dist = sum((sorted_vels_123[[1]] - vel_set2[[1]])^2),
      
      inferred_set = ifelse(set1_dist < set2_dist, 1, 2)
    ) %>%
    ungroup()
  
  trial_velocities <- trial_velocities %>%
    left_join(velocity_table, by = c("var_name", "inferred_set" = "set")) %>%
    mutate(
      car1_vel = case_when(
        car1_rank_group1 == 1 ~ vel1,
        car1_rank_group1 == 2 ~ vel2,
        car1_rank_group1 == 3 ~ vel3
      ),
      car2_vel = case_when(
        car2_rank_group1 == 1 ~ vel1,
        car2_rank_group1 == 2 ~ vel2,
        car2_rank_group1 == 3 ~ vel3
      ),
      car3_vel = case_when(
        car3_rank_group1 == 1 ~ vel1,
        car3_rank_group1 == 2 ~ vel2,
        car3_rank_group1 == 3 ~ vel3
      ),
      car4_vel = case_when(
        car4_rank_group2 == 1 ~ vel1,
        car4_rank_group2 == 2 ~ vel2,
        car4_rank_group2 == 3 ~ vel3
      ),
      car5_vel = case_when(
        car5_rank_group2 == 1 ~ vel1,
        car5_rank_group2 == 2 ~ vel2,
        car5_rank_group2 == 3 ~ vel3
      ),
      car6_vel = case_when(
        car6_rank_group2 == 1 ~ vel1,
        car6_rank_group2 == 2 ~ vel2,
        car6_rank_group2 == 3 ~ vel3
      ),
      
      car1_rank = ifelse(var_name == "no", 2, car1_rank_group1),
      car2_rank = ifelse(var_name == "no", 2, car2_rank_group1),
      car3_rank = ifelse(var_name == "no", 2, car3_rank_group1),
      car4_rank = ifelse(var_name == "no", 2, car4_rank_group2),
      car5_rank = ifelse(var_name == "no", 2, car5_rank_group2),
      car6_rank = ifelse(var_name == "no", 2, car6_rank_group2)
    ) %>%
    select(participant, session, trial_within_session, var_name, ttc, inferred_set,
           car1_vel, car2_vel, car3_vel, car4_vel, car5_vel, car6_vel,
           car1_rank, car2_rank, car3_rank, car4_rank, car5_rank, car6_rank)
  
  return(trial_velocities)
}

car_velocities_trials_conf_df <- infer_car_velocities(cont_conf_df)
car_velocities_trials_cross_befmov_df <- infer_car_velocities(cont_cross_befmov_df)

#define function to have the proportion of timeruns each car is looked at per trial, with velocity rank information
prepare_looking_by_velocity_data <- function(cont_df, velocity_df) {
  
  trial_looking <- cont_df %>%
    lazy_dt() %>%
    filter(as.character(var_name) != "no") %>%
    group_by(participant, session, trial_within_session, var_name, ttc) %>%
    summarize(
      n_timeruns = n(),
      car1_looks = sum(car1_look, na.rm = TRUE),
      car2_looks = sum(car2_look, na.rm = TRUE),
      car3_looks = sum(car3_look, na.rm = TRUE),
      car4_looks = sum(car4_look, na.rm = TRUE),
      car5_looks = sum(car5_look, na.rm = TRUE),
      car6_looks = sum(car6_look, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      car1_prop = car1_looks / n_timeruns,
      car2_prop = car2_looks / n_timeruns,
      car3_prop = car3_looks / n_timeruns,
      car4_prop = car4_looks / n_timeruns,
      car5_prop = car5_looks / n_timeruns,
      car6_prop = car6_looks / n_timeruns
    ) %>%
    as_tibble()
  
  trial_with_vel <- trial_looking %>%
    mutate(var_name = as.character(var_name)) %>%
    left_join(
      velocity_df, 
      by = c("participant", "session", "trial_within_session", "ttc", "var_name")
    )
  
  trial_level <- trial_with_vel %>%
    pivot_longer(
      cols = c(car1_prop, car2_prop, car3_prop, car4_prop, car5_prop, car6_prop),
      names_to = "car",
      values_to = "prop_looking"
    ) %>%
    mutate(
      car = str_remove(car, "car"),
      car = str_remove(car, "_prop"),
      car_num = as.numeric(car),
      velocity_rank = case_when(
        car_num == 1 ~ car1_rank,
        car_num == 2 ~ car2_rank,
        car_num == 3 ~ car3_rank,
        car_num == 4 ~ car4_rank,
        car_num == 5 ~ car5_rank,
        car_num == 6 ~ car6_rank
      ),
      velocity = case_when(
        car_num == 1 ~ car1_vel,
        car_num == 2 ~ car2_vel,
        car_num == 3 ~ car3_vel,
        car_num == 4 ~ car4_vel,
        car_num == 5 ~ car5_vel,
        car_num == 6 ~ car6_vel
      )
    ) %>%
    select(participant, session, trial_within_session, var_name, ttc, 
           car_num, velocity_rank, velocity, prop_looking)
  
  return(trial_level)
}

proplook_eachcar_conf_df <- prepare_looking_by_velocity_data(cont_conf_df, car_velocities_trials_conf_df) #run for confidence condition
proplook_eachcar_cross_befmov_df <- prepare_looking_by_velocity_data(cont_cross_befmov_df, car_velocities_trials_cross_befmov_df) #run for cross_befmov

cont_conf_withinmatchedrt_df <- cont_conf_df %>% 
  left_join(cross_matchedrts_ttcvar_sin, by = c("participant", "ttc", "var_name")) %>%
  mutate(within_matched_rt = ifelse(timerun > matched_rt,0,1)) %>%
  filter(within_matched_rt == 1) #create subset of cont_conf_df that belong to timeruns matching time before sidewalk leaving times in the cross condition

proplook_eachcar_conf_withinmatchedrt_df <- prepare_looking_by_velocity_data(cont_conf_withinmatchedrt_df, car_velocities_trials_conf_df) #run for confidence condition within matched rt

#summary dfs
proplook_velrank_near_cross_befmov_sin <- proplook_eachcar_cross_befmov_df %>%
  filter(car_num %in% c(1,2,3)) %>%
  group_by(participant,var_name,velocity_rank) %>%
  summarise(prop_looking = mean(prop_looking)) #cross_befmov, only 3 nearest cars, individual summaries

proplook_velrank_near_cross_befmov_sav <- proplook_velrank_near_cross_befmov_sin %>%
  group_by(var_name,velocity_rank) %>%
  summarise_with_ci(prop_looking) #cross_befmov, only 3 nearest cars, across_participant summaries


proplook_velrank_near_conf_withinmatchedrt_sin <- proplook_eachcar_conf_withinmatchedrt_df %>%
  filter(car_num %in% c(1,2,3)) %>%
  group_by(participant,var_name,velocity_rank) %>%
  summarise(prop_looking = mean(prop_looking)) #conf_withinmatchedrt, only 3 nearest cars, individual summaries

proplook_velrank_near_conf_withinmatchedrt_sav <- proplook_velrank_near_conf_withinmatchedrt_sin %>%
  group_by(var_name,velocity_rank) %>%
  summarise_with_ci(prop_looking) #conf_withinmatchedrt, only 3 nearest cars, across_participant summaries


#### CALCULATE PROPORTION OF LOOKING AT EACH OF THE THREE NEAREST CARS INDIVIDUALLY, FOR TIMERUNS BEFORE MOVEMENT INITIATION AND MATCHED RTS ####
calculate_propcarlook_eachnearcar_novar <- function(df) {
  
  dftoret <- df %>%
    lazy_dt() %>%
    filter(as.character(var_name) == "no") %>%
    group_by(participant, session, trial_within_session, var_name, ttc) %>%
    summarize(
      n_timeruns = n(),
      car1_looks = sum(car1_look, na.rm = TRUE),
      car2_looks = sum(car2_look, na.rm = TRUE),
      car3_looks = sum(car3_look, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      car1_prop = car1_looks / n_timeruns,
      car2_prop = car2_looks / n_timeruns,
      car3_prop = car3_looks / n_timeruns
    ) %>%
    as_tibble()
  
  return(dftoret)
  
}

propcarlook_eachnearcar_novar_conf <- calculate_propcarlook_eachnearcar_novar(cont_conf_withinmatchedrt_df)
propcarlook_eachnearcar_novar_cross_befmov <- calculate_propcarlook_eachnearcar_novar(cont_cross_befmov_df)

propcarlook_eachnearcar_novar_sin <- rbind(
  propcarlook_eachnearcar_novar_conf %>% mutate(Condition = "Confidence condition"),
  propcarlook_eachnearcar_novar_cross_befmov %>% mutate(Condition = "Cross condition")) %>%
  group_by(participant,Condition,var_name) %>%
  summarise(
    car1 = mean(car1_prop, na.rm = TRUE),
    car2 = mean(car2_prop, na.rm = TRUE),
    car3 = mean(car3_prop, na.rm = TRUE)) %>%
  pivot_longer(cols = c(car1,car2,car3), names_to = "car", values_to = "prop_looking") %>%
  mutate(
    car_rank = case_when(
      car == "car1" ~ 1,
      car == "car2" ~ 2,
      car == "car3" ~ 3))

propcarlook_eachnearcar_novar_sav <- propcarlook_eachnearcar_novar_sin %>%
  group_by(Condition,var_name,car,car_rank) %>%
  summarise_with_ci(prop_looking)



#DO SUMMARIES HERE XXX !

#### MANIPULATIONS FOR FIGURE THRESHOLD CORRELATION ####
get_correlation_string <- function(df, var1, var2) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  
  result <- cor.test(dplyr::pull(df, !!var1), dplyr::pull(df, !!var2))
  r <- unname(result$estimate)
  p <- result$p.value
  
  create_correlation_string(r, p)
}

do_ortfit_tibble <- function(df, var1, var2) {
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  
  ortreg <- orthogonal_regression(dplyr::pull(df, !!var1), dplyr::pull(df, !!var2))
  as_tibble(ortreg)
}

dotsforcors_thres_var_sd <- psyfit_thresholds_sin %>%
  filter(!participant %in% outlier_subjs) %>%
  select(-prob) %>%
  pivot_wider(
    names_from = cond,
    values_from = thre) %>%
  rename(var_sd = var_name)

corstrings_thres_var_sd <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs),conf, cross),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs),conf, cross),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs),conf, cross)
  ),
  var_sd_forfacet = c("no variability","medium variability","high variability")
)

thres_ortfits <- rbind(
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs), conf,cross),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs), conf,cross),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs), conf,cross)
)
thres_ortfits$var_sd <- c("no","medium","high")
thres_ortfits$var_sd_forfacet <- c("no variability","medium variability","high variability")

dotsforcors_thres_var_sd <- left_join(dotsforcors_thres_var_sd,thres_ortfits)

dotsforcors_thres_var_sd$var_sd_forfacet <- factor(dotsforcors_thres_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_thres_var_sd$var_sd_forfacet <- factor(corstrings_thres_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))

#### MANIPULATIONS FOR FIGURE THRESHOLD CORRELATION INVOLVING RISK CONDITION ####

#confidence and risk
corstrings_thres_conf_risk_var_sd <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs_withrisk),conf, risk),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs_withrisk),conf, risk),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs_withrisk),conf, risk)
  ),
  var_sd_forfacet = c("no variability","medium variability","high variability")
)

thres_ortfits_conf_risk <- rbind(
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs_withrisk), conf,risk),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs_withrisk), conf,risk),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs_withrisk), conf,risk)
)
thres_ortfits_conf_risk$var_sd <- c("no","medium","high")
thres_ortfits_conf_risk$var_sd_forfacet <- c("no variability","medium variability","high variability")

dotsforcors_thres_var_sd_conf_risk <- left_join(
  dotsforcors_thres_var_sd %>% 
    filter(!participant %in% outlier_subjs_withrisk) %>%
    select(participant,var_sd,conf,risk),
  thres_ortfits_conf_risk)

dotsforcors_thres_var_sd_conf_risk$var_sd_forfacet <- factor(dotsforcors_thres_var_sd_conf_risk$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_thres_conf_risk_var_sd$var_sd_forfacet <- factor(corstrings_thres_conf_risk_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))

#risk and cross
corstrings_thres_risk_cross_var_sd <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs_withrisk),risk, cross),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs_withrisk),risk, cross),
    get_correlation_string(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs_withrisk),risk, cross)
  ),
  var_sd_forfacet = c("no variability","medium variability","high variability")
)

thres_ortfits_risk_cross <- rbind(
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "no" & !participant %in% outlier_subjs_withrisk), risk, cross),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "medium" & !participant %in% outlier_subjs_withrisk), risk, cross),
  do_ortfit_tibble(dotsforcors_thres_var_sd %>% filter(var_sd == "high" & !participant %in% outlier_subjs_withrisk), risk, cross)
)
thres_ortfits_risk_cross$var_sd <- c("no","medium","high")
thres_ortfits_risk_cross$var_sd_forfacet <- c("no variability","medium variability","high variability")

dotsforcors_thres_var_sd_risk_cross <- left_join(
  dotsforcors_thres_var_sd %>% 
    filter(!participant %in% outlier_subjs_withrisk) %>%
    select(participant,var_sd,risk,cross),
  thres_ortfits_risk_cross)

dotsforcors_thres_var_sd_risk_cross$var_sd_forfacet <- factor(dotsforcors_thres_var_sd_risk_cross$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_thres_risk_cross_var_sd$var_sd_forfacet <- factor(corstrings_thres_risk_cross_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))

#### MANIPULATIONS FOR CORRELATION INVOLVING THRESHOLDS AND SIDEWALK LEAVING TIMES ####

dotsforcors_thres_kin_em_wide_filt <- dotsforcors_thres_kin_em_wide %>% 
  filter(!participant %in% outlier_subjs)

corstrings_thres_rt <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
    
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
    
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross)
    
  ),
  var_name_forfacet = rep(c("no variability","medium variability","high variability"),3),
  cond = rep(c("conf","risk","cross"), each = 3)
)

ortfits_thres_rt <- rbind(
  
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs),thre_conf, leaving_time_norm_cross),
  
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs_withrisk),thre_risk, leaving_time_norm_cross),
  
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high" & !participant %in% outlier_subjs),thre_cross, leaving_time_norm_cross)
)

ortfits_thres_rt$cond <- rep(c("conf","risk","cross"),each=3)
ortfits_thres_rt$var_name <- rep(c("no","medium","high"),3)
ortfits_thres_rt$var_name_forfacet <- rep(c("no variability","medium variability","high variability"),3)

dotsforcors_thres_rt <- left_join(
  dotsforcors_thres_kin_em %>%
    select(participant,cond,var_name,thre,leaving_time_norm),
  ortfits_thres_rt)

dotsforcors_thres_rt$var_name_forfacet <- factor(dotsforcors_thres_rt$var_name_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_thres_rt$var_name_forfacet <- factor(corstrings_thres_rt$var_name_forfacet, levels = c("no variability","medium variability","high variability"))

#### CORRELATIONS PROPORTIONS EYE MOVEMENTS CONFIDENCE AND CROSS CONDITIONS ####

corstrings_propcarlook <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no"),propcarlook_conf, propcarlook_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium"),propcarlook_conf, propcarlook_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high"),propcarlook_conf, propcarlook_cross)
  ),
  var_name_forfacet = c("no variability","medium variability","high variability"))

corstrings_lanediflook <- tibble(
  label = c(
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no"),lanediflook_conf, lanediflook_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium"),lanediflook_conf, lanediflook_cross),
    get_correlation_string(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high"),lanediflook_conf, lanediflook_cross)
  ),
  var_name_forfacet = c("no variability","medium variability","high variability"))

ortfits_propcarlook <- rbind(
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" ),propcarlook_conf, propcarlook_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium"),propcarlook_conf, propcarlook_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high"),propcarlook_conf, propcarlook_cross))

ortfits_propcarlook$var_name <- c("no","medium","high")
ortfits_propcarlook$var_name_forfacet <- c("no variability","medium variability","high variability")

ortfits_lanediflook <- rbind(
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "no" ),lanediflook_conf, lanediflook_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "medium"),lanediflook_conf, lanediflook_cross),
  do_ortfit_tibble(dotsforcors_thres_kin_em_wide_filt %>% filter(var_name == "high"),lanediflook_conf, lanediflook_cross))

ortfits_lanediflook$var_name <- c("no","medium","high")
ortfits_lanediflook$var_name_forfacet <- c("no variability","medium variability","high variability")

dotsforcors_propcarlook <- left_join(
  dotsforcors_thres_kin_em_wide_filt %>%
    select(participant,var_name,propcarlook_conf,propcarlook_cross),
  ortfits_propcarlook)

dotsforcors_propcarlook$var_name_forfacet <- factor(dotsforcors_propcarlook$var_name_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_propcarlook$var_name_forfacet <- factor(corstrings_propcarlook$var_name_forfacet, levels = c("no variability","medium variability","high variability"))

dotsforcors_lanediflook <- left_join(
  dotsforcors_thres_kin_em_wide_filt %>%
    select(participant,var_name,lanediflook_conf,lanediflook_cross),
  ortfits_lanediflook)

dotsforcors_lanediflook$var_name_forfacet <- factor(dotsforcors_lanediflook$var_name_forfacet, levels = c("no variability","medium variability","high variability"))
corstrings_lanediflook$var_name_forfacet <- factor(corstrings_lanediflook$var_name_forfacet, levels = c("no variability","medium variability","high variability"))
#### REPLICATION EXPERIMENT WITH DIFFERENT TTCs, INITIAL MANIPULATIONS ####

#correct responses from sessions with switched controllers
disc_df_difttc <- disc_df_difttc %>%
  mutate(
    resp = case_when(
      participant == "pvs" & session == 1 ~ 1 - resp,
      participant == "abe" & session == 6 ~ 1 - resp,
      .default = resp
    )
  )

#manually define whether in cross condition trials a crossing was attempted or successful
crossings_attsuc_trials_difttc <- cont_cross_df_difttc %>%
  group_by(participant,cond,session,trial_within_session) %>%
  summarise(
    helmet_posz_max = max(helmet_posz),
    helmet_posz_last = last(helmet_posz)) %>%
  mutate(
    cross_att = ifelse(helmet_posz_max > -2, 1,0),
    cross_suc = ifelse(cross_att == 1 & helmet_posz_last >= 2.8, 1,0)) %>%
  ungroup() %>%
  select(participant,session,trial_within_session,cross_att, cross_suc)

disc_df_difttc <- left_join(disc_df_difttc,crossings_attsuc_trials_difttc)

disc_df_difttc <- disc_df_difttc %>%
  mutate(
    t_cross = ifelse(cross_att == 1,1,0), 
    cross = ifelse(cross_suc == 1,1,0))

#add new variables
disc_df_difttc <- disc_df_difttc %>%
  mutate(
    go = ifelse(cond == "cross",t_cross,resp),
    var_rank = case_when(
      vel1 == vel2 & vel2 == vel3 ~ 1,
      if_any(c(vel1, vel2, vel3), ~ . %in% c(7.16, 8.59)) ~ 2,
      if_any(c(vel1, vel2, vel3), ~ . %in% c(3.51, 4.22)) ~ 3),
    
    var_name = case_when(
      vel1 == vel2 & vel2 == vel3 ~ "no",
      if_any(c(vel1, vel2, vel3), ~ . %in% c(7.16, 8.59)) ~ "medium",
      if_any(c(vel1, vel2, vel3), ~ . %in% c(3.51, 4.22)) ~ "high"),
    
    var_sd = case_when(
      vel1 == vel2 & vel2 == vel3 ~ 0,
      if_any(c(vel1, vel2, vel3), ~ . == 7.16) ~ sd_vels_set1_medium,
      if_any(c(vel1, vel2, vel3), ~ . == 3.51) ~ sd_vels_set1_high,
      if_any(c(vel1, vel2, vel3), ~ . == 8.59) ~ sd_vels_set2_medium,
      if_any(c(vel1, vel2, vel3), ~ . == 4.22) ~ sd_vels_set2_high)
  )

#turning some variables into factors
disc_df_difttc$ttcg_factor <- factor(disc_df_difttc$ttcg)
disc_df_difttc$var_sd_factor <- factor(disc_df_difttc$var_sd)
disc_df_difttc$var_rank_factor <- factor(disc_df_difttc$var_rank)
disc_df_difttc$session_factor <- factor(disc_df_difttc$session)
disc_df_difttc$var_name <- factor(disc_df_difttc$var_name, levels = c("no", "medium", "high"))

#add variables from discrete df_difttc to continuous dfs
disc_df_difttc_cross_tojoin <- disc_df_difttc %>%
  filter(cond == "cross") %>%
  select(participant, cond, session, trial_within_session,ttcg,var_sd,ttcg_factor,var_sd_factor,var_name,t_cross,cross)

cont_cross_df_difttc <- left_join(cont_cross_df_difttc, disc_df_difttc_cross_tojoin, by= c("participant", "cond", "session", "trial_within_session","ttcg"))

disc_df_difttc_conf_tojoin <- disc_df_difttc %>%
  filter(cond == "conf") %>%
  select(participant, cond, session, trial_within_session,ttcg,var_sd,ttcg_factor,var_sd_factor,var_name)

cont_conf_df_difttc <- left_join(cont_conf_df_difttc, disc_df_difttc_conf_tojoin, by= c("participant", "cond", "session", "trial_within_session","ttcg"))

#### REPLICATION EXPERIMENT WITH DIFFERENT TTCs ####

disc_df_difttc_nofilt <- disc_df_difttc

unique_participants_difttc <- sort(unique(disc_df_difttc$participant))

outlier_subjs_difttc <- c("")

disc_df_difttc <- disc_df_difttc %>% filter(! participant %in% outlier_subjs_difttc)

cont_conf_df_difttc <- cont_conf_df_difttc %>% filter(! participant %in% outlier_subjs_difttc)
cont_cross_df_difttc <- cont_cross_df_difttc %>% filter(! participant %in% outlier_subjs_difttc)

#### REPLICATION EXPERIMENT WITH DIFFERENT TTCs, PSYCHOMETRIC FUNCTIONS SEPARATE PER VARIABILITY LEVEL ####

#fit
psyfit_results_difttc <- process_participants_difttc(
  df = disc_df_difttc_nofilt, 
  unique_participants = unique_participants_difttc, 
  df_name = "psyfit_difttc"
)

#store different fit results in individual dfs
psyfit_difttc_conf_averages <- psyfit_results_difttc$averages %>% filter(cond == "conf")
psyfit_difttc_cross_averages <- psyfit_results_difttc$averages %>% filter(cond == "cross")

psyfit_difttc_conf_curves <- psyfit_results_difttc$curves %>% filter(cond == "conf")
psyfit_difttc_cross_curves <- psyfit_results_difttc$curves %>% filter(cond == "cross")

psyfit_difttc_conf_thresholds <- psyfit_results_difttc$thresholds %>% filter(cond == "conf")
psyfit_difttc_cross_thresholds <- psyfit_results_difttc$thresholds %>% filter(cond == "cross")

#reorder factor levels for var_name
psyfit_difttc_conf_averages$var_name <- factor(psyfit_difttc_conf_averages$var_name, levels = c("no", "medium", "high"))
psyfit_difttc_conf_curves$var_name <- factor(psyfit_difttc_conf_curves$var_name, levels = c("no", "medium", "high"))
psyfit_difttc_conf_thresholds$var_name <- factor(psyfit_difttc_conf_thresholds$var_name, levels = c("no", "medium", "high"))
psyfit_difttc_cross_averages$var_name <- factor(psyfit_difttc_cross_averages$var_name, levels = c("no", "medium", "high"))
psyfit_difttc_cross_curves$var_name <- factor(psyfit_difttc_cross_curves$var_name, levels = c("no", "medium", "high"))
psyfit_difttc_cross_thresholds$var_name <- factor(psyfit_difttc_cross_thresholds$var_name, levels = c("no", "medium", "high"))

#extract threshold dfs and create across-participant summaries
psyfit_difttc_thresholds_sin <- psyfit_results_difttc$thresholds %>%
  filter(! participant %in% participants_to_exclude)

psyfit_difttc_thresholds_sav <- psyfit_difttc_thresholds_sin %>%
  filter(! participant %in% outlier_subjs_difttc) %>%
  group_by(cond,var_name) %>%
  summarise_with_ci(thre)

#### REPLICATION EXPERIMENT WITH DIFFERENT TTCs, TIME AT WHICH SIDEWALK IS LEFT (RT) FOR ATTEMPTED CROSSINGS, AND VELOCITY VARIABLES ####
cont_cross_df_difttc <- cont_cross_df_difttc %>%
  arrange(participant,session,trial_within_session,timerun) %>%
  group_by(participant, session, trial_within_session) %>%
  mutate(
    velocity_unfilt = velocity,
    velocity = ifelse(velocity > -1 & velocity < 3, velocity, NA),
    ismoving = velocity > velocity_threshold,
    ismoving = replace_na(ismoving, FALSE),
    movement_segment = cumsum(c(TRUE, diff(ismoving) != 0))
  )

cont_cross_df_movementsegments_difttc <- cont_cross_df_difttc %>%
  group_by(participant, session, trial_within_session, movement_segment) %>%
  filter(all(ismoving) & n() >= threshold_cont_timepoints)

movinitrt_df_difttc <- cont_cross_df_difttc %>%
  filter(t_cross == 1 & helmet_posz >= -3) %>%
  group_by(participant,session,trial_within_session,ttcg,var_name) %>%
  summarise(
    leaving_time = first(timerun)) %>%
  mutate(
    leaving_time_norm = leaving_time / ttcg,
    leaving_time_tottc = ttcg - leaving_time)

movementsegments_trialsum_difttc <- cont_cross_df_movementsegments_difttc %>% 
  filter(t_cross == 1) %>%
  group_by(participant,session,trial_within_session,ttcg,var_name) %>%
  summarise(
    peakvel = max(velocity, na.rm = TRUE),
    timetopeakvel = timerun[which.max(velocity)],
    meanvel = mean(velocity, na.rm = TRUE))

movementsegments_trialsum_difttc <- left_join(movementsegments_trialsum_difttc, movinitrt_df_difttc)

movend_df_difttc <- cont_cross_df_difttc %>%
  filter(t_cross == 1 & helmet_posz <= 3) %>%
  group_by(participant,session,trial_within_session,ttcg,var_name) %>%
  summarise(last_mtime = last(timerun))

movementsegments_trialsum_difttc <- left_join(movementsegments_trialsum_difttc, movend_df_difttc)

movementsegments_trialsum_difttc <- movementsegments_trialsum_difttc %>%
  mutate(
    movement_time = last_mtime - leaving_time,
    movement_time_norm = movement_time / ttcg)

#summaries leaving time
leaving_time_norm_var_difttc_sin <- movinitrt_df_difttc %>%   
  group_by(participant,var_name) %>%
  summarise(leaving_time_norm = mean(leaving_time_norm))
leaving_time_norm_var_difttc_sin$var_name <- factor(leaving_time_norm_var_difttc_sin$var_name, levels = c("no","medium","high"))

leaving_time_norm_var_difttc_sav <- leaving_time_norm_var_difttc_sin %>%
  group_by(var_name) %>%
  summarise_with_ci(leaving_time_norm)

#summaries peak vel
peakvel_varname_difttc_sin <- movementsegments_trialsum_difttc %>%
  group_by(participant, var_name) %>%
  summarise(peakvel = mean(peakvel, na.rm = TRUE))

peakvel_varname_difttc_sin$var_name <- factor(peakvel_varname_difttc_sin$var_name, levels = c("no","medium", "high"))

peakvel_varname_difttc_sav <- peakvel_varname_difttc_sin %>%
  group_by(var_name) %>%
  summarise_with_ci(peakvel)

