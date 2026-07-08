#### DEFINE OUTLIERS ####

################################### MAIN TEXT ################################### 

#### PSYCHOMETRIC THRESHOLDS ####

#thresholds across variabilities, confidence condition
modthre_sd_conf <- lmer(thre ~ var_name + (1 | participant), data = psyfit_thresholds_sin %>% filter(! participant %in% outlier_subjs & cond == "conf"))
anova(modthre_sd_conf)

r2beta(modthre_sd_conf, method = 'nsj', partial = TRUE) #R² + 95% CI
#effectsize::omega_squared(modthre_sd_conf, partial = TRUE, ci = 0.95) #effect size and 95% CI

emm_modthre_sd_conf <- emmeans(modthre_sd_conf, ~ var_name) #estimated marginal means for var_name
summary(emm_modthre_sd_conf, type = "response") #probability estimates for each variability level
summary(contrast(emm_modthre_sd_conf, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#thresholds across variabilities, cross condition
modthre_sd_cross <- lmer(thre ~ var_name + (1 | participant), data = psyfit_thresholds_sin %>% filter(! participant %in% outlier_subjs & cond == "cross"))
anova(modthre_sd_cross)

r2beta(modthre_sd_cross, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_modthre_sd_cross <- emmeans(modthre_sd_cross, ~ var_name) #estimated marginal means for var_name
summary(emm_modthre_sd_cross, type = "response") #probability estimates for each variability level
summary(contrast(emm_modthre_sd_cross, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#effect of variability on successful crosses
# mod_succcrosses_sd <- afex::mixed(cross ~ var_name + (1|participant), data=disc_df %>% filter(! participant %in% outlier_subjs & cond == "cross" & t_cross == 1), method = "LRT", family = binomial)
# anova(mod_succcrosses_sd)
# 
# emm_mod_succcrosses_sd <- emmeans(mod_succcrosses_sd, ~ var_name) #cross model, estimated marginal means for var_name
# summary(emm_mod_succcrosses_sd, type = "response") #probability estimates for each variability level
# summary(contrast(emm_mod_succcrosses_sd, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences
# 
# #effect of TTC on successful crosses
# mod_succcrosses_ttc <- afex::mixed(cross ~ ttc + (1|participant), data=disc_df %>% filter(! participant %in% outlier_subjs & cond == "cross" & t_cross == 1), method = "LRT", family = binomial)
# anova(mod_succcrosses_ttc)

#modulation of car velocity variability on the psychometric function between TTC and crossing success
fit_perf_subjs <- quickpsy(
  disc_df %>% filter(cond == "cross" & t_cross == 1) , #only cross condition trials where crossing is attempted
  ttc,
  k = cross, #DV is crossing success
  grouping = .(participant,var_name),
  guess = FALSE,
  lapses = FALSE,
  bootstrap = 'none'
)

1 - (fit_perf_subjs$par %>% filter(parn == "p2" & par<15 & par>0) %>% nrow() / fit_perf_subjs$par %>% filter(parn == "p2") %>% nrow()) #proportion of fits to remove ( because of outlier SD values )

modparvar_sd <- lmer(par~var_name+(1|participant),data = fit_perf_subjs$par %>% filter(parn == "p2" & par<15 & par>0) )
anova(modparvar_sd)

r2beta(modparvar_sd, method = 'nsj', partial = TRUE) #R² + 95% CI

1 - (fit_perf_subjs$thresholds %>% filter(thre > 0 & thre < 20) %>% nrow() / fit_perf_subjs$thresholds %>% nrow()) #proportion of fits to remove ( because of outlier threshold values )

modparvar_thre <- lmer(thre ~ var_name + (1|participant), data = fit_perf_subjs$thresholds %>% filter(thre > 0 & thre < 20))
anova(modparvar_thre)

r2beta(modparvar_thre, method = 'nsj', partial = TRUE) #R² + 95% CI

#correlations between confidence and attempted crossings, split by variability level

cor.test(
  dotsforcors_thres_var_sd$conf[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "no"],
  dotsforcors_thres_var_sd$cross[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "no"]
) #no variability trials

cor.test(
  dotsforcors_thres_var_sd$conf[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "medium"],
  dotsforcors_thres_var_sd$cross[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "medium"]
) #medium variability trials

cor.test(
  dotsforcors_thres_var_sd$conf[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "high"],
  dotsforcors_thres_var_sd$cross[!(dotsforcors_thres_var_sd$participant %in% outlier_subjs) & dotsforcors_thres_var_sd$var_sd == "high"]
) #high variability trials

#### KINEMATICS ####

#predicting leaving time
mod_leaving_time_norm <- lmer(leaving_time_norm ~ ttc + var_name + (1 | participant), data = movinitrt_df)
anova(mod_leaving_time_norm) #anova

r2beta(mod_leaving_time_norm, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_leaving_time_norm <- emmeans(mod_leaving_time_norm, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_leaving_time_norm, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_leaving_time_norm, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#predicting peak velocity
mod_peakvelrt <- lmer(peakvel ~ ttc + var_name + leaving_time_norm  + (1 | participant), data = movementsegments_trialsum) #model predicting peak velocity by leaving time, controlling for TTC and variability
anova(mod_peakvelrt) #anova

r2beta(mod_peakvelrt, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_peakvelrt <- emmeans(mod_peakvelrt, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_peakvelrt, type = "response")
summary(contrast(emm_mod_peakvelrt, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted pairwise comparisons

#individual slopes normalized sidewalk leaving time predicting peak vel
df_lt_peakvel_slopes <- movementsegments_trialsum %>%
  group_by(participant,var_name) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(peakvel ~ leaving_time_norm, data = .x)),
    tidy_model = map(model, ~ broom::tidy(.x))
  ) %>%
  unnest(tidy_model) %>%
  filter(term == "leaving_time_norm") %>%  # Only keep slope estimates
  select(participant, estimate, p.value) %>%
  rename(slope = estimate)

df_lt_peakvel_slopes %>% filter(var_name == "no" & slope > 0) %>% nrow()
df_lt_peakvel_slopes %>% filter(var_name == "medium" & slope > 0) %>% nrow()
df_lt_peakvel_slopes %>% filter(var_name == "high" & slope > 0) %>% nrow()

#### EYE MOVEMENTS ####
#### EYE MOVEMENT DYNAMICS, PEAKS AND TROUGHS ANALYSIS ####
#proportion car-looking time, confidence condition
mod_propcarlook_firstpeak_conf <- lmer(peak_time ~ var_name * go_factor + (1 | participant), data = peaktroughs_propcarlook_conf)
anova(mod_propcarlook_firstpeak_conf)

r2beta(mod_propcarlook_firstpeak_conf, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_propcarlook_firstpeak_conf_go_factor <- emmeans(mod_propcarlook_firstpeak_conf, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_propcarlook_firstpeak_conf_go_factor, type = "response")

mod_propcarlook_firsttrough_conf <- lmer(trough_height ~ var_name * go_factor + (1 | participant), data = peaktroughs_propcarlook_conf)
anova(mod_propcarlook_firsttrough_conf)

r2beta(mod_propcarlook_firsttrough_conf, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_propcarlook_firsttrough_conf_go_factor <- emmeans(mod_propcarlook_firsttrough_conf, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_propcarlook_firsttrough_conf_go_factor, type = "response")

#proportion car-looking time, cross condition
mod_peaktroughs_propcarlook_cross_befmov <- lmer(carslooked ~ var_name * go_factor + (1 | participant), data = peaktroughs_propcarlook_cross_befmov)
anova(mod_peaktroughs_propcarlook_cross_befmov)

r2beta(mod_peaktroughs_propcarlook_cross_befmov, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_peaktroughs_propcarlook_cross_befmov <- emmeans(mod_peaktroughs_propcarlook_cross_befmov, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_peaktroughs_propcarlook_cross_befmov, type = "response")

#difference far minus near looking time, confidence condition
mod_diflanelook_firsttrough_conf <- lmer(trough_time ~ var_name * go_factor + (1 | participant), data = peaktroughs_diflanelook_conf)
anova(mod_diflanelook_firsttrough_conf)

r2beta(mod_diflanelook_firsttrough_conf, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_diflanelook_firsttrough_conf_go_factor <- emmeans(mod_diflanelook_firsttrough_conf, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_diflanelook_firsttrough_conf_go_factor, type = "response")

mod_diflanelook_firstpeak_conf <- lmer(peak_time ~ var_name * go_factor + (1 | participant), data = peaktroughs_diflanelook_conf)
anova(mod_diflanelook_firstpeak_conf)

r2beta(mod_diflanelook_firstpeak_conf, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_diflanelook_firstpeak_conf_go_factor <- emmeans(mod_diflanelook_firstpeak_conf, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_diflanelook_firstpeak_conf_go_factor, type = "response")

#difference far minus near looking time, cross condition
mod_peaktroughs_diflanelook_cross_befmov <- lmer(carslane_looked_dif ~ var_name * go_factor + (1 | participant), data = peaktroughs_diflanelook_cross_befmov)
anova(mod_peaktroughs_diflanelook_cross_befmov)

r2beta(mod_peaktroughs_diflanelook_cross_befmov, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_peaktroughs_diflanelook_cross_befmov_go_factor <- emmeans(mod_peaktroughs_diflanelook_cross_befmov, ~ go_factor) #estimated marginal means for go_factor
summary(emm_mod_peaktroughs_diflanelook_cross_befmov_go_factor, type = "response")

########################### SUPPLEMENTARY MATERIAL ########################### 
#### NON-PSYCHOMETRIC RESPONSES ANALYSES ####

#responses across variabilities, confidence condition
mod_go_ttc_var_conf <- afex::mixed(go ~ ttc + var_name + (1|participant), data=disc_df %>% filter(cond == "conf"), method = "LRT", family = binomial) #model
anova(mod_go_ttc_var_conf)

emm_mod_go_ttc_var_conf <- emmeans(mod_go_ttc_var_conf, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_go_ttc_var_conf, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_go_ttc_var_conf, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#responses across variabilities, cross condition
mod_go_ttc_var_cross <- afex::mixed(go ~ ttc + var_name + (1|participant), data=disc_df %>% filter(cond == "cross"), method = "LRT", family = binomial) #model
anova(mod_go_ttc_var_cross)

emm_mod_go_ttc_var_cross <- emmeans(mod_go_ttc_var_cross, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_go_ttc_var_cross, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_go_ttc_var_cross, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#correlations between confidence and attempted crossings (as logit of responses), split by variability level

cor.test(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "no"],
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "no"]) #no variability trials

cor.test(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "medium"],
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "medium"]) #medium variability trials

cor.test(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "high"],
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "high"]) #high variability trials

#### ANALYSES RISK CONDITION ####

#thresholds across variabilities, risk condition
modthre_sd_risk <- lmer(thre ~ var_name + (1 | participant), data = psyfit_thresholds_sin %>% filter(! participant %in% outlier_subjs_withrisk & cond == "risk"))
anova(modthre_sd_risk)

r2beta(modthre_sd_risk, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_modthre_sd_risk <- emmeans(modthre_sd_risk, ~ var_name) #estimated marginal means for var_name
summary(emm_modthre_sd_risk, type = "response") #probability estimates for each variability level
summary(contrast(emm_modthre_sd_risk, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#responses across variabilities, risk condition
mod_go_ttc_var_risk <- afex::mixed(go ~ ttc + var_name + (1|participant), data=disc_df %>% filter(cond == "risk"), method = "LRT", family = binomial) #model
anova(mod_go_ttc_var_risk)

emm_mod_go_ttc_var_risk <- emmeans(mod_go_ttc_var_risk, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_go_ttc_var_risk, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_go_ttc_var_risk, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#correlations risk and confidence / attempted crossings

cor.test(
  dotsforcors_thres_var_sd_conf_risk$conf[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "no"],
  dotsforcors_thres_var_sd_conf_risk$risk[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "no"]
) #with confidence, no variability trials

cor.test(
  dotsforcors_thres_var_sd_conf_risk$conf[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "medium"],
  dotsforcors_thres_var_sd_conf_risk$risk[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "medium"]
) #with confidence, medium variability trials

cor.test(
  dotsforcors_thres_var_sd_conf_risk$conf[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "high"],
  dotsforcors_thres_var_sd_conf_risk$risk[!(dotsforcors_thres_var_sd_conf_risk$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_conf_risk$var_sd == "high"]
) #with confidence, high variability trials

cor.test(
  dotsforcors_thres_var_sd_risk_cross$risk[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "no"],
  dotsforcors_thres_var_sd_risk_cross$cross[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "no"]
) #with cross, no variability trials

cor.test(
  dotsforcors_thres_var_sd_risk_cross$risk[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "medium"],
  dotsforcors_thres_var_sd_risk_cross$cross[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "medium"]
) #with cross, medium variability trials

cor.test(
  dotsforcors_thres_var_sd_risk_cross$risk[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "high"],
  dotsforcors_thres_var_sd_risk_cross$cross[!(dotsforcors_thres_var_sd_risk_cross$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_var_sd_risk_cross$var_sd == "high"]
) #with cross, high variability trials


#### CORRELATIONS BETWEEN THRESHOLDS AND SIDEWALK LEAVING TIME ####

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "no"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "no"]) #confidence condition, no variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "medium"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "medium"]) #confidence condition, medium variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "high"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "conf" & dotsforcors_thres_rt$var_name == "high"]) #confidence condition, high variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "no"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "no"]) #risk condition, no variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "medium"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "medium"]) #risk condition, medium variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "high"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs_withrisk) & dotsforcors_thres_rt$cond == "risk" & dotsforcors_thres_rt$var_name == "high"]) #risk condition, high variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "no"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "no"]) #cross condition, no variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "medium"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "medium"]) #cross condition, medium variability trials

cor.test(
  dotsforcors_thres_rt$thre[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "high"],
  dotsforcors_thres_rt$leaving_time_norm[!(dotsforcors_thres_rt$participant %in% outlier_subjs) & dotsforcors_thres_rt$cond == "cross" & dotsforcors_thres_rt$var_name == "high"]) #cross condition, high variability trials

#### CORRELATIONS PROPORTIONS EYE MOVEMENTS CONFIDENCE AND CROSS CONDITIONS ####

cor.test(
  dotsforcors_propcarlook$propcarlook_conf[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "no"],
  dotsforcors_propcarlook$propcarlook_cross[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "no"]) #proportion car looking time, no variability trials

cor.test(
  dotsforcors_propcarlook$propcarlook_conf[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "medium"],
  dotsforcors_propcarlook$propcarlook_cross[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "medium"]) #proportion car looking time, medium variability trials

cor.test(
  dotsforcors_propcarlook$propcarlook_conf[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "high"],
  dotsforcors_propcarlook$propcarlook_cross[!(dotsforcors_propcarlook$participant %in% outlier_subjs) & dotsforcors_propcarlook$var_name == "high"]) #proportion car looking time, high variability trials

cor.test(
  dotsforcors_lanediflook$lanediflook_conf[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "no"],
  dotsforcors_lanediflook$lanediflook_cross[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "no"]) #proportion car looking time far minus near lanes, no variability trials

cor.test(
  dotsforcors_lanediflook$lanediflook_conf[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "medium"],
  dotsforcors_lanediflook$lanediflook_cross[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "medium"]) #proportion car looking time far minus near lanes, medium variability trials

cor.test(
  dotsforcors_lanediflook$lanediflook_conf[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "high"],
  dotsforcors_lanediflook$lanediflook_cross[!(dotsforcors_lanediflook$participant %in% outlier_subjs) & dotsforcors_lanediflook$var_name == "high"]) #proportion car looking time far minus near lanes, high variability trials

#### GAZE ACROSS VELOCITY RANK ####
proplook_velrank_near_conf_withinmatchedrt_formod <- proplook_eachcar_conf_withinmatchedrt_df %>%
  filter(car_num %in% c(1, 2, 3)) %>%
  mutate(
    trial_id = paste(participant, session, trial_within_session, sep = "_"),
    velocity_rank = factor(velocity_rank),
    var_name = factor(var_name)) #cross condition, subset of data for the model

mod_proplook_velrank_near_conf_withinmatchedrt <- lmer(prop_looking ~ var_name + velocity_rank + (1 | participant / trial_id), data = proplook_velrank_near_conf_withinmatchedrt_formod) #lmm, conf_withinmatchedrt
anova(mod_proplook_velrank_near_conf_withinmatchedrt)

r2beta(mod_proplook_velrank_near_conf_withinmatchedrt, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_proplook_velrank_near_conf_withinmatchedrt <- emmeans (mod_proplook_velrank_near_conf_withinmatchedrt, ~ velocity_rank) #EMM
summary(emm_mod_proplook_velrank_near_conf_withinmatchedrt) #show EMMs
contrast(emm_mod_proplook_velrank_near_conf_withinmatchedrt, method = "pairwise", adjust = "bonferroni") #post-hoc comparisons across velocity ranks within each variability level

proplook_velrank_near_cross_befmov_formod <- proplook_eachcar_cross_befmov_df %>%
  filter(car_num %in% c(1, 2, 3)) %>%
  mutate(
    trial_id = paste(participant, session, trial_within_session, sep = "_"),
    velocity_rank = factor(velocity_rank),
    var_name = factor(var_name)) #confidence condition, subset of data for the model

mod_proplook_velrank_near_cross_befmov <- lmer(prop_looking ~ var_name + velocity_rank + (1 | participant / trial_id), data = proplook_velrank_near_cross_befmov_formod) #lmm, cross_befmov
anova(mod_proplook_velrank_near_cross_befmov)

r2beta(mod_proplook_velrank_near_cross_befmov, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_proplook_velrank_near_cross_befmov <- emmeans (mod_proplook_velrank_near_cross_befmov, ~ velocity_rank) #EMM
summary(emm_mod_proplook_velrank_near_cross_befmov) #show EMMs
contrast(emm_mod_proplook_velrank_near_cross_befmov, method = "pairwise", adjust = "bonferroni") #post-hoc comparisons across velocity ranks within each variability level

########################### SUPPLEMENTARY MATERIAL, FOLLOW-UP EXPERIMENT WITH DIFFERENT TTCs ########################### 

#### PSYCHOMETRIC THRESHOLDS ####

#thresholds across variabilities, confidence condition
modthre_sd_conf_difttc <- lmer(thre ~ var_name + (1 | participant), data = psyfit_difttc_thresholds_sin %>% filter(! participant %in% outlier_subjs_difttc & cond == "conf"))
anova(modthre_sd_conf_difttc)

r2beta(modthre_sd_conf_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_modthre_sd_conf_difttc <- emmeans(modthre_sd_conf_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_modthre_sd_conf_difttc, type = "response") #probability estimates for each variability level
summary(contrast(emm_modthre_sd_conf_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#thresholds across variabilities, cross condition
modthre_sd_cross_difttc <- lmer(thre ~ var_name + (1 | participant), data = psyfit_difttc_thresholds_sin %>% filter(! participant %in% outlier_subjs_difttc & cond == "cross"))
anova(modthre_sd_cross_difttc)

r2beta(modthre_sd_cross_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_modthre_sd_cross_difttc <- emmeans(modthre_sd_cross_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_modthre_sd_cross_difttc, type = "response") #probability estimates for each variability level
summary(contrast(emm_modthre_sd_cross_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#### NON-PSYCHOMETRIC RESPONSES ANALYSES ####

#responses across variabilities, confidence condition
mod_go_ttc_var_conf_difttc <- afex::mixed(go ~ ttcg + var_name + (1|participant), data=disc_df_difttc %>% filter(cond == "conf"), method = "LRT", family = binomial) #model
anova(mod_go_ttc_var_conf_difttc)

emm_mod_go_ttc_var_conf_difttc <- emmeans(mod_go_ttc_var_conf_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_go_ttc_var_conf_difttc, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_go_ttc_var_conf_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#responses across variabilities, cross condition
mod_go_ttc_var_cross_difttc <- afex::mixed(go ~ ttcg + var_name + (1|participant), data=disc_df_difttc %>% filter(cond == "cross"), method = "LRT", family = binomial) #model
anova(mod_go_ttc_var_cross_difttc)

emm_mod_go_ttc_var_cross_difttc <- emmeans(mod_go_ttc_var_cross_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_go_ttc_var_cross_difttc, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_go_ttc_var_cross_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#### SUCCESSFUL CROSSINGS ####

#effect of variability on successful crosses
# mod_succcrosses_sd_difttc <- afex::mixed(cross ~ var_name + (1|participant), data=disc_df_difttc %>% filter(! participant %in% outlier_subjs_difttc & cond == "cross" & t_cross == 1), method = "LRT", family = binomial)
# anova(mod_succcrosses_sd_difttc)
# 
# emm_mod_succcrosses_sd_difttc <- emmeans(mod_succcrosses_sd_difttc, ~ var_name) #cross model, estimated marginal means for var_name
# summary(emm_mod_succcrosses_sd_difttc, type = "response") #probability estimates for each variability level
# summary(contrast(emm_mod_succcrosses_sd_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#effect of TTC on successful crosses
# mod_succcrosses_ttc_difttc <- afex::mixed(cross ~ ttcg + (1|participant), data=disc_df_difttc %>% filter(! participant %in% outlier_subjs_difttc & cond == "cross" & t_cross == 1), method = "LRT", family = binomial)
# anova(mod_succcrosses_ttc_difttc)

#modulation of car velocity variability on the psychometric function between TTC and crossing success
fit_perf_subjs_difttc <- quickpsy(
  disc_df_difttc %>% filter(cond == "cross" & t_cross == 1) , #only cross condition trials where crossing is attempted
  ttcg,
  k = cross, #DV is crossing success
  grouping = .(participant,var_name),
  guess = FALSE,
  lapses = FALSE,
  bootstrap = 'none'
)

1 - (fit_perf_subjs_difttc$par %>% filter(parn == "p2" & par<15 & par>0) %>% nrow() / fit_perf_subjs_difttc$par %>% filter(parn == "p2") %>% nrow()) #proportion of fits to remove ( because of outlier SD values )

modparvar_sd_difttc <- lmer(par~var_name+(1|participant),data = fit_perf_subjs_difttc$par %>% filter(parn == "p2" & par<15 & par>0) )
anova(modparvar_sd_difttc)

r2beta(modparvar_sd_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

1 - (fit_perf_subjs_difttc$thresholds %>% filter(thre > 0 & thre < 20) %>% nrow() / fit_perf_subjs_difttc$thresholds %>% nrow()) #proportion of fits to remove ( because of outlier threshold values )

modparvar_thre_difttc <- lmer(thre ~ var_name + (1|participant), data = fit_perf_subjs$thresholds %>% filter(thre > 0 & thre < 20))

anova(modparvar_thre_difttc)

r2beta(modparvar_thre_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

#### KINEMATICS ####

#predicting leaving time
mod_leaving_time_norm_difttc <- lmer(leaving_time_norm ~ ttcg + var_name + (1 | participant), data = movinitrt_df_difttc)
anova(mod_leaving_time_norm_difttc) #anova

r2beta(mod_leaving_time_norm_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_leaving_time_norm_difttc <- emmeans(mod_leaving_time_norm_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_leaving_time_norm_difttc, type = "response") #probability estimates for each variability level
summary(contrast(emm_mod_leaving_time_norm_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted p-values for pairwise differences

#predicting peak velocity
mod_peakvelrt_difttc <- lmer(peakvel ~ ttcg + var_name + leaving_time_norm  + (1 | participant), data = movementsegments_trialsum_difttc) #model predicting peak velocity by leaving time, controlling for TTC and variability
anova(mod_peakvelrt_difttc) #anova

r2beta(mod_peakvelrt_difttc, method = 'nsj', partial = TRUE) #R² + 95% CI

emm_mod_peakvelrt_difttc <- emmeans(mod_peakvelrt_difttc, ~ var_name) #estimated marginal means for var_name
summary(emm_mod_peakvelrt_difttc, type = "response")
summary(contrast(emm_mod_peakvelrt_difttc, method = "pairwise", adjust = "bonferroni")) #Bonferroni-adjusted pairwise comparisons

#individual slopes normalized sidewalk leaving time predicting peak vel
df_lt_peakvel_slopes_difttc <- movementsegments_trialsum_difttc %>%
  group_by(participant,var_name) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(peakvel ~ leaving_time_norm, data = .x)),
    tidy_model = map(model, ~ broom::tidy(.x))
  ) %>%
  unnest(tidy_model) %>%
  filter(term == "leaving_time_norm") %>%  # Only keep slope estimates
  select(participant, estimate, p.value) %>%
  rename(slope = estimate)

df_lt_peakvel_slopes_difttc %>% filter(var_name == "no" & slope > 0) %>% nrow()
df_lt_peakvel_slopes_difttc %>% filter(var_name == "medium" & slope > 0) %>% nrow()
df_lt_peakvel_slopes_difttc %>% filter(var_name == "high" & slope > 0) %>% nrow()
