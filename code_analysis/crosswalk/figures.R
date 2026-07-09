############################## MAIN TEXT ##############################
#### Figure 2: psychometric-derived results ####
resxplot <- 1

#psychometric curve one participant, confidence condition
fig_psycur_onepart_conf <- ggplot(psyfit_conf_averages %>% filter(participant == "isd"), aes(ttc,prob)) +
  geom_line(data = psyfit_conf_curves %>% filter(participant == "isd"), aes(x,y,col = var_name), linewidth = size_line_lineplot) +
  geom_point(aes(col = var_name), fill = hue_border_conf, size = 2) +
  geom_vline(data = psyfit_conf_thresholds %>% filter(participant == "isd"), aes(xintercept = thre, col = var_name),linewidth = size_line_lineplot) +
  scale_x_ttc +
  scale_y_fullprob +
  xlab_ttc +
  ylab_propconf +
  pal_col_var +
  theme(strip.background = element_blank()) +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(legend.position = "none")

#psychometric curve one participant, cross condition
fig_psycur_onepart_cross <- ggplot(psyfit_cross_averages %>% filter(participant == "isd"), aes(ttc,prob)) +
  geom_line(data = psyfit_cross_curves %>% filter(participant == "isd"), aes(x,y,col = var_name), linewidth = size_line_lineplot) +
  geom_point(aes(col = var_name), fill = hue_border_cross, size = 2) +
  geom_vline(data = psyfit_cross_thresholds %>% filter(participant == "isd"), aes(xintercept = thre, col = var_name),linewidth = size_line_lineplot) +
  scale_x_ttc +
  scale_y_fullprob +
  xlab_ttc +
  ylab_propcross +
  scale_color_manual(name = "Car velocity \n variability", values=c(hue_border_varno,hue_border_varmedium,hue_border_varhigh)) +
  theme(strip.background = element_blank()) +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.35))

#thresholds, confidence condition
psyfit_thresholds_sav_noout <- psyfit_thresholds_sin %>%
  filter(!participant %in% outlier_subjs) %>%
  group_by(cond,var_name) %>%
  summarise_with_ci(thre) #across-participants summary

fig_carsd_confthres <- create_violin2(
  summary_df = psyfit_thresholds_sav_noout %>% filter(cond == "conf"),
  individual_df = psyfit_thresholds_sin %>% filter(cond == "conf" & !participant %in% outlier_subjs),
  variable_x = var_name,
  variable_y = thre,
  variable_y_error = se_thre,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Threshold: \n TTC 50% 'more confident' (s)",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,2,4,6,8), labels = c(0,2,4,6,8), limits = c(0,9.5))
)

#thresholds, cross condition
fig_carsd_crossthres <- create_violin2(
  summary_df = psyfit_thresholds_sav %>% filter(cond == "cross"),
  individual_df = psyfit_thresholds_sin %>% filter(cond == "cross" & !participant %in% outlier_subjs),
  variable_x = var_name,
  variable_y = thre,
  variable_y_error = se_thre,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Threshold: \n TTC 50% attempted crossings (s)",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,2,4,6,8), labels = c(0,2,4,6,8), limits = c(0,9.5))
)

#correlation with thresholds
fig_cors_thres_confcross <- ggplot(dotsforcors_thres_var_sd %>% filter(!participant %in% outlier_subjs)) +
  geom_point(aes(x=conf,y=cross, fill = var_sd_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_sd_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_sd_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(breaks = c(0,2,4,6,8,10), labels = c(0,2,4,6,8,10), limits = c(0,10)) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10), labels = c(0,2,4,6,8,10), limits = c(0,10)) +
  pal_col_var +
  pal_fill_var +
  coord_fixed(ratio = 1) +
  xlab("Threshold: TTC 50% 'more confident' (s)") +
  ylab("Threshold: \n TTC 50% attempted crossings (s)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = corstrings_thres_var_sd, aes(x = 6, y = 10, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE) 

#plot together and save
fig2_toprow_forplot <- plot_grid(
  fig_psycur_onepart_conf, fig_psycur_onepart_cross,
  fig_carsd_confthres, fig_carsd_crossthres,
  nrow = 2,
  labels = c("A", "B", "C", "D"))

fig2_bottomrow_forplot <- plot_grid(
  fig_cors_thres_confcross,
  nrow = 1,
  labels = c("E"))

  figure2 <- plot_grid(
  fig2_toprow_forplot,
  fig2_bottomrow_forplot,
  nrow = 2,
  rel_heights = c(2, 1))

save_plot("figures/figure2.pdf", figure2, ncol = 2, nrow =3, base_height = 4,base_width = 4)


 #### Figure 3: kinematics results ####

#normalized leaving time
fig_leaving_time_norm_vars <- create_violin2(
  summary_df = leaving_time_norm_var_sav,
  individual_df = leaving_time_norm_var_sin,
  variable_x = var_name,
  variable_y = leaving_time_norm,
  variable_y_error = se_leaving_time_norm,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Normalized sidewalk leaving time",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,0.1,0.3,0.3), labels = c(0,0.1,0.3,0.3), limits = c(0,0.32))
  )

#Peak velocity
fig_varname_peakvel <- create_violin2(
  summary_df = peakvel_varname_sav,
  individual_df = peakvel_varname_sin,
  variable_x = var_name,
  variable_y = peakvel,
  variable_y_error = se_peakvel,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = "Car velocity variability",
  plot_ylab = "Peak velocity (m/s)",
  plot_scaley = scale_y_continuous(breaks = c(0,1,2,3,4), labels = c(0,1,2,3,4), limits = c(0,4.5)))


fig_ltpeakvel_slopes <- ggplot(movementsegments_trialsum, aes(x = leaving_time_norm, y = peakvel)) +
  geom_smooth(aes(group = participant),method = "lm", se = FALSE, color = "grey", linewidth = size_indivline) +
  geom_smooth(aes(col = var_name, fill = var_name),method = "lm", se = TRUE, linewidth = size_line_lineplot, alpha = 0.1) +
  pal_col_var +
  pal_fill_var +
  theme(
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.4, 0.8)) +
  xlab("Normalized sidewalk leaving time") +
  ylab("Peak velocity (m/s)") +
  scale_y_continuous(breaks = c(0,1,2,3,4), labels = c(0,1,2,3,4), limits = c(0,4.5))

#plot together and save
figure3 <- plot_grid(
  fig_leaving_time_norm_vars,
  fig_varname_peakvel,
  fig_ltpeakvel_slopes,
  nrow = 1,
  labels = c("A","B", "C"))

save_plot("figures/figure3.pdf", figure3, ncol = 3, nrow =1, base_height = 4,base_width = 4)

#### Figure 4: eye movement results ####
bin_circular_angles <- function(df, angle_col = "gaze_angy", binwidth = 15) {
  angles <- df[[angle_col]] %% 360
  
  bin_centers <- seq(0, 360 - binwidth, by = binwidth)
  n_bins <- length(bin_centers)
  
  shifted_angles <- (angles + binwidth / 2) %% 360 #adjust angles so that bin centered at 0 goes from -7.5 to 7.5
  
  bin_edges <- seq(0, 360, by = binwidth)
  
  bin_factor <- cut(
    shifted_angles,
    breaks = bin_edges,
    include.lowest = TRUE,
    labels = FALSE,
    right = FALSE
  ) #cut and label bins
  
  group_vars <- group_vars(df) #get grouping variables if they exist
  
  #create tibble with bin and grouping variables
  binned_data <- df %>%
    mutate(bin_label = bin_factor) %>%
    group_by(across(all_of(group_vars)), bin_label) %>%
    summarise(count = n(), .groups = "drop_last") %>%
    mutate(
      bin_label = as.integer(bin_label),
      angle = bin_centers[bin_label]
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(proportion = count / sum(count)) %>%
    ungroup() %>%
    arrange(across(all_of(group_vars)), bin_label)
  
  return(binned_data)
}

angles_binwidth <- 15
gaze_angy_bins_conf_firsttimerun <- bin_circular_angles(cont_conf_df %>% filter(timerun == 0) %>% ungroup(), "gaze_angy", binwidth = angles_binwidth)
gaze_angy_bins_cross_firsttimerun <- bin_circular_angles(cont_cross_df %>% filter(timerun == 0) %>% ungroup(), "gaze_angy", binwidth = angles_binwidth)

gaze_angy_bins_conf <- bin_circular_angles(cont_conf_df %>% ungroup(), "gaze_angy", binwidth = angles_binwidth)
gaze_angy_bins_cross_befmov <- bin_circular_angles(cont_cross_befmov_df %>% ungroup(), "gaze_angy", binwidth = angles_binwidth)

carslooked_bin_confcrossbefmov_var <- rbind(
  carslooked_bin_conf_var %>% mutate(cond = "Confidence condition") ,
  carslooked_crossbefmov_bin_var %>% mutate(cond = "Cross condition"))

carslooked_bin_confcrossbefmov_var_sav <- rbind(
  carslooked_bin_conf_var_sav %>% mutate(cond = "Confidence condition") ,
  carslooked_crossbefmov_bin_var_sav %>% mutate(cond = "Cross condition"))

matched_rt_var <- cont_cross_df %>%
  filter(t_cross == 1 & helmet_posz <= -3) %>%
  group_by(participant,session,trial_within_session,var_name) %>%
  mutate(time_norm = timerun / ttc) %>%
  summarise(matched_time_norm = last(time_norm)) %>%
  group_by(participant,var_name) %>%
  summarise(matched_time_norm = mean(matched_time_norm, na.rm = TRUE)) %>% 
  group_by(var_name) %>%
  summarise_with_ci(matched_time_norm)

matched_rt_forplot <- tibble(
  cond = c(rep("Confidence condition",3),rep("Cross condition",3)),
  var_name = rep(matched_rt_var$var_name,2),
  rt = c(matched_rt_var$matched_time_norm,rep(NA,3)),
  se_rt = c(matched_rt_var$se_matched_time_norm,rep(NA,3)),
)

draw_radial_inset <- function(df) {
  
  plot <-  ggplot(df, aes(x = angle, y = proportion)) +
    geom_hline(yintercept = c(max(df$proportion), (max(df$proportion) / 2)), col = "grey80",linewidth   = 0.3) + 
    geom_col(col = "black", fill = "black", width = angles_binwidth, alpha = 0.8, linewidth = 0.2) +
    scale_x_continuous(
      breaks = seq(0, 330, by = 30),
      minor_breaks = NULL, 
      limits = c(-angles_binwidth / 2, 360 - angles_binwidth / 2),
      labels = function(x) paste0(x %% 360, "°")) +
    coord_polar(start = rotation, clip = "off") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey80", size = 0.3),
      panel.grid.major.y = element_blank(),
      legend.position = "none")
  
  return(plot)
}
rotation <- -angles_binwidth / 2 / 360 * 2 * pi #turn necessary rotation to radians

fig_inset_gazeangles_firsttimerun_conf <- draw_radial_inset(gaze_angy_bins_conf_firsttimerun)
fig_inset_gazeangles_firsttimerun_cross <- draw_radial_inset(gaze_angy_bins_cross_firsttimerun)

fig_carlookdyn_base <- ggplot(carslooked_bin_confcrossbefmov_var, aes(x = time_bin_center, y = carslooked, col = var_name, linetype = factor(go))) +
  #geom_smooth(se = FALSE, linewidth = size_line_lineplot) +
  geom_ribbon(data = carslooked_bin_confcrossbefmov_var_sav, aes(ymin = carslooked - se_carslooked, ymax = carslooked + se_carslooked, fill = var_name, group = interaction(var_name, go)), alpha = 0.1, colour = NA) +
  geom_line(data = carslooked_bin_confcrossbefmov_var_sav) +
  geom_rect(data = matched_rt_forplot %>% filter(!is.na(rt)),
            aes(xmin = rt - se_rt, xmax = rt + se_rt, ymin = -Inf, ymax = Inf, fill = var_name),
            alpha = 0.1, colour = NA, inherit.aes = FALSE) +
  geom_vline(data = matched_rt_forplot,aes(xintercept = rt, color = var_name),linetype = "solid") +
  facet_wrap(vars(cond)) +
  labs(x = "Normalized time",y = "Prop. looking at any car",linetype = NULL) +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("'less confident' / crossing not attempted", "'more confident' / crossing attempted")) +
  pal_col_var +
  pal_fill_var +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),labels = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75),labels = c(0, 0.25, 0.5, 0.75),limits = c(0, 0.85),expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.45, 0.2),
    legend.box           = "horizontal") +
  theme(legend.position = "none")


annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = FALSE, params = list(grob = grob, 
                                           xmin = xmin, xmax = xmax, 
                                           ymin = ymin, ymax = ymax))
}

fig_carlookdyn <- fig_carlookdyn_base + 
  annotation_custom2(grob=ggplotGrob(fig_inset_gazeangles_firsttimerun_conf), 
                     data = data.frame(cond="Confidence condition"),
                     ymin = 0, ymax=0.5, xmin=0.3, xmax=0.8) +
  annotation_custom2(grob=ggplotGrob(fig_inset_gazeangles_firsttimerun_cross), 
                     data = data.frame(cond="Cross condition"),
                     ymin = 0, ymax=0.5, xmin=0.1, xmax=0.6)


carslanediflooked_bin_confcrossbefmov_var <- rbind(
  carslanediflooked_bin_conf_var %>% mutate(cond = "Confidence condition") ,
  carslanediflooked_bin_cross_befmov_var %>% mutate(cond = "Cross condition"))

carslanediflooked_bin_confcrossbefmov_var_sav <- rbind(
  carslanediflooked_bin_conf_var_sav %>% mutate(cond = "Confidence condition") ,
  carslanediflooked_bin_cross_befmov_var_sav %>% mutate(cond = "Cross condition"))


fig_carlanelookdyn <- ggplot(carslanediflooked_bin_confcrossbefmov_var, aes(x = time_bin_center, y = carslane_looked_dif, col = var_name, linetype = factor(go))) +
  #geom_smooth(se = FALSE, linewidth = size_line_lineplot) +
  geom_ribbon(data = carslanediflooked_bin_confcrossbefmov_var_sav, aes(ymin = carslane_looked_dif - se_carslane_looked_dif, ymax = carslane_looked_dif + se_carslane_looked_dif, fill = var_name, group = interaction(var_name, go)), alpha = 0.1, colour = NA) +
  geom_line(data = carslanediflooked_bin_confcrossbefmov_var_sav) +
  geom_rect(data = matched_rt_forplot %>% filter(!is.na(rt)),
            aes(xmin = rt - se_rt, xmax = rt + se_rt, ymin = -Inf, ymax = Inf, fill = var_name),
            alpha = 0.1, colour = NA, inherit.aes = FALSE) +
  geom_vline(data = matched_rt_forplot,aes(xintercept = rt, color = var_name),linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(vars(cond)) +
  labs(x = "Normalized time",y = "Dif. prop. looking at far minus near cars",linetype = NULL) +
  scale_linetype_manual(
    values = c("dashed", "solid"),
    labels = c("'less confident' / crossing not attempted", "'more confident' / crossing attempted"),
    guide  = guide_legend(order = 1)) +
  scale_color_manual(name = "Car velocity variability", values=c(hue_border_varno,hue_border_varmedium,hue_border_varhigh), guide  = guide_legend(
    order = 2, title.position = "top", nrow = 1, byrow = TRUE)) +
  scale_fill_manual(
    name = "Car velocity variability",
    values = c(hue_border_varno, hue_border_varmedium, hue_border_varhigh),
    guide = guide_legend(order = 2, title.position = "top", nrow = 1, byrow = TRUE)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),labels = c(0, 0.25, 0.5, 0.75, 1),limits = c(0, 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = c(-0.5, -0.25, 0, 0.25),labels = c(-0.5, -0.25, 0, 0.25), limits = c(-0.6, 0.41),expand = c(0, 0)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "inside",
    legend.position.inside =  c(0.35,0.25),
    legend.spacing.x    = unit(2, "pt"), #reduce space around legend
    legend.margin       = margin(t = 0, r = 0, b = 0, l = 0),
    legend.background   = element_rect(fill = alpha("white", 0), color = NA)) 

#plot together
figure4 <- plot_grid(
  fig_carlookdyn, fig_carlanelookdyn,
  nrow = 2,
  labels = c("A", "B"))

save_plot("figures/figure4.pdf", figure4, ncol = 2, nrow =2, base_height = 4,base_width = 4)


#### Figure 5: replication experiment: psychometric-derived results ####
#thresholds, confidence condition
fig_carsd_confthres_difttc <- create_violin2(
  summary_df = psyfit_difttc_thresholds_sav %>% filter(cond == "conf"),
  individual_df = psyfit_difttc_thresholds_sin %>% filter(cond == "conf" & !participant %in% outlier_subjs_difttc),
  variable_x = var_name,
  variable_y = thre,
  variable_y_error = se_thre,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Threshold: \n TTC 50% 'more confident' (s)",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,2,4,6,8), labels = c(0,2,4,6,8), limits = c(0,8))
)

#thresholds, cross condition
fig_carsd_crossthres_difttc <- create_violin2(
  summary_df = psyfit_difttc_thresholds_sav %>% filter(cond == "cross"),
  individual_df = psyfit_difttc_thresholds_sin %>% filter(cond == "cross" & !participant %in% outlier_subjs_difttc),
  variable_x = var_name,
  variable_y = thre,
  variable_y_error = se_thre,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Threshold: \n TTC 50% attempted crossings (s)",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,2,4,6,8), labels = c(0,2,4,6,8), limits = c(0,8))
)

#plot together and save
fig_thresholds_difttc <- plot_grid(
  fig_carsd_confthres_difttc,
  fig_carsd_crossthres_difttc,
  nrow = 1,
  labels = c("A","B"))

save_plot("figures/figure5.pdf", fig_thresholds_difttc, ncol = 2, nrow =1, base_height = 4,base_width = 4)

####  Figure 6: replication experiment: kinematics results ####

#normalized leaving time
fig_leaving_time_norm_vars_difttc <- create_violin2(
  summary_df = leaving_time_norm_var_difttc_sav,
  individual_df = leaving_time_norm_var_difttc_sin,
  variable_x = var_name,
  variable_y = leaving_time_norm,
  variable_y_error = se_leaving_time_norm,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Normalized sidewalk leaving time",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,0.1,0.2), labels = c(0,0.1,0.2), limits = c(0,0.25)))

#Peak velocity
fig_varname_peakvel_difttc <- create_violin2(
  summary_df = peakvel_varname_difttc_sav,
  individual_df = peakvel_varname_difttc_sin,
  variable_x = var_name,
  variable_y = peakvel,
  variable_y_error = se_peakvel,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = "Car velocity variability",
  plot_ylab = "Peak velocity (m/s)",
  plot_scaley = scale_y_continuous(breaks = c(0,1,2,3), labels = c(0,1,2,3), limits = c(0,3.5)))

fig_ltpeakvel_slopes_difttc <- ggplot(movementsegments_trialsum_difttc, aes(x = leaving_time_norm, y = peakvel)) +
  geom_smooth(aes(group = participant),method = "lm", se = FALSE, color = "grey", linewidth = size_indivline) +
  geom_smooth(aes(col = var_name, fill = var_name),method = "lm", se = TRUE, linewidth = size_line_lineplot, alpha = 0.1) +
  pal_col_var +
  pal_fill_var +
  theme(
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.4, 0.25)) +
  xlab("Normalized sidewalk leaving time") +
  ylab("Peak velocity (m/s)") +
  scale_y_continuous(breaks = c(0,1,2,3), labels = c(0,1,2,3), limits = c(0,3.5))

#plot together and save
fig_kinematics_difttc <- plot_grid(
  fig_leaving_time_norm_vars_difttc,
  fig_varname_peakvel_difttc,
  fig_ltpeakvel_slopes_difttc,
  nrow = 1,
  labels = c("A","B", "C"))

save_plot("figures/figure6.pdf", fig_kinematics_difttc, ncol = 3, nrow =1, base_height = 4,base_width = 4)

##################################### SUPPLEMENTARY MATERIAL ######################
#### Figures S1 and S2: individual thresholds for confidence and cross condition ####

participant_id_map <- tibble(
  participant = unique_participants,
  participant_number = seq_along(unique_participants),
  participant_label = factor(
    paste("Participant", seq_along(unique_participants)),
    levels = paste("Participant", seq_along(unique_participants))
  )
)

#plot confidence
psyfit_conf_averages$var_name <- factor(psyfit_conf_averages$var_name, levels = c("no", "medium", "high"))
psyfit_conf_curves$var_name <- factor(psyfit_conf_curves$var_name, levels = c("no", "medium", "high"))
psyfit_conf_thresholds$var_name <- factor(psyfit_conf_thresholds$var_name, levels = c("no", "medium", "high")) #reorder factor levels for var_name

psyfit_conf_averages <- psyfit_conf_averages %>% left_join(participant_id_map, by = "participant")
psyfit_conf_curves <- psyfit_conf_curves %>% left_join(participant_id_map, by = "participant")
psyfit_conf_thresholds <- psyfit_conf_thresholds %>% left_join(participant_id_map, by = "participant")

fig_psyfit_curves_conf <- ggplot(psyfit_conf_averages, aes(ttc,prob)) +
  geom_line(data = psyfit_conf_curves, aes(x,y,col = var_name)) +
  geom_point(aes(col = var_name)) +
  geom_vline(data = psyfit_conf_thresholds, aes(xintercept = thre, col = var_name)) +
  scale_x_ttc +
  scale_y_fullprob +
  pal_col_var +
  xlab_ttc +
  ylab_propconf +
  theme(strip.background = element_blank()) +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(
    legend.direction="horizontal",
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.03)) +
  facet_wrap(vars(participant_label), ncol=6)

save_plot("figures/figures1.pdf", fig_psyfit_curves_conf, ncol = 1, nrow =1, base_height =12,base_width = 10)

#plot cross
psyfit_cross_averages$var_name <- factor(psyfit_cross_averages$var_name, levels = c("no", "medium", "high"))
psyfit_cross_curves$var_name <- factor(psyfit_cross_curves$var_name, levels = c("no", "medium", "high"))
psyfit_cross_thresholds$var_name <- factor(psyfit_cross_thresholds$var_name, levels = c("no", "medium", "high")) #reorder factor levels for var_name

psyfit_cross_averages <- psyfit_cross_averages %>% left_join(participant_id_map, by = "participant")
psyfit_cross_curves <- psyfit_cross_curves %>% left_join(participant_id_map, by = "participant")
psyfit_cross_thresholds <- psyfit_cross_thresholds %>% left_join(participant_id_map, by = "participant")

fig_psyfit_curves_cross <- ggplot(psyfit_cross_averages, aes(ttc,prob)) +
  geom_line(data = psyfit_cross_curves, aes(x,y,col = var_name)) +
  geom_point(aes(col = var_name)) +
  geom_vline(data = psyfit_cross_thresholds, aes(xintercept = thre, col = var_name)) +
  scale_x_ttc +
  scale_y_fullprob +
  pal_col_var +
  xlab_ttc +
  ylab_propcross +
  theme(strip.background = element_blank()) +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(
    legend.direction="horizontal",
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.03)) +
  facet_wrap(vars(participant_label), ncol=6)

save_plot("figures/figures2.pdf", fig_psyfit_curves_cross, ncol = 1, nrow =1, base_height =12,base_width = 10)

#### Figure S3: non-psychometric responses ####
resxplot <- 1

psyfit_thresholds_sav_noout <- psyfit_thresholds_sin %>%
  filter(!participant %in% outlier_subjs) %>%
  group_by(cond,var_name) %>%
  summarise_with_ci(thre) #across-participants summary

#across ttc, confidence condition
pal_fill_ttc <- scale_fill_manual(name = 'TTC',values =  c("#dadaeb","#bcbddc","#9e9ac8","#807dba","#54278f"))
pal_col_ttc <- scale_color_manual(name = 'TTC',values =  rep("#54278f",5))

fig_resps_ttc_conf <- create_violin2(
  summary_df = go_ttc_sav %>% filter(cond == "conf"),
  individual_df = go_ttc_sin %>% filter(cond == "conf"), #& !participant %in% outlier_subjs
  variable_x = ttc_factor,
  variable_y = go,
  variable_y_error = se_go,
  variable_participant = participant,
  col_palette = pal_col_ttc,
  fill_palette = pal_fill_ttc,
  plot_xlab = xlab_ttc,
  plot_ylab = ylab_propconf,
  plot_scalex = scale_x_ttc_discrete,
  plot_scaley = scale_y_fullprob)

#across ttc, cross condition
fig_resps_ttc_cross <- create_violin2(
  summary_df = go_ttc_sav %>% filter(cond == "cross"),
  individual_df = go_ttc_sin %>% filter(cond == "cross"), #& !participant %in% outlier_subjs
  variable_x = ttc_factor,
  variable_y = go,
  variable_y_error = se_go,
  variable_participant = participant,
  col_palette = pal_col_ttc,
  fill_palette = pal_fill_ttc,
  plot_xlab = xlab_ttc,
  plot_ylab = ylab_propcross,
  plot_scalex = scale_x_ttc_discrete,
  plot_scaley = scale_y_fullprob)

#across car velocity variabilities, confidence condition
fig_resps_var_conf <- create_violin2(
  summary_df = go_var_sav %>% filter(cond == "conf"),
  individual_df = go_var_sin %>% filter(cond == "conf"), #& !participant %in% outlier_subjs
  variable_x = var_rank_factor,
  variable_y = go,
  variable_y_error = se_go,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = ylab_propconf,
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_fullprob)

#across car velocity variabilities, cross condition
fig_resps_var_cross <- create_violin2(
  summary_df = go_var_sav %>% filter(cond == "cross"),
  individual_df = go_var_sin %>% filter(cond == "cross"), #& !participant %in% outlier_subjs
  variable_x = var_rank_factor,
  variable_y = go,
  variable_y_error = se_go,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = ylab_propcross,
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_fullprob)

#plot first row together
figresps_toprow_forplot <- plot_grid(fig_resps_ttc_conf, fig_resps_ttc_cross, fig_resps_var_conf, fig_resps_var_cross, nrow = 2, align = "h",labels = c("A","B","C","D"))

#correlation between confidence and cross across variability level, three panels, as logit
ortfit_logit_no <- orthogonal_regression(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "no"], 
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "no"])
ortfit_logit_medium <- orthogonal_regression(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "medium"], 
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "medium"])
ortfit_logit_high <- orthogonal_regression(
  dotsforcors_3lev_logit_var_sd$conf[dotsforcors_3lev_logit_var_sd$var_sd == "high"], 
  dotsforcors_3lev_logit_var_sd$cross[dotsforcors_3lev_logit_var_sd$var_sd == "high"])

confcor_ortfits <- rbind(as_tibble(ortfit_logit_no),as_tibble(ortfit_logit_medium),as_tibble(ortfit_logit_high))
confcor_ortfits$var_sd <- c("no","medium","high")

dotsforcors_3lev_logit_var_sd <- left_join(dotsforcors_3lev_logit_var_sd,confcor_ortfits)

dotsforcors_3lev_logit_var_sd <- dotsforcors_3lev_logit_var_sd %>%
  mutate(var_sd_forfacet = paste (var_sd,"variability", sep = " "))

dotsforcors_3lev_logit_var_sd$var_sd_forfacet <- factor(dotsforcors_3lev_logit_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))

corstrings_3lev_logit_var_sd <- corstrings_3lev_logit_var_sd %>%
  mutate(var_sd_forfacet = ifelse(var_sd == "no","no variability",ifelse(var_sd == "medium","medium variability", "high variability")))
corstrings_3lev_logit_var_sd$var_sd_forfacet <- factor(corstrings_3lev_logit_var_sd$var_sd_forfacet, levels = c("no variability","medium variability","high variability"))

fig_corevolindiv_logit_forplot <- ggplot(dotsforcors_3lev_logit_var_sd) +
  geom_point(aes(x=conf,y=cross, fill = var_sd_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_sd_forfacet), size = genplotwidth) +
  facet_wrap(vars(var_sd_forfacet), nrow = 1) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3), labels = c(-4,-3,-2,-1,0,1,2,3), limits = c(-4,3)) +
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0,1,2), labels = c(-4,-3,-2,-1,0,1,2), limits = c(-3.1,3)) +
  coord_fixed(ratio = 1) +
  xlab_propconf_logodds +
  ylab_propcross_logodds +
  pal_col_var + 
  pal_fill_var + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_segment(x = 0, y = -3, yend = 2, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  geom_text(data = corstrings_3lev_logit_var_sd, aes(x = 0, y = 2.9, label = corrstring_conf_cross),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE)

#plot second row
figresps_bottomrow_forplot <- plot_grid(fig_corevolindiv_logit_forplot, nrow = 1, align = "h",labels = c("E"))

#unite both rows and save
figsupp_resps <- plot_grid(
  figresps_toprow_forplot,
  figresps_bottomrow_forplot,
  nrow = 2,
  rel_heights = c(2, 1))

save_plot("figures/figures3.pdf", figsupp_resps, ncol = 2, nrow =3, base_height = 4,base_width = 4)

#### Figure S4: individual thresholds for risk condition ####
psyfit_risk_averages$var_name <- factor(psyfit_risk_averages$var_name, levels = c("no", "medium", "high"))
psyfit_risk_curves$var_name <- factor(psyfit_risk_curves$var_name, levels = c("no", "medium", "high"))
psyfit_risk_thresholds$var_name <- factor(psyfit_risk_thresholds$var_name, levels = c("no", "medium", "high")) #reorder factor levels for var_name

psyfit_risk_averages <- psyfit_risk_averages %>% left_join(participant_id_map, by = "participant")
psyfit_risk_curves <- psyfit_risk_curves %>% left_join(participant_id_map, by = "participant")
psyfit_risk_thresholds <- psyfit_risk_thresholds %>% left_join(participant_id_map, by = "participant")

fig_psyfit_curves_risk <- ggplot(psyfit_risk_averages, aes(ttc,prob)) +
  geom_line(data = psyfit_risk_curves, aes(x,y,col = var_name)) +
  geom_point(aes(col = var_name)) +
  geom_vline(data = psyfit_risk_thresholds, aes(xintercept = thre, col = var_name)) +
  scale_x_ttc +
  scale_y_fullprob +
  pal_col_var +
  xlab_ttc +
  ylab_proprisk +
  theme(strip.background = element_blank()) +
  theme(strip.background = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  theme(
    legend.direction="horizontal",
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.03)) +
  facet_wrap(vars(participant_label), ncol=6)

save_plot("figures/figures4.pdf", fig_psyfit_curves_risk, ncol = 1, nrow =1, base_height =12,base_width = 10)

#### Figure S5: Risk condition thresholds ####
psyfit_thresholds_sav_nooutwithrisk <- psyfit_thresholds_sin %>%
  filter(!participant %in% outlier_subjs_withrisk) %>%
  group_by(cond,var_name) %>%
  summarise_with_ci(thre) #across-participants summary

fig_varsd_riskthres <- create_violin2(
  summary_df = psyfit_thresholds_sav_nooutwithrisk %>% filter(cond == "risk"),
  individual_df = psyfit_thresholds_sin %>% filter(cond == "risk" & !participant %in% outlier_subjs_withrisk),
  variable_x = var_name,
  variable_y = thre,
  variable_y_error = se_thre,
  variable_participant = participant,
  col_palette = pal_col_var,
  fill_palette = pal_fill_var,
  plot_xlab = xlab_carsd,
  plot_ylab = "Threshold: \n TTC 50% 'less risky' (s)",
  plot_scalex = scalex_3sds,
  plot_scaley = scale_y_continuous(breaks = c(0,2,4,6), labels = c(0,2,4,6), limits = c(0,7.5)))

save_plot("figures/figures5.pdf", fig_carsd_riskthres, ncol = 1, nrow =1, base_height = 4,base_width = 4)

#### Figure S6: threshold correlations, risk condition against other conditions ####

#Correlation confidence and risk thresholds
fig_cors_thres_confrisk <- ggplot(dotsforcors_thres_var_sd_conf_risk %>% filter(!participant %in% outlier_subjs_withrisk)) +
  geom_point(aes(x=conf,y=risk, fill = var_sd_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_sd_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_sd_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(breaks = c(2,4,6,8,10), labels = c(2,4,6,8,10), limits = c(2,10)) +
  scale_y_continuous(breaks = c(2,4,6,8,10), labels = c(2,4,6,8,10), limits = c(2,10)) +
  pal_col_var +
  pal_fill_var +
  coord_fixed(ratio = 1) +
  xlab("Threshold: TTC 50% 'more confident' (s)") +
  ylab("Threshold: \n TTC 50% 'less risky' (s)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = corstrings_thres_conf_risk_var_sd, aes(x = 6, y = 10, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE) 

#Correlation risk and cross thresholds
fig_cors_thres_riskcross <- ggplot(dotsforcors_thres_var_sd_risk_cross %>% filter(!participant %in% outlier_subjs_withrisk)) +
  geom_point(aes(x=risk,y=cross, fill = var_sd_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_sd_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_sd_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(breaks = c(2,4,6,8,10), labels = c(2,4,6,8,10), limits = c(2,10)) +
  scale_y_continuous(breaks = c(2,4,6,8,10), labels = c(2,4,6,8,10), limits = c(2,10)) +
  pal_col_var +
  pal_fill_var +
  coord_fixed(ratio = 1) +
  xlab("Threshold: TTC 50% 'less risky' (s)") +
  ylab("Threshold: \n TTC 50% attempted crossings (s)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = corstrings_thres_risk_cross_var_sd, aes(x = 6, y = 10, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE) 

#plot together and save
fig_riskcors <- plot_grid(fig_cors_thres_confrisk, fig_cors_thres_riskcross, nrow = 2, labels = c("A","B"))
save_plot("figures/figures6.pdf", fig_riskcors, ncol = 2, nrow =2, base_height = 4,base_width = 4)


#### Figure S7: Correlations thresholds and normalized sidewalk leaving time ####

fig_cors_thres_rt_conf <- ggplot(dotsforcors_thres_rt %>% filter(cond == "conf" & !participant %in% outlier_subjs)) +
  geom_point(aes(x=thre,y=leaving_time_norm, fill = var_name_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_name_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_name_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,0.33)) +
  pal_col_var +
  pal_fill_var +
  xlab("Threshold: TTC 50% 'more confident' (s)") +
  ylab("Normalized sidewalk leaving time") +
  geom_text(data = corstrings_thres_rt %>% filter(cond == "conf"), aes(x = 6, y = 0.32, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE)


fig_cors_thres_rt_risk <- ggplot(dotsforcors_thres_rt %>% filter(cond == "risk" & !participant %in% outlier_subjs_withrisk)) +
  geom_point(aes(x=thre,y=leaving_time_norm, fill = var_name_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_name_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_name_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,0.33)) +
  pal_col_var +
  pal_fill_var +
  xlab("Threshold: TTC 50% 'less risky' (s)") +
  ylab("Normalized sidewalk leaving time") +
  geom_text(data = corstrings_thres_rt %>% filter(cond == "risk"), aes(x = 6, y = 0.32, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE) 

fig_cors_thres_rt_cross <- ggplot(dotsforcors_thres_rt %>% filter(cond == "cross" & !participant %in% outlier_subjs)) +
  geom_point(aes(x=thre,y=leaving_time_norm, fill = var_name_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_name_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_name_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,0.33)) +
  pal_col_var +
  pal_fill_var +
  xlab("Threshold: TTC 50% attempted crossings (s)") +
  ylab("Normalized sidewalk leaving time") +
  geom_text(data = corstrings_thres_rt %>% filter(cond == "cross"), aes(x = 6, y = 0.32, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE)


fig_cors_thres_rt <- plot_grid(fig_cors_thres_rt_conf, fig_cors_thres_rt_risk,fig_cors_thres_rt_cross,nrow = 3,labels = c("A", "B", "C"))

save_plot("figures/figures7.pdf", fig_cors_thres_rt, ncol = 3, nrow =3, base_height = 4,base_width = 4)

#### Figure S8: Correlations proportions car-looking confidence and cross conditions ####

fig_cors_propcarlook <- ggplot(dotsforcors_propcarlook) +
  geom_point(aes(x=propcarlook_conf,y=propcarlook_cross, fill = var_name_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_name_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_name_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), limits = c(0,1)) + 
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1), limits = c(0,1)) + 
  pal_col_var +
  pal_fill_var +
  xlab("Prop. looking at any car (confidence condition)") +
  ylab("Prop. looking at any car \n (cross condition)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = corstrings_propcarlook, aes(x = 0.25, y = 0.9, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE)

fig_cors_lanediflook <- ggplot(dotsforcors_lanediflook) +
  geom_point(aes(x=lanediflook_conf,y=lanediflook_cross, fill = var_name_forfacet),shape = 21, size = size_indivdot +0.5, stroke = stroke_indivdot, col = "white") +
  geom_segment(aes(x = x_min, y = y_min, xend = x_max, yend = y_max, col = var_name_forfacet), size = size_line_lineplot) +
  facet_wrap(vars(var_name_forfacet), nrow = 1) +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.position = "none") +
  scale_x_continuous(breaks = c(-1,-0.5,0,0.5,1), labels = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) + 
  scale_y_continuous(breaks = c(-1,-0.5,0,0.5,1), labels = c(-1,-0.5,0,0.5,1), limits = c(-1,1)) + 
  pal_col_var +
  pal_fill_var +
  xlab("Dif. prop. looking at far minus near cars (confidnce condition)") +
  ylab("Dif. prop. looking at far \n minus near cars (cross condition)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text(data = corstrings_lanediflook, aes(x = -0.5, y = 0.9, label = label),inherit.aes = FALSE,size = 4, hjust = 0.5, vjust = 1, parse = TRUE)

fig_cors_carlook <- plot_grid(fig_cors_propcarlook,fig_cors_lanediflook, nrow = 2,labels = c("A", "B"))

save_plot("figures/figures8.pdf", fig_cors_carlook, ncol = 3, nrow =2, base_height = 4,base_width = 4)

#### Figure S9: Car-looking proportions across velocity rank, near-lane cars only ####

#mid and high variability conditions
proplook_velrank_forplot_sin <- rbind(
  proplook_velrank_near_conf_withinmatchedrt_sin %>% mutate(
    Condition = "Confidence condition",
    cond_var = ifelse(var_name == "medium","cond_medium","cond_high"))
  ,
  proplook_velrank_near_cross_befmov_sin %>% mutate(
    Condition = "Cross condition",
    cond_var = ifelse(var_name == "medium","cross_medium","cross_high"))
) %>%
  mutate(
    velocity_rank_shifted = ifelse(var_name == "medium",velocity_rank - 0.2,velocity_rank + 0.2))

proplook_velrank_forplot_sav <- rbind(
  proplook_velrank_near_conf_withinmatchedrt_sav %>% mutate(Condition = "Confidence condition")
  ,
  proplook_velrank_near_cross_befmov_sav %>% mutate(
    Condition = "Cross condition")
)  %>%
  mutate(velocity_rank_shifted = ifelse(var_name == "medium",velocity_rank - 0.1,velocity_rank + 0.1))


fig_proplook_velrank_midhigh <- ggplot(proplook_velrank_forplot_sin, aes(velocity_rank, prop_looking)) +
  geom_half_violin(data = proplook_velrank_forplot_sin %>% filter(var_name == "medium"), 
                   aes(col = var_name, fill = var_name, group = velocity_rank), 
                   side = "l", position = "identity", alpha = 0.5) +
  geom_half_violin(data = proplook_velrank_forplot_sin %>% filter(var_name == "high"),
                   aes(col = var_name, fill = var_name, group = velocity_rank),
                   side = "r", position = "identity", alpha = 0.5) +
  geom_point(data = proplook_velrank_forplot_sin, 
             aes(x = velocity_rank_shifted, y = prop_looking, col = var_name), 
             position = position_jitter(width = 0.1, height = 0, seed = jitter_seed), 
             shape = 21, fill = "white", size = size_indivdot) +
  geom_errorbar(data = proplook_velrank_forplot_sav, 
                aes(x = velocity_rank_shifted, ymin = prop_looking - se_prop_looking, 
                    ymax = prop_looking + se_prop_looking), 
                position = position_identity(), width = 0) +
  geom_line(data = proplook_velrank_forplot_sav, 
            aes(x = velocity_rank_shifted, y = prop_looking, group = var_name), 
            linewidth = 0.25) +
  geom_point(data = proplook_velrank_forplot_sav, 
             aes(x = velocity_rank_shifted, y = prop_looking), 
             position = position_identity(), size = size_sumdot * 0.75, stroke = stroke_sumdot) +
  facet_grid(. ~ Condition, scales = "free_x", space = "free_x") +
  scale_color_manual(
    name = "Car vel. variability",
    values = c("medium" = hue_border_varmedium, "high" = hue_border_varhigh),
    breaks = c("medium", "high")
  ) +
  scale_fill_manual(
    name = "Car vel. variability",
    values = c("medium" = hue_border_varmedium, "high" = hue_border_varhigh),
    breaks = c("medium", "high")
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("minimum", "medium", "maximum")) +
  xlab("Car velocity rank") +
  ylab("Prop. total car looking time") +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = "inside",
    legend.position.inside = c(0.28, 0.92))

#no variability condition (split by car, for reference)
fig_proplook_velrank_no <- ggplot(propcarlook_eachnearcar_novar_sin, aes(car_rank, prop_looking)) +
  geom_half_violin(data = propcarlook_eachnearcar_novar_sin, 
                   aes(col = var_name, fill = var_name, group = car_rank), 
                   side = "l", position = "identity", alpha = 0.5) +
  geom_point(data = propcarlook_eachnearcar_novar_sin, 
             aes(x = car_rank - 0.2, y = prop_looking, col = var_name), 
             position = position_jitter(width = 0.1, height = 0, seed = jitter_seed), 
             shape = 21, fill = "white", size = size_indivdot) +
  geom_errorbar(data = propcarlook_eachnearcar_novar_sav, 
                aes(x = car_rank - 0.1, ymin = prop_looking - se_prop_looking, 
                    ymax = prop_looking + se_prop_looking), 
                position = position_identity(), width = 0) +
  geom_line(data = propcarlook_eachnearcar_novar_sav, 
            aes(x = car_rank - 0.1, y = prop_looking, group = var_name), 
            linewidth = 0.25) +
  geom_point(data = propcarlook_eachnearcar_novar_sav, 
             aes(x = car_rank - 0.1, y = prop_looking), 
             position = position_identity(), size = size_sumdot * 0.75, stroke = stroke_sumdot) +
  facet_grid(. ~ Condition, scales = "free_x", space = "free_x") +
  scale_color_manual(
    name = "Car vel. var.",
    values = c("no" = hue_border_varno),
    breaks = c("no")
  ) +
  scale_fill_manual(
    name = "Car vel. var.",
    values = c("no" = hue_border_varno),
    breaks = c("no")
  ) +
  scale_x_continuous(breaks = c(0.8, 1.8, 2.8), labels = c("closest", "medium", "furthest")) +
  xlab("Car (proximity of its lane to participant)") +
  ylab("Prop. total car looking time") +
  theme(
    strip.background = element_blank(), 
    strip.text = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.position = "inside",
    legend.position.inside = c(0.28, 0.92))


fig_proplook_velrank <- plot_grid(
  fig_proplook_velrank_midhigh,
  fig_proplook_velrank_no,
  nrow = 1,
  rel_widths = c(2, 1.2),
  labels = c("A","B"))

save_plot("figures/figures9.pdf", fig_proplook_velrank, ncol = 3, nrow =1, base_height = 4,base_width = 4)
