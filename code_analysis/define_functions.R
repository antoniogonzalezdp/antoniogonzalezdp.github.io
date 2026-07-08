########## DEFINE FUNCTIONS TO USE FOR DATA ANALYSES ########## 
#### create summary for a variable including mean, standard deviation (SD), standard error (SE), and 95% confidence intervals (CI) ####

summarise_with_ci <- function(data, var) {
  var <- enquo(var) #convert variable to quosure, so it can be evaluated later in the function
  var_name <- quo_name(var) #convert quosure to string to create new variable names dynamically
  
  data %>%
    summarise( #create new variables and when necessary append relevant variable name to their new names
      !!paste0("sd_", var_name) := sd(!!var, na.rm = TRUE), #standard deviation
      !!var_name := mean(!!var, na.rm = TRUE), #mean
      !!paste0("n_", var_name) := n() #n
    ) %>%
    mutate(
      !!paste0("se_", var_name) := !!sym(paste0("sd_", var_name)) / sqrt(!!sym(paste0("n_", var_name))), #standard error
      !!paste0("low_ci_", var_name) := !!sym(var_name) - qt(1 - (0.05 / 2), !!sym(paste0("n_", var_name)) - 1) * !!sym(paste0("se_", var_name)), #lower 95% CI
      !!paste0("up_ci_", var_name) := !!sym(var_name) + qt(1 - (0.05 / 2), !!sym(paste0("n_", var_name)) - 1) * !!sym(paste0("se_", var_name)) #upper 95% CI
    )
}

#### create violin plot given a series of parameters ####
create_violin2 <- function(summary_df, individual_df, variable_x, variable_y, variable_y_error, variable_participant, col_palette, fill_palette, plot_scalex = NULL, plot_scaley = NULL, plot_xlab = NULL, plot_ylab = NULL) {
  
  p <- ggplot(summary_df, aes(x = {{ variable_x }}, y = {{ variable_y }})) +
    geom_violin(data = individual_df, aes(x = {{ variable_x }}, y = {{ variable_y }}, group = {{ variable_x }}, col = {{ variable_x }}, fill = {{ variable_x }}), size = stroke_sumdot, alpha = 0.5) + 
    geom_path(data = individual_df, aes(x = {{ variable_x }}, y = {{ variable_y }}, group = {{ variable_participant }}), position = position_jitter(width = 0.2 * resxplot, seed = jitter_seed), color = "grey", size = size_indivline) +
    geom_point(data = individual_df, aes(x = {{ variable_x }}, y = {{ variable_y }}, group = {{ variable_participant }},col = {{ variable_x }}), position = position_jitter(width = 0.2 * resxplot, height = 0, seed = jitter_seed), shape = 21, fill = "white", size = size_indivdot) +
    geom_errorbar(aes(ymin = {{ variable_y }} - {{ variable_y_error }}, ymax = {{ variable_y }} + {{ variable_y_error }}), width = 0, size = 0.75, color = "black") +
    geom_point(size = size_sumdot * 0.75, stroke = stroke_sumdot, shape = 19, color = "black") +
    col_palette +
    fill_palette +
    theme(legend.position = "none")
  
  #add labels and scales only if provided
  if (!is.null(plot_scalex)) {
    p <- p + plot_scalex
  }
  if (!is.null(plot_scaley)) {
    p <- p + plot_scaley
  }
  if (!is.null(plot_xlab)) {
    p <- p + xlab(plot_xlab)
  }
  if (!is.null(plot_ylab)) {
    p <- p + ylab(plot_ylab)
  }
  
  return(p)
}

#### create correlation label ####
create_correlation_label <- function(df, col_x, col_y) {
  result <- df %>%
    summarise(correlation = cor.test({{ col_x }}, {{ col_y }}, method = "pearson")$estimate,
              p_value = cor.test({{ col_x }}, {{ col_y }}, method = "pearson")$p.value) %>%
    mutate(label = paste0("italic(r)==", round(correlation, 2), "*','~italic(p)==", format.pval(p_value, digits = 2, eps = 0.001)))
  
  return(result)
}

#### process participants for psychometric fits ####
process_participants <- function(df, unique_participants, df_name) {
  results_list <- list()
  
  for (p in unique_participants) {
    for (c in c("conf", "cross", "risk")) {
      
      # Filter data for this participant and condition
      filtered_data <- df %>% 
        filter(participant == p, cond == c)
      
      # Skip if no data
      if (nrow(filtered_data) == 0) next
      
      # Fit psychometric curve
      fit <- quickpsy(
        filtered_data,
        ttc,
        k = go,
        grouping = .(var_name),
        guess = FALSE,
        lapses = FALSE,
        bootstrap = 'none'
      )
      
      # Store results
      results_list[[paste(p, c, sep = "_")]] <- list(
        participant = p,
        cond = c,
        thresholds = fit$thresholds %>% mutate(participant = p, cond = c),
        curves = fit$curves %>% mutate(participant = p, cond = c),
        averages = fit$averages %>% mutate(participant = p, cond = c),
        par = fit$par %>% mutate(participant = p, cond = c)
      )
    }
  }
  
  # Combine all results
  all_thresholds <- bind_rows(lapply(results_list, function(x) x$thresholds))
  all_curves <- bind_rows(lapply(results_list, function(x) x$curves))
  all_averages <- bind_rows(lapply(results_list, function(x) x$averages))
  all_par <- bind_rows(lapply(results_list, function(x) x$par))
  
  return(list(
    thresholds = all_thresholds,
    curves = all_curves,
    averages = all_averages,
    par = all_par
  ))
}

#### process participants for psychometric fits (different TTC experiment) ####
process_participants_difttc <- function(df, unique_participants, df_name) {
  results_list <- list()
  
  for (p in unique_participants) {
    for (c in c("conf", "cross")) {
      
      # Filter data for this participant and condition
      filtered_data <- df %>% 
        filter(participant == p, cond == c)
      
      # Skip if no data
      if (nrow(filtered_data) == 0) next
      
      # Fit psychometric curve
      fit <- quickpsy(
        filtered_data,
        ttcg,
        k = go,
        grouping = .(var_name),
        guess = FALSE,
        lapses = FALSE,
        bootstrap = 'none'
      )
      
      # Store results
      results_list[[paste(p, c, sep = "_")]] <- list(
        participant = p,
        cond = c,
        thresholds = fit$thresholds %>% mutate(participant = p, cond = c),
        curves = fit$curves %>% mutate(participant = p, cond = c),
        averages = fit$averages %>% mutate(participant = p, cond = c),
        par = fit$par %>% mutate(participant = p, cond = c)
      )
    }
  }
  
  # Combine all results
  all_thresholds <- bind_rows(lapply(results_list, function(x) x$thresholds))
  all_curves <- bind_rows(lapply(results_list, function(x) x$curves))
  all_averages <- bind_rows(lapply(results_list, function(x) x$averages))
  all_par <- bind_rows(lapply(results_list, function(x) x$par))
  
  return(list(
    thresholds = all_thresholds,
    curves = all_curves,
    averages = all_averages,
    par = all_par
  ))
}

#### log odds transformation ####
log_odds <- function(p, epsilon = 1e-6) {
  p_clipped <- pmax(pmin(p, 1 - epsilon), epsilon)
  log(p_clipped / (1 - p_clipped))
}

#### calculate pairwise correlations ####
calculate_pairwise_correlations <- function(data1, data2, variable) {
  # Prepare data
  if (variable == "ttc") {
    data1_prep <- data1 %>%
      filter(cond != "risk") %>%
      select(participant, cond, session, trial_within_session, ttc, go) %>%
      pivot_wider(names_from = cond, values_from = go)
    
    data2_prep <- data2 %>%
      select(participant, session, trial_within_session, ttc, crossperf) %>%
      distinct()
    
    combined_data <- data1_prep %>%
      left_join(data2_prep, by = c("participant", "session", "trial_within_session", "ttc"))
    
    # Calculate correlations for each TTC level
    correlations <- combined_data %>%
      group_by(ttc) %>%
      summarise(
        cor_conf_cross = cor(conf, cross, use = "complete.obs"),
        p_conf_cross = cor.test(conf, cross)$p.value,
        cor_conf_crossperf = cor(conf, crossperf, use = "complete.obs"),
        p_conf_crossperf = cor.test(conf, crossperf)$p.value,
        cor_cross_crossperf = cor(cross, crossperf, use = "complete.obs"),
        p_cross_crossperf = cor.test(cross, crossperf)$p.value
      )
  } else if (variable == "var_sd") {
    data1_prep <- data1 %>%
      filter(cond != "risk") %>%
      select(participant, cond, session, trial_within_session, var_sd, go) %>%
      pivot_wider(names_from = cond, values_from = go)
    
    data2_prep <- data2 %>%
      select(participant, session, trial_within_session, var_sd, crossperf) %>%
      distinct()
    
    combined_data <- data1_prep %>%
      left_join(data2_prep, by = c("participant", "session", "trial_within_session", "var_sd"))
    
    # Calculate correlations for each variability level
    correlations <- combined_data %>%
      group_by(var_sd) %>%
      summarise(
        cor_conf_cross = cor(conf, cross, use = "complete.obs"),
        p_conf_cross = cor.test(conf, cross)$p.value,
        cor_conf_crossperf = cor(conf, crossperf, use = "complete.obs"),
        p_conf_crossperf = cor.test(conf, crossperf)$p.value,
        cor_cross_crossperf = cor(cross, crossperf, use = "complete.obs"),
        p_cross_crossperf = cor.test(cross, crossperf)$p.value
      )
  } else if (variable == "var_name") {
    data1_prep <- data1 %>%
      filter(cond != "risk") %>%
      select(participant, cond, session, trial_within_session, var_name, go) %>%
      pivot_wider(names_from = cond, values_from = go)
    
    data2_prep <- data2 %>%
      select(participant, session, trial_within_session, var_name, crossperf) %>%
      distinct()
    
    combined_data <- data1_prep %>%
      left_join(data2_prep, by = c("participant", "session", "trial_within_session", "var_name"))
    
    # Calculate correlations for each variability level
    correlations <- combined_data %>%
      group_by(var_name) %>%
      summarise(
        cor_conf_cross = cor(conf, cross, use = "complete.obs"),
        p_conf_cross = cor.test(conf, cross)$p.value,
        cor_conf_crossperf = cor(conf, crossperf, use = "complete.obs"),
        p_conf_crossperf = cor.test(conf, crossperf)$p.value,
        cor_cross_crossperf = cor(cross, crossperf, use = "complete.obs"),
        p_cross_crossperf = cor.test(cross, crossperf)$p.value
      )
  }
  
  return(correlations)
}

#### calculate pairwise correlations from correlation points ####
calculate_pairwise_correlations_fromcorpoints <- function(data, measure, conditions) {
  # Initialize results list
  results <- list()
  
  # Get unique levels of the measure
  if (measure == "ttc") {
    levels <- unique(data$ttc)
  } else if (measure == "var_sd") {
    levels <- unique(data$var_sd)
  }
  
  # Calculate correlations for each level
  for (level in levels) {
    level_data <- data %>% filter(!!sym(measure) == level)
    
    cors <- list()
    for (i in 1:(length(conditions)-1)) {
      for (j in (i+1):length(conditions)) {
        cond1 <- conditions[i]
        cond2 <- conditions[j]
        
        cor_test <- cor.test(level_data[[cond1]], level_data[[cond2]])
        cors[[paste0("cor_", cond1, "_", cond2)]] <- cor_test$estimate
        cors[[paste0("p_", cond1, "_", cond2)]] <- cor_test$p.value
      }
    }
    
    result_row <- c(list(level = level), cors)
    results[[length(results) + 1]] <- result_row
  }
  
  # Convert to dataframe
  df <- bind_rows(results)
  names(df)[1] <- measure
  
  return(df)
}

#### create significance label ####
create_significance_label <- Vectorize(function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
})

#### create correlation string ####
create_correlation_string <- Vectorize(function(r, p) {
  if (is.na(r) || is.na(p)) return("")
  sig_label <- create_significance_label(p)
  sprintf("italic(r)==%.2f*'%s'", r, sig_label)
})

#### create correlation strings dataframe ####
create_correlation_strings <- function(paircors, meas, crossperf_include) {
  
  if (crossperf_include == 0) {
    corstrings <- paircors %>%
      mutate(
        corrstring_conf_cross = create_correlation_string(cor_conf_cross, p_conf_cross)
      ) %>%
      select(!!sym(meas), corrstring_conf_cross)
  } else {
    corstrings <- paircors %>%
      mutate(
        corrstring_conf_cross = create_correlation_string(cor_conf_cross, p_conf_cross),
        corrstring_conf_crossperf = create_correlation_string(cor_conf_crossperf, p_conf_crossperf),
        corrstring_cross_crossperf = create_correlation_string(cor_cross_crossperf, p_cross_crossperf)
      ) %>%
      select(!!sym(meas), corrstring_conf_cross, corrstring_conf_crossperf, corrstring_cross_crossperf)
  }
  
  return(corstrings)
}

#### orthogonal regression ####
orthogonal_regression <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric vectors.")
  }
  
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  
  x_centered <- x - mean(x)
  y_centered <- y - mean(y)
  
  pca <- prcomp(cbind(x_centered, y_centered))
  
  slope <- pca$rotation[2, 1] / pca$rotation[1, 1]
  
  intercept <- mean(y) - slope * mean(x)
  
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  y_min <- max(min(y, na.rm = TRUE), slope * x_min + intercept)
  y_max <- min(max(y, na.rm = TRUE), slope * x_max + intercept)
  
  return(list(
    slope = slope,
    intercept = intercept,
    x_min = x_min,
    x_max = x_max,
    y_min = y_min,
    y_max = y_max
  ))
}

#### raycast for gaze detection ####
raycast_vec <- function(car_x, car_z, helmet_x, helmet_z, gaze_x, gaze_z, theta_x = 10, theta_z = 5) {
  dx <- gaze_x - helmet_x
  dz <- gaze_z - helmet_z
  length_sq <- dx^2 + dz^2
  
  d <- sqrt((car_x - helmet_x)^2 + (car_z - helmet_z)^2)
  radius_x <- 2 * d * tan((theta_x / 2) * pi / 180)
  radius_z <- 2 * d * tan((theta_z / 2) * pi / 180)
  
  t <- ((car_x - helmet_x) * dx + (car_z - helmet_z) * dz) / length_sq
  t[t < 0] <- NA
  
  closest_x <- helmet_x + t * dx
  closest_z <- helmet_z + t * dz
  
  distance_x <- abs(car_x - closest_x)
  distance_z <- abs(car_z - closest_z)
  
  hit <- (distance_x / radius_x)^2 + (distance_z / radius_z)^2 <= 1
  hit[is.na(hit)] <- FALSE
  
  return(hit)
}

#### detect gaze on cars ####
detect_gaze_on_cars_vec <- function(df, gaze_length = 400, theta_x = 10, theta_z = 5) {
  df %>%
    lazy_dt() %>%
    mutate(
      gaze_end_x = helmet_posx + gaze_length * cos(gaze_angx * pi / 180) * cos((-gaze_angy + 90) * pi / 180),
      gaze_end_z = helmet_posz + gaze_length * cos(gaze_angx * pi / 180) * sin((-gaze_angy + 90) * pi / 180),
      car1_look = raycast_vec(car1_posx, car1_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z),
      car2_look = raycast_vec(car2_posx, car2_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z),
      car3_look = raycast_vec(car3_posx, car3_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z),
      car4_look = raycast_vec(car4_posx, car4_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z),
      car5_look = raycast_vec(car5_posx, car5_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z),
      car6_look = raycast_vec(car6_posx, car6_posz, helmet_posx, helmet_posz, gaze_end_x, gaze_end_z, theta_x, theta_z)
    ) %>%
    as_tibble()
}

#### calculate proportion of car looking ####
calculate_propcarlook <- function(df) {
  dfprops <- df %>%
    group_by(participant, cond, session, trial_within_session, ttc, var_sd, var_name) %>%
    mutate(carslooked = ifelse(car1_look | car2_look | car3_look | car4_look | car5_look | car6_look,1,0)) %>%
    summarise(propcarlook = sum(carslooked) / n())
  
  return(dfprops)
}

#### create lane difference proportions ####
create_lanedif_props <- function(df, resps_df) {
  
  cur_cond <- unique(df$cond)
  df %>%
    ungroup() %>%
    mutate(
      carsnear_looked = as.integer(car1_look | car2_look | car3_look),
      carsfar_looked  = as.integer(car4_look | car5_look | car6_look)
    ) %>%
    group_by(participant, cond, session, trial_within_session, ttc, var_name) %>%
    summarise(
      near_prop = mean(carsnear_looked),
      far_prop  = mean(carsfar_looked),
      diff_far_minus_near = far_prop - near_prop,
      .groups = "drop"
    ) %>%
    left_join(resps_df %>% filter(cond == cur_cond), by = c("participant", "cond", "session", "trial_within_session", "ttc","var_name"))
}
