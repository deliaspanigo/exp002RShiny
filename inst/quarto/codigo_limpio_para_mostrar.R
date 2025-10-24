---
title: "Rscience - _script_modulo"
format: 
  html:
    theme: 
      - custom_theme.scss
    grid:
      body-width: 2000px
      margin-width: 250px
      gutter-width: 1.5rem
    toc: true
    toc-float: true
    toc-location: right
    self-contained: true
    link-citations: true # Util para ver referencias en el margen (ver `tufte.html` en la galería de Quarto)

knitr: 
    opts_chunk: 
        collapse: false
        comment: ""
params:
    file_name: "mtcars"
    file_source: "r_source" 
    var_name_rv: "mpg"
    var_name_factor: "cyl"
    alpha_value: "0.05"
    vector_ordered_levels:
      - "6"
      - "4"
      - "8"
    vector_ordered_colors: 
      - "#000000"
      - "#00FF00"
      - "#0000FF"
---

# Libraries
  library("htmlwidgets")
  library("knitr")

# Initials
file_name   <- params$"file_name"
file_source <- params$"file_source"
var_name_rv <- params$"var_name_rv"
var_name_factor <- params$"var_name_factor"
alpha_value <- as.numeric(as.character(params$"alpha_value"))
vector_ordered_levels <- params$"vector_ordered_levels"
vector_ordered_colors <- params$"vector_ordered_colors"

#vector_ordered_colors <- params$"vector_ordered_colors"

# Basics lvl 01
check_r_source        <- file_source == "r_source"
check_rscience_source <- file_source == "rscience_source"
check_excel_source    <- file_source == "excel_source"
check_csv_source      <- file_source == "csv_source"

if(check_r_source) my_dataset <- get(file_name)

### INIT CODE ###
# # # # # Section 01 - Libraries ---------------------------------------------
  library("stats")     # General Linear Models
  library("agricolae") # Tukey test
  library("plotly")    # Advanced graphical functions
  library("dplyr")     # Developing with %>%

# # # # # Section 02 - Import dataset ----------------------------------------
  #+++---my_dataset <- _+A+_my_import_sentence_+A+_
  head(x = my_dataset, n = 5)
  
# # # # # Section 03 - Settings ----------------------------------------------
  #+++---var_name_rv     <- "_+B+_var_name_rv_+B+_"
  #+++---var_name_factor <- "_+B+_var_name_factor_+B+_"
  #+++---alpha_value     <- _+B+_alpha_value_+B+_
  #+++---vector_ordered_levels <- _+C+_vector_ordered_levels_+C+_
  #+++---vector_ordered_colors <- _+C+_vector_ordered_colors_+C+_


# # # # # Section 04 - Standard actions --------------------------------------
  # The factor must be factor data type on R.
  my_dataset[,var_name_factor] <- as.factor(as.character(my_dataset[,var_name_factor]))
  my_dataset[,var_name_factor] <- factor(my_dataset[,var_name_factor], levels = vector_ordered_levels)

# # # # # Section 05 - Alpha and confidence value ----------------------------
  confidence_value <- 1 - alpha_value
  
  df_alpha_confidence <- data.frame(
    "order" = 1:2,
    "detail" = c("alpha value", "confidence value"),
    "probability" = c(alpha_value, confidence_value),
    "percentaje" =  paste0(c(alpha_value, confidence_value)*100, "%")
  )
  df_alpha_confidence

# # # # # Section 06 - Selected variables and roles  -------------------------
  vector_all_var_names <- colnames(my_dataset)
  vector_name_selected_vars <- c(var_name_rv, var_name_factor)
  vector_rol_vars <- c("RV", "FACTOR")
  

  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars
  
  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected variabless. 
  # Only completed rows. 
  # Factor columns as factor object in R.
  minibase <- na.omit(my_dataset[vector_name_selected_vars])
  #colnames(minibase) <- vector_rol_vars
  minibase[,var_name_factor] <- as.factor(minibase[,var_name_factor])
  head(x = minibase, n = 5)
  

  
  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minibase <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$"var_name",
    "var_role" = df_selected_vars$"var_role",
    "control" = c("is.numeric()", "is.factor()"),
    "verify" = c(is.numeric(minibase[,var_name_rv]), is.factor(minibase[,var_name_factor]))
  )
  df_control_minibase
  
  
  
  # # # my_dataset and minibase reps
  # Our 'n' is from minibase
  df_show_n <- data.frame(
    "object" = c("my_dataset", "minibase"),
    "n_col" = c(ncol(my_dataset), ncol(minibase)),
    "n_row" = c(nrow(my_dataset), nrow(minibase))
  )
  df_show_n
  
  
  
  # # # Factor info
  # Default order for levels its alphabetic order.
  df_factor_info <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "color" = vector_ordered_colors
  )
  rownames(df_factor_info) <- 1:nrow(df_factor_info)
  df_factor_info
  
  
  
  # # # Unbalanced reps for levels?
  # Important information for Tukey.
  # If reps its equal or not equal in all levels must be detailled
  # on Tukey.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps
  
  phrase_yes_unbalanced <- "The design is unbalanced in repetitions. A correction is applied to the Tukey test."
  phrase_no_unbalanced  <- "The design is unbalanced in replicates. A correction should be applied to the Tukey test."
  phrase_selected_check_unbalanced <- ifelse(test = check_unbalanced_reps, 
                                  yes = phrase_yes_unbalanced,
                                  no  = phrase_no_unbalanced)
  
  phrase_selected_check_unbalanced
  
  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  the_formula <- paste0(var_name_rv,  " ~ " , var_name_factor)
  the_formula <- as.formula (the_formula)
  lm_anova <- lm(formula = the_formula, data = minibase)               # Linear model
  aov_anova <- aov(lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(aov_anova)[[1]])   # Common anova table
  df_table_anova
  
  
  
  # # # Standard error from model for each level
  model_error_var_MSE <- df_table_anova$`Mean Sq`[2]
  model_error_sd <- sqrt(model_error_var_MSE)
  
  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var_MSE" = model_error_var_MSE,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error
  
  
  
  
  
  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on my_dataset there are on minibase
  dt_rows_my_dataset_ok <- rowSums(!is.na(my_dataset[vector_name_selected_vars])) == ncol(minibase)
  
  
  
  # # # Object minibase_mod and new cols
  minibase_mod <- minibase
  minibase_mod$"lvl_order_number" <- as.numeric(minibase_mod[,2])
  minibase_mod$"lvl_color" <- df_factor_info$color[minibase_mod$"lvl_order_number"]
  minibase_mod$"fitted.values" <- df_factor_info$"mean"[minibase_mod$"lvl_order_number"]
  minibase_mod$"residuals" <- lm_anova$residuals
  minibase_mod$"id_my_dataset" <- c(1:nrow(my_dataset))[dt_rows_my_dataset_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"studres" <- minibase_mod$"residuals"/model_error_sd
  
  
  
  
  
  # # # # # Section 08 - Requeriments for residuals-------------------------------
  # # # Normality test (Shapiro-Wilk)
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality
  
  
  
  
  # # # Homogeinidy test (Bartlett)
  the_formula_bartlett <- paste0("residuals", " ~ ", var_name_factor)
  the_formula_bartlett <- as.formula(the_formula_bartlett)
  test_residuals_homogeneity <- bartlett.test(the_formula_bartlett, data = minibase_mod)
  test_residuals_homogeneity
  
  
  
  # # # Residuals variance from levels from original residuals
  df_raw_error <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length),
    "raw_error_var" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "raw_error_sd" = tapply(minibase_mod$residuals, minibase_mod[,2], sd)
  )
  df_model_error["raw_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$"n")
  rownames(df_raw_error) <- 1:nrow(df_raw_error)
  df_raw_error
  
  phrase_info_errors <- c("Anova and Tukey use MSE from model.", 
                          "Bartlett use variance from raw error on each level.",
                          "Only if there is homogeneity from raw error variances then is a good idea take desition from MSE.")
  phrase_info_errors
  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals
  
  
  
  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals
  
  
  ##################################
  p_value_shapiro  <- test_residuals_normality$p.value
  p_value_bartlett <- test_residuals_homogeneity$p.value
  p_value_anova    <- df_table_anova$"Pr(>F)"[1]
  
  vector_p_value <- c(p_value_shapiro, p_value_bartlett, p_value_anova)
  vector_logic_rejected <- vector_p_value < alpha_value
  vector_ho_decision <- ifelse(test = vector_logic_rejected, yes = "Ho Rejected", "Ho no rejected")
  vector_ho_rejected <- ifelse(test = vector_logic_rejected, yes = "Yes", "No")
  
  df_summary_anova <- data.frame(
    "test" = c("Shapiro-Wilk test", "Bartlett test", "Anova 1 way"),
    "aim"  = c("Normality", "Homogeneity", "Mean"),
    "variable"    = c("residuals", "residuals", var_name_rv),
    "p_value"     = vector_p_value,
    "alpha_value" = c(alpha_value, alpha_value, alpha_value),
    "Decision"    = vector_ho_decision
  )
  
  df_summary_anova
  
  check_shapiro_rejected      <- p_value_shapiro < alpha_value
  phrase_shapiro_yes_rejected <- "The null hypothesis of normal distribution of residuals is rejected."
  phrase_shapiro_no_rejected  <- "The null hypothesis of normal distribution of residuals is not rejected."
  phrase_shapiro_selected     <- ifelse(test = check_shapiro_rejected, 
                                        yes = phrase_shapiro_yes_rejected, 
                                        no = phrase_shapiro_no_rejected)
  phrase_shapiro_selected 
  
  
  check_bartlett_rejected      <- p_value_bartlett < alpha_value
  phrase_bartlett_yes_rejected <- "The hypothesis of homogeneity of variances (homoscedasticity) is rejected."
  phrase_bartlett_no_rejected  <- "The hypothesis of homogeneity of variances (homoscedasticity) is not rejected."
  phrase_bartlett_selected     <- ifelse(test = check_bartlett_rejected, 
                                         yes = phrase_bartlett_yes_rejected, 
                                         no = phrase_bartlett_no_rejected)
  phrase_bartlett_selected
  
  
  check_ok_all_requeriments     <- sum(vector_logic_rejected[c(1,2)]) == 2
  phrase_requeriments_yes_valid <- "All residual assumptions are met, so it is valid to draw conclusions from the ANOVA test."
  phrase_requeriments_no_valid  <- "Not all model assumptions are met, so it is NOT valid to draw conclusions from the ANOVA test."
  phrase_requeriments_selected  <- ifelse(test = check_ok_all_requeriments, 
                                          yes = phrase_requeriments_yes_valid, 
                                          no = phrase_requeriments_no_valid)
  
  phrase_requeriments_selected  
  
  
  
  check_anova_rejected      <- p_value_anova < alpha_value
  phrase_anova_yes_rejected <- "The null hypothesis of the ANOVA test is rejected. There are statistically significant differences in at least one level of the factor."
  phrase_anova_no_rejected  <- "The null hypothesis of the ANOVA test is not rejected. All levels of the factor are statistically equal."
  phrase_anova_selected     <- ifelse(test = check_anova_rejected, 
                                      yes = phrase_anova_yes_rejected, 
                                      no = phrase_anova_no_rejected)
  
  phrase_anova_selected <- ifelse(
    test = check_ok_all_requeriments,
    yes = phrase_anova_selected,
    no = "Regardless of the p-value obtained in ANOVA, it is not valid to draw conclusions."
  )
  
  phrase_anova_selected
  ##############################################################################
  tukey01_full_groups <- agricolae::HSD.test(y = lm_anova,
                                             trt = colnames(minibase_mod)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)
  
  
  
  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = lm_anova,
                                            trt = colnames(minibase_mod)[2],
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)
  
  
  
  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table
  
  
  
  # # # New table about Tukey
  df_tukey_table <- data.frame(
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
  df_tukey_table
  
  
  
  
  
  # # # # # Section 10 - Partitioned Measures (VR)--------------------------------
  # # # Partitioned Measures of Position (VR)
  df_vr_position_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "Q1" = tapply(minibase[,1], minibase[,2], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "Q3" = tapply(minibase[,1], minibase[,2], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )
  
  
  
  # # # Partitioned Measures of Dispersion (VR)
  df_vr_dispersion_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )
  df_vr_dispersion_levels
  
  
  
  # # # General Measures of Position (VR)
  df_vr_position_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )
  df_vr_position_general
  
  
  
  # # # General Measures of Dispersion (VR)
  df_vr_dispersion_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )
  df_vr_dispersion_general
  
  
  
  
  
  # # # # # Section 11 - Partitioned Measures (Residuals)-------------------------
  # # # Partitioned Measures of Position (residuals)
  df_residuals_position_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_position_levels
  
  
  
  # # # Partitioned Measures of Dispersion (residuals)
  df_residual_dispersion_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residual_dispersion_levels
  
  
  
  # # # General Measures of Position (residuals)
  df_residuals_position_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_position_general
  
  
  
  # # # General Measures of Dispersion (residuals)
  df_residuals_dispersion_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_dispersion_general
  
  
  
  
  
  # # # # # Section 12 - Model estimators ----------------------------------------
  # # # Means for each level
  vector_est_mu_i <- df_vr_position_levels$mean
  vector_est_mu_i
  
  
  
  # # # Mean of means
  est_mu <- mean(vector_est_mu_i)
  vector_est_mu <- rep(est_mu, length(vector_est_mu_i))
  vector_est_mu
  
  
  
  # # # Tau efects
  vector_est_tau_i <- vector_est_mu_i - vector_est_mu
  vector_est_tau_i
  
  
  
  # # # Sum of tau efects
  sum_est_tau_i <- sum(vector_est_tau_i)
  sum_est_tau_i
  
  
  
  # # # Long model information on dataframe
  df_anova1way_model_long <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu" = vector_est_mu,
    "est_tau_i" = vector_est_tau_i
  )
  df_anova1way_model_long
  
  
  
  # # # Short model information on dataframe
  df_anova1way_model_short <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu_i" = vector_est_mu_i
  )
  df_anova1way_model_short
  
  
  
  
  
  # # # # # Section 13 - Special table to plots ----------------------------------
  
  # # # Table for plot001
  df_table_factor_plot001 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "sd" = tapply(minibase[,1], minibase[,2], sd),
    "var" = tapply(minibase[,1], minibase[,2], var)
  )
  
  df_table_factor_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_sd" = df_model_error$model_error_sd
  )
  df_table_factor_plot002["lower_limit"] <- df_table_factor_plot002$mean - df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["upper_limmit"] <- df_table_factor_plot002$mean + df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["color"] <- df_factor_info$color
  df_table_factor_plot002
  
  
  
  df_table_factor_plot003 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_table_factor_plot003["lower_limit"] <- df_table_factor_plot003$mean - df_table_factor_plot003$model_error_se
  df_table_factor_plot003["upper_limmit"] <- df_table_factor_plot003$mean + df_table_factor_plot003$model_error_se
  df_table_factor_plot003["color"] <- df_factor_info$color
  df_table_factor_plot003
  
  
  
  # # # Table for plot004
  df_table_factor_plot004 <- df_vr_position_levels
  df_table_factor_plot004["color"] <- df_factor_info$color
  
  # # # Table for plot005
  df_table_factor_plot005 <- df_table_factor_plot004
  
  # # # Table for plot006
  df_table_factor_plot006 <- df_table_factor_plot004
  
  
  df_table_factor_plot007 <- df_table_factor_plot003
  correct_pos_letters <- order(df_tukey_table$level)
  vector_letters <- df_tukey_table$group[correct_pos_letters]
  df_table_factor_plot007["group"] <- vector_letters
  
  # # # Table for plot006
  df_table_residuals_plot001 <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "var" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "sd" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "color" = df_factor_info$color
  )
  df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot002 <- df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot003 <- df_table_residuals_plot001
  
  # # # Table for plot006
  df_table_residuals_plot004 <- data.frame(
    "variable" = "residuals",
    "n" = length(minibase_mod$residuals),
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "var" = var(minibase_mod$residuals),
    "sd" = sd(minibase_mod$residuals),
    "model_error_var_MSE" = model_error_var_MSE,
    "model_error_sd" = model_error_sd
  )
  
  phrase_coment_errors <- "Model Error (MSE) "
  
  # # # Table for plot006
  df_table_residuals_plot005  <- df_table_residuals_plot004
  
  # # # Table for plot006
  df_table_residuals_plot006 <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "n" = tapply(minibase_mod$studres, minibase_mod[,2], length),
    "min" = tapply(minibase_mod$studres, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$studres, minibase_mod[,2], mean),
    "max" = tapply(minibase_mod$studres, minibase_mod[,2], max),
    "var" = tapply(minibase_mod$studres, minibase_mod[,2], var),
    "sd" = tapply(minibase_mod$studres, minibase_mod[,2], sd),
    "color" = df_factor_info$color
  )
  
  
  # # # Table for plot006
  df_table_residuals_plot007 <- df_table_residuals_plot006
  
  
  df_table_residuals_plot008 <- data.frame(
    "variable" = "studres",
    "n" = length(minibase_mod$studres),
    "min" = min(minibase_mod$studres),
    "mean" = mean(minibase_mod$studres),
    "max" = max(minibase_mod$studres),
    "var" = var(minibase_mod$studres),
    "sd" = sd(minibase_mod$studres)
  )
  
  
  df_table_residuals_plot009 <- df_table_residuals_plot008
  
  df_table_residuals_plot010 <- df_table_residuals_plot008
  
  #############################################################
  # # # Create a new plot...
  plot001_factor <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot001_factor <- plotly::add_trace(p = plot001_factor,
                                      type = "scatter",
                                      mode = "markers",
                                      x = minibase_mod[,var_name_factor],
                                      y = minibase_mod[,var_name_rv],
                                      color = minibase_mod[,var_name_factor],
                                      colors = df_factor_info$color,
                                      marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     title = "Plot 001 - Scatterplot",
                                     font = list(size = 20),
                                     margin = list(t = 100))
  
  
  # # # Without zerolines
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))
  
  
  # # # Plot output
  plot001_factor
  
  
  ##############################################################################
  
  # # # Create a new plot...
  plot002_factor <- plot_ly()
  
  
  # # # Adding errors...
  plot002_factor <-   add_trace(p = plot002_factor,
                                type = "scatter",
                                mode = "markers",
                                x = df_table_factor_plot002$level,
                                y = df_table_factor_plot002$mean,
                                color = df_table_factor_plot002$level,
                                colors = df_table_factor_plot002$color,
                                marker = list(symbol = "line-ew-open",
                                              size = 50,
                                              opacity = 1,
                                              line = list(width = 5)),
                                error_y = list(type = "data", array = df_table_factor_plot002$model_error_sd)
  )
  
  
  # # # Title and settings...
  plot002_factor <- plotly::layout(p = plot002_factor,
                                   title = "Plot 002 - Mean and model standard deviation",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot002_factor <-plotly::layout(p = plot002_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  # # # Plot output
  plot002_factor
  
  ##############################################################################
  
  
  # # # Create a new plot...
  plot003_factor <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot003_factor <-   plotly::add_trace(p = plot003_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_factor_plot003$level,
                                        y = df_table_factor_plot003$mean,
                                        color = df_table_factor_plot003$level,
                                        colors = df_table_factor_plot003$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_factor_plot003$model_error_se)
  )
  
  
  # # # Title and settings...
  plot003_factor <- plotly::layout(p = plot003_factor,
                                   title = "Plot 003 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot003_factor <-plotly::layout(p = plot003_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  # # # Plot output
  plot003_factor
  
  ##############################################################################
  
  
  # # # New plotly...
  plot004_factor <- plotly::plot_ly()
  
  # # # Boxplot and info...
  plot004_factor <- plotly::add_trace(p = plot004_factor,
                                      type = "box",
                                      x = df_table_factor_plot004$level ,
                                      color = df_table_factor_plot004$level,
                                      colors = df_table_factor_plot004$color,
                                      lowerfence = df_table_factor_plot004$min,
                                      q1 = df_table_factor_plot004$Q1,
                                      median = df_table_factor_plot004$median,
                                      q3 = df_table_factor_plot004$Q3,
                                      upperfence = df_table_factor_plot004$max,
                                      boxmean = TRUE,
                                      boxpoints = FALSE,
                                      line = list(color = "black", width = 3)
  )
  
  # # # Title and settings...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   title = "Plot 004 - Boxplot and means",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))
  
  # # # Output plot004_anova...
  plot004_factor
  
  ##############################################################################
  
  all_levels <- levels(minibase_mod[,2])
  n_levels <- length(all_levels)
  all_color <- rainbow(length(all_levels))
  
  
  
  plot005_factor <- plot_ly()
  
  # Violinplot
  for (k in 1:n_levels){
    
    # Selected values
    selected_level <- all_levels[k]
    selected_color <- all_color[k]
    dt_filas <- minibase_mod[,2] == selected_level
    
    # Plotting selected violinplot
    plot005_factor <- plot005_factor %>%
      add_trace(x = minibase_mod[,2][dt_filas],
                y = minibase_mod[,1][dt_filas],
                type = "violin",
                name = paste0("violin", k),
                points = "all",
                marker = list(color = selected_color),
                line = list(color = selected_color),
                fillcolor = I(selected_color)
                
      )
    
    
  }
  
  
  
  
  # Boxplot
  plot005_factor <- plotly::add_trace(p = plot005_factor,
                                      type = "box",
                                      name = "boxplot",
                                      x = df_table_factor_plot005$level ,
                                      color = df_table_factor_plot005$level ,
                                      lowerfence = df_table_factor_plot005$min,
                                      q1 = df_table_factor_plot005$Q1,
                                      median = df_table_factor_plot005$median,
                                      q3 = df_table_factor_plot005$Q3,
                                      upperfence = df_table_factor_plot005$max,
                                      boxmean = TRUE,
                                      boxpoints = TRUE,
                                      fillcolor = df_table_factor_plot005$color,
                                      line = list(color = "black", width = 3),
                                      opacity = 0.5,
                                      width = 0.2)
  
  
  # # # Title and settings...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   title = "Plot 005 - Violinplot",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot005_factor
  
  ##############################################################################
  
  
  #library(plotly)
  plot006_factor <- plotly::plot_ly()
  
  # Add traces
  plot006_factor <- plotly::add_trace(p = plot006_factor,
                                      type = "violin",
                                      y = minibase_mod$VR,
                                      x = minibase_mod$FACTOR,
                                      showlegend = TRUE,
                                      side = "positive",
                                      points = "all",
                                      name = "Violinplot",
                                      color = minibase_mod$FACTOR,
                                      colors = df_table_factor_plot006$color)
  
  
  
  # # # Title and settings...
  plot006_factor <- plotly::layout(p = plot006_factor,
                                   title = "Plot 006 - Scatterplot + Jitter +  Smoothed",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot006_factor <- plotly::layout(p = plot006_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot006_factor
  
  ##############################################################################
  
  # # # Create a new plot...
  plot007_factor <- plotly::plot_ly()
  
  
  # # # Adding errors...
  plot007_factor <-   plotly::add_trace(p = plot007_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_factor_plot007$level,
                                        y = df_table_factor_plot007$mean,
                                        color = df_table_factor_plot007$level,
                                        colors = df_table_factor_plot007$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_factor_plot007$model_error_se)
  )
  
  
  
  plot007_factor <-  add_text(p = plot007_factor,
                              x = df_table_factor_plot007$level,
                              y = df_table_factor_plot007$mean,
                              text = df_table_factor_plot007$group, name = "Tukey Group",
                              size = 20)
  
  # # # Title and settings...
  plot007_factor <- plotly::layout(p = plot007_factor,
                                   title = "Plot 007 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))
  
  # # # Without zerolines
  plot007_factor <-plotly::layout(p = plot007_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))
  
  
  # # # Plot output
  plot007_factor
  
  #######----
  
  ####### DESDE ACAAAAAAAAAAAAAAAAAAAA
  # # # Create a new plot...
  plot001_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot001_residuals <- plotly::add_trace(p = plot001_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$FACTOR,
                                         y = minibase_mod$residuals,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        title = "Plot 001 - Scatterplot - Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot001_residuals
  
  
  
  
  
  #library(plotly)
  plot002_residuals <- plotly::plot_ly()
  
  # Add traces
  plot002_residuals <- plotly::add_trace(p = plot002_residuals,
                                         type = "violin",
                                         y = minibase_mod$residuals,
                                         x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = "Violinplot",
                                         color = minibase_mod$FACTOR,
                                         colors = df_table_residuals_plot002$color)
  
  
  
  # # # Title and settings...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      title = "Plot 002 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      xaxis = list(zeroline = FALSE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot002_residuals
  
  
  
  
  
  
  plot003_residuals <- plotly::plot_ly()
  
  # Add traces
  plot003_residuals <- plotly::add_trace(p = plot003_residuals,
                                         type = "violin",
                                         x = minibase_mod$residuals,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = FALSE,
                                         #name = levels(minibase_mod$FACTOR)[minibase_mod$lvl_order_number],
                                         color = minibase_mod$FACTOR,
                                         colors = df_table_residuals_plot003$color)
  
  
  
  # # # Title and settings...
  plot003_residuals <- plotly::layout(p = plot003_residuals,
                                      title = "Plot 003 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot003residuals <- plotly::layout(p = plot003_residuals,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot003_residuals
  
  
  
  
  
  plot004_residuals <- plotly::plot_ly()
  
  # Add traces
  plot004_residuals <- plotly::add_trace(p = plot004_residuals,
                                         type = "violin",
                                         x = minibase_mod$residuals,
                                         #x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minibase_mod$FACTOR,
  #colors = df_table_factor_plot006$color)
  
  
  
  # # # Title and settings...
  plot004_residuals <- plotly::layout(p = plot004_residuals,
                                      title = "Plot 004 - Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot004_residuals <- plotly::layout(p = plot004_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot004_residuals
  
  
  
  
  # - el 5
  qq_info <- EnvStats::qqPlot(x = minibase_mod$residuals, plot.it = F,
                              param.list = list(mean = mean(minibase_mod$residuals),
                                                sd = sd(minibase_mod$residuals)))
  
  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y
  
  #library(plotly)
  plot005_residuals <- plotly::plot_ly()
  
  # Crear el gráfico QQ plot
  plot005_residuals <-add_trace(p = plot005_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")
  
  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0
  
  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto
  
  # Agregar la recta de identidad
  plot005_residuals <- add_trace(p = plot005_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")
  
  
  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))
  
  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      title = "Plot 005 - QQ Plot Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  # Mostrar el gráfico
  plot005_residuals
  # - Fin el 5
  
  
  
  # # # Create a new plot...
  plot006_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot006_residuals <- plotly::add_trace(p = plot006_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$fitted.values,
                                         y = minibase_mod$residuals,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot006_residuals <-   plotly::layout(p = plot006_residuals,
                                        title = "Plot 006 - Scatterplot - Residuals vs Fitted.values",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot006_residuals <-   plotly::layout(p = plot006_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot006_residuals
  
  
  
  
  # # # Create a new plot...
  plot007_residuals <- plotly::plot_ly()
  
  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot007_residuals <- plotly::add_trace(p = plot007_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$FACTOR,
                                         y = minibase_mod$studres,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))
  
  # # # Title and settings...
  plot007_residuals <-   plotly::layout(p = plot007_residuals,
                                        title = "Plot 007 - Scatterplot - Studentized Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))
  
  
  # # # Without zerolines
  plot007_residuals <-   plotly::layout(p = plot007_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))
  
  
  # # # Plot output
  plot007_residuals
  
  
  
  
  
  
  
  #library(plotly)
  plot008_residuals <- plotly::plot_ly()
  
  # Add traces
  plot008_residuals <- plotly::add_trace(p = plot008_residuals,
                                         type = "violin",
                                         x = minibase_mod$studres,
                                         #x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minibase_mod$FACTOR,
  #colors = df_table_factor_plot006$color)
  
  
  
  # # # Title and settings...
  plot008_residuals <- plotly::layout(p = plot008_residuals,
                                      title = "Plot 008 - Studentized Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  
  # # # Without zerolines...
  plot008_residuals <- plotly::layout(p = plot008_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))
  
  # # # Output plot003_anova...
  plot008_residuals
  
  
  
  
  # el 9
  
  x <- seq(-4, 4, length.out = 100)
  y <- dnorm(x, mean = 0, 1)
  #  x <- x*model_error_sd
  densidad_suavizada <- density(x, kernel = "gaussian", adjust = 0.5)
  hist_data_studres <- hist(minibase_mod$studres, plot = FALSE)
  hist_data_studres$"rel_frec" <- hist_data_studres$counts/sum(hist_data_studres$counts)
  
  densidad_studres <-  density(x = minibase_mod$studres, kernel = "gaussian", adjust =0.5)
  
  #library(plotly)
  plot009_residuals <- plotly::plot_ly()
  
  
  # plot005_residuals <- add_trace(p = plot005_residuals,
  #                                x = densidad_studres$x,
  #                                y = densidad_studres$y,
  #                                type = 'scatter',
  #                                mode = 'lines',
  #                                name = 'densidad_studres')
  
  plot009_residuals <- add_trace(p = plot009_residuals,
                                 x = x,
                                 y = y,
                                 type = 'scatter',
                                 mode = 'lines',
                                 name = 'Normal Standard')
  
  
  
  
  
  # # Add traces
  # plot005_residuals <- plotly::add_trace(p = plot005_residuals,
  #                                        type = "violin",
  #                                        x = minibase_mod$residuals,
  #                                        #x = minibase_mod$FACTOR,
  #                                        showlegend = TRUE,
  #                                        side = "positive",
  #                                        points = FALSE,
  #                                        name = "violinplot")#
  
  plot009_residuals <- plotly::add_trace(p = plot009_residuals,
                                         type = "bar",
                                         x = hist_data_studres$"mids",
                                         y = hist_data_studres$"density",
                                         name = "hist - studres")
  
  plot009_residuals <- plotly::layout(p = plot009_residuals,
                                      bargap = 0)
  
  plot009_residuals <- plotly::layout(p = plot009_residuals,
                                      title = "Plot 009 - Studres Distribution",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  plot009_residuals
  # fin el 9
  
  
  
  
  qq_info <- EnvStats::qqPlot(x = minibase_mod$studres, plot.it = F,
                              param.list = list(mean = 0,
                                                sd = 1))
  
  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y
  
  #library(plotly)
  plot010_residuals <- plotly::plot_ly()
  
  # Crear el gráfico QQ plot
  plot010_residuals <-add_trace(p = plot010_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")
  
  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0
  
  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto
  
  # Agregar la recta de identidad
  plot010_residuals <- add_trace(p = plot010_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")
  
  
  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))
  
  plot010_residuals <- plotly::layout(p = plot010_residuals,
                                      title = "Plot 010 - QQ Plot - studres",
                                      font = list(size = 20),
                                      margin = list(t = 100))
  
  # Mostrar el gráfico
  plot010_residuals
  
