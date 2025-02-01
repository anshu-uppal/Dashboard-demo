pacman::p_load(
  here,
  conflicted,
  tidyverse,
  survey,
  srvyr,
  kableExtra,
  gtsummary,
  flextable,
  conflicted,
  broom.helpers,
  epikit
)

# library(flextable)

# library(conflicted)
# library(tidyverse)
# library(flextable)
# #library(webshot) # for flextable output
# #library(patchwork)
# #library(gtsummary) #for clean summary table function
# #library(janitor) #Use of tabyl function and adorn_totals
# #library(scales) #for plotting options
# #library(rrtable) #for plotting options
# library(survey) #to rake
# library(srvyr)
# #library(ggsurvey)
# #library(skimr)
# #library(gt) #to save the tbl_summary table

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::group_rows)

# run oxygen to document the functions (not working just yet)
# roxygen2::roxygenise()

# Standard format of our flextables -------
myflextable = function(data, ...) {
  set_flextable_defaults(na_str = "NA", theme_fun = theme_booktabs, font.size = 12, padding.bottom = 1, padding.top = 1)
  x = flextable(data, ...)
  x = colformat_int(x, big.mark = "")
  x = colformat_double(x, big.mark = "", digits = 2, na_str = "NA")
  return(x)
}

# mode = the most observed value --------
Mode = function(x, na.rm = FALSE) {
  if(na.rm) {
    x = x[!is.na(x)]
  }
  ux = unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Calculation functions ---------
# calculate svyciprop zero/one for each possible level ??
svyciprop_fun = function(this_level, design, var1, var2, ci = FALSE , ... ){
  this_formula <- as.formula( paste0( '~as.numeric( "' , this_level , '" == interaction(', var1, ',',  var2, '))'))
  
  res <- svyciprop( this_formula , design , ... )
  if (ci) res <- confint(res)
  res
}

#' Calculate population weighted means and CIs for one variable
#'
#' @param design A survey design object
#' @param var Variable to calculate population weighted mean for. 
#' Must be in the design!
#' @param proportion If TRUE, computes confidence intervals for proportions 
#' using methods that may be more accurate near 0 and 1 ("logit" method of svyciprop 
#' fits a logistic regression model and computes a Wald-type interval on the log-odds 
#' scale, which is then transformed to the probability scale).
#' I recommend putting TRUE, but you must specify it to show you've thought about it!
#'
#' @return Tibble with means and CIs for each level of var.
#' Columns with "total" and "Total" are included for ease of later binding.
#' Totals of the weights for each level of var are
#' included along with total number of non-missing data.
#'
#' @examples calc_weighted_univar(weighted_dat, dgs_physical_risk_group)
calc_weighted_univar = function(design, var, proportion) {
  design %>%
    as_survey_design() %>% # create a srvyr object from a survey::svydesign
    drop_na({{var}}) %>% # drop NAs
    group_by({{var}}) %>%  # specify that the further calculutions have to be done grouping by var
    (\(.df) summarise(.df, # hack for using piped object more than once e.g. https://stackoverflow.com/a/76013271/3275826
                      survey_mean(vartype = "ci", na.rm = TRUE,
                                  proportion = proportion, prop_method = "xlogit"),# specify the vartype to have the confidence interval. it could be "se" for standard error 
                      n_non_miss = as.integer(unwtd.count(~{{var}}, .df)),# number of non-missing observations in each subset. Observations with exactly zero weight will also be counted as missing
                      group_wt_total = survey_total(vartype = NULL),
                      unweighted_N = as.integer(unweighted(n()))
    ))() |> # don't think we need the total weight variability so set vartype=NULL
    ungroup() %>%# used to cancel the group_by effect
    drop_na() %>% # drop all the NAs from the entire df
    add_column(level = "Total", .before = 1) %>% # create and add a column to the dataframe named "level" and containing "Total" in all the lines. The column is added to the left of the df
    add_column(var = "total", .before = 1) %>%  # create a column as previous but named "var"....
    mutate(across(c(coef, `_low`, `_upp`), \(value) { 100*value}))# Multiply (coef, _low, -upp) by 100
}

#' Calculate population weighted means and CIs for one variable, stratified by another
#'
#' @param design A survey design object
#' @param var Variable to stratify by. Must be in the design! 
#' @param fill_var Variable to calculate population-weighted mean for. 
#' Must be in the design!
#' @param proportion If TRUE, computes confidence intervals for proportions 
#' using methods that may be more accurate near 0 and 1 ("logit" method of svyciprop 
#' fits a logistic regression model and computes a Wald-type interval on the log-odds 
#' scale, which is then transformed to the probability scale).
#' I recommend putting TRUE, but you must specify it to show you've thought about it!
#'
#' @return Tibble with means and CIs for each level of fill_var, stratified by
#' all levels of var. Totals of the weights of these grouping combinations are
#' included along with total number of non-missing data.
#'
#' @examples calc_weighted_stacked(weighted_dat, sex, dgs_physical_risk_group)
calc_weighted_stacked = function(design, var, fill_var, proportion) {
  var_name = rlang::as_name(substitute(var))# don't know how to do this directly in the add_column :-(
  design %>%
    as_survey_design() |> # create a survey object with a survey design
    drop_na({{var}}, {{fill_var}}) |> # drop NAs from the variable and the fill variable
    group_by({{var}}, {{fill_var}}) |> # specify that the further calculations have to be done grouping by var and fill_var
    (\(.df) summarise(.df, # hack for using piped object more than once e.g. https://stackoverflow.com/a/76013271/3275826
                      survey_mean(vartype = "ci", na.rm = TRUE,
                                  proportion = proportion, prop_method = "xlogit"),# specify the vartype to have the confidence interval. it could be "se" for standard error 
                      n_non_miss = as.integer(unwtd.count(~{{var}} + {{fill_var}}, .df)),# number of non-missing observations in each subset. Observations with exactly zero weight will also be counted as missing
                      group_wt_total = survey_total(vartype = NULL),
                      unweighted_N = as.integer(unweighted(n()))
    ))() |> # don't think we need the total weight variability so set vartype=NULL
    ungroup() |> # used to cancel the group_by effect
    drop_na() |> # drop the NAs of the entire df
    rename(level = {{var}}) |> # change the name of the var
    add_column(var = var_name, .before = 1)  |> # add a new column "var" and all the lines are filled with the name of the var we put in the arguments
    mutate(across(c(coef, `_low`, `_upp`), \(value) { 100*value})) |> # Multiply (coef, _low, -upp) by 100
    mutate(level = as.character(level))
}

#' Round numbers greater than or equal to z to 1 decimal place, numbers smaller than z to 2 decimal places
#'
#' @param x a single number or a vector of numbers to round
#' @param z single number specifying threshold (default = 10)
#'
#' @return a single number or vector of numbers, same as input x
#' @export
#'
#' @examples
#' estimate_digits(head(ToothGrowth$len))
estimate_digits = function(x, z=10){
  y = rep(NA, length(x))
  for(i in 1:length(x)){
    if(is.na(x[i])){y[i] = "NA"} else if(abs(x[i])<z){y[i] = sprintf("%.2f", x[i])} else{y[i] = sprintf("%.1f", x[i])}
  }
  return(y)
}

#' Calculate population weighted means and CIs for one variable, stratified by multiple other variables
#'
#' @param design A survey design object
#' @param var Variable(s) to stratify by. Must be in the design! 
#' @param clean_var Vector of clean names
#' @param fill_var Variable to calculate population-weighted mean for. 
#' Must be in the design!
#' @param proportion If TRUE, computes confidence intervals for proportions 
#' using methods that may be more accurate near 0 and 1 ("logit" method of svyciprop 
#' fits a logistic regression model and computes a Wald-type interval on the log-odds 
#' scale, which is then transformed to the probability scale).
#' I recommend putting TRUE, but you must specify it to show you've thought about it!
#'
#' @return Tibble with means and CIs for each level of fill_var, stratified by
#' all levels of var. Totals of the weights of these grouping combinations are
#' included along with total number of non-missing data.
#'
#' @examples calc_weighted_stacked_multiple(design = weighted_dat_gh, var = c("Sex", "age_cat", "education_level_rec","Swiss_nat_rec"), clean_var = c("Sexe", "Classe d'âge", "Education", "Nationalité"), fill_var = "one_hour_smoke", proportion = TRUE)
calc_weighted_stacked_multiple = function(design, var = c("Sex", "age_cat", "education_level_rec"), clean_var = NULL, fill_var, proportion) {
  
  # Below is an option to integrate clean names, but other functions break, so for now will keep clean_var the same as var
  # if(is.null(clean_var) & setequal(var, c("Sex", "age_cat", "education_level_rec"))){
  #   clean_var <- c("Sexe", "Classe d'âge", "Education")} else if(is.null(clean_var)){clean_var = var}
  clean_var <- var # Simply keep clean_var as var for now, to not break other functions
  
  # First make "Totals" rows
  a <- design |> 
    as_survey_design() |> # create a survey object with a survey design
    drop_na(.data[[fill_var]]) |> # drop NAs from the variable and the fill variable
    group_by(.data[[fill_var]]) |> # specify that the further calculations have to be done grouping by fill_var
    summarise(
      survey_mean(vartype = "ci", na.rm = TRUE,  # specify the vartype to have the confidence interval. it could be "se" for standard error 
                  proportion = proportion, prop_method = "xlogit"),
      # n_non_miss = as.integer(unwtd.count(Sex + one_hour_smoke)),# number of non-missing observations in each subset. Observations with exactly zero weight will also be counted as missing
      group_wt_total = survey_total(vartype = NULL),
      unweighted_N = as.integer(unweighted(n())) # for each level of var
    ) |> ungroup() |> mutate(n_non_miss = sum(.data$unweighted_N)) |> # A total unweighted N of non-missing rows
    mutate(
      var = "total", # add a new column "var" and all the lines are filled with the name of the var we put in the arguments
      across(c(coef, `_low`, `_upp`), \(value) { 100*value}) # Multiply (coef, _low, -upp) by 100
    ) |> 
    mutate(level = "Total") |>
    relocate(var) |> 
    group_by(level) |> mutate(unweighted_N_level = sum(.data$unweighted_N)) |> ungroup() |> 
    relocate(unweighted_N_level, .after = unweighted_N) |> mutate(p_value = NA)
  
  # Run calculations again for each var
  for(i in 1:length(var)){
    result_df <- design |> 
      as_survey_design() |> # create a survey object with a survey design
      drop_na(.data[[var[i]]], .data[[fill_var]]) |> # drop NAs from the variable and the fill variable
      group_by(.data[[var[i]]], .data[[fill_var]]) |> # specify that the further calculations have to be done grouping by var and fill_var
      summarise(
        survey_mean(vartype = "ci", na.rm = TRUE,  # specify the vartype to have the confidence interval. it could be "se" for standard error 
                    proportion = proportion, prop_method = "xlogit"),
        # n_non_miss = as.integer(unwtd.count(Sex + one_hour_smoke)),# number of non-missing observations in each subset. Observations with exactly zero weight will also be counted as missing
        group_wt_total = survey_total(vartype = NULL),
        unweighted_N = as.integer(unweighted(n())) # for each level of var
      ) |> ungroup() |> mutate(n_non_miss = sum(.data$unweighted_N)) |> # A total unweighted N of non-missing rows
      mutate(
        var = clean_var[i], # add a new column "var" and all the lines are filled with the name of the var we put in the arguments
        across(c(coef, `_low`, `_upp`), \(value) { 100*value}) # Multiply (coef, _low, -upp) by 100
      ) |> 
      rename(level = var[i]) |>
      group_by(level) |> mutate(unweighted_N_level = sum(.data$unweighted_N)) |> ungroup() |> 
      relocate(unweighted_N_level, .after = unweighted_N)
    
    p_value_df <- svychisq(
      as.formula(paste("~", var[i], " + ", fill_var)),
      design = design |>
        as_survey_design() |> # create a survey object with a survey design
        drop_na(.data[[var[i]]], .data[[fill_var]]),
      na.rm = TRUE)
    result_df <- result_df |>
      mutate(
        p_value = p_value_df$p.value
      )
    
    # Iteratively bind all rows together
    a <- bind_rows(a, result_df) |> relocate(var, level)
  }
  a <- a |> 
    filter(!is.na(level)) |> 
    mutate(
      # across(c(coef, `_low`, `_upp`), estimate_digits),
      # across(c(coef, `_low`, `_upp`), function(x)(sprintf("%.2f", x))),
      # across(c(coef, `_low`, `_upp`), function(x)(round(x,digits = 2))),
      coef_full = paste0(estimate_digits(coef), " [",estimate_digits(`_low`),", ",estimate_digits(`_upp`),"]"),
      # coef_full = paste0(sprintf("%.2f", coef), " [",sprintf("%.2f", `_low`),", ",sprintf("%.2f", `_upp`),"]"),
      p_value_short = case_when(p_value < 0.001 ~ "<0.001",
                                p_value >= 0.001 ~ sprintf("%.3f",p_value),
                                .default = NA)) |> 
    relocate(p_value, .after = coef_full) |> 
    filter(!is.na(.data[[fill_var]]))
  return(a)
}

# calc weights by 2 variables
calc_weighted_by_2vars = function(design, var, var2, fill_var) {
  var_name = rlang::as_name(substitute(var))# don't know how to do this directly in the add_column :-(
  var2_name = rlang::as_name(substitute(var2)) # try to make optional 
  design %>%
    as_survey_design() |> # the data have to be considered as a survey design 
    drop_na({{var}}, {{var2}}, {{fill_var}}) |> # drop NA's on all the variables
    group_by({{var}}, {{var2}}, {{fill_var}}) |># group by these variables for the further calculus
    summarise(survey_mean(vartype = "ci", na.rm = TRUE)) |>
    ungroup() |> #  cancel the group_by() effect
    drop_na() |> # drop the NA's of the entire df
    rename(level = {{var}}, # The name of the var is replaced by "level"
           var2_level = {{var2}}) |> # The name of the var2 is replaced by "var2_level"
    add_column(var = var_name, .before = 1) |> # add a new column "var" and all the lines are filled with the name of the var we put in the arguments
    add_column(var2 = var2_name, .before = "var2_level") |> # add a new column "var2" just before the variable "var2_level". All the lines of this new variable are filled with the name of the var we put in the arguments
    mutate(across(where(is.numeric), \(value) { 100*value})) |> # Multiply by 100 all the variables that are numeric (coef, _low, -upp)
    complete(var, level, var2, var2_level, {{fill_var}}, fill = list(coef = 0, `_low` = NA, `_upp` = NA))# Normally to add all the combinations of two or more variables when they are missing. If they are missing. while creating the combination, the "coef" must be "0 and the "_low" and "-upp" are NA. 
}

# Plotting functions ---------
#' Make a barplot, optionally facetted by categories
#'
#' @param df Dataframe to be plotted with columns including coef, `_upp`, `_low`
#' @param facet If TRUE, do facet plot with socio-demographic variables: sex, age category and education level.
#' Make sure these variables are in the dataframe in the `var` column!
#'
#' @return A ggplot
esslike_plot = function(df, facet = FALSE) {
  df <- df %>% mutate(level = case_when(             # recode age groups to make them more readable in plots
    level == "[18,25)" ~ "18-24",
    level == "[20,25)" ~ "20-24",
    level == "[25,45)" ~ "25-44",
    level == "[45,65)" ~ "45-64",
    .default = level)
  )
  layer0 = ggplot(df, aes(x = fct_inorder(level), y = coef)) + # specify where what is the x and the y  variable from the table created with calc_weighted_stacked_new() or calc_weighted_stacked_new_prop() 
    # Fill Colour below is now official HUG digital secondaire couleur from Charte graphique 
    # (others are mixed with black, 5th from top, 10-11th from bottom using https://www.w3schools.com/colors/colors_mixer.asp)
    geom_bar(position = "dodge", stat = "identity", fill = "#65C6C1", colour = "#519e9a") + # specify the type of the bar with "position=", the color fill with "fill" and the bar outline with "colour"
    geom_errorbar(aes(y = coef, ymax = ifelse(`_upp`<100, `_upp`, 100), ymin = if_else(`_low`> 0, `_low`,0 )), # To have the minimum of the errorbar on 0 and maximum  of the errorbar on 100 %
                  position = position_dodge(width = 0.9), 
                  width = 0.3, colour = "#2F6158", # specify the width and the color of the errorbar
    ) + 
    scale_y_continuous(name = "Total (en %)", # specify the name of the y axe
                       expand = expansion(mult = c(0, 0.05)),
                       #limits=c(0,100), # To have the maximum of the bars on 100 %
                       n.breaks = 6) + # to have the percentage every 20%
    #labs(caption= paste("source : questionnaire Santé-Travail, Octobre 2022, \nN =", sum(!is.na(var)), "Répondant-es")) + # Add the number of individuals took in count
    theme_minimal() +
    theme(
      text = element_text(size = 13), # specify the font size of the x and y axis
      axis.title.x = element_blank(), # specify that the x axis have to be blank
      panel.grid.major.x = element_blank(), # Remove the vertical grids on the x-axis
      plot.title = element_text(size = 16),
      strip.placement = "outside",
      axis.line = element_line(colour = "grey50"),
      plot.caption = element_text(size = 12, hjust =0, vjust = 0 ), # I (Aminata) change the size of the text because Hélène finds the 9 too small
      plot.caption.position = "plot"
    )
  if (!facet) {
    return(layer0)
  } else if (facet) { 
    layer1 = layer0 + 
      facet_grid(~ fct_inorder(var), 
                 scales = "free_x", space = "free_x", switch = "x", 
                 labeller = as_labeller(c("total" = "", 
                                          "Sex" = "Sexe", 
                                          "sex" = "Sexe",
                                          "age_cat" = "Classe d'âge",
                                          "education_level" = "Education",
                                          "education_level_rec" = "Education",
                                          "education_rec_fr" = "Education"
                                          ))) # this line specify how the axis titles have to be arranged and "labeller = as_labeller()"specify the name of the axis.x title, the variable names are replaced by the given name
    return(layer1)
  }
}

#' Make a facetted barplot, either stacked or dodged
#'
#' @param df Dataframe to be plotted with columns including coef, `_upp`, `_low`
#' @param fill_var Variable to calculate population-weighted mean for. 
#' Must be in the dataframe!
#' @param type Either "stack" or "dodge"
#'
#' @details
#' To use this function, it is necessary to have created a dataframe with calc_weighted_stacked() or 
#' calc_weighted_univar() or all together. 
#' 
#' @return A ggplot
#'
#' @examples esslike_by_plot(W_physical_risk_all, dgs_physical_risk_group, type = "stack")
esslike_by_plot = function(df, fill_var, type) { # the df is the created data frame with calc_weighted_stacked etc...
  df <- df %>% mutate(level = case_when(             # recode age groups to make them more readable in plots
    level == "[18,25)" ~ "18-24",
    level == "[20,25)" ~ "20-24",
    level == "[25,45)" ~ "25-44",
    level == "[45,65)" ~ "45-64",
    .default = level))
  layer0 = ggplot(df, aes(x = fct_inorder(level), y = coef, fill = {{fill_var}})) + # specify where what is the x, the y and the fill variable from the table created with calc_weighted_stacked()
    scale_y_continuous(name = "Total (en %)",
                       #limits=c(0,100.1),# To have the maximum of the bars at 100% but numeric error might put it just above 100
                       expand = expansion(mult = c(0, 0.05)),# specify that the 0 has to be exactly on the axis (no space between the data and the axis)
                       n.breaks = 6) + # to have the percentage every 20%) 
    facet_grid(~ fct_inorder(var), 
               scales = "free_x", 
               space = "free_x", 
               switch = "x", 
               labeller = as_labeller(c("total" = "", 
                                        "sex" = "Sexe",
                                        "Sex" = "Sexe",
                                        "age_cat" = "Classe d'âge",
                                        "education_level" = "Education",
                                        "education_rec_fr" = "Education",
                                        "education_level_rec" = "Education"))) + # this line specify how the axis titles have to be arranged and "labeller = as_labeller()"specify the name of the axis.x title, the variable names are replaced by the given name
    theme_minimal() + # backgroud and axis title are minimal, could be dark, classic or void
    theme(
      text = element_text(size = 15), # specify the axis texts
      axis.title.x = element_blank(), 
      panel.grid.major.x = element_blank(), # Remove the vertical grids on the x-axis
      strip.placement = "outside", # specify where the strip of each variable have to be
      axis.line = element_line(colour = "grey50"), # specify the color of the strip
      plot.caption = element_text(size = 11, hjust =0, vjust = 0 ), # if caption is added, specify the size of the caption and the place the caption have to be printed
      plot.caption.position = "plot",
      legend.position = "bottom",legend.title = element_blank() #specify the posistion of the legend and that the legend title have to blanked, because added direcly in the word file
    ) +
    scale_fill_manual(
      # specify manually the colors of the stack
      values = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"),
      # wrap the labels
      labels = function(x) str_wrap(x, width = 20)) 
  
  #labs(caption= paste("Source : questionnaire Santé-Travail octobre 2022, \nN =", sum(!is.na(design$variables$fillvar)), "répondant-es"))
  if (type == "stack") {
    layer1 = layer0 + 
      geom_col() +
      geom_text(aes(label = paste0(sprintf("%1.1f", coef), "%")),#pecify the percentage that have to appear on the different part of the bar- This line specify that it s 1 decimal format with sprintf(), where is the percentage variable in the created table (in coef) and "%" have to appear to see that it s percentage
                data = drop_na(df),# drop all NA from the data frame
                position = position_stack(vjust = 0.5), size = 4) # specify the position and the size of the percentage on the bar
  } else if (type == "dodge") {
    layer1 = layer0 + 
      geom_col(position = position_dodge(width = 0.9, preserve = "single")) +# specify the width of the col
      geom_errorbar(aes(y = coef, ymax = ifelse(`_upp`<100, `_upp`, 100), ymin = if_else(`_low`> 0, `_low`,0 )),# To have the minimum of the errorbar on 0 and maximum  of the errorbar on 100 %
                    position = position_dodge(width = 0.9, preserve = "single"),
                    width = 0.3, colour = "#396f56")#specify the width and the color of the error bar
  } else {
    stop("Must specify 'stack' or 'dodge' for type")
  }
  return(layer1)
}

#' Calculate odds ratios adjusted by age, sex and education
#'
#' @param outcome Outcome variable
#' @param predictor Other adjustment variable
#' @param design Survey design with weights
#' @param age By default it will be age_cat but if e.g. you only have one age
#' group, you can pass another variable instead like a continuous age one
#'
#' @return A tibble with levels, ORs with CIs, p value
#'
#' @examples calc_ORs_DGS(health_2_cat, hh_income_cat_fr, weighted_dat_axe2) # # For the version for only age, sex, edu and another predictor
#' @examples calc_ORs_DGS(health_2_cat, design = weighted_dat_axe2) # For the version for only age, sex, edu
#' 
calc_ORs_DGS = function(outcome, predictor = NULL, design, age = "age_cat") {
  
  outcome = rlang::as_string(ensym(outcome))
  model_string = paste(outcome, 
                       "~ sex +",  age, "+ education_level")
  
  # Very fragile way of adding the predictor
  if (nargs() == 4) {
    predictor = rlang::as_string(ensym(predictor))
    model_string = paste(model_string, "+", predictor)
  }
  model = svyglm(model_string,
                 design = design, 
                 family = "quasibinomial")
  
  OR_res = cbind(OR = exp(coef(model)),
                 exp(confint(model)),
                 p_value = coef (summary(model))[,'Pr(>|t|)']) |> 
    as_tibble(rownames = "model_levels") |> 
    mutate(across(2:4, \(.x) round(.x, digits = 2)), # ORs and CIs
           "Multivariable OR (95% CI)" = paste0(OR, " (", `2.5 %`, "-", `97.5 %`, ")")) |> 
    select(model_levels, "Multivariable OR (95% CI)", p_value) |> 
    add_column("predictor" = predictor, .before = 1) |> 
    add_column(outcome, .before = 1)
  
  return(OR_res)
}

# Table-making functions ---------

#' @param data The output object of using the calc_weighted_stacked_multiple() function, using only c("Sex", "age_cat", "education_level_rec") as var
#' @param caption String with the caption of the table
#' @param data_source String specifying the data source
#'
#' @return flextable with values, CIs, and P values for fill_var, stratified by
#' all levels of var. Totals of the weights of these grouping combinations are
#' included along with total number of non-missing data.
#'
#' @examples esslike_table(data = calc_weighted_stacked_multiple(weighted_dat_gh24, var = c("Sex", "age_cat", "education_level_rec"), fill_var = "doctor", proportion = TRUE) |> filter(doctor == "Oui"))
esslike_table = function(data, caption = "IL FAUT SPECIFIER UN CAPTION", data_source = "IL FAUT SPECIFIER LA SOURCE DE DONNEES"){
  new_table <- data |> 
    select(var, level, coef_full, unweighted_N_level, p_value_short) |> 
    mutate(
      level = case_when(             # recode age groups to make them more readable in plots
        level == "[18,25)" ~ "18-24",
        level == "[20,25)" ~ "20-24",
        level == "[25,45)" ~ "25-44",
        level == "[45,65)" ~ "45-64",
        .default = level)
    ) |>
    mutate(                             # Clean up variable names
      var = case_when(
        var == "total" ~ "Total",
        var == "Sex" ~ "Sexe",
        str_detect(var, "age_cat") ~ "Classe d'âge",
        str_detect(var, "education_level") ~ "Education",
        .default = var
      ),
      
      # Remove unnecessary duplication
      p_value_short = case_when(
        var == "Total" ~ "",
        !duplicated(var) ~ p_value_short, 
        .default = ""),
      var = ifelse(!duplicated(var), var, "")
      ,level = case_when(level == "Total" ~ "", .default = level)
    )
  
  a <- new_table %>%           # Crude way to ID the group labels and for later coloring
    rowid_to_column() %>% 
    filter(var != "", var != "Total")
  groupnames <- (a$rowid-1)
  
  # Convert to flextable
  table_output <- myflextable(new_table) |> 
    set_header_labels(values = c("", "", "Part en % [IC 95%]*",
                                 "Echantillon", "P")) |> 
    hline(i = groupnames, border = fp_border_default(color = "black")) |> 
    width(j = c(1), width = 3, unit = "cm") |> 
    width(j = c(3), width = 4.4, unit = "cm") |>
    width(j = c(4), width = 3, unit = "cm") |>
    width(j = c(5), width = 2, unit = "cm") |>
    align(align = "center", part = "header") |> 
    align(align = "center", part = "body") |> 
    align(align = "left", part = "body", j = c(1,2)) |> 
    add_footer_lines(c(paste0("Source: ", data_source), 
                       "* Pondérée par l’âge, le sexe et le niveau d’éducation", 
                       "IC = Intervalle de confiance")) |> 
    bold(part = "header") |> 
    bold(j = c(1,2), part = "body") |> 
    line_spacing(space = 1.5, part = "body") |> 
    # height_all(height = 5, part = "all", unit = "cm") |> 
    fontsize(size = 11, part = "all") |> 
    set_caption(caption = caption,
                align_with_table = FALSE)
  
  return(table_output)
}


#' @param data The output object of using the calc_weighted_stacked_multiple() function, using only c("Sex", "age_cat", "education_level_rec") as var
#' @param caption String with the caption of the table
#' @param data_source String specifying the data source
#'
#' @return flextable with values, CIs, and P values for fill_var, stratified by
#' all levels of var. Totals of the weights of these grouping combinations are
#' included along with total number of non-missing data.
#'
#' @examples esslike_table_outcome_2plus_levels(data = calc_weighted_stacked_multiple(weighted_dat_gh24, var = c("Sex", "age_cat", "education_level_rec"), fill_var = "doctor", proportion = TRUE) |> filter(doctor == "Oui"))
esslike_table_outcome_2plus_levels = function(data, outcome_text = "VARIABLE OUTCOME TEXT", caption = "IL FAUT SPECIFIER UN CAPTION", data_source = "IL FAUT SPECIFIER LA SOURCE DE DONNEES"){
  new_table <- data |> 
    select(1:3, coef_full, unweighted_N_level, unweighted_N, p_value_short) |> 
    mutate(
      level = case_when(             # recode age groups to make them more readable in plots
        level == "[18,25)" ~ "18-24",
        level == "[20,25)" ~ "20-24",
        level == "[25,45)" ~ "25-44",
        level == "[45,65)" ~ "45-64",
        .default = level)
    ) |>
    mutate(                             # Clean up variable names
      var = case_when(
        var == "total" ~ "Total",
        str_detect(var, "Sex|sex") ~ "Sexe",
        str_detect(var, "age") ~ "Classe d'âge",
        str_detect(var, "education") ~ "Education",
        .default = var
      ),
      
      # Remove unnecessary duplication
      p_value_short = case_when(
        var == "Total" ~ "",
        !duplicated(var) ~ p_value_short, 
        .default = ""),
      var = ifelse(!duplicated(var), var, "")
      ,level = case_when(level == "Total" ~ "", .default = level),
      level = ifelse(!duplicated(level), level, "")
      , unweighted_N_level = case_when(var == "Total" ~ as.character(unweighted_N_level),
                                       level != "" ~ as.character(unweighted_N_level),
                                       .default = "")
    ) |> 
    relocate(unweighted_N_level, .after = level)
  
  a <- new_table %>%           # Crude way to ID the group labels and for later coloring
    rowid_to_column() %>% 
    filter(var != "", var != "Total")
  groupnames <- (a$rowid-1)
  
  # Convert to flextable
  table_output <- myflextable(new_table) |> 
    set_header_labels(values = c("", "", "Echantillon", outcome_text, "Part en % [IC 95%]*",
                                 "n", "P")) |> 
    hline(i = groupnames, border = fp_border_default(color = "black")) |>
    autofit() |> 
    width(j = c(1), width = 2.1, unit = "cm") |>
    width(j = c(2), width = 1.9, unit = "cm") |>
    width(j = c(3), width = 1.9, unit = "cm") |>
    # width(j = c(4), width = 5, unit = "cm") |>
    width(j = c(5), width = 3.3, unit = "cm") |>
    width(j = c(6), width = 1, unit = "cm") |>
    width(j = c(7), width = 1.4, unit = "cm") |>
    align(align = "center", part = "header") |> 
    align(align = "center", part = "body") |> 
    align(align = "left", part = "body", j = c(1,2,4)) |> 
    add_footer_lines(c(paste0("Source: ", data_source
                              # , "; N = ", max(data$n_non_miss, na.rm = TRUE)
    ), 
    "* Pondérée par l’âge, le sexe, et le niveau d’éducation", 
    "IC = Intervalle de confiance")) |> 
    bold(part = "header") |> 
    bold(j = c(1,2), part = "body") |> 
    line_spacing(space = 1, part = "body") |> 
    # height_all(height = 5, part = "all", unit = "cm") |> 
    fontsize(size = 8, part = "all") |> 
    padding(padding = 0, part = "all") |> 
    set_caption(caption = caption,
                align_with_table = FALSE)
  
  return(table_output)
}

# Experimental functions ---------

#' Generate the regression model data for the forestplots
#' 
#' An alternative function to generate dataframes from regression models
#' 
#' @param design Survey design with weights
#' @param outcomes A string or character vector of the outcome variable(s). For logistic regression, outcomes should either be two-level factors or binary (1/0).
#' @param outcomes_clean_name Optional string or character vector with clean names corresponding to the `outcomes` variable
#' @param family Specify the family used for the model (default = "quasibinomial")
#' @param main_variable string or character vector specifying variables to include in the formula (they must be categorical, as numeric variables break the function --> still to fix). Deafault is c("sex", "age_cat", "education_level")
#' @param main_variable_clean_name Optional string or character vector with clean names corresponding to the `main_variable`.
#' @param covariates String or character vector specifying covariates for the formula (default is c("sex", "age_cat", "education_level"))
#' @returns A dataframe containing the regression data
#' @examples
#' UEP_regression_data(weighted_dat, outcomes = "binary_health")
#' 
UEP_regression_data = function(
    design, 
    outcomes, 
    outcomes_clean_name = NULL, 
    family = rep("quasibinomial",length(outcomes)), 
    main_variable = c("sex", "age_cat", "education_level"),
    main_variable_clean_name = NULL, 
    covariates = c("sex", "age_cat", "education_level"), 
    estimate_rounding = 2
){
  require("tidyverse")
  require("broom.helpers")
  # If a clean name is not provided, then default to the outcome name
  if(is.null(outcomes_clean_name)){
    outcomes_clean_name = outcomes
  }
  # If a clean name is not provided, then default to the variable name
  if(is.null(main_variable_clean_name)){
    main_variable_clean_name = main_variable
  }
  
  # Initialize an empty dataframe that will be filled out
  a <- tibble()
  
  # Start the triple loop
  for (i in 1:length(outcomes)) {
    for (j in 1:length(main_variable)) {
      # Create model
      # Get outcome levels
      outcome_levels <- design %>% filter(!is.na(get(outcomes[i]))) %>% reframe(levels = fct_unique(get(outcomes[i])))
      outcome_levels <- levels(unique(outcome_levels$levels))
      outcome_levels <- paste0("'", outcome_levels[2],"'", " vs ref:", "'", outcome_levels[1],"'")
      
      ## vector of main_variable and covariate column names
      r_model <- c(main_variable[j], covariates) %>%
        str_c(collapse = "+") %>% # combine variables separated by a plus
        ## combine the variables with outcome in formula style
        str_c(outcomes[i]," ~ ", .) %>%
        svyglm(family = family[i], design = design)
      
      # # Get number of observations
      # N_obs <- broom.helpers::model_get_n(r_model) #%>%
      #   mutate(term = str_remove(term,paste0(main_variable[j]))) #%>%
      #   rename(model_levels = term) %>%
      #   mutate(model_levels = case_when(
      #     model_levels == "(Intercept)" ~ main_variable_clean_name[j],
      #     .default = model_levels))
      
      # Get exponentiated coefficients depending on family
      if(family[i] == "quasibinomial"){
        model_data <- cbind(
          estimate = exp(coef(r_model)),
          exp(confint(r_model)),
          pvalue = coef(summary(r_model))[,'Pr(>|t|)']) %>%
          as_tibble(rownames = "model_levels")} else {
            model_data <- cbind(
              estimate = coef(r_model),
              confint(r_model),
              pvalue = coef(summary(r_model))[,'Pr(>|t|)']
            )%>% as_tibble(rownames = "model_levels")}
      
      # Add empty n_event column for UV estimates (see add_column below)
      cols <- c(n_event = NA)
      if(family[i] != "quasibinomial"){
        model_data <- model_data %>%
          add_column(!!!cols[!names(cols) %in% names(.)])
      }
      
      # Clean the output
      model_data_clean <- model_data %>% 
        mutate(variable = main_variable_clean_name[j], 
               .before = 1) %>% 
        # Keep only main_variable estimates
        filter(str_detect(model_levels, paste0(main_variable[j]))|str_detect(model_levels,"(Intercept)")) %>%
        rename(conf.low = `2.5 %`, conf.high = `97.5 %`) %>%
        add_row(variable = main_variable_clean_name[j],
                model_levels = main_variable_clean_name[j],
                .before = 1) %>% 
        mutate(
          label = if_else(model_levels == main_variable_clean_name[j], TRUE, FALSE),
          family = if_else(family[i] == "quasibinomial", "OR", "Beta"),
          model = case_when(is.null(covariates)~"Univariable", .default = "Multivariable"),
          outcome = outcomes_clean_name[i],
          outcome_levels = outcome_levels,
          reference = if_else(model_levels == "(Intercept)", TRUE, FALSE),
          model_levels = c(main_variable_clean_name[j], r_model$xlevels[[1]]),
          estimate = case_when(family == "Beta" & reference ~ 0,
                               family == "OR" & reference ~ 1,
                               .default = round(estimate,estimate_rounding)),
          conf.low = case_when(family == "Beta" & reference ~ 0,
                               family == "OR" & reference ~ 1,
                               .default = round(conf.low,estimate_rounding)),
          conf.high = case_when(family == "Beta" & reference ~ 0,
                                family == "OR" & reference ~ 1,
                                .default = round(conf.high,estimate_rounding)),
          full_estimate = case_when(label ~ NA,
                                    reference ~ paste0("(ref)") ,
                                    .default = paste0(
                                      format(estimate, nsmall = estimate_rounding), 
                                      " (", 
                                      format(conf.low, nsmall = estimate_rounding), 
                                      ", ", 
                                      format(conf.high, nsmall = estimate_rounding), 
                                      ")")),
          pvalue = if_else(reference, NA, pvalue),
          p.value = case_when(label | reference ~ NA,
                              pvalue<0.001 ~ "<0.001", .default = format(round(pvalue, 3), nsmall = 3))
        ) %>% 
        relocate(c("outcome","outcome_levels"), .before = "variable") %>% 
        relocate("reference", .after = "model_levels") %>%
        relocate("p.value", .after = "conf.high") %>%
        relocate("pvalue", .after = last_col())
      #%>% left_join(N_obs) %>% relocate(n_event, .after = last_col())
      # iteratively bind each model_data output
      a <- rbind(a,model_data_clean)
    }
  }
  # Print out the final dataframe
  return(a)
}

#' Function to generate forestplots from regression data
#' 
#' @param data The output dataframe of running the `UEP_regression_data()` function
#' @returns a forestplot object
#' @examples
#' # UEP_forestplot(df)
#' 
UEP_forestplot = function(data){
  require("forestplot")
  # Crude way to ID the group labels and for later coloring
  groupnames <- data %>%           
    rowid_to_column() %>% 
    filter(label) ; groupnames <- (groupnames$rowid)
  # Label of family
  coeff_label = unique(data$family)
  
  # label of outcome
  outcome_label = unique(data$outcome)
  outcome_levels = unique(data$outcome_levels)
  
  # define min and max of x-axis estimates
  if(coeff_label == "OR"){
    min.est = 1/3
    max.est = ceiling(max(data$estimate, na.rm = TRUE))+1} else {
      min.est = floor(min(data$estimate, na.rm = TRUE)-1)
      max.est = ceiling(max(data$estimate, na.rm = TRUE))
    }
  
  if(coeff_label == "OR"){
    my_ticks = c(log(1/3),log(0.5), log(0.67), log(1), log(1.5), sapply(2:max.est, log))
    attr(my_ticks, "labels") <- c("0.33", "0.5", "0.67","1","1.5", paste0(2:max.est))} else {
      my_ticks = c(min.est:max.est)
      attr(my_ticks, "labels") <- c(min.est:max.est)
    }
  
  
  data %>% forestplot(
    labeltext = c(model_levels, full_estimate, p.value),
    align = "lcl",
    
    # Set the estimate and the confidence intervals
    mean = estimate,
    lower = conf.low,
    upper = conf.high,
    
    title = paste0("Outcome: ", outcome_label,"\n(",outcome_levels, ")"),
    
    # Point and line aesthetics
    fn.ci_norm = fpDrawCircleCI,
    boxsize = 0.4,
    ci.vertices = TRUE,
    ci.vertices.height = 0.12,
    clip = c(min.est, max.est), 
    # Set alignment of the column values
    # align = "llll",
    # hrzl_lines = list("7" = gpar(lty = 1)),
    is.summary = label,
    xlab = case_when(coeff_label == "Beta" ~ "Beta estimate (with 95% CI)",
                     .default = "Odds ratio (with 95% CI)"),
    # xticks = my_ticks,
    xlog = (coeff_label == "OR")     # Show x-axis on log scale
  ) %>% 
    fp_add_lines("#999999") %>%
    fp_add_header(
      model_levels = c("Variables", "") %>% fp_align_left(),
      full_estimate = c("Adjusted", "OR (IC 95%)") %>% fp_align_center(),
      p.value =  c("", "p-value") %>% fp_align_center()
    ) %>%
    fp_set_zebra_style("#EFEFEF") %>% 
    fp_set_style(box = c("#4271bd"),
                 line = c("#4271bd"),
                 # Font settings
                 txt_gp = fpTxtGp(summary = gpar(fontface = "bold", cex = 1),
                                  label = gpar(cex = 0.9),
                                  ticks = gpar(cex = 0.9),
                                  xlab = gpar(cex = 1)))
}

# Metadata functions ####

#' Function to generate first part of metadata file for OCS online database after first processing steps (see prep_online_database.R)
#' @param df The dataframe object to summarize
#' @returns a dataframe with the summarized metadata
#' @examples
#' # metadata_generator_any(mtcars, variable_names = enframe(c("mpg" = "Miles per gallon")))
#' 
metadata_generator <- function(df) {
  # Initialize an empty list to store results
  summary_list <- list()
  
  # Ensure "Questionnaire" column exists
  if (!("questionnaire" %in% names(df))) {
    stop("The 'questionnaire' column is missing in the dataframe.")
  }
  
  # Ensure "Questionnaire" column exists
  if (!("year" %in% names(df))) {
    stop("The 'year' column is missing in the dataframe.")
  }
  
  # Iterate through each column in the dataframe
  for (col_name in names(df)) {
    col_class <- class(df[[col_name]])
    
    # Handle numeric/integer columns
    if (any(col_class %in% c("numeric", "integer"))) {
      value_range <- paste0("Range: ", 
                            min(df[[col_name]], na.rm = TRUE), 
                            " - ",
                            max(df[[col_name]], na.rm = TRUE))
      num_values <- NA # Not applicable for numeric/integer
    } 
    
    # Handle Date columns
    else if (any(col_class %in% c("Date", "POSIXct", "POSIXt"))) {
      value_range <- paste(format(min(df[[col_name]], na.rm = TRUE), "%Y-%m-%d"), 
                           format(max(df[[col_name]], na.rm = TRUE), "%Y-%m-%d"), 
                           sep = " à ")
      num_values <- NA
    }
    
    # Handle categorical/boolean columns
    else if (any(col_class %in% c("factor", "character", "logical"))) {
      # Exclude NA values when getting levels and unique values using [!is.na(df[[col_name]])]
      if (col_class == "factor") {
        # For factors, use levels to get values in order
        value_range <- paste0('["', paste(levels(df[[col_name]][!is.na(df[[col_name]])]), collapse = '", "'),'"]')
      } else {
        value_range <- paste0('["', paste(unique(df[[col_name]][!is.na(df[[col_name]])]), collapse = '", "'),'"]')
      }
      #
      num_values <- length(unique(df[[col_name]][!is.na(df[[col_name]])]))
    } 
    
    # Handle other data types (you can add more cases as needed)
    else {
      value_range <- "Unsupported data type"
      num_values <- NA
    }
    
    # Get unique questionnaires associated with this variable (excluding NAs)
    questionnaires <- unique(df$questionnaire[!is.na(df[[col_name]])])
    questionnaire_values <- if (length(questionnaires) > 0) {
      paste(questionnaires, collapse = ", ")
    } else {
      NA
    }
    num_questionnaires <- length(questionnaires)
    
    # Get unique questionnaires associated with this variable (excluding NAs)
    years <- unique(df$year[!is.na(df[[col_name]])])
    year_values <- if (length(years) > 0) {
      paste(years, collapse = ", ")
    } else {
      NA
    }
    
    # Append the summary for this column to the list
    summary_list[[col_name]] <- data.frame(
      Variable = col_name,
      Values = value_range,
      Class = col_class,
      Num_Values = num_values,
      Questionnaires = questionnaire_values,
      Years = year_values,
      Num_Questionnaires = num_questionnaires,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine the list elements into a single dataframe
  return(do.call(rbind, summary_list))
}


#' Function to generate metadata file from any dataframe object
#' 
#' @param df The dataframe object to summarize
#' @param variable_names Optional object for adding clean names, created using enframe(c("variable1" = "Clean name 1"))
#' @returns a dataframe with the summarized metadata
#' @examples
#' # metadata_generator_any(mtcars, variable_names = enframe(c("mpg" = "Miles per gallon")))
#' 
metadata_generator_any <- function(df, variable_names = NULL) {
  require("tidyverse")
  # Initialize an empty list to store results
  summary_list <- list()
  
  # Iterate through each column in the dataframe
  for (col_name in names(df)) {
    col_class <- class(df[[col_name]])
    
    # Handle numeric/integer columns
    if (any(col_class %in% c("numeric", "integer"))) {
      value_range <- paste0("Range: ", 
                            min(df[[col_name]], na.rm = TRUE), 
                            " - ",
                            max(df[[col_name]], na.rm = TRUE),
                            "; mean: ", round(mean(df[[col_name]], na.rm = TRUE), digits = 2))
      num_values <- length(unique(df[[col_name]][!is.na(df[[col_name]])]))
    } 
    
    # Handle Date columns
    else if (any(col_class %in% c("Date", "POSIXct", "POSIXt"))) {
      value_range <- paste(format(min(df[[col_name]], na.rm = TRUE), "%Y-%m-%d"), 
                           format(max(df[[col_name]], na.rm = TRUE), "%Y-%m-%d"), 
                           sep = " à ")
      num_values <- length(unique(df[[col_name]][!is.na(df[[col_name]])]))
    }
    
    # Handle categorical/boolean columns
    else if (any(col_class %in% c("factor", "character", "logical"))) {
      # Exclude NA values when getting levels and unique values using [!is.na(df[[col_name]])]
      if (col_class == "factor") {
        # For factors, use levels to get values in order
        value_range <- paste0('["', paste(levels(df[[col_name]][!is.na(df[[col_name]])]), collapse = '", "'),'"]')
      } else {
        value_range <- paste0('["', paste(unique(df[[col_name]][!is.na(df[[col_name]])]), collapse = '", "'),'"]')
      }
      #
      num_values <- length(unique(df[[col_name]][!is.na(df[[col_name]])]))
    } 
    
    # Handle other data types (you can add more cases as needed)
    else {
      value_range <- "Unsupported data type"
      num_values <- NA
    }
    
    # Calculate the number of missing values
    num_missing <- sum(is.na(df[[col_name]]))
    
    # Append the summary for this column to the list
    summary_list[[col_name]] <- data.frame(
      Variable = col_name,
      Values = value_range,
      Class = col_class,
      Num_Values = num_values,
      Num_Missing = num_missing,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine the list elements into a single dataframe
  metadata_0 <- do.call(rbind, summary_list)
  if(is.null(variable_names)){
    return(metadata_0 |> mutate(Clean_name = NA) |> 
             relocate(Clean_name, .after = Variable))
  } else{
    return(metadata_0 |> 
             left_join(variable_names |> rename(Variable = name, Clean_name = value)) |> 
             relocate(Clean_name, .after = Variable))}
}

# Make OCS tables ####

#' Make OCS WIDE tables
#'
#' @param axe String specifying which axe to take variables from
#' @param questionnaire_table String specifying which questionnaire to use as dataset (default= "Suivi de votre santé (avril 2024)")
#' @param age_var Age category variable to use in the function (default is age_cat)
#'
#' @return a wide-format table with estimates by age, sex, and education
#' @export
#'
#' @examples
#' make_wide_OCS_table(axe = "Axe 1")
make_wide_OCS_table <- function(axe, questionnaire_table = "Suivi de votre santé (avril 2024)", age_var = age_cat, sex_var = sex, optional_filter = NULL){
  require(gtsummary)
  require(survey)
  require(srvyr)
  
  # Read in datasets
  metadata_complete <- read.csv(here("output", "metadata_complete.csv"), encoding = "UTF-8")
  database_complete <- readRDS(here("output", "database_complete.rds"))
  # GE_pop_edu <- read.csv(here("output", "GE_pop_edu_weights.csv")) # Population weights
  # Select variables
  axe_variables <- metadata_complete |> 
    filter(Axe %in% axe) |> 
    filter(Indicateur =="Oui")|> 
    filter(str_detect(Questionnaires, gsub("([()])", "\\\\\\1", questionnaire_table))) |>
    filter(Class %in% c("character", "factor", "logical"))
  # Make a subset of data
  table_df <- database_complete |>
    filter(questionnaire %in% questionnaire_table) |>
    droplevels() |> 
    select(participant_identifier, {{age_var}}, {{sex_var}}, education_rec_fr, 
           any_of(axe_variables$Variable), surv_weight,
           questionnaire, year) |> 
    # filter(year == "2024") |>
    mutate(education_level = education_rec_fr) |> 
    droplevels()
  
  # Check if an optional filter is provided
  if (!is.null(optional_filter)) {
    # Apply the filter dynamically
    table_df <- dplyr::filter(table_df, eval(optional_filter)) |> droplevels()
  }
  
  # Apply weighting for GE population
  # table_df_unweighted <- survey::svydesign(ids = ~0, data = table_df)
  # table_df_weighted = srvyr::as_survey_design(
  #   postStratify(table_df_unweighted, ~sex + {{age_var}} + education_level, 
  #                GE_pop_edu, partial = TRUE)
  # )
  table_df_weighted = srvyr::as_survey_design(table_df, weights = surv_weight)
  
  # Styling
  style_percent_1digits <- purrr::partial(gtsummary::style_percent, digits = 1)
  style_number_0digits <- purrr::partial(gtsummary::style_number, digits = 0)
  style_number_1digits <- purrr::partial(gtsummary::style_number, digits = 1)
  
  # Make tables
  ## Overall
  tbl_overall <- gtsummary::tbl_svysummary(
    data = table_df_weighted,
    include = all_of(axe_variables$Variable),  
    missing = "no",
    percent = "column",
    digits = list(gtsummary::all_categorical() ~ c(0, 1)) 
    ,statistic = list(all_of(axe_variables$Variable) ~ "{n_unweighted} ({p}%")
  ) |> 
    gtsummary::bold_labels() |> 
    # gtsummary::add_overall(col_label = "**Total**  \nN = {style_number(N_unweighted)}") |> 
    gtsummary::add_ci(statistic = list(all_of(axe_variables$Variable) ~ "{conf.low}, {conf.high}"),
                      pattern = "{stat} [{ci}])",
                      style_fun = list(gtsummary::all_categorical() ~ style_percent_1digits,
                                       gtsummary::all_continuous() ~ style_number_1digits))|> 
    gtsummary::modify_header(list(any_of(c("stat_0", "stat_1", "stat_2", "stat_3", "stat_4")) ~ "N = {n_unweighted}")
                             # ,label="**Indicateur**"
    )
  
  ## Age
  tbl_age <- gtsummary::tbl_svysummary(
    data = table_df_weighted,
    by = {{age_var}}, 
    include = all_of(axe_variables$Variable),  
    missing = "no",
    percent = "column",
    digits = list(gtsummary::all_categorical() ~ c(0, 1)) 
    ,statistic = list(all_of(axe_variables$Variable) ~ "{n_unweighted} ({p}%")
  ) |> 
    gtsummary::bold_labels() |> 
    # gtsummary::add_overall(col_label = "**Total**  \nN = {style_number(N_unweighted)}") |> 
    gtsummary::add_ci(statistic = list(all_of(axe_variables$Variable) ~ "{conf.low}, {conf.high}"),
                      pattern = "{stat} [{ci}])",
                      style_fun = list(gtsummary::all_categorical() ~ style_percent_1digits,
                                       gtsummary::all_continuous() ~ style_number_1digits))|> 
    gtsummary::modify_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**{level}**\nN = {n_unweighted}")
                             # ,label="**Indicateur**"
    ) |>
    gtsummary::modify_spanning_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**Classe d'âge**")) |> 
    gtsummary::add_p() |> 
    gtsummary::bold_p()
  
  ## Sexe
  tbl_sex <- gtsummary::tbl_svysummary(
    data = table_df_weighted,
    by = {{sex_var}}, 
    include = all_of(axe_variables$Variable), 
    missing = "no",
    percent = "column",
    digits = list(gtsummary::all_categorical() ~ c(0, 1)) 
    ,statistic = list(all_of(axe_variables$Variable) ~ "{n_unweighted} ({p}%")
  ) |> 
    gtsummary::bold_labels() |> 
    # gtsummary::add_overall(col_label = "**Total**  \nN = {style_number(N_unweighted)}") |> 
    gtsummary::add_ci(statistic = list(all_of(axe_variables$Variable) ~ "{conf.low}, {conf.high}"),
                      pattern = "{stat} [{ci}])",
                      style_fun = list(gtsummary::all_categorical() ~ style_percent_1digits,
                                       gtsummary::all_continuous() ~ style_number_1digits))|> 
    gtsummary::modify_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**{level}**\nN = {n_unweighted}")
                             # , label="**Indicateur**"
    ) |>
    # gtsummary::modify_column_header(columns = "stat_0", label = "**Total**")|> 
    gtsummary::modify_spanning_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**Sexe**")) |> 
    gtsummary::add_p() |> 
    gtsummary::bold_p()
  
  ### Niveau d'éducation
  tbl_educ <- gtsummary::tbl_svysummary(
    data = table_df_weighted,
    by = education_level, 
    include = all_of(axe_variables$Variable),
    missing = "no",
    percent = "column",
    digits = list(gtsummary::all_categorical() ~ c(0, 1)) 
    ,statistic = list(all_of(axe_variables$Variable) ~ "{n_unweighted} ({p}%")
  ) |> 
    gtsummary::bold_labels() |> 
    # gtsummary::add_overall(col_label = "**Total**  \nN = {style_number(N_unweighted)}") |> 
    gtsummary::add_ci(statistic = list(all_of(axe_variables$Variable) ~ "{conf.low}, {conf.high}"),
                      pattern = "{stat} [{ci}])",
                      style_fun = list(gtsummary::all_categorical() ~ style_percent_1digits,
                                       gtsummary::all_continuous() ~ style_number_1digits))|> 
    gtsummary::modify_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**{level}**\nN = {n_unweighted}")
                             # ,label="**Indicateur**"
    ) |>
    gtsummary::modify_spanning_header(list(any_of(c("stat_1", "stat_2", "stat_3", "stat_4")) ~ "**Niveau de formation**")) |> 
    gtsummary::add_p() |> 
    gtsummary::bold_p()
  
  table_axe = tbl_merge(tbls = list(tbl_overall, tbl_sex, tbl_age, tbl_educ),
                        tab_spanner = c("**Total**", "**Sexe**", "**Classe d'âge**", "**Niveau de formation**"))
  
  table_axe$table_body <- table_axe$table_body |> 
    left_join(axe_variables |> select(Variable, Clean_name) |> rename(variable = Variable)) |> 
    mutate(label = if_else(row_type == "label", Clean_name, label))
  
  table_axe |> gtsummary::modify_header(label = "**Indicateur**") # Print the final wide table as the output
}

#' Make LONG OCS tables by sex, age, and education
#'
#' @param axe Specify the Axe
#' @param questionnaire_table Specify the dataset
#' @param age_var Specify the age variable
#' @param sex_var Specify the sex variable
#' @param education_var Specify the education variable
#' @param indicateurs_only generate tables only for Indicateurs? If FALSE, then generate for all variables in metadata file
#'
#' @return A list object that holds the generated table for each "indicateur", and it also saves all tables automatically as word files
#' @export
#'
#' @examples
#' OCS_long_tables(axe = "Axe 1", questionnaire_table = "Suivi de votre santé (avril 2024)")
make_long_OCS_tables <- function(axe = "Axe 1", 
                            questionnaire_table = "Suivi de votre santé (avril 2024)", 
                            age_var = "age_cat", 
                            sex_var = "sex",
                            education_var = "education_rec_fr",
                            indicateurs_only = TRUE
){
  source(here::here("code", "functions.R"))
  
  # Effort to somewhat automate this using the metadata and database file
  metadata_complete <- read.csv(here::here("output", "metadata_complete.csv"), encoding = "UTF-8")
  database_complete <- readRDS(here::here("output", "database_complete.rds"))
  
  axe_variables <- metadata_complete |> 
    filter(Axe %in% axe) |> 
    filter(str_detect(Questionnaires, gsub("([()])", "\\\\\\1", questionnaire_table))) |>
    filter(Class %in% c("character", "factor", "logical"))
  if(indicateurs_only == TRUE){axe_variables <- axe_variables |> filter(Indicateur =="Oui")}
  
  # Make a subset of data
  table_df <- database_complete |>
    filter(questionnaire %in% questionnaire_table) |>
    droplevels() |> 
    select(participant_identifier, {{age_var}}, {{sex_var}}, {{education_var}}, 
           any_of(axe_variables$Variable), surv_weight,
           questionnaire, year) |> 
    # filter(year == "2024") |>
    mutate(education_level = {{education_var}}) |> 
    droplevels()
  
  table_df_weighted = srvyr::as_survey_design(table_df, weights = surv_weight)
  
  tables_indicateurs <- list()
  for(i in 1:length(axe_variables$Variable)){
    num_levels = axe_variables$Num_Values[i]
    table_indicateur = table_df_weighted |> 
      calc_weighted_stacked_multiple(fill_var = axe_variables$Variable[i], 
                                     var = c({{sex_var}}, {{age_var}}, {{education_var}}),
                                     proportion = TRUE) |> 
      esslike_table_outcome_2plus_levels( 
        caption = paste0("Indicateur (", axe_variables$Axe[i], "): ", axe_variables$Clean_name[i]),
        outcome_text = "Indicateur", 
        data_source = questionnaire_table
      )
    tables_indicateurs[[i]] <- table_indicateur
    
    # Save the tables to output
    save_as_docx(
      table_indicateur, path = here("output", "long_OCS_tables", 
                                    paste0(axe_variables$Axe[i], "_", axe_variables$Variable[i], 
                                           format(Sys.time(), "_%Y-%m-%d-%H%M.docx")))
      # , pr_section = sect_properties
    )
  }
  return(tables_indicateurs)
}