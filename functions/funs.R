
# source libraries

# function for installing dependencies
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# required packages
packages <- c(
  "tidyverse",
  "fs",
  "here",
  "clarify",
  "arrow",
  "skimr",
  "ggeffects",
  "parameters",
  "table1",
  "stdReg",
  "ggplot2",
  "mice",
  "conflicted",
  "geepack",
  "janitor",
  "clarify",
  "rlang",
  "MatchThem",
  "WeightIt",
  "cobalt",
  "optmatch",
  "glue",
  "rlang",
  "survey",
  "CBPS", # propensity scores
  "msm", # for validating change in the exposure
  "kableExtra",
  "naniar", # for inspecting missing data
  "miceadds"
)

# install packages
ipak(packages)

# conflits preferred
conflict_prefer("pool", "mice")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("cbind", "base")
conflict_prefer("lead", "dplyr")
conflict_prefer("lag", "dplyr")



# functions for descriptive tables using libary(table1) # reduces clutter
my_render_cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3),
       c("",
         "Mean (SD)" =
           sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my_render_cat <- function(x) {
  c("", sapply(stats.default(x), function(y)
    with(y,
         sprintf(
           "%d (%0.0f %%)", FREQ, PCT
         ))))
}


# baseline table # currently not using the shorten option

baseline_table <- function(df, output_format = "markdown") {

  df_new <- df %>%
    select(starts_with("t0")) %>%
    rename_all(~stringr::str_replace(., "^t0_", "")) %>%
    mutate(wave = factor(rep("baseline", nrow(df)))) |>
    janitor::clean_names(case = "screaming_snake")


  # Function to create a formula string
  create_formula_string <- function(df_new) {
    baseline_vars_names <- df_new %>%
      select(-WAVE) %>%
      colnames()

    table_baseline_vars <- paste(baseline_vars_names, collapse = "+")
    formula_string_table_baseline <- paste("~", table_baseline_vars, "|WAVE")

    return(as.formula(formula_string_table_baseline))
  }

  # Custom rendering functions for table1
  # my_render_cont <- function(x) {
  #   with(stats.apply.rounding(stats.default(x), digits = 3),
  #        c("", "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)))
  # }

  # my_render_cat <- function(x) {
  #   c("", sapply(stats.default(x), function(y) {
  #     with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))
  #   }))
  # }

  # Create baseline table
  create_table_baseline <- function(df_new, formula_obj) {
    # removed "shorten" option
    # render_cont <- if (shorten) my_render_cont else NULL
    # render_cat <- if (shorten) my_render_cat else NULL

    table1::table1(
      formula_obj,
      data = df_new,
      overall = FALSE#,
      #  render.continuous = if (!is.null(render_cont)) render_cont else NULL,
      #  render.categorical = if (!is.null(render_cat)) render_cat else NULL
    )
  }


  # Convert table to specified format
  table_to_output <- function(table, format) {
    table %>%
      as.data.frame() %>%
      kbl(format = format)
  }

  formula_obj_baseline <- create_formula_string(df_new)
  table_baseline <- create_table_baseline(df_new, formula_obj_baseline)
  table_output <- table_to_output(table_baseline, output_format)
  print(table_output)
}



# create wide data --------------------------------------------------------


create_wide_data <- function(dat_long, baseline_vars, exposure_var, outcome_vars, exclude_vars = c()) {
  require(tidyverse)
  # Add the 'time' column to the data
  data_with_time <- dat_long %>%
    mutate(time = as.numeric(wave) - 1) %>%
    arrange(id, time)

  # Filter the data based on the time condition
  data_filtered <- data_with_time %>%
    filter(time >= 0)

  # Create the wide data frame
  wide_data <- data_filtered %>%
    dplyr::select(-exclude_vars)  %>%  # Exclude specified variables
    pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, time),
      names_glue = "t{time}_{.value}",
      names_prefix = "t"
    )

  # Define a custom function to filter columns based on conditions
  custom_col_filter <- function(col_name) {
    if (startsWith(col_name, "t0_")) {
      return(col_name %in% c(
        paste0("t0_", baseline_vars),
        paste0("t0_", exposure_var),
        paste0("t0_", outcome_vars)
      ))
    } else if (startsWith(col_name, "t1_")) {
      return(col_name %in% paste0("t1_", exposure_var))
    } else if (startsWith(col_name, "t2_")) {
      return(col_name %in% paste0("t2_", outcome_vars))
    } else if (startsWith(col_name, "t3_")) {
      return(col_name %in% paste0("t3_", outcome_vars))
    } else {
      return(FALSE)
    }
  }

  # Apply the custom function to select the desired columns
  wide_data_filtered <- wide_data %>%
    dplyr::select(id, which(sapply(colnames(wide_data), custom_col_filter))) %>%
    dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
    dplyr::relocate(starts_with("t2_"), .after = starts_with("t1_"))  %>%
    dplyr::relocate(starts_with("t3_"), .after = starts_with("t2_"))  %>%
    arrange(id)

  # Reorder t0_ columns
  t0_column_order <-
    c(
      paste0("t0_", baseline_vars),
      paste0("t0_", exposure_var),
      paste0("t0_", outcome_vars)
    )
  wide_data_ordered <- wide_data_filtered %>%
    select(id, t0_column_order, everything()) #%>%
    #select(-id)

  return(wide_data_ordered)
}


create_wide_data_general <- function(dat_long, baseline_vars, exposure_var, outcome_vars, exclude_vars = c()) {
  require(tidyverse)
  # Add the 'time' column to the data
  data_with_time <- dat_long %>%
    mutate(time = as.numeric(wave) - 1) %>%
    arrange(id, time)

  # Filter the data based on the time condition
  data_filtered <- data_with_time %>%
    filter(time >= 0)

  # Create the wide data frame
  wide_data <- data_filtered %>%
    dplyr::select(-exclude_vars)  %>%  # Exclude specified variables
    pivot_wider(
      id_cols = id,
      names_from = time,
      values_from = -c(id, time),
      names_glue = "t{time}_{.value}",
      names_prefix = "t"
    )

  # Define a custom function to filter columns based on conditions
  custom_col_filter <- function(col_name) {
    if (startsWith(col_name, "t0_")) {
      return(col_name %in% c(
        paste0("t0_", baseline_vars),
        paste0("t0_", exposure_var),
        paste0("t0_", outcome_vars)
      ))
    } else if (startsWith(col_name, "t1_")) {
      return(col_name %in% paste0("t1_", exposure_var))
    } else if (grepl("^t[2-9][0-9]*_", col_name)) {
      return(col_name %in% paste0("t2_", outcome_vars))
    } else {
      return(FALSE)
    }
  }

  # Apply the custom function to select the desired columns
  wide_data_filtered <- wide_data %>%
    dplyr::select(id, which(sapply(colnames(wide_data), custom_col_filter))) %>%
    dplyr::relocate(starts_with("t0_"), .before = starts_with("t1_"))  %>%
    arrange(id)

  # Extract unique time values from column names
  time_values <- gsub("^t([0-9]+)_.+$", "\\1", colnames(wide_data_filtered))
  time_values <- time_values[grepl("^[0-9]+$", time_values)]
  time_values <- unique(as.numeric(time_values))
  time_values <- time_values[order(time_values)]

  # Relocate columns iteratively
  for (i in 2:(length(time_values) - 1)) {
    wide_data_filtered <- wide_data_filtered %>%
      dplyr::relocate(starts_with(paste0("t", time_values[i + 1], "_")), .after = starts_with(paste0("t", time_values[i], "_")))
  }

  # Reorder t0_ columns
  t0_column_order <- c(paste0("t0_", baseline_vars), paste0("t0_", exposure_var), paste0("t0_", outcome_vars))
  wide_data_ordered <- wide_data_filtered %>%
    select(id, t0_column_order, everything()) #%>%
   # select(-id)

  return(wide_data_ordered)
}


# create_filter_wide_dataframes -------------------------------------------

#Here is a function create_filtered_wide_dataframes that takes the output of create_wide_data_general and returns a list of dataframes filtered according to the levels of the factor variable provided in exposure_var.


create_filtered_wide_dataframes <- function(dat_wide, exposure_vars) {
  # Check if exposure_vars are in dat_wide
  for (exposure_var in exposure_vars) {
    if (!exposure_var %in% names(dat_wide)) {
      stop(paste("exposure_var", exposure_var, "is not in the dataframe"))
    }
  }

  # Get factor and continuous exposure variables
  factor_exposure_vars <- exposure_vars[sapply(dat_wide[exposure_vars], is.factor)]
  continuous_exposure_vars <- setdiff(exposure_vars, factor_exposure_vars)

  if (length(factor_exposure_vars) > 1) {
    stop("More than one factor exposure variable is not allowed")
  }

  # Create a list to store the filtered dataframes
  list_filtered_df <- list()

  if (length(factor_exposure_vars) == 1) {
    # Get levels of the factor
    factor_levels <- levels(dat_wide[[factor_exposure_vars]])

    # Loop over each level and filter the dataframe
    for (level in factor_levels) {
      filtered_df <- dat_wide %>%
        filter((!!rlang::sym(factor_exposure_vars)) == level) %>%
        arrange(id)

      list_filtered_df[[level]] <- filtered_df
    }
  } else {
    # If there are no factor exposure variables, just arrange by id
    filtered_df <- dat_wide %>% arrange(id)
    list_filtered_df[["data"]] <- filtered_df
  }

  return(list_filtered_df)
}

# test
# Assume dat_long, baseline_vars, exposure_var, outcome_vars, and exclude_vars are defined

# # Create wide data
# wide_data <- create_wide_data_general(dat_long, baseline_vars, exposure_var, outcome_vars, exclude_vars)
#
# # Create filtered dataframes
# list_filtered_df <- create_filtered_wide_dataframes(wide_data, exposure_var)
#
# # Access individual filtered dataframe
# q1_df <- list_filtered_df[["q1"]]
# q2_df <- list_filtered_df[["q2"]]
# # ...and so on for each level


# impute data by exposure level of variable -------------------------------
impute_and_combine <- function(list_df, m = 10, exclude_vars = c("t0_sample_frame", "id")) {
  if (!require(mice, quietly = TRUE) || !require(dplyr, quietly = TRUE) || !require(miceadds, quietly = TRUE)) {
    stop("The 'mice', 'dplyr', and 'miceadds' packages are required for this function to work. Please install them.")
  }

  # The lapply function is used here to iterate over the list of data frames,
  # which is more efficient and cleaner than a for loop.
  list_completed_df <- lapply(list_df, function(df) {
    # Create predictor matrix
    init = mice::mice(df, maxit = 0)
    predictorMatrix = init$predictorMatrix

    # Exclude variables
    predictorMatrix[, intersect(colnames(predictorMatrix), exclude_vars)] = 0

    # Perform multiple imputation
    mice_df <- mice::mice(df, m = m, predictorMatrix = predictorMatrix)

    # Complete the data
    completed_df <- mice::complete(mice_df, action = "long", include = TRUE)

    # Reset rownames
    rownames(completed_df) <- NULL

    completed_df
  })

  # Bind rows
  complete_df <- dplyr::bind_rows(list_completed_df)

  # Assign new .imp and .id values
  complete_df <- complete_df %>%
    dplyr::group_by(.imp) %>%
    dplyr::mutate(
      .id = row_number()
    ) %>%
    dplyr::ungroup()

  # Convert to a list of data frames
  data_list <- split(complete_df, complete_df$.imp)

  # Convert to mids
  mids_df <- miceadds::datalist2mids(data_list, progress=FALSE)

  return(mids_df)
}


# matching ----------------------------------------------------------------
# method for propensity scores

match_mi <- function(data, X, baseline_vars ,estimand, method, sample_weights) {
  require(WeightIt)
  require(MatchThem)


  # if not binary, we model the interacton to obtain better weights
  # not we can add survey weights at this point.

  formula_str <- paste(X, "~", paste(baseline_vars, collapse = "+"))

  dt_match <- weightthem(
    as.formula(formula_str),
    weights = sample_weights,
    data,
    estimand = estimand,
    stabilize = TRUE,
    method = method
  )
  dt_match
}



# general function (work in progress)
match_mi_general <- function(data, X, baseline_vars, estimand, method,  subgroup = NULL, focal = NULL) {
  if (!requireNamespace("WeightIt", quietly = TRUE)) {
    stop("Package 'WeightIt' is required but not installed. Please install it using 'install.packages(\"WeightIt\")'.")
  }

  if (!requireNamespace("MatchThem", quietly = TRUE)) {
    stop("Package 'MatchThem' is required but not installed. Please install it using 'install.packages(\"MatchThem\")'.")
  }

  data_class <- class(data)

  if (!data_class %in% c("mids", "data.frame")) {
    stop("Input data must be either 'mids' or 'data.frame' object")
  }

  formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))

  weight_function <- if (data_class == "mids") weightthem else weightit

  perform_matching <- function(data_subset) {
    weight_function(
      formula = formula_str,
      data = data_subset,
      estimand = estimand,
      stabilize = TRUE,
      method = method,
      focal = focal
    )
  }

  if (is.null(subgroup)) {
    dt_match <- perform_matching(data)
  } else {
    levels_list <- unique(data[[subgroup]])

    dt_match_list <- lapply(levels_list, function(level) {
      data_subset <- data[data[[subgroup]] == level, ]
      perform_matching(data_subset)
    })

    names(dt_match_list) <- levels_list
    dt_match <- dt_match_list
  }

  return(dt_match)
}




# Compute the Average Treatement Effects (ATT and ATE)

# newest version allows for single dataframes

# causal_contrast_general <- function(df, Y, X, baseline_vars = "1", treat_0 = 0, treat_1 = 1, estimand = c("ATE", "ATT"), scale = c("RR","RD"), nsims = 200, cores = parallel::detectCores(), family = binomial(), weights = TRUE, continuous_X = FALSE, splines = FALSE) {
#   # Load required packages
#   require("clarify")
#   require("rlang") # for building dynamic expressions
#   require("glue") # for easier string manipulation
#   require("parallel") # detect cores
#   #require("survey") # correctly computes standard errors: to do
#
#   if (continuous_X) {
#     estimand <- "ATE"
#     # warning("When continuous_X = TRUE, estimand is always set to 'ATE'")
#   }
#
#   # Check if df is a mice object or a data.frame
#   if ("wimids" %in% class(df)) {
#     # Fit models using the complete datasets (all imputations)
#     fits <-  lapply(complete(df, "all"), function(d) {
#       # Set weights variable based on the value of 'weights' argument
#       weight_var <- if (weights) d$weights else NULL
#
#       # Check if continuous_X and splines are both TRUE
#       if (continuous_X && splines) {
#         require(splines) # splines package
#         formula_str <- paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
#       } else {
#         formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
#       }
#
#       glm(
#         as.formula(formula_str),
#         weights = if (!is.null(weight_var)) weight_var else NULL,
#         family = family,
#         data = d
#       )
#     })
#     # A `clarify_misim` object
#
#     sim.imp <- misim(fits, n = nsims, vcov = "HC3") #robust standard errors see CLARIFY package
#
#   } else {
#     # Fit models using the input data.frame
#     # Set weights variable based on the value of 'weights' argument
#     weight_var <- if (weights) df$weights else NULL
#
#     # Check if continuous_X and splines are both TRUE
#     if (continuous_X && splines) {
#       require(splines) # splines package
#       formula_str <- paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
#     } else {
#       formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
#     }
#
#     fit <- glm(
#       as.formula(formula_str),
#       weights = if (!is.null(weight_var)) weight_var else NULL,
#       family = family,
#       data = df
#     )
#     # A `clarify_sim` object
#
#     sim.imp <- sim(fit, n = nsims, vcov = "HC3")# robust covariance matrix see clarify package
#   }
#   # Compute the Average Marginal Effects
#
#   if (!continuous_X && estimand == "ATT") {
#     # Build dynamic expression for subsetting
#     subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)
#
#     sim_estimand <- sim_ame(sim.imp,
#                             var = X,
#                             subset = eval(subset_expr),
#                             cl = cores,
#                             verbose = FALSE)
#   } else { # For ATE
#     sim_estimand <- sim_ame(sim.imp,
#                             var = X,
#                             cl = cores,
#                             verbose = FALSE)
#   }
#
#   if (continuous_X) {
#     return(summary(sim_estimand))
#     # sim_estimand <- as.data.frame(sim_estimand). # not working
#     # rownames(sim_estimand) <- scale
#
#   } else {
#     # Convert sim_estimand to a data frame
#     sim_estimand_df <- as.data.frame(sim_estimand)
#
#     # Transform the results based on the specified scale
#     if (scale == "RR") {
#       sim_estimand_df$RR <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] / sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
#
#     } else {
#       sim_estimand_df$RD <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] - sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
#
#     }
#
#     # Calculate the desired summary statistics
#     out <- t(sapply(sim_estimand_df, function(x) {
#       c(Estimate = round(mean(x), 4), `2.5 %` = round(quantile(x, 0.025), 4), `97.5 %` = round(quantile(x, 0.975), 4))
#     }))
#
#     # Set the row names of the output to match the desired format
#     rownames(out) <- colnames(sim_estimand_df)
#
#
#     # Rename the column names to avoid duplicates
#     colnames(out) <- c("Estimate", "2.5 %", "97.5 %")
#
#     return(out)
#   }
# }
#https://eprints.whiterose.ac.uk/169886/3/Robust%20SE.%20manuscript.%20in%20White%20Rose.pdf
causal_contrast_general <- function(df, Y, X, baseline_vars = "1", treat_0 = 0, treat_1 = 1, estimand = c("ATE", "ATT"), scale = c("RR","RD"), nsims = 200, cores = parallel::detectCores(), family = binomial(), weights = TRUE, continuous_X = FALSE, splines = FALSE, vcov = "HC2") {
  # Load required packages
  require("clarify")
  require("rlang") # for building dynamic expressions
  require("glue") # for easier string manipulation
  require("parallel") # detect cores
  require("purrr")

  # Set vcov default based on family argument
  if (is.null(vcov)) {
    if (inherits(family, "quasibinomial")) {
      vcov <- vcov # to fix later
    } else {
      vcov <- vcov # to fix later
    }
  }

  if (continuous_X) {
    estimand <- "ATE"
  }

  if ("wimids" %in% class(df)) {
    fits <- complete(df, "all") %>%
      purrr::map(function(d) {
        weight_var <- if (weights) d$weights else NULL
        formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
        glm(as.formula(formula_str), weights = weight_var, family = family, data = d)
      })

    sim.imp <- misim(fits, n = nsims, vcov = vcov) #robust standard errors see CLARIFY package
  } else {
    weight_var <- if (weights) df$weights else NULL
    formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
    fit <- glm(as.formula(formula_str), weights = weight_var, family = family, data = df)
    sim.imp <- sim(fit, n = nsims, vcov = vcov)# robust covariance matrix see clarify package
  }

  if (!continuous_X && estimand == "ATT") {
    subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)
    sim_estimand <- sim_ame(sim.imp, var = X, subset = eval(subset_expr), cl = cores, verbose = FALSE)
  } else {
    sim_estimand <- sim_ame(sim.imp, var = X, cl = cores, verbose = FALSE)
  }

  if (continuous_X) {
    return(summary(sim_estimand))
  } else {
    sim_estimand_df <- as.data.frame(sim_estimand) %>%
      purrr::map_df(function(x) {
        c(Estimate = round(mean(x), 4), `2.5 %` = round(quantile(x, 0.025), 4), `97.5 %` = round(quantile(x, 0.975), 4))
      }) %>%
      mutate(
        RR = case_when(scale == "RR" ~ !!rlang::sym(paste0("E[Y(", treat_1, ")]")) / !!rlang::sym(paste0("E[Y(", treat_0, ")]"))),
        RD = case_when(scale == "RD" ~ !!rlang::sym(paste0("E[Y(", treat_1, ")]")) - !!rlang::sym(paste0("E[Y(", treat_0, ")]")))
        )

        colnames(sim_estimand_df) <- c("Estimate", "2.5 %", "97.5 %")

        return(sim_estimand_df)
        }
  }

build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
  if (continuous_X && splines) {
    return(paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")"))
  } else {
    return(paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")"))
  }
}






# slightly older
causal_contrast <- function(df, Y, X, baseline_vars = "1", treat_0 = 0, treat_1 = 1, estimand = c("ATE", "ATT"), scale = c("RR","RD"), nsims = 200, cores = parallel::detectCores(), family = binomial(), weights = TRUE, continuous_X = FALSE, splines = FALSE) {
  # Load required packages
  require("clarify")
  require("rlang") # for building dynamic expressions
  require("glue") # for easier string manipulation
  require("parallel") # detect cores
  #require("survey") # correctly computes standard errors: to do

  if (continuous_X) {
    estimand <- "ATE"
   # warning("When continuous_X = TRUE, estimand is always set to 'ATE'")
  }

  # Fit models using the complete datasets (all imputations)
  fits <-  lapply(complete(df, "all"), function(d) {
    # Set weights variable based on the value of 'weights' argument
    weight_var <- if (weights) d$weights else NULL

    # Check if continuous_X and splines are both TRUE
    if (continuous_X && splines) {
      require(splines) # splines package
      formula_str <- paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
    } else {
      formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
    }

    glm(
      as.formula(formula_str),
      weights = if (!is.null(weight_var)) weight_var else NULL,
      family = family,
      data = d
    )
  })
  # A `clarify_misim` object

  sim.imp <- misim(fits, n = nsims, vcov = "HC2")

  # Compute the Average Marginal Effects

  if (!continuous_X && estimand == "ATT") {
      # Build dynamic expression for subsetting
    subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)

    sim_estimand <- sim_ame(sim.imp,
                              var = X,
                              subset = eval(subset_expr),
                              cl = cores,
                              verbose = FALSE)
    } else { # For ATE
      sim_estimand <- sim_ame(sim.imp,
                              var = X,
                              cl = cores,
                              verbose = FALSE)
    }

    if (continuous_X) {
      return(summary(sim_estimand))
     # sim_estimand <- as.data.frame(sim_estimand). # not working
     # rownames(sim_estimand) <- scale

    } else {
      # Convert sim_estimand to a data frame
      sim_estimand_df <- as.data.frame(sim_estimand)

      # Transform the results based on the specified scale
      if (scale == "RR") {
        sim_estimand_df$RR <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] / sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]

      } else {
        sim_estimand_df$RD <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] - sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]

      }

      # Calculate the desired summary statistics
      out <- t(sapply(sim_estimand_df, function(x) {
        c(Estimate = round(mean(x), 4), `2.5 %` = round(quantile(x, 0.025), 4), `97.5 %` = round(quantile(x, 0.975), 4))
      }))

      # Set the row names of the output to match the desired format
      rownames(out) <- colnames(sim_estimand_df)


      # Rename the column names to avoid duplicates
      colnames(out) <- c("Estimate", "2.5 %", "97.5 %")

      return(out)
    }
  }


# general contrast table --------------------------------------------------

tab_ate <- function(x, new_name, delta = 1, sd = 1, type = c("RD","RR"), continuous_X = FALSE) {
  require("EValue")
  require(dplyr)

  type <- match.arg(type)

  x <- as.data.frame(x)

  if (continuous_X) {
    rownames(x) <- type
  } else{
   x
  }

  out <- x %>%
    dplyr::filter(row.names(x) == type) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 4))

  if (type == "RD") {
    out <- out %>%
      dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
  } else {
    out <- out %>%
      dplyr::rename("E[Y(1)]/E[Y(0)]" = Estimate)
  }

  rownames(out)[1] <- paste0(new_name)
  out <- as.data.frame(out)

  if (type == "RD") {
    tab0 <- out |>  dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
    evalout <- as.data.frame(round(EValue::evalues.OLS(tab0[1, 1],
                                                       se = tab0[1, 4],
                                                       sd = sd,
                                                       delta = delta,
                                                       true = 0
    ),
    3
    ))
  } else {
    evalout <- as.data.frame(round(EValue::evalues.RR(out[1, 1],
                                                      lo = out[1, 2],
                                                      hi = out[1, 3],
                                                      true = 1
    ),
    3
    ))
  }

  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E_Value", "E_Val_bound")

  if (type == "RD") {
    tab <- cbind.data.frame(tab0, evalout3) |> dplyr::select(-c(standard_error))
  } else {
    tab <- cbind.data.frame(out, evalout3)
  }

  return(tab)
}

# combine causal contrast and tab ate -------------------------------------

gcomp_sim <- function(df, Y, X, new_name, baseline_vars = "1", treat_0 = 0, treat_1 = 1, estimand = "ATE", scale = c("RR","RD"), nsims = 200,
                      cores = parallel::detectCores(), family = quasibinomial(), weights = TRUE, continuous_X = FALSE, splines = FALSE, delta = 1, sd = 1, type = c("RD", "RR"), vcov = "HC2") {
  # Call the causal_contrast_general() function
  causal_contrast_result <- causal_contrast_general(df, Y, X, baseline_vars, treat_0, treat_1,estimand, scale, nsims, cores, family, weights, continuous_X, splines, vcov = "HC2")

  # Call the tab_ate() function with the result from causal_contrast()
  tab_ate_result <- tab_ate(causal_contrast_result, new_name, delta, sd, type, continuous_X)

  return(tab_ate_result)
}



# group tab ---------------------------------------------------------------
group_tab <- function(df, type = c("RR", "RD")) {
  type <- match.arg(type)

  require(dplyr)

  if (type == "RR") {
    out <- df %>%
      arrange(desc(`E[Y(1)]/E[Y(0)]`)) %>%
      dplyr::mutate(Estimate  = as.factor(ifelse(
        `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
        "positive",
        ifelse( `E[Y(1)]/E[Y(0)]` < 1 &
                  `97.5 %` < 1, "negative",
                "not reliable")
      ))) %>%
      rownames_to_column(var = "outcome") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(`E[Y(1)]/E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
      )
  } else {
    out <- df %>%
      arrange(desc(`E[Y(1)]-E[Y(0)]`)) %>%
      dplyr::mutate(Estimate  = as.factor(ifelse(
        `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
        "positive",
        ifelse( `E[Y(1)]-E[Y(0)]` < 0 &
                  `97.5 %` < 0, "negative",
                "not reliable")
      ))) %>%
      rownames_to_column(var = "outcome") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
      )
  }

  return(out)
}

#
# group_tab <- function(df, type = c("RR", "RD")) {
#   type <- match.arg(type)
#
#   require(dplyr)
#
#   if (type == "RR") {
#     out <- df %>%
#       arrange(desc(`E[Y(1)]/E[Y(0)]`)) %>%
#       dplyr::mutate(Estimate  = as.factor(ifelse(
#         `E[Y(1)]/E[Y(0)]` > 1 & `2.5 %` > 1,
#         "positive",
#         ifelse( `E[Y(1)]/E[Y(0)]` < 1 &
#                   `97.5 %` < 1, "negative",
#                 "not reliable")
#       ))) %>%
#       rownames_to_column(var = "outcome") %>%
#       mutate(
#         across(where(is.numeric), round, digits = 3),
#         estimate_lab = paste0(`E[Y(1)]/E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
#       )
#   } else {
#     out <- df %>%
#       arrange(desc(`E[Y(1)]-E[Y(0)]`)) %>%
#       dplyr::mutate(Estimate  = as.factor(ifelse(
#         `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0,
#         "positive",
#         ifelse( `E[Y(1)]-E[Y(0)]` < 0 &
#                   `97.5 %` < 0, "negative",
#                 "not reliable")
#       ))) %>%
#       rownames_to_column(var = "outcome") %>%
#       mutate(
#         across(where(is.numeric), round, digits = 3),
#         estimate_lab = paste0(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
#       )
#   }
#
#   return(out)
# }



# group_plot  -------------------------------------------------------------------

group_plot_ate <- function(.data, type = "RD", title, subtitle, xlab, ylab,
                           x_offset = 0,
                           x_lim_lo = 0,
                           x_lim_hi = 1.5) {
  type <- match.arg(type)

  xintercept <- if (type == "RR") 1 else 0
  x_axis_label <- if (type == "RR") "Causal Risk Ratio" else "Causal Risk Difference"

  out <- ggplot(
    data = .data,
    aes(
      y = reorder(outcome, .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]]),
      x = .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]],
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      group = Estimate,
      color = Estimate
    )
  ) +
    geom_errorbarh(aes(color = Estimate), height = .3, position = position_dodge(width = 0.3)) +
    geom_point(size = .5, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = xintercept, linetype = "solid") +
    theme_classic(base_size = 10) +
    scale_color_manual(values = c("orange", "black", "dodgerblue")) +
    labs(
      x = x_axis_label,
      y = " ",
      title = title,
      subtitle = subtitle
    ) +
    geom_text(
      aes(x = x_offset, label = estimate_lab),
      size = 2,
      hjust = 0,
      fontface = ifelse(.data$Estimate == "not reliable", "plain", "bold")
    ) +
    coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )

  return(out)
}


# possible ways of adjusting the plot
# theme(
#   panel.border = element_blank(),
#   axis.line = element_blank(),
#   panel.background = element_blank(),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   axis.title.x = element_text(size = 12),  # x-axis label font size
#   axis.title.y = element_text(size = 12), # y-axis label font size
#   plot.title = element_text(face = "bold", size = 16, hjust = 0),  # increase title font size and align it to the left
#   plot.subtitle = element_text(size = 14),  # Increase title font size
#   axis.text = element_text(size = 12),  # Increase axis text font size
#   plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt") # Add space to accommodate text outside the plot area
# ) +



# interpret table ---------------------------------------------------------

library(tidyverse)
library(glue)


interpret_table <- function(df, causal_scale, estimand) {
  estimand_description <- case_when(
    estimand %in% c("PATE", "ATE") ~ "Average Treatment Effect (ATE) represents the expected difference in outcomes between treatment and control groups for the whole population.",
    estimand %in% c("PATT", "ATT") ~ "Average Treatment Effect on the Treated (ATT) represents the expected difference in outcomes between treatment and control groups for the individuals who received the treatment.",
    estimand %in% "CATE" ~ "Conditional Average Treatment Effect (CATE) represents the expected difference in outcomes between treatment and control groups for a specific subgroup of individuals.",
    estimand %in% c("SATE", "SATT") ~ "Sample Average Treatment Effect (SATE) represents the expected difference in outcomes between treatment and control groups within the sampled population.",
    TRUE ~ "The specified estimand is not recognized. Please use one of the following: 'PATE', 'PATT', 'ATE', 'ATT', 'CATE', 'SATE', 'SATT'."
  )

  if (causal_scale == "risk_ratio") {
    interpretation <- df %>%
      mutate(
        causal_contrast = round(`E[Y(1)]/E[Y(0)]`, 3),
        strength_of_evidence = case_when(
          E_Value >= 1.25 ~ "reliable evidence for causality",
          E_Value >= 1.1 ~ "evidence for causality is not conclusive",
          TRUE ~ "no reliable evidence for causality"
        ),
        outcome_interpretation = glue(
          "For the outcome '{outcome}', the {estimand} causal contrast is {causal_contrast}. ",
          "The confidence interval ranges from {round(`2.5 %`, 3)} to {round(`97.5 %`, 3)}. ",
          "The E-value for this outcome is {round(E_Value, 3)}, indicating {strength_of_evidence}."
        )
      )
  } else if (causal_scale == "risk_difference") {
    interpretation <- df %>%
      mutate(
        causal_contrast = round(`E[Y(1)]-E[Y(0)]`, 3),
        strength_of_evidence = case_when(
          E_Value >= 1.25 ~ "reliable evidence for causality",
          E_Value >= 1.1 ~ "evidence for causality is not conclusive",
          TRUE ~ "no reliable evidence for causality"
        ),
        outcome_interpretation = glue(
          "For the outcome '{outcome}', the {estimand} causal contrast is {causal_contrast}. ",
          "The confidence interval ranges from {round(`2.5 %`, 3)} to {round(`97.5 %`, 3)}. ",
          "The E-value for this outcome is {round(E_Value, 3)}, indicating {strength_of_evidence}."
        )
      )
  } else {
    stop("Invalid causal_scale argument. Please use 'risk_ratio' or 'risk_difference'.")
  }

  result <- glue("Table interpretation:\n\n{estimand_description}\n\n{paste(interpretation$outcome_interpretation, collapse = '\n\n')}")
  return(result)
}




# new combo function ------------------------------------------------------

# mine:
gcomp_delta <- function(df, X, Y, baseline_vars, family, m = 10, min, max, r = 0, f = 1, splines = FALSE, delta = 1, sd = 1, new_name) {
  require("splines")
  require("mice")
  require("EValue")
  require("dplyr")
  require("stdReg")

  # Fit model with or without splines
  if (splines) {
    fits <- mice_generalised(df, X, Y, baseline_vars, family)
  } else {
    fits <- mice_generalised_lin(df, X, Y, baseline_vars, family)
  }

  # Pool and compute the g-formula
  pooled_results <-   pool_stglm_contrast(
    fits,
    df = df,
    m = m,
    X = X,
    x = x,
    r = r
  )

  focal_pooled_results <- pooled_results[match(p, x), ]

  # Calculate RD
  rd_result <- c(focal_pooled_results$est[2] - pooled_results$est[1])


  # Calculate upper 97.5
  rd_ui <- focal_pooled_results$ui[2] - focal_pooled_results$ui[1]

  # Calculate lower 2.5%
  rd_li <- focal_pooled_results$li[2] - focal_pooled_results$li[1]

  # Calculate EVALUES
  evalues_results <-  vanderweelevalue_ols(pooled_results, f - min, delta, sd)

  # Create compatible dataframe with tab_ate()
  compatible_df <- data.frame(
    "Estimate" = rd_result,
    "2.5 %" = rd_li,
    "97.5 %" = rd_ui,
    "E_Value" = evalues_results$`E-value`,
    "E_Val_bound" = evalues_results$`threshold`,
    row.names = "RD"
  )

  compatible_df <- round(compatible_df, 4)

  colnames(compatible_df) <- c("E[Y(1)]-E[Y(0)]", "2.5 %", "97.5 %", "E_Value", "E_Val_bound")
  rownames(compatible_df) <- new_name

  return(compatible_df)
}



# functions for table1 -----------------------------------------------------

#table
# functions for table
my_render_cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=3), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my_render_cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}



## IGNORE FROM HERE THESE ARE THE OLD METHODS ##

# glm_contrast_mi --------------------------------------------------------------
# current standard for causal contrasts with mi datasets in mice

# value is contrast from 0
# dt_match is a matchthem object -- if no propensity scores are used then this is just the mice output
# nsims is number of bootstraps
# baseline_vars is as it sounds
# cl is number of computer cores
# family is as it sounds

glm_contrast_mi <- function(dt_match, nsims, Y, X, baseline_vars, cl,family, delta) {
  require("clarify")
  require("rlang") # for building dynamic expressions

  fits <-  lapply(complete(dt_match, "all"), function(d) {
    glm(as.formula(paste( paste(Y, "~", X , "*", "("), paste(baseline_vars, collapse = "+"),paste(")"))),
        weights = d$weights, # specify weights column from the dataset
        family = family,
        data = d
    )
  })

  sim.imp <- misim(fits, n = nsims, vcov = "HC3")

  # Build dynamic expression for subsetting
  subset_expr <- rlang::expr(!!rlang::sym(X) == !!delta)

  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = eval(subset_expr),
    # Evaluate the subset_expr expression
    cl = cl,
    verbose = FALSE
  )

  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)

  out <- summary(sim_est)

  out
}








# full table --------------------------------------------------------------
#
#
# tab_ate_rd <- function(x, new_name, delta, sd) {
#   require("EValue")
#   # x = output from function glm_contrast_mi: new_name is for the new table name
#   # value = contrast
#   # sd = standard deviation of the outcome
#   require(dplyr)
#   # make clarify object into a data frame
#   x <- as.data.frame(x)
#   out <- x %>%
#     # take row that is needed
#     dplyr::slice(3) %>%
#     # use only three digits
#     dplyr::mutate(across(where(is.numeric), round, digits = 4)) %>%
#     # Estimand of interest is risk difference
#     dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
#
#   #rename row
#   rownames(out)[1] <- paste0(new_name)
#   out <- as.data.frame(out)
#   out
#   # make evalue column, which is needed four evalues
#   # Calculate the standard error
#   tab0 <- out |>  dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
#   evalout <- as.data.frame(round(EValue::evalues.OLS(tab0[1, 1],
#                                                      se = tab0[1, 4],
#                                                      sd = sd,
#                                                      delta = delta,
#                                                      true = 0
#   ),
#   3
#   ))
#
#   evalout2 <- subset(evalout[2, ])
#   evalout3 <- evalout2 |>
#     select_if( ~ !any(is.na(.)))
#   colnames(evalout3) <- c("E_Value", "E_Val_bound")
#   tab <- cbind.data.frame(tab0, evalout3) |> dplyr::select(-c(standard_error)) # keep Evalue
#   return(tab)
# }
#
#
#
# # table on risk ratio scale
#
#
# tab_ate_rr <- function(x, new_name, delta, sd) {
#   require("EValue")
#   # x = output from function glm_contrast_mi: new_name is for the new table name
#   # value = contrast
#   # sd = standard deviation of the outcome
#   require(dplyr)
#   # make clarify object into a data frame
#   x <- as.data.frame(x)
#   out <- x %>%
#     # take row that is needed
#     dplyr::slice(3) %>%
#     # use only three digits
#     dplyr::mutate(across(where(is.numeric), round, digits = 4)) %>%
#     # Estimand of interest is risk difference
#     dplyr::rename("E[Y(1)]/E[Y(0)]" = Estimate)
#
#   #rename row
#   rownames(out)[1] <- paste0(new_name)
#   out <- as.data.frame(out)
#   out
#   # make evalue column, which is needed four evalues
#   # Calculate the standard error
#   tab0 <- out #|>  dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
#   evalout <- as.data.frame(round(EValue::evalues.RR(tab0[1, 1],
#                                                     lo = tab0[1, 2],
#                                                     hi = tab0[1, 3],
#                                                     true = 1
#   ),
#   3
#   ))
#   evalout2 <- subset(evalout[2, ])
#   evalout3 <- evalout2 |>
#     select_if( ~ !any(is.na(.)))
#   colnames(evalout3) <- c("E_Value", "E_Val_bound")
#   tab <- cbind.data.frame(tab0, evalout3) #|> dplyr::select(-c(Evalue)) # keep table minimal
#   return(tab)
# }
#
#
#
#
#
# # table for many outcomes -------------------------------------------------
#
#
# # function to create group tables
# ## Evalue plot CRD
# group_tab_ate <- function(df) {
#   # use rbind to gather muliple tab_ate_ols outputs e.g.:
#   # df <-
#   #   rbind(
#   #     tb_1,
#   #     tb_2,
#   #     tb_3,...
#   #   )
#   require(dplyr)
#   # take group data frame, make a column for reliable estimates,
#   out  <-  df |>
#     arrange(desc(`E[Y(1)]-E[Y(0)]`)) |>
#     dplyr::mutate(Estimate  = as.factor(ifelse(
#       `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0 ,
#       "positive",
#       ifelse(`E[Y(1)]-E[Y(0)]` < 0 &
#                `97.5 %` < 0 , "negative",
#              "not reliable")
#     ))) |>
#     rownames_to_column(var = "outcome") |>
#     #label for graph
#     mutate(
#       across(where(is.numeric), round, digits = 3),
#       estimate_lab = paste0(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`,"]")
#     )
#
#   out
# }
#
#




## Evalue plot RR
#
#
# group_plot_ate_rr <- function(df, title, subtitle, xlab, ylab,
#                               x_offset= 0,
#                               x_lim_lo = 0,
#                               x_lim_hi = 1.5) {
#   # Convert the title string to a symbol
#   title_sym <- sym(title)
#   # create plot
#   out <-ggplot(
#     data = df,
#     aes(
#       y = reorder(outcome, `E[Y(1)]/E[Y(0)]`),
#       x = `E[Y(1)]/E[Y(0)]`,
#       xmin = `2.5 %`,
#       xmax = `97.5 %`,
#       group = Estimate,
#       color = Estimate
#     )
#   ) +
#     geom_errorbarh(aes(color = Estimate), height = .3, position = position_dodge(width = 0.3)) + # Add color to geom_errorbarh
#     geom_point(size = 4, position = position_dodge(width = 0.3)) + # Replace geom_col with geom_point
#     geom_vline(xintercept = 1, linetype = "solid") +
#     theme_classic(base_size = 12) +
#     scale_color_manual(values = c("orange", "black", "dodgerblue")) + # Set custom color scale
#     labs(
#       x = "Causal Risk Ratio",
#       y = " ",
#       title = title,
#       subtitle = subtitle
#     ) +
#     geom_text(
#       aes(x = x_offset, label = estimate_lab), # Display only estimate labels
#       size = 4,
#       hjust = 0,
#       fontface = ifelse(df$Estimate == "not reliable", "plain", "bold")
#     ) +
#     coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
#
#     # coord_fixed(clip = "off",
#     #             xlim = c(x_lim_lo, x_lim_hi)
#     #             ) +
#     theme(
#       legend.position = "top",
#       legend.direction = "horizontal",
#       plot.title = element_text(face = "bold", size = 12, hjust = 0), # Align title to the left
#       plot.subtitle = element_text(size = 10, hjust = 0) # Align subtitle to the left
#     )
#   # plot
#   out
# }
#
#



# table for contrast of causal effect estimates ---------------------------

# ignore

# library(tidyverse)
# library(glue)
# #
# calculate_difference <- function(df, df1, causal_scale) {
#   # Check if both data frames have the same outcomes
#   if (!all(df$outcome == df1$outcome)) {
#     stop("The outcomes in the provided data frames do not match.")
#   }
#
#   if (causal_scale == "risk_difference") {
#     # Calculate the differences in causal effect estimates and their variances
#     differences <- df %>%
#       inner_join(df1, by = "outcome", suffix = c("_1", "_2")) %>%
#       mutate(
#         causal_difference = `E[Y(1)]-E[Y(0)]_1` - `E[Y(1)]-E[Y(0)]_2`,
#         variance_1 = (`97.5 %_1` - `2.5 %_1`)^2 / 16,
#         variance_2 = (`97.5 %_2` - `2.5 %_2`)^2 / 16,
#         variance_difference = variance_1 + variance_2,
#         ci_lower = causal_difference - 1.96 * sqrt(variance_difference),
#         ci_upper = causal_difference + 1.96 * sqrt(variance_difference)
#       ) %>%
#       select(outcome, causal_difference, ci_lower, ci_upper)
#   } else if (causal_scale == "risk_ratio") {
#     differences <- df %>%
#       inner_join(df1, by = "outcome", suffix = c("_1", "_2")) %>%
#       mutate(
#         log_ca_diff = log(`E[Y(1)]/E[Y(0)]_1`) - log(`E[Y(1)]/E[Y(0)]_2`),
#         variance_1 = (log(`97.5 %_1`) - log(`2.5 %_1`))^2 / 16,
#         variance_2 = (log(`97.5 %_2`) - log(`2.5 %_2`))^2 / 16,
#         variance_difference = variance_1 + variance_2,
#         ci_lower_log = log_ca_diff - 1.96 * sqrt(variance_difference),
#         ci_upper_log = log_ca_diff + 1.96 * sqrt(variance_difference),
#         causal_difference = exp(log_ca_diff),
#         ci_lower = exp(ci_lower_log),
#         ci_upper = exp(ci_upper_log)
#       ) %>%
#       select(outcome, causal_difference, ci_lower, ci_upper)
#   } else {
#     stop("Invalid causal_scale argument. Please use 'risk_ratio' or 'risk_difference'.")
#   }
#
#   interpretation_text <- differences %>%
#     mutate(
#       outcome_text = glue("For the outcome '{outcome}', the difference in causal effects between the two treatments is {causal_difference} (95% CI: {ci_lower}-{ci_upper})."),
#       significant_text = case_when(
#         (causal_scale == "risk_difference" & (ci_lower > 0 | ci_upper < 0)) |
#           (causal_scale == "risk_ratio" & (ci_lower > 1 | ci_upper < 1)) ~ "This difference is statistically reliable",
#         TRUE ~ "This difference is not statistically reliable."
#       )
#     ) %>%
#     summarise(
#       full_text = glue_collapse(glue("{outcome_text} {significant_text}\n"), sep = "\n")
#     ) %>%
#     pull(full_text)
#
#   return(list(table = differences, text = interpretation_text))
# }
#   return(differences)
# }
#
# # result <- calculate_difference(df, df1, causal_scale = "risk_difference")
# # print(result$table)
# # cat(result$text)
#

# forest plots studies OLD ------------------------------------


# Made this better in the outcomewide attacks script -- under "scripts" --> "functions"

### Ignore
bind_forestplot = function(x) {
  ls_output  <- lapply(x, function(zed) {
    dt_zed <- as.data.frame(zed$data)
    dt_zed$id <- rep(zed$labels$y, nrow(dt_zed))
    dt_zed
  })
  df_output <- do.call(rbind, ls_output)
  df_output
}

## create forest plot, using a "bind_forestplot" object

gcomp_forestplot = function(out, title, ylim, xlab) {
  # provisional
  require(ggplot2)
  require(viridisLite)
  ggplot(data = out, aes(
    y = id,
    x = est,
    xmin = ui,
    xmax = li,
    colour = factor(row)
  )) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbarh(height = .3, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_vline(
      xintercept = c(-.5,-.25, .25, .5),
      linetype = "twodash",
      alpha = .5
    ) + # experimental
    scale_x_continuous(limits = ylim) +
    # theme_forest() +
    theme_classic(base_size = 10) +
    theme(
      panel.border = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    #  scale_color_discrete(name = "Change in exposure from baseline (SD)", direction = -1) +
    scale_color_viridis_d(name = "Change in exposure\nfrom baseline (SD)",
                          direction = -1,
                          option = "D") +
    labs(x = "Change in outcome (SD)",
         y = "Many Outcomes",
         title = title)
}


gcomp_forestplot_rr = function(out, title, ylim, xlab) {
  require(ggplot2)
  require(viridisLite)
  ggplot(data = out, aes(
    y = id,
    x = est,
    xmin = ui,
    xmax = li,
    colour = factor(row)
  )) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbarh(height = 0.3, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = 1, linetype = "twodash") +
    scale_x_continuous(limits = ylim) +
    # theme_forest() +
    theme_classic(base_size = 10) +
    theme(
      panel.border = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_color_viridis_d(name = "Change in exposure\nfrom baseline (SD)",
                          direction = -1,
                          option = "D") +
    labs(x = "RR of outcome (SD)",
         y = "Many Outcomes",
         title = title)
}


# mice models -----------------------------------------------------------

# df here is an object of the class "mids" in the mice package.



mice_gaussian = function(df, X, Y, cvars) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  ))))
  out
}

# note this has change
mice_generalised = function(df, X, Y, baseline_vars, family) {
  require("splines")
  require("mice")
  require("survey") # correct stnadard errors

  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(baseline_vars, collapse = "+")
  )), family = family))
  out
}

mice_generalised_lin = function(df, X, Y, baseline_vars, family) {
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~", X, "+"),
    paste(baseline_vars, collapse = "+")
  )), family = family))
  out
}



mice_gaussian_pre = function(df, X, Y, baseline_vars, pre_y) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(pre_y, "+"),
    paste(baseline_vars, collapse = "+")
  ))))
  out
}



mice_generalised_pre = function(df, X, Y, baseline_vars, pre_x, family) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(pre_x, "+"),
    paste(baseline_vars, collapse = "+")
  )), family = family))
  out
}


#  IPTW ----------------------------------------------------------------
# functions



mice_iptw = function(X, Y, df, family = "gaussian") {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  require("mice")
  out_m <- with(df, glm(as.formula(paste(Y, "~ bs(", X , ")")),
                        weights = weights,
                        family = family))
  return(out_m)
}


# modified early 23
# mice_iptw_lin = function(X,Y,df, family) {
#   # requires that a MATCH THEM dataset is converted to a mice object
#   # weights must be called "weights)
#   require("mice")
#   out_m <- with(df, glm(
#     as.formula(paste(Y, "~ (", X , ")")),    weights = weights,
#     family = family
#   ))
#   return(out_m)
# }


mice_iptw_lin = function(X, Y, df, baseline_vars, family = "gaussian") {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("mice")
  out_m <- with(df, glm(as.formula(paste(
    paste(Y, "~", X, "+"),
    paste(baseline_vars, collapse = "+")
  )),
  weights = weights,
  family = family))
  return(out_m)
}






# mice_iptw_lin = function(X, Y, df, family = "gaussian") {
#   # requires that a MATCH THEM dataset is converted to a mice object
#   # weights must be called "weights)
#   require("mice")
#   out_m <- with(df, glm(as.formula(paste(Y, "(", X , ")")),
#                         weights = weights,
#                         family = family))
#   return(out_m)
# }




# regression without imputation -------------------------------------------


# for regression without mi
glm_nomi = function(X, Y, df, baseline_vars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  out_m <- glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(baseline_vars, collapse = "+")
  )), family = family, data = df)
  return(out_m)
}


glm_nomi_lin = function(X, Y, df, baseline_vars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  out_m <- glm(as.formula(paste(
    paste(Y, "~ (", X , ")+"),
    paste(baseline_vars, collapse = "+")
  )), family = family,
  weights = weights,
  data = df)
  return(out_m)
}



glm_nomi_lin = function(X, Y, df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  out_m <- glm(as.formula(paste(
    paste(Y, "~ (", X , ")+"),
    paste(cvars, collapse = "+")
  )), family = family, data = df)
  return(out_m)
}



# require("mice")
# out_m <- with(df, glm(
#   as.formula(paste(Y, "~ (", X , ")")),    weights = weights,
#   family = family
# ))
# return(out_m)

# g-computation engine ----------------------------------------------------


# function for mice models
pool_stglm <- function(models, df, m, x, X) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = models$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x
      )
    est.all[, i] <- g.comp$est
    var.all[, i] <- diag(g.comp$vcov)
  }
  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)

# ammend? see:
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://thestatsgeek.com/wp-content/uploads/2023/02/gformulaMI_CSM_2023_02_08.pdf

  #total variance
  var <- W + (1 + 1 / m) * B

  #total standard error
  se <- sqrt(var)
  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)
  # lower interval
  ui <- est + (1.96 * se)
  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}

## Contrast models
# m = number of data sets imputed
# x

pool_stglm_contrast <- function(out, df, m, x, X, r) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = out$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x
      )
    ss <- summary(object = g.comp,
                  contrast = "difference",
                  reference = r)
    #est.all[, i] <- g.comp$est
    est.all[, i] <- ss$est.table[, "Estimate"]
    #var.all[, i] <- diag(g.comp$vcov)
    var.all[, i] <- ss$est.table[, "Std. Error"] ^ 2
  }

  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)

  #total variance
  var <- W #+ (1 + 1 / m) * B  # amended RUBINS rule overstates

  #total standard error
  se <- sqrt(var)

  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)

  # lower interval
  ui <- est + (1.96 * se)

  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}


pool_stglm_contrast_ratio <- function(out, df, m, x, X, r) {
  nx <- length(x)
  est.all <- matrix(nrow = nx, ncol = m)
  var.all <- matrix(nrow = nx, ncol = m)
  for (i in 1:m) {
    g.comp <-
      stdGlm(
        fit = out$analyses[[i]],
        data = complete(df, i),
        X = X,
        x = x
      )
    ss <- summary(object = g.comp,
                  contrast = "ratio",
                  reference = r)
    #est.all[, i] <- g.comp$est
    est.all[, i] <- ss$est.table[, "Estimate"]
    #var.all[, i] <- diag(g.comp$vcov)
    var.all[, i] <- ss$est.table[, "Std. Error"] ^ 2
  }

  #estimate
  est <- rowMeans(est.all)

  #within-variance
  W <- rowMeans(var.all)

  #between-variance
  B <- apply(X = est.all, MARGIN = 1, FUN = var)

  #total variance
  var <- W #+ (1 + 1 / m) * B  # amended RUBINS rule overstates

  #total standard error
  se <- sqrt(var)

  #confidence intervals
  ci <- cbind(est - 1.96 * se, est + 1.96 * se)

  # lower interval
  ui <- est + (1.96 * se)

  #upper interval
  li <- est - (1.96 * se)
  # row units
  row <- x
  # make data frame
  outp <- as.data.frame(cbind(row, est, se, ui, li))
  outp
}



# ggplot g-comp engine -----------------------------------------------------------


ggplot_stglm <- function(out, ylim, main, xlab, ylab, min, p, sub) {
  require(ggplot2)
  g1 <- out[match(p, x), ]
  g1
  ggplot2::ggplot(out, aes(x = row, y = est)) +
    geom_point() +
    geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
    scale_y_continuous(limits = ylim) +
    labs(
      title = main,
      subtitle = sub,
      x = xlab,
      y = ylab
    ) +
    geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
    theme_classic()
}

# plots for no mi impute
# plots

ggplot_stglm_nomi <-
  function(out, ylim, main, xlab, ylab, min, p, sub) {
    require(ggplot2)
    out_p <- as.data.frame(print(summary(out)))
    rows <- 1:nrow(out_p)
    rows
    out_p$rows <- sapply(rows, function(x)
      x - 1)
    out_p$rows <- 1:nrow(out_p)
    # out <- out |> dplyr::rename(est = "Estimate",
    #                             li = "lower.0.95",
    #                             ui = "upper.0.95",
    #                             se = "Std..Error")
    out_p <- out_p |> dplyr::rename(est = "Estimate",
                                    li = "lower 0.95",
                                    ui = "upper 0.95",
                                    se = "Std. Error")
    g1 <- out_p[match(p, x), ]
    g1
    ggplot2::ggplot(out_p, aes(x = rows, y = est)) +
      geom_point() +
      geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
      scale_y_continuous(limits = ylim) +
      labs(
        title = main,
        subtitle = sub,
        x = xlab,
        y = ylab
      ) +
      geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
      theme_classic()
  }


# vanderweelevalues -------------------------------------------------------


# Create risk ratio table
vanderweelevalue_rr = function(out, f) {
  require("EValue")
  coef <- round(out, 3) %>%
    slice(f + 1) |>
    select(-row)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 4],
      hi = coef[1, 3],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}


vanderweelevalue_rr_lo = function(out, f) {
  require("EValue")
  coef <- round(out, 3) %>%
    slice(r + 1) |>
    select(-row)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 4],
      hi = coef[1, 3],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}



## create table with evalues

ggplot_stglm_nomi <-
  function(out_ct, ylim, main, xlab, ylab, min, p, sub) {
    require(ggplot2)
    out <-  out_ct
    out$row <- 1:nrow(out)
    out <- out |> dplyr::rename(est = "Estimate",
                                li = "lower.0.95",
                                ui = "upper.0.95",
                                se = "Std..Error")
    g1 <- out[match(p, x),]
    g1
    ggplot2::ggplot(out, aes(x = row, y = est)) +
      geom_point() +
      geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
      scale_y_continuous(limits = ylim) +
      labs(
        title = main,
        subtitle = sub,
        x = xlab,
        y = ylab
      ) +
      geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
      theme_classic()
  }

# plots
ggplot_stglm_nomi <-
  function(out_ct, ylim, main, xlab, ylab, min, p, sub) {
    require(ggplot2)
    out <-  out_ct
    out$row <- 1:nrow(out)
    out <- out |> dplyr::rename(est = "Estimate",
                                li = "lower.0.95",
                                ui = "upper.0.95",
                                se = "Std..Error")
    g1 <- out[match(p, x),]
    g1
    ggplot2::ggplot(out, aes(x = row, y = est)) +
      geom_point() +
      geom_pointrange(aes(ymin =  li, ymax = ui), colour = "darkgray")  +
      scale_y_continuous(limits = ylim) +
      labs(
        title = main,
        subtitle = sub,
        x = xlab,
        y = ylab
      ) +
      geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
      theme_classic()
  }


# vanderweelevalues -------------------------------------------------------


vanderweelevalue_ols = function(out, f, delta, sd) {
  require("EValue")
  coef <- round(out, 3) %>%
    slice(f + 1) |>
    select(-row)
  evalout <-
    as.data.frame(round(
      EValue::evalues.OLS(
        coef[1, 1],
        se = coef[1, 2],
        sd = 1,
        delta = delta,
        true = 0
      ),
      3
    ))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- main
  return(tab)
}



vanderweelevalue_ols_lo = function(out, f, delta, sd) {
  require("EValue")
  coef <- round(out, 3) %>%
    slice(r + 1) |>
    select(-row)
  evalout <-
    as.data.frame(round(
      EValue::evalues.OLS(
        coef[1, 1],
        se = coef[1, 2],
        sd = 1,
        delta = delta,
        true = 0
      ),
      3
    ))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- main
  return(tab)
}

## risk ratio vanderweelevalue
vanderweelevalue_rr_nomi = function(out, f) {
  require("EValue")
  coef <- round(out, 3) |>  slice(f + 1)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 3],
      hi = coef[1, 4],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}


vanderweelevalue_ols_nomi = function(out_ct, f, delta, sd) {
  coef <- round(out_ct, 3)  |>  slice(f + 1)
  evalout <-
    as.data.frame(round(
      EValue::evalues.OLS(
        coef[1, 1],
        se = coef[1, 2],
        sd = 1,
        delta = delta,
        true = 0
      ),
      3
    ))
  evalout2 <- subset(evalout[2,])
  evalout2
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
  evalout3
  colnames(evalout3) <- c("E-value", "threshold")
  evalout3
  tab <- round(cbind.data.frame(coef, evalout3), 3)
  rownames(tab) <- main
  return(tab)
}


# vanderweelevalue_rr_nomi = function(out_ct, f) {
#   require("EValue")
#   coef <- round(out_ct, 3)  |>  slice(f + 1)
#   evalout <-
#     as.data.frame(round(EValue::evalues.RR(
#       coef[1, 1] ,
#       lo =  coef[1, 4],
#       hi = coef[1, 3],
#       true = 1
#     ), 3))
#   evalout2 <- subset(evalout[2, ])
#   evalout3 <- evalout2 |>
#     select_if( ~ !any(is.na(.)))
#   colnames(evalout3) <- c("E-value", "threshold")
#   tab <- cbind.data.frame(coef, evalout3)
#   rownames(tab) <- c(main)
#   return(tab)
# }


vanderweelevalue_rr_nomi = function(out_ct, f) {
  require("EValue")
  coef <- round(out_ct, 3) |>  slice(f + 1)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 3],
      hi = coef[1, 4],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}



vanderweelevalue_rr_nomi_lo = function(out, r) {
  require("EValue")
  coef <- round(out, 3) |>  slice(r + 1)
  evalout <-
    as.data.frame(round(EValue::evalues.RR(
      coef[1, 1] ,
      lo =  coef[1, 3],
      hi = coef[1, 4],
      true = 1
    ), 3))
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}



# multi-level model -------------------------------------------------------
#
# lmer_gaussian = function(data_raw, xlmer, ylmer, cvars_sans) {
#   require("splines")
#   require("lme4")
#   out <- lme4::lmer(as.formula(paste(
#     paste(ylmer, "~ bs(", xlmer , ")+"),
#     paste(cvars_sans,
#           collapse = "+")
#   ))))
# out
# }
# lmer_generalised = function(data_raw, xlmer, ylmer, cvars_sans, family) {
#   require("splines")
#   require("lme4")
#   out <- lme4::glmer(as.formula(paste(
#     paste(ylmer, "~ bs(", xlmer , ")+"),
#     paste(cvars_sans,
#           collapse = "+"),
#     paste(cvars_sans,
#           collapse = "+ (1|Id)"),
#     family = family
#   ))))
# out
# }
# ggplot_lmer <- function(model, xlmer, minmax, title) {
#   # note the var "x" is the range
#   out <- ggeffects::ggpredict(
#     model = model,
#     terms = paste(ylmer, xlmer, minmax)
#     ci.lvl = 0.95,
#     type = "fe",
#     typical = "mean",
#     back.transform = TRUE,
#     ppd = FALSE,
#     plot(out, facets = F)  +  theme_classic() + ggtitle(paste(title))
# }
# ggplot_lmer <- function(model, timevar, xlmer, minmax, title) {
#   # note the var "x" is the range
#   out <- ggeffects::ggpredict(
#     model = model,
#     terms = paste(ylmer, xlmer, minmax)
#     ci.lvl = 0.95,
#     type = "fe",
#     typical = "mean",
#     back.transform = FALSE,
#     ppd = FALSE,
#     plot(out, facets = F)  +  theme_classic() + ggtitle(paste(title))
# }
# unused plots ------------------------------------------------------------
# base R plot for mice pool_stglm outputs  (above)
#
# # not used
# plot_stglm <- function(out, ylim, main, xlab, ylab) {
#   plot(
#     out$row,
#     out$est,
#     type = "l",
#     ylim = ylim,
#     main = main,
#     xlab = xlab,
#     ylab = ylab,
#     col.main = "black",
#     sub = "Marginal predictions by g-computation",
#     col.sub = "black",
#     col.lab = "black",
#     cex.lab = 0.75
#   )
#   polygon(c(x, rev(x)),
#           c(out$li, rev(out$ui)),
#           col = "grey75",
#           border = FALSE)
#   lines(out$row, out$est, lwd = 1)
#   lines(out$row, out$li, col = "red", lty = 2)
#   lines(out$row, out$ui, col = "red", lty = 2)
# }
#
#
# ## Contrast plot not used
# plot_stglm_contrast <- function(out, ylim, main, xlab, ylab) {
#   plot(
#     out$row,
#     out$est,
#     type = "l",
#     ylim = ylim,
#     main = main,
#     xlab = xlab,
#     ylab = ylab,
#     col.main = "black",
#     sub = "Marginal contrasts relative to baseline by g-computation",
#     col.sub = "black",
#     col.lab = "black",
#     cex.lab = 0.75
#   )
#   polygon(c(x, rev(x)),
#           c(out$li, rev(out$ui)),
#           col = "grey75",
#           border = FALSE)
#   lines(out$row, out$est, lwd = 1)
#   lines(out$row, out$li, col = "red", lty = 2)
#   lines(out$row, out$ui, col = "red", lty = 2)
# }
# function for ggplot g-comp
# ggplot_stglm_contrast <- function(out, ylim, main, xlab, ylab, s) {
#   require(ggplot2)
#   g1 <- out[match(s, x),]
#   g1
#   ggplot2::ggplot(out, aes(x = row, y = est)) +
#     geom_point(colour = "black") +
#     geom_pointrange(aes(ymin =  li, ymax = ui))  +
#     scale_y_continuous(limits = ylim) +
#     labs(
#       title = main,
#       subtitle = "Marginal contrasts relative to baseline by g-computation",
#       x = xlab,
#       y = ylab
#     ) + geom_pointrange(data=g1, aes(ymin = li, ymax = ui), colour="red") +  # highlight contrast
#     # this adds a red point geom_text(data=g1, label="Contrast", vjust=1) + # this adds a label for the red point
#     theme_classic()
# }
## FUNCTION FOR OUTCOME WIDE REGRESSION
# Y is the outcome
# df is the dataframe -- a mice object
# X is the exposure,
# we assume a spline model
