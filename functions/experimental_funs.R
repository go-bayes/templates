# Experimental functions

# simulation of data


if (!require(kableExtra)) {
  install.packages("kableExtra")
  library(kableExtra)
}
# Install the package if not already installed
if (!require(WeightIt)) {
  install.packages("WeightIt")
  library(WeightIt)
}

if (!require(ggokabeito)) {
  install.packages("ggokabeito")
  library(ggokabeito)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
}
# Load the package

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library("ggplot2")
}

if (!require(EValue)) {
  install.packages("EValue")
  library("EValue")
}


if (!require(patchwork)) {
  install.packages("patchwork")
  library("patchwork")
}

if (!require(janitor)) {
  install.packages("janitor")
  library("janitor")
}


if (!require(lubridate)) {
  install.packages("lubridate")
  library("lubridate")
}



# A_effect: denotes the effect of L1 on A in the logistic regression model used to generate A. In the model, we use a logistic link function to bind A (a binary variable) as a function of L1. A_effect is the coefficient of L1 in this model, representing the log odds ratio of A for a one-unit increase in L1.
#
# Y_effect:  denotes the effect of A on Y in the model used to generate Y. In this model, we use a logistic link function to bind Y (a binary variable) as a function of A and L1. Y_effect is the coefficient of A in this model, representing the log odds ratio of Y for a one-unit increase in A.
#
# the term bias_L1 denotes the effect of L1 on Y when A is not present


# function to create msm table
library(tidyverse)
library(knitr)
library(kableExtra)


# transition objects takes the output of an library(msm) object
# e.g.
# out <- msm::statetable.msm(round(hours_exercise_coarsen_n, 0), id, data = dt_exposure_maori)


transition_table <- function(data, state_names = NULL) {
  # Ensure the data is a dataframe
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Check if state names are provided
  if (is.null(state_names)) {
    state_names <- paste0("State ", sort(unique(c(
      data$from, data$to
    ))))
  }

  # Convert the data frame to a wide format
  df <- data %>%
    pivot_wider(names_from = to, values_from = Freq) %>%
    mutate(from = factor(from, levels = sort(unique(from)))) %>%
    arrange(from) %>%
    mutate(from = state_names[from]) %>%
    setNames(c("From", state_names))

  # Create the markdown table using knitr's kable function
  markdown_table <- df %>%
    kbl(format = "markdown", align = 'c')

  # Create the explanation
  explanation <- paste(
    "This transition matrix describes the shifts from one state to another between the baseline wave and the following wave.",
    "The numbers in the cells represent the number of individuals who transitioned from one state (rows) to another (columns).",
    "For example, the cell in the first row and second column shows the number of individuals who transitioned from the first state (indicated by the left-most cell in the row) to the second state.",
    "The top left cell shows the number of individuals who remained in the first state."
  )

  list(explanation = explanation, table = markdown_table)
}





# margot_plot_experimental ------------------------------------------------


margot_plot_exp <- function(.data,
                        type = c("RD", "RR"),
                        title,
                        subtitle,
                        xlab,
                        ylab,
                        reorder_outcome = TRUE,  # new argument
                        estimate_scale = 1,
                        base_size = 11,
                        text_size = 2.75,
                        point_size = .5,
                        title_size = 10,
                        subtitle_size = 9,
                        legend_text_size = 6,
                        legend_title_size = 6,
                        x_offset = ifelse(type == "RR", 0, -1.75),
                        x_lim_lo = ifelse(type == "RR", .1, -1.75),
                        x_lim_hi = ifelse(type == "RR", 2.5, 1)) {


  type <- match.arg(type)
  xintercept <- if (type == "RR") 1 else 0
  x_axis_label <- if (type == "RR") "Causal Risk Ratio Scale" else "Causal Difference Scale"

  # Define Reliability based on type
  if(type == "RR") {
    .data$Reliability <- ifelse(.data$`2.5 %` > 1 & .data$`97.5 %` > 1, "positive",
                                ifelse(.data$`2.5 %` < 1 & .data$`97.5 %` < 1, "negative", "zero_crossing"))
  } else {
    .data$Reliability <- ifelse(.data$`2.5 %` > 0 & .data$`97.5 %` > 0, "positive",
                                ifelse(.data$`2.5 %` < 0 & .data$`97.5 %` < 0, "negative", "zero_crossing"))
  }


  # conditionally apply reorder
  y_aes <- if (reorder_outcome) {
    reorder(outcome, .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]])
  }

  out <- ggplot(
    data = .data,
    aes(
      y = outcome,
      x = .data[[paste0("E[Y(1)]", ifelse(type == "RR", "/", "-"), "E[Y(0)]")]],
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      group = Estimate,
      color = Reliability
    )
  ) +
    geom_errorbarh(aes(color = Reliability), height = .3, position = position_dodge(width = 0.3)) +
    geom_point(size = point_size, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = xintercept, linetype = "solid") +
    theme_classic(base_size = base_size) +
    scale_color_manual(values = c("positive" = "dodgerblue", "zero_crossing" = "black", "negative" = "orange")) +
    labs(x = x_axis_label, y = " ", title = title, subtitle = subtitle) +
    geom_text(aes(x = x_offset * estimate_scale, label = estimate_lab), size = text_size, hjust = 0,
              fontface = ifelse(.data$Estimate == "unreliable", "plain", "bold")) +
    coord_cartesian(xlim = c(x_lim_lo, x_lim_hi)) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      plot.title = element_text(face = "bold", size = title_size, hjust = 0),
      plot.subtitle = element_text(face = "bold", size = subtitle_size, hjust = 0),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )


  return(out)
}





# interpretation of results -----------------------------------------------

# takes an output from the sub_group_ate

# create individual summaries
# sum_e_d <- summary(sim_estimand_all_e_d)
# sum_m_d <- summary(sim_estimand_all_m_d)
#
#
# # create individual tables
# tab_ed <- sub_tab_ate(sum_e_d, new_name = "NZ Euro Depression")
# tab_md <- sub_tab_ate(sum_m_d, new_name = "Māori Depression")
#
#
# # expand tables
# plot_ed <- sub_group_tab(tab_ed, type= "RD")
# plot_md <- sub_group_tab(tab_md, type= "RD")
#
# big_tab_d <- rbind(plot_ed,plot_md)
#
#
# # table for anxiety outcome --format as "markdown" if you are using quarto documents
# big_tab_d |>
#   kbl(format="markdown")
interpret_results_subgroup <- function(df, outcome, exposure) {
  df <- df %>%
    mutate(
      report = case_when(
        E_Val_bound > 1.2 & E_Val_bound < 2 ~ paste0(
          "For the ", group, ", our results suggest that ", exposure, " may potentially influence ", outcome, ", with an estimated causal contrast value (E[Y(1)]-E[Y(0)]) of ", `E[Y(1)]-E[Y(0)]`, ".\n",
          "The associated confidence interval, ranging from ", `2.5 %`, " to ", `97.5 %`, ", does not cross zero, providing more certainty in our estimate. ",
          "The E-values indicate that any unmeasured confounder would need to have a minimum risk ratio of ", E_Value, " with both the treatment and outcome to explain away the observed effect, and a minimum risk ratio of ", E_Val_bound, " to shift the confidence interval to include the null effect. This suggests stronger confidence in our findings."
        ),
        E_Val_bound >= 2 ~ paste0(
          "For the ", group, ", our results suggest that ", exposure, " may potentially influence ", outcome, ", with an estimated causal contrast value (E[Y(1)]-E[Y(0)]) of ", `E[Y(1)]-E[Y(0)]`, ".\n",
          "The associated confidence interval, ranging from ", `2.5 %`, " to ", `97.5 %`, ", does not cross zero, providing more certainty in our estimate. ",
          "With an observed risk ratio of RR = ", E_Value, ", an unmeasured confounder that was associated with both the outcome and the exposure by a risk ratio of ", E_Val_bound, "-fold each, above and beyond the measured confounders, could explain away the estimate, but weaker joint confounder associations could not; to move the confidence interval to include the null, an unmeasured confounder that was associated with the outcome and the exposure by a risk ratio of ", E_Val_bound, "-fold each could do so, but weaker joint confounder associations could not. Here we find stronger evidence that the result is robust to unmeasured confounding."
        ),
        E_Val_bound < 1.2 & E_Val_bound > 1 ~ paste0(
          "For the ", group, ", our results suggest that ", exposure, " may potentially influence ", outcome, ", with an estimated causal contrast value (E[Y(1)]-E[Y(0)]) of ", `E[Y(1)]-E[Y(0)]`, ".\n",
          "The associated confidence interval, ranging from ", `2.5 %`, " to ", `97.5 %`, ", does not cross zero, providing more certainty in our estimate. ",
          "The E-values indicate that any unmeasured confounder would need to have a minimum risk ratio of ", E_Value, " with both the treatment and outcome to explain away the observed effect, and a minimum risk ratio of ", E_Val_bound, " to shift the confidence interval to include the null effect. This suggests we should interpret these findings with caution given uncertainty in the model."
        ),
        E_Val_bound == 1 ~ paste0(
          "For the ", group, ", the data suggests a potential effect of ", exposure, " on ", outcome, ", with a causal contrast value of ", `E[Y(1)]-E[Y(0)]`, ".\n",
          "However, the confidence interval for this estimate, ranging from ", `2.5 %`," to ", `97.5 %`, ", crosses zero, indicating considerable uncertainties. The E-values indicate that an unmeasured confounder that is associated with both the ", outcome, " and the ", exposure, " by a risk ratio of ", E_Value, " could explain away the observed associations, even after accounting for the measured confounders. ",
          "This finding further reduces confidence in a true causal effect. Hence, while the estimates suggest a potential effect of ", exposure, " on ", outcome, " for the ", group, ", the substantial uncertainty and possible influence of unmeasured confounders mean these findings should be interpreted with caution."
        )
      )
    )
  return(df$report)
}





# Test the function
# data <- data.frame(
#   from = rep(1:4, each = 4),
#   to = rep(1:4, 4),
#   Freq = c(415, 187, 293, 66, 187, 213, 424, 67, 403, 471, 2837, 753, 105, 103, 939, 1178)
# )
# state_names <- c("Inactive", "Somewhat Active", "Active", "Extremely Active")
# result <- transition_table(out, state_names)
# result

# Function to calculate ATE in SD units
# generate_data <- function(N, prob_L1, A_on_Y, L_on_A, L_on_Y) {
#   L1 <- rbinom(N, 1, prob_L1)
#   prob_A <- plogis(L_on_A * L1)
#   A <- rbinom(N, 1, prob_A)
#   Y <-
#     rnorm(N, A_on_Y * A + L_on_Y * L1, sd = (A_on_Y * A + L_on_Y * L1) / 2)
#
#   # Convert Y to z-scores
#   Y <- scale(Y)
#
#   data <- data.frame(L1 = L1, A = A, Y = Y)
#
#   return(data)
# }
# # Function to estimate ATE using IPTW
# estimate_ATE_iptw <- function(data, method = "ps") {
#   # Obtain propensity score weights using WeightIt package
#   weight_out <-
#     weightit(A ~ L1,
#              data = data,
#              method = method,
#              estimand = "ATE")
#   data$weights <- weight_out$weights
#
#   # Fit a weighted regression with only an intercept
#   weighted_model <- lm(Y ~ A, data = data, weights = data$weights)
#
#   # Predict outcomes for everyone setting their treatment value to A = 0 and A = 1
#   data$A <- 0
#   Y_0 <- predict(weighted_model, newdata = data)
#   data$A <- 1
#   Y_1 <- predict(weighted_model, newdata = data)
#
#   # Obtain the difference of these predicted values
#   ATE_iptw <- mean(Y_1 - Y_0)
#
#   return(ATE_iptw)
# }


# Function to estimate ATE using G-computation
# Function to estimate ATE using g-computation
estimate_ATE_gcomp <- function(data) {
  # Fit a regression model
  model <- lm(Y ~ A * L1, data = data)

  # Predict outcomes for everyone setting their treatment value to A = 0 and A = 1
  data$A <- 0
  Y_0 <- predict(model, newdata = data)
  data$A <- 1
  Y_1 <- predict(model, newdata = data)

  # Obtain the difference of these predicted values
  ATE_gcomp <- mean(Y_1 - Y_0)

  return(ATE_gcomp)
}



# Function to estimate ATE using doubly robust estimation
estimate_ATE_dr <- function(data, method = "ps") {
  # Obtain propensity score weights using WeightIt package
  weight_out <-
    weightit(A ~ L1,
             data = data,
             method = method,
             estimand = "ATE")
  data$weights <- weight_out$weights

  # Fit a weighted regression
  weighted_model <-
    lm(Y ~ A * L1, data = data, weights = data$weights)

  # Predict outcomes for everyone setting their treatment value to A = 0 and A = 1
  data$A <- 0
  Y_0 <- predict(weighted_model, newdata = data)
  data$A <- 1
  Y_1 <- predict(weighted_model, newdata = data)

  # Obtain the difference of these predicted values
  ATE_dr <- mean(Y_1 - Y_0)

  return(ATE_dr)
}



# Function to run simulations
# Function to run simulations
run_simulations <- function(num_simulations,
                            N,
                            prob_L1,
                            A_on_Y,
                            L_on_A,
                            L_on_Y,
                            method = "ps") {
  # Create a placeholder for the results
  simulations <- matrix(ncol = 4, nrow = num_simulations)

  for (i in seq_len(num_simulations)) {
    # Generate data
    data <- generate_data(N, prob_L1, A_on_Y, L_on_A, L_on_Y)

    # Estimate ATE
    ATE_unadjusted <- with(data, mean(Y[A == 1]) - mean(Y[A == 0]))
    ATE_iptw <- estimate_ATE_iptw(data, method)
    ATE_gcomp <- estimate_ATE_gcomp(data)
    ATE_dr <- estimate_ATE_dr(data, method)

    # Store results
    simulations[i,] <-
      c(ATE_unadjusted, ATE_iptw, ATE_gcomp, ATE_dr)
  }

  colnames(simulations) <-
    c("ATE_unadjusted", "ATE_iptw", "ATE_gcomp", "ATE_dr")

  # Calculate means and confidence intervals
  mean_ATE <- colMeans(simulations, na.rm = TRUE)
  CI_lower <-
    apply(simulations, 2, function(x)
      quantile(x, 0.025, na.rm = TRUE))
  CI_upper <-
    apply(simulations, 2, function(x)
      quantile(x, 0.975, na.rm = TRUE))

  # Return results as a data frame
  results <- tibble(
    estimator = colnames(simulations),
    mean_ATE = mean_ATE,
    CI_lower = CI_lower,
    CI_upper = CI_upper
  )

  return(results)
}






# Run function
# results <- run_simulations(
#   num_simulations = 1000,
#   N = 1000,
#   prob_L1 = .5, # maximize
#   A_on_Y = 0,
#   L_on_A = .6,
#   L_on_Y = 1
# )

# results |>
#   kbl(format = "markdown")

# note: maximum bias occurs when prob_L1 = 0.5 because this maximizes the variability (and thus the influence) of the confounder L1.
#
# When prob_L1 = 0.5, the binary variable L1 is equally likely to be 0 or 1, and thus there is maximum variability or "mixing" of the confounder across the treatment groups. This allows for the maximum potential for confounding, as L1 has the most influence on both the treatment A and the outcome Y.

# If prob_L1 is close to 0 or 1, then almost all subjects will have the same value for L1 (either 0 or 1), and there will be less potential for L1 to confound the relationship between A and Y. This is because, with a lack of variation in L1, its ability to differently affect the treatment groups (and therefore introduce bias) is reduced.

# Function to create a coefficient plot
create_coefficient_plot <- function(results) {
  # Create the plot
  p <-
    ggplot(results,
           aes(
             x = estimator,
             y = mean_ATE,
             ymin = CI_lower,
             ymax = CI_upper
           )) +
    geom_pointrange(aes(col = estimator), fatten = 2) +
    labs(x = "Estimator", y = "ATE") +
    theme_bw() +
    ggokabeito::scale_color_okabe_ito() +
    theme(legend.position = "none")

  return(p)
}



# Use the function
#create_coefficient_plot(results) + scale_y_continuous(limits = c(-.5,.5))



# New function to construct the formula string
construct_formula <-
  function(Y,
           X = 1,
           baseline_vars,
           continuous_X,
           splines,
           subclass = NULL) {
    if (X == 1) {
      return(paste(Y, "~ 1"))
    }

    # Interaction terms
    interaction_terms <- if (!is.null(subclass)) {
      paste0(subclass,
             "*",
             "(",
             X ,
             "*",
             "(",
             paste(baseline_vars, collapse = "+"),
             ")",
             ")")
    } else {
      paste0(X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
    }

    if (continuous_X && splines) {
      require(splines)
      formula_str <-
        paste(Y,
              "~ bs(",
              X ,
              ")",
              "*",
              "(",
              paste(baseline_vars, collapse = "+"),
              ")")
    } else {
      formula_str <- paste(Y, "~", interaction_terms)
    }

    return(formula_str)
  }


# New function to compute the desired summary statistics
compute_summary_stats <- function(sim_estimand_df, scale) {
  out <- t(sapply(sim_estimand_df, function(x) {
    c(
      Estimate = round(mean(x), 4),
      `2.5 %` = round(quantile(x, 0.025), 4),
      `97.5 %` = round(quantile(x, 0.975), 4)
    )
  }))
  colnames(out) <- c("Estimate", "2.5 %", "97.5 %")
  return(out)
}

# Modified main function
causal_contrast_x <-
  function(df,
           Y,
           X,
           baseline_vars = "1",
           treat_0 = 0,
           treat_1 = 1,
           estimand = c("ATE", "ATT"),
           scale = c("RR", "RD"),
           nsims = 1000,
           cores = parallel::detectCores(),
           family = gaussian(),
           weights = TRUE,
           continuous_X = FALSE,
           splines = FALSE) {
    if (continuous_X) {
      estimand <- "ATE"
    }

    formula_str <-
      construct_formula(Y, X, baseline_vars, continuous_X, splines)

    if ("mids" %in% class(df)) {
      fits <-  lapply(complete(df, "all"), function(d) {
        fit_model(d, formula_str, family, weights)
      })
      sim.imp <- misim(fits, n = nsims, vcov = "HC")
    } else {
      fit <- fit_model(df, formula_str, family, weights)
      sim.imp <- sim(fit, n = nsims, vcov = "HC")
    }

    if (!continuous_X && estimand == "ATT") {
      subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)
      sim_estimand <-
        sim_ame(
          sim.imp,
          var = X,
          subset = eval(subset_expr),
          cl = cores,
          verbose = FALSE
        )
    } else {
      sim_estimand <-
        sim_ame(sim.imp,
                var = X,
                cl = cores,
                verbose = FALSE)
    }

    if (continuous_X) {
      return(summary(sim_estimand))
    } else {
      sim_estimand_df <- as.data.frame(sim_estimand)

      if (scale == "RR") {
        sim_estimand_df$RR <-
          sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] / sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
      } else {
        sim_estimand_df$RD <-
          sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] - sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
      }

      out <- compute_summary_stats(sim_estimand_df, scale)
      rownames(out) <- colnames(sim_estimand_df)
      return(out)
    }
  }





### Matching with subgroups

match_mi_sub <-
  match_mi_general_subgroup <-
  function(data,
           X,
           baseline_vars,
           estimand,
           method,
           subgroup,
           focal = NULL,
           super = FALSE,
           SL.library = c("SL.glm", "SL.stepAIC", "SL.glm.interaction")) {
    if (!requireNamespace("WeightIt", quietly = TRUE)) {
      stop(
        "Package 'WeightIt' is required but not installed. Please install it using 'install.packages(\"WeightIt\")'."
      )
    }

    if (!requireNamespace("MatchThem", quietly = TRUE)) {
      stop(
        "Package 'MatchThem' is required but not installed. Please install it using 'install.packages(\"MatchThem\")'."
      )
    }

    data_class <- class(data)

    if (!data_class %in% c("mids", "data.frame")) {
      stop("Input data must be either 'mids' or 'data.frame' object")
    }

    formula_str <-
      as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))

    weight_function <-
      if (data_class == "mids")
        weightthem
    else
      weightit

    if (super) {
      perform_matching <- function(data_subset) {
        weight_function(
          formula = formula_str,
          data = data_subset,
          estimand = estimand,
          stabilize = TRUE,
          method = method,
          focal = focal,
          use.kernel = TRUE,
          SL.library = SL.library
        )
      }
    } else {
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
    }

    levels_list <- unique(data[[subgroup]])

    dt_match_list <- lapply(levels_list, function(level) {
      data_subset <- data[data[[subgroup]] == level, ]
      perform_matching(data_subset)
    })

    names(dt_match_list) <- levels_list
    dt_match <- dt_match_list

    return(dt_match)
  }



# table for subgroup ATE --------------------------------------------------


sub_tab_ate<- function(x, new_name, delta = 1, sd = 1, type = c("RD","RR"), continuous_X = FALSE) {
  require("EValue")
  require(dplyr)

  type <- match.arg(type)

  dff <- as.data.frame(x)

  x <- dff[grepl("^RD_", rownames(dff)), ]


  if (continuous_X) {
    rownames(x) <- type
  }

  out <- x %>%
    dplyr::filter(stringr::str_detect(row.names(x), paste0("^", type))) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 4))

  out <- out %>%
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


sub_group_tab <- function(df, type = c("RR", "RD")) {
  type <- match.arg(type)

  require(dplyr)

  if (type == "RR") {
    out <- df %>%
      dplyr::mutate(Estimate  = as.factor(ifelse(
        `2.5 %` > 1 & `97.5 %` > 1,
        "positive",
        ifelse( `2.5 %` < 1 & `97.5 %` < 1, "negative", "unreliable")
      ))) |>
      rownames_to_column(var = "group") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(`E[Y(1)]/E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
      )
  } else {
    out <- df %>%
      dplyr::mutate(Estimate  = as.factor(ifelse(
        `2.5 %` > 0 & `97.5 %` > 0,
        "positive",
        ifelse( `2.5 %` < 0 & `97.5 %` < 0, "negative", "unreliable")
      ))) |>
      rownames_to_column(var = "group") %>%
      mutate(
        across(where(is.numeric), round, digits = 3),
        estimate_lab = paste0(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")", " [EV ", `E_Value`, "/",  `E_Val_bound`, "]")
      )
  }

  return(out)
}


sub_group_plot_ate <- function(.data, type = "RD", title, subtitle, xlab, ylab,
                               x_offset = 0,
                               x_lim_lo = 0,
                               x_lim_hi = 1.5) {
  type <- match.arg(type)

  xintercept <- if (type == "RR") 1 else 0
  x_axis_label <- if (type == "RR") "Causal Risk Ratio" else "Causal Risk Difference"

  out <- ggplot(
    data = .data,
    aes(
      y = group,
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
      fontface = ifelse(.data$Estimate == "unreliable", "plain", "bold")
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


# DELETE

tab_ate_subgroup_rd <- function(x,
                                new_name,
                                delta = 1,
                                sd = 1) {
  # Check if required packages are installed
  required_packages <- c("EValue", "dplyr")
  new_packages <-
    required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages))
    stop("Missing packages: ", paste(new_packages, collapse = ", "))

  require(EValue)
  require(dplyr)

  # Check if input data is a dataframe
  if (!is.data.frame(x))
    stop("Input x must be a dataframe")

  # Check if required columns are in the dataframe
  required_cols <- c("estimate", "lower_ci", "upper_ci")
  missing_cols <- required_cols[!(required_cols %in% colnames(x))]
  if (length(missing_cols) > 0)
    stop("Missing columns in dataframe: ",
         paste(missing_cols, collapse = ", "))

  # Check if lower_ci and upper_ci do not contain NA values
  if (any(is.na(x$lower_ci), is.na(x$upper_ci)))
    stop("Columns 'lower_ci' and 'upper_ci' should not contain NA values")

  x <- x %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 3)) %>%
    dplyr::rename("E[Y(1)]-E[Y(0)]" = estimate)

  x$standard_error <- abs(x$lower_ci - x$upper_ci) / 3.92

  evalues_list <- lapply(seq_len(nrow(x)), function(i) {
    row_evalue <- EValue::evalues.OLS(
      x[i, "E[Y(1)]-E[Y(0)]"],
      se = x[i, "standard_error"],
      sd = sd,
      delta = delta,
      true = 0
    )
    # If E_value is NA, set it to 1
    # if (is.na(row_evalue[2, "lower"])) {
    #   row_evalue[2, "lower"] <- 1
    # }
    # if (is.na(row_evalue[2, "upper"])) {
    #   row_evalue[2, "upper"] <- 1
    # }
    data.frame(round(as.data.frame(row_evalue)[2,], 3)) # exclude the NA column
  })

  evalues_df <- do.call(rbind, evalues_list)
  colnames(evalues_df) <- c("E_Value", "E_Val_bound")

  tab_p <- cbind(x, evalues_df)

  tab <-
    tab_p |> select(c(
      "E[Y(1)]-E[Y(0)]",
      "lower_ci",
      "upper_ci",
      "E_Value",
      "E_Val_bound"
    ))

  return(tab)
}



# plot the subgroups ------------------------------------------------------

plot_sub_forest <- function(df) {
  require(ggplot2)

  # Check if required packages are installed
  required_packages <- c("ggplot2")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages))
    stop("Missing packages: ", paste(new_packages, collapse = ", "))

  # Check if required columns are in the dataframe
  required_cols <- c("estimate", "lower_ci", "upper_ci")
  missing_cols <- required_cols[!(required_cols %in% colnames(df))]
  if (length(missing_cols) > 0)
    stop("Missing columns in dataframe: ", paste(missing_cols, collapse = ", "))

  # Order the factor levels by the estimate column in decreasing order

  ggplot(df, aes(x=estimate, y=factor(row.names(df)))) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height=0.3) +
    geom_vline(xintercept = 0, linetype="dashed", color = "red") +
    theme_bw() +
    xlab("Estimate") +
    ylab("")
}


# calculate dates after 30 June 2009 --------------------------------------

library(lubridate)

# Function to calculate TSCORE for a given date
calculate_tscore <- function(date) {
  base_date <- dmy("30.06.2009")
  return(as.integer(date - base_date))
}

# Function to generate string for date range and TSCORE range
generate_date_tscore_string <- function(start_date, end_date) {
  start_tscore <- calculate_tscore(start_date)
  end_tscore <- calculate_tscore(end_date)
  return(sprintf("(%s -- %s): (TSCORE >= %d & TSCORE <= %d)",
                 format(start_date, "%d.%m.%Y"),
                 format(end_date, "%d.%m.%Y"),
                 start_tscore,
                 end_tscore))
}

# TEST
# Define date pairs
# date_pairs <- list(c("31.12.2019", "27.02.2020"),
#                    c("28.02.2020", "25.03.2020"),
#                    c("26.03.2020", "27.04.2020"),
#                    c("28.04.2020", "13.05.2020"),
#                    c("14.05.2020", "08.06.2020"),
#                    c("09.06.2020", "11.08.2020"),
#                    c("12.08.2020", "30.08.2020"),
#                    c("31.08.2020", "21.09.2020"),
#                    c("22.09.2020", "07.10.2020"),
#                    c("08.10.2020", "30.09.2021"))
#
# # Generate strings for each date pair
# for (pair in date_pairs) {
#   start_date <- dmy(pair[1])
#   end_date <- dmy(pair[2])
#   print(generate_date_tscore_string(start_date, end_date))
# }

