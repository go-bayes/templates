# Experimental functions


# New function to construct the formula string
construct_formula <- function(Y, X, baseline_vars, continuous_X, splines) {
  if (continuous_X && splines) {
    require(splines)
    formula_str <- paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
  } else {
    formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
  }
  return(formula_str)
}

# New function to fit the model
fit_model <- function(df, formula_str, family, weights) {
  weight_var <- if (weights) df$weights else NULL
  fit <- glm(
    as.formula(formula_str),
    weights = if (!is.null(weight_var)) weight_var else NULL,
    family = family,
    data = df
  )
  return(fit)
}

# New function to compute the desired summary statistics
compute_summary_stats <- function(sim_estimand_df, scale) {
  out <- t(sapply(sim_estimand_df, function(x) {
    c(Estimate = round(mean(x), 4), `2.5 %` = round(quantile(x, 0.025), 4), `97.5 %` = round(quantile(x, 0.975), 4))
  }))
  colnames(out) <- c("Estimate", "2.5 %", "97.5 %")
  return(out)
}

# Modified main function
causal_contrast_x <- function(df, Y, X, baseline_vars = "1", treat_0 = 0, treat_1 = 1, estimand = c("ATE", "ATT"), scale = c("RR","RD"), nsims = 1000, cores = parallel::detectCores(), family = gaussian(), weights = TRUE, continuous_X = FALSE, splines = FALSE) {

  if (continuous_X) {
    estimand <- "ATE"
  }

  formula_str <- construct_formula(Y, X, baseline_vars, continuous_X, splines)

  if ("mids" %in% class(df)) {
    fits <-  lapply(complete(df, "all"), function(d) {
      fit_model(d, formula_str, family, weights)
    })
    sim.imp <- misim(fits, n = nsims, vcov = "HC3")
  } else {
    fit <- fit_model(df, formula_str, family, weights)
    sim.imp <- sim(fit, n = nsims, vcov = "HC3")
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
    sim_estimand_df <- as.data.frame(sim_estimand)

    if (scale == "RR") {
      sim_estimand_df$RR <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] / sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
    } else {
      sim_estimand_df$RD <- sim_estimand_df[[paste0("E[Y(", treat_1, ")]")]] - sim_estimand_df[[paste0("E[Y(", treat_0, ")]")]]
    }

    out <- compute_summary_stats(sim_estimand_df, scale)
    rownames(out) <- colnames(sim_estimand_df)
    return(out)
  }
}



### Matching with subgroups

match_mi_sub <- match_mi_general_subgroup <- function(data, X, baseline_vars, estimand, method, subgroup, focal = NULL, super = FALSE, SL.library = c("SL.glm", "SL.stepAIC", "SL.glm.interaction")) {
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

