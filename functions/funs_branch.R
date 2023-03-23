

# to do
# write a function where `cvars` = vector of baseline_confounders, X at baseline (the exposure), and Y at baseline
# for each model, we will uniquely impute the dataset we use for the analysis. ;
# cvars -- X, Y, at baseline.



# also to note, we might want to do item by item outcomes.



# baseline_confounders -- take them only at baseline, or T0
# Y - take at baseline and T2
# X -- take at baseliine at T1

# impute everything
# cvars = baseline_confounders + Y_t0 + X_t0
# exposure = X_t1
# outcome = Y_t2

# After imputation but not before, add back the population weights as a weights variable.
# We then do the matching propensity scores are needed.
# We then use an approach similar to the outcome-wide attacks method (see "scripts" in that repository)

# for Population Average Treatement Effects (PATE), we add weights after imputation.  If we have propensity scores we obtain a single weights column by multiplying the propensity scores $\times$ the post-stratification weights, as described in Noah's twitter to Joe.

## default should be CVARS = Y_t0 + X_t0 and the weights (if any).

## for the regression model, we need flexibility such that the relationship of X to Y can be modelled as a non-linear function or as a linear function (for example if there are only two levels of X)


# for imputation, impute item by item for each outcome Y, and then combine after imputation as illustrated in the scripts in the outcome-wide. # mice package suggests smaller imputations models

# functions.R



# set paths
# probably want to do this explicitly
# ** to change
# push_mods <- here::here("mods")
# push_figs <- here::here("figs")

# read raw data

## function for saving

# ** to change not needed
# saveh <- function(df, name) {
#   x = df
#   arrow::write_parquet(x,  here::here(push_mods,  paste0(name, '')))
# }


# function for reading
# readh <- function(name) {
#   df = arrow::read_parquet(here::here(push_mods,  paste0(name, '')))
#   df
# }



# test space - to delete --------------------------------------------------


# libraries and functions
# read libraries
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/libs2.R")

# read functions
source("https://raw.githubusercontent.com/go-bayes/templates/main/functions/funs.R")

#source(here::here("scripts", "functions", "funs_here.R"))

# read data

pull_path <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/time13"
  )

# for saving models
push_mods <-
  fs::path_expand(
    "/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/outcomewide/outcomewide-attacks/mods"
  )


# read multiply imputed data

# mi data
ml <- readRDS(here::here(push_mods, "at-mice-ml"))

# long data
mf <- readRDS(here::here(push_mods, "at-mice-mf"))
length(unique(mf$Id))

# baselin vars ---------------------------------------------------------

cvars = mf |>
  dplyr::select(-c(.imp,
                   .id,
                   Id,
                   time,
                   weights)) |> # include?
  dplyr::select(!starts_with("Warm.")) |>
  colnames()

#cvars





# set data in case we use matching
d <- ml

# same as d because we did not use matching
df <- ml

# number of simulations for confidence intervals

nsims = 100 # simulated coeffs

cl = 8 # number of cores

# set family
family <- "gaussian"

# set exposure
X <- "time"

# for estimate causal contrast
subset_val = 1

cvars = cvars  # covariates for adjustment -- here to improve precision of estimates (prob not needed)


# muslims ----------------------------------------------------------------

Y <- "Warm.Muslims_z"

sim_est_warm_muslims <- geeglm_mi_ate(df, d, nsims, Y, X, cvars, cl)

# effect is lower
sim_est_warm_muslims

# save
saveRDS(sim_est_warm_muslims,
        here::here(push_mods, "sim_est_warm_muslims"))



# control variable table --------------------------------------------------

# basis for a table with control variables (draft, needs work)

for_table = function(cvars, dt){
  # cvars are the control variables at baseline
  output_string <- paste(cvars, collapse = "+")
  # Remove double quotes from the beginning and the end of the string
  output_string_no_quotes <-  gsub('^"|"$', '', output_string)
  # Make formula
  output_string_formula <- as.formula(paste("~", output_string_no_quotes))
  # Make table (tb improved for use with latex & etc)
  table_out <- table1::table1(output_string_formula, data = dt)
}





# general method for estimating causal effects using GEE ----------------------------

# df is a mids object
# d is a matched them object
# n sims is number of simulations for causal contrasts
# cvars are confounders measured at baseline
# cl is number of clusters
# family is the statistical distribution
# weights is the survey and/or propensity score weights

geeglm_mi_ate = function(df,
                         d,
                         nsims,
                         Y,
                         X,
                         cvars,
                         cl,
                         family = family,
                         weights = weights) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures

  fits <- lapply(complete(ml, "all"), function(d) {
    geepack::geeglm(
      # as.formula(paste(
      #   paste(Y, "~", X, "+"),
      #   paste(cvars, collapse = "+")
      # )),
      # interaction -
      as.formula(paste(
        paste(Y, "~", X , "*", "("),
        paste(cvars, collapse = "+"),
        paste(")")
      )),
      weights = weights,
      id = Id,
      corstr = "ar1",
      # repeated measures
      family = family,
      data = d
    )
  })
  # sim coefficients
  sim.imp <- misim(fits, n = nsims)

  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = time  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )

  # risk differ
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)

  # output
  out <-  summary(sim_est)

  out

}



# function for obtaining causal contrasts on the risk difference scale

geeglm_ate = function(df, nsims, Y, X, cvars, cl) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures

  fit <-  geepack::geeglm(
    as.formula(paste(
      paste(Y, "~", X , "*", "("),
      paste(cvars, collapse = "+"),
      paste(")")
    )),
    weights = weights,
    id = Id,
    corstr = "ar1",
    # repeated measures
    family = family,
    data = df
  )
  # sim coefficients
  sim.imp <- sim(fit, n = nsims)

  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = time  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )

  # risk difference
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)

  # output
  out <-  summary(sim_est)

  out

}


# general method for estimating causal effects using glm ------------------


glm_mi_ate = function(df,
                         d,
                         nsims,
                         Y,
                         X,
                         cvars,
                         cl,
                         family = family,
                         weights = weights) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures

  fits <- lapply(complete(ml, "all"), function(d) {
    glm(
      as.formula(paste(
        paste(Y, "~", X , "*", "("),
        paste(cvars, collapse = "+"),
        paste(")")
      )),
      weights = weights,
      family = family,
      # if using propensity scores
      data = d
    )
  })
  # sim coefficients
  sim.imp <- misim(fits, n = nsims)

  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = !!sym(X)  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )

  # risk differ
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)

  # output
  out <-  summary(sim_est)

  out

}





# forest plots studies ------------------------------------


#  Takes a list of ggplot graph objects and creates a combined object
# e.g. dt_new <- bind_forestplot(list(alcoholfreq_p,alcoholintensity_p, bmi_p))


# Made this better in the outcomewide attacks script -- under "scripts" --> "functions"

### To be superceded
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
      xintercept = c(-.5, -.25, .25, .5),
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



# closer to how we will role

mice_generalised = function(df, X, Y, cvars, family) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  )), family = family))
  out
}


# for a non linear function

mice_generalised_lin = function(df, X, Y, cvars, family) {
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~", X, "+"),
    paste(cvars, collapse = "+")
  )), family = family))
  out
}



mice_gaussian_pre = function(df, X, Y, cvars, pre_y) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(pre_y, "+"),
    paste(cvars, collapse = "+")
  ))))
  out
}



mice_generalised_pre = function(df, X, Y, cvars, pre_x, family) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(pre_x, "+"),
    paste(cvars, collapse = "+")
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


mice_iptw_lin = function(X, Y, df, cvars, family = "gaussian") {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("mice")
  out_m <- with(df, glm(as.formula(paste(
    paste(Y, "~", X, "+"),
    paste(cvars, collapse = "+")
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
glm_nomi = function(X, Y, df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  out_m <- glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  )), family = family, data = df)
  return(out_m)
}


glm_nomi_lin = function(X, Y, df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  out_m <- glm(
    as.formula(paste(
      paste(Y, "~ (", X , ")+"),
      paste(cvars, collapse = "+")
    )),
    family = family,
    weights = weights,
    data = df
  )
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
    geom_pointrange(data = g1,
                    aes(ymin = li, ymax = ui),
                    colour = "red") +  # highlight contrast
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
    g1 <- out_p[match(p, x),]
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
      geom_pointrange(data = g1,
                      aes(ymin = li, ymax = ui),
                      colour = "red") +  # highlight contrast
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
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
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
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
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
      geom_pointrange(data = g1,
                      aes(ymin = li, ymax = ui),
                      colour = "red") +  # highlight contrast
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
      geom_pointrange(data = g1,
                      aes(ymin = li, ymax = ui),
                      colour = "red") +  # highlight contrast
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
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
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
  evalout2 <- subset(evalout[2,])
  evalout3 <- evalout2 |>
    select_if(~ !any(is.na(.)))
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
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
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
  evalout2 <- subset(evalout[2, ])
  evalout2
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  evalout3
  colnames(evalout3) <- c("E-value", "threshold")
  evalout3
  tab <- round(cbind.data.frame(coef, evalout3), 3)
  rownames(tab) <- main
  return(tab)
}
#
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
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
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
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("E-value", "threshold")
  tab <- cbind.data.frame(coef, evalout3)
  rownames(tab) <- c(main)
  return(tab)
}
#

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
