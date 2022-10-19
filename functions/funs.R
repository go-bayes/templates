# functions.R
library("here")
library("fs")
library("stdReg")
library("ggplot2")
library("mice")
library("conflicted")
conflict_prefer("pool", "mice")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("cbind", "base")


# set paths
push_mods <- here::here("mods")
push_figs <- here::here("figs")


# read raw data
read_raw <- function() {
  readRDS(here::here("data", "data_raw"))
}

# read  imputed data
read_imputed <- function() {
  readRDS(here::here("data", "data_imputed"))

}

# read imputated dat long

read_long <- function() {
  readRDS(here::here("data", "data_long"))
}

read_ml <- function() {
  readRDS(here::here("data", "data_ml"))
}

# or avoids to avoid pushing/ pulling large objects on github use something like the following
 # push_mods <- fs::path_expand("~/The\ Virtues\ Project\ Dropbox/outcomewide/mods")
 # push_figs <- fs::path_expand("~/Users/joseph/The\ Virtues\ Project\ Dropbox/outcomewide/figs")


## function for saving
saveh <- function(df, name) {
  x = df
  saveRDS(x,  here::here(push_mods,  paste0(name, '')))
}


# function for reading
readh <- function(name) {
  df = readRDS(here::here(push_mods,  paste0(name, '')))
  df
}





# forest plots studies ------------------------------------


#  Takes a list of ggplot graph objects and creates a combined object
# e.g. dt_new <- bind_forestplot(list(alcoholfreq_p,alcoholintensity_p, bmi_p))

bind_forestplot = function(x){
  ls_output  <- lapply(x, function(zed) {
    dt_zed <- as.data.frame(zed$data)
    dt_zed$id <- rep(zed$labels$y, nrow(dt_zed))
    dt_zed
  })
  df_output <- do.call( rbind, ls_output)
  df_output
}

## creat forest plot, using a "bind_forestplot" object

gcomp_forestplot= function(out, title, ylim, xlab) { # provisional
  require(ggplot2)
  require(viridisLite)
  ggplot(data=out, aes(y=id, x=est, xmin=ui, xmax=li, colour = factor(row))) +
    geom_point( position = position_dodge(width = 0.1)) +
    geom_errorbarh(height=.1, position = position_dodge(width = 0.1)) +
    geom_vline(xintercept = 0, linetype="solid") +
    geom_vline(xintercept = c(-.5, -.25,.25, .5), linetype="twodash", alpha = .5) + # experimental
    scale_x_continuous(limits = ylim) +
    # theme_forest() +
    theme_classic(base_size = 10) +
    theme(panel.border=element_blank(),
          axis.line=element_blank(),
          panel.background=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    #  scale_color_discrete(name = "Change in exposure from baseline (SD)", direction = -1) +
    scale_color_viridis_d(name = "Change in exposure\nfrom baseline (SD)", direction = -1, option = "D") +
    labs(x = "Change in outcome (SD)",
         y = "Many Outcomes",
         title = title)
}



gcomp_forestplot_rr = function(out, title, ylim, xlab) { # provisional
  require(ggplot2)
  require(viridisLite)
  ggplot(data=out, aes(y=id, x=est, xmin=ui, xmax=li, colour = factor(row))) +
    geom_point( position = position_dodge(width = 0.1)) +
    geom_errorbarh(height=.1, position = position_dodge(width = 0.1)) +
    geom_vline(xintercept = 1, linetype="twodash") +
    scale_x_continuous(limits = ylim) +
    # theme_forest() +
    theme_classic(base_size = 10) +
    theme(panel.border=element_blank(),
          axis.line=element_blank(),
          panel.background=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_color_viridis_d(name = "Change in exposure\nfrom baseline (SD)", direction = -1, option = "D") +
    labs(x = "RR of outcome (SD)",
         y = "Many Outcomes",
         title = title)
}


# mice models -----------------------------------------------------------


mice_gaussian = function(df, X, Y, cvars) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")  ))))
  out
}




mice_generalised = function(df, X, Y, cvars, family) {
  require("splines")
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars,collapse = "+")
  )), family = family))
  out
}

mice_generalised_lin = function(df, X, Y, cvars, family) {
  require("mice")
  out <- with(df, glm(as.formula(paste(
    paste(Y,"~", X,"+"),
    paste(cvars,collapse = "+")
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



mice_iptw = function(X,Y,df, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  require("mice")
  out_m <- with(df, glm(
    as.formula(paste(Y, "~ bs(", X , ")")),
    weights = weights,
    family = family
  ))
  return(out_m)
}


mice_iptw_lin = function(X,Y,df, family = "gaussian") {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("mice")
  out_m <- with(df, glm(
    as.formula(paste(Y, "~ (", X , ")")),    weights = weights,
    family = family
  ))
  return(out_m)
}

# for regression without mi
glm_nomi = function(X,Y,df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  require("splines")
  out_m <- glm(as.formula(paste(
    paste(Y, "~ bs(", X , ")+"),
    paste(cvars, collapse = "+")
  )), family = family, data = df)
  return(out_m)
}


glm_nomi_lin = function(X,Y,df, cvars, family = family) {
  # requires that a MATCH THEM dataset is converted to a mice object
  # weights must be called "weights)
  out_m <- glm(
    as.formula(paste(
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
    geom_pointrange(data = g1, aes(ymin = li, ymax = ui), colour = "red") +  # highlight contrast
    theme_classic()
}

# plots for no mi impute
# plots

ggplot_stglm_nomi <- function(out, ylim, main, xlab, ylab, min, p, sub) {
  require(ggplot2)
  out_p <- as.data.frame(print(summary(out)))
  rows <- 1:nrow(out_p)
  rows
  out_p$rows<- sapply(rows, function(x)x-1)
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

