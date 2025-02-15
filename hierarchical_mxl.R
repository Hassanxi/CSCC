####################################################################################################
## Hierarchical Mixed Logit (MXL) Model
####################################################################################################

# 1. Load Required Packages
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "doParallel", "foreach", "parallel", "sf", 
  "stringr", "data.table", "pbapply", "caret", "mlogit", "Rcpp", "RcppArmadillo",
  "bayesm", "pracma", "stats"
)
invisible(lapply(packages, load_or_install))

# 2. Load the Estimation Data
data_path <- "data/Estimation_Data_tab.Rdata"  # update with your relative path
load(data_path)

# 3. Define Attribute Labels
attributelabels_tablet <- c(
  'Brand 1', 'Brand 2', 'Brand 3', 'Brand 4', 'Brand 5', 'Brand 6', 'Brand 7',
  'System B', '8 Inches', '10 Inches', '12 Inches', '13 Inches',
  'High Resolution (264 ppi)', '16 GB', '32 GB', '64 GB', '128GB',
  'Without SD-Slot', '1.6 GHz', '2.2 GHz', '8-12 h. Runtime', 'WLAN + UMTS/3G',
  'WLAN + LTE/4G', 'S-phone Synch.', 'Value Pack', 'Cover', 'Keyboard', 'Mouse',
  'Pencil', '32 GB Memory Card', 'Keyboard + Pencil', 'Keyboard + Mouse + Pencil',
  'Price', '50 Cash Back', '100 Cash Back', '150 Cash Back'
)

# 4. Sampling Parameters
N <- length(E_Data_mod$lgtdata)
sample_fraction <- 0.7
set.seed(123)
n_sample <- round(N * sample_fraction)
sample_indices <- sample(1:N, n_sample)
E_Data_mod_sampled <- list(
  lgtdata = E_Data_mod$lgtdata[sample_indices],
  p = E_Data_mod$p,
  t = E_Data_mod$t
)
cat("Sampled Dataset Size:", length(E_Data_mod_sampled$lgtdata), "\n")
Road <- E_Data_mod_sampled
yforBayes <- unlist(lapply(Road$lgtdata, function(indiv) indiv$y))
XforBayes <- do.call(rbind, lapply(Road$lgtdata, function(indiv) indiv$X))
for (i in seq_along(Road$lgtdata)) {
  colnames(Road$lgtdata[[i]]$X) <- attributelabels_tablet
}

# 5. Aggregate Bayesian Estimation
if (!exists("out_Bayes")) {
  out_Bayes <- rmnlIndepMetrop(
    Data = list(y = yforBayes, X = as.matrix(XforBayes), p = Road$p),
    Mcmc = list(R = 200000, keep = 40)
  )
}
colnames(out_Bayes$betadraw) <- colnames(XforBayes)
coefnames <- colnames(XforBayes)
cat("\nSummary of Betadraw (Aggregate Bayesian Estimation):\n")
print(summary(out_Bayes$betadraw))
plot(out_Bayes$loglike)
bayes_summary <- round(cbind(
  Mean = colMeans(out_Bayes$betadraw),
  SD = apply(out_Bayes$betadraw, 2, sd)
), 3)
print("Bayesian Summary (Mean and SD):")
print(bayes_summary)

# 6. Define Sign Restrictions
PriceSignRes <- rep(0, ncol(Road$lgtdata[[1]]$X))
PriceSignRes[which(coefnames == "Price")] <- -1
SignRes <- rep(0, ncol(Road$lgtdata[[1]]$X))
SignRes[which(coefnames == "High Resolution (264 ppi)")] <- 1
SignRes[which(coefnames == "Without SD-Slot")] <- -1
SignRes[which(coefnames == "8-12 h. Runtime")] <- 1
SignRes[which(coefnames == "S-phone Synch.")] <- 1
SignRes[which(coefnames == "Value Pack")] <- 1
SignRes[which(coefnames == "Price")] <- -1

# 7. Hierarchical Mixed Logit Models
if (!exists("out_HB")) {
  out_HB <- rhierMnlRwMixture(
    Data = list(lgtdata = Road$lgtdata, p = Road$p),
    Prior = list(ncomp = 1),
    Mcmc = list(R = 200000, keep = 40)
  )
}
plot(out_HB$loglike)
if (!exists("price_HB_constr")) {
  price_HB_constr <- rhierMnlRwMixture(
    Data = list(lgtdata = Road$lgtdata, p = Road$p),
    Prior = list(ncomp = 1, SignRes = PriceSignRes),
    Mcmc = list(R = 100000, keep = 20)
  )
}
plot(price_HB_constr$loglike)

# 8. Helper Function: hilfana
hilfana <- function(outinput, coefnames, burnin, labels) {
  plot(outinput$loglike); grid()
  par(mfrow = c(3,3))
  for (i in 1:9) {
    plot(outinput$betadraw[i,33,], type = 'l', ylab = coefnames[33], main = i); grid()
  }
  betadrawconverged <- outinput$betadraw[,,(burnin+1):dim(outinput$betadraw)[3]]
  par(mfrow = c(3,3))
  for (i in 1:9) {
    plot(density(outinput$betadraw[i,33,]), ylab = coefnames[33], main = i); grid()
  }
  dimnames(betadrawconverged) <- list(NULL, as.list(coefnames), NULL)
  print(round(cbind(
    rowMeans(betadrawconverged[10,,]),
    rowMeans(betadrawconverged[11,,])
  ), digits = 2))
  betaexchange <- array(aperm(betadrawconverged, perm = c(1, 3, 2)),
                        dim = c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                                dim(betadrawconverged)[2]))
  colnames(betaexchange) <- coefnames
  mpref <- cbind(colMeans(betaexchange), apply(betaexchange, 2, sd))
  comppref <- cbind(mpref[,1], colMeans(out_Bayes$betadraw))
  compWTP <- round(comppref / -matrix(comppref[33,], nrow = 2), digits = 3)
  return(list(betaexchange = betaexchange, mpref = mpref, comppref = comppref, compWTP = compWTP))
}
labels <- setNames(coefnames, coefnames)
hilf_HB <- hilfana(outinput = out_HB, coefnames = coefnames, burnin = 1500, labels = labels)
hilf_priceHB <- hilfana(outinput = price_HB_constr, coefnames = coefnames, burnin = 1000, labels = labels)

# 9. Order Constraints
Rcpp::sourceCpp("cpp/rhierMnlRwMixture_rcpp_loop_Sim_modHP.cpp", showOutput = FALSE)
source("R/rhierMnlRwMixture_main.R")
for (i in 1:length(Road$lgtdata)) {
  Road_R$lgtdata[[i]]$X <- Road$lgtdata[[i]]$X[, c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17,
                                                     19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32,
                                                     34, 35, 36, 1:12)]
}
coefnames_R <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17,
                            19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32,
                            34, 35, 36, 1:12)]
nvar_c <- 24
Amu <- diag(1/10, nrow = nvar_c)
mustarbarc <- rep(0, nvar_c)
nu <- 15 + nvar_c
V <- nu * diag(nvar_c) * 0.5
Prior <- list(ncomp = 1, Amu = Amu, mustarbarc = mustarbarc, nu = nu, V = V)
Mcmc <- list(R = 300000, keep = 60)
if (!exists("out_order")) {
  out_order <- rhierMnlRwMixture_SR_ord(
    Data = list(p = p, lgtdata = Road_R$lgtdata),
    Prior = Prior,
    Mcmc = Mcmc,
    nvar_c = nvar_c,
    flag = "approx"
  )
}
plot(out_order$loglike)
out <- out_order
betadraws <- out$betadraw
orderedbetadraw <- array(NA, dim = dim(betadraws))
for (i in 1:dim(betadraws)[1]) {
  for (r in 1:dim(betadraws)[3]) {
    orderedbetadraw[i, , r] <- startobeta(betadraws[i, , r])
  }
}
dimnames(orderedbetadraw) <- list(NULL, coefnames_R, NULL)
out$betadraw <- orderedbetadraw
original_beta <- betadraws[1, , 1]
transformed_beta <- orderedbetadraw[1, , 1]
print("Original Beta:")
print(original_beta)
print("Transformed Beta:")
print(transformed_beta)
if (transformed_beta[which(coefnames_R == "Price")] < 0) {
  print("Price constraint is satisfied.")
} else {
  print("Price constraint is NOT satisfied.")
}
hilfana_order <- function(outinput, coefnames, burnin, labels) {
  plot(outinput$loglike); grid()
  par(mfrow = c(3,3))
  for (i in 1:9) {
    plot(outinput$betadraw[i,1,], type = 'l', ylab = coefnames[1], main = i); grid()
  }
  betadrawconverged <- outinput$betadraw[,,(burnin+1):dim(outinput$betadraw)[3]]
  par(mfrow = c(3,3))
  for (i in 1:9) {
    plot(density(outinput$betadraw[i,1,]), ylab = coefnames[1], main = i); grid()
  }
  dimnames(betadrawconverged) <- list(NULL, as.list(coefnames), NULL)
  print(round(cbind(
    rowMeans(betadrawconverged[10,,]),
    rowMeans(betadrawconverged[11,,])
  ), digits = 2))
  betaexchange <- array(aperm(betadrawconverged, perm = c(1, 3, 2)),
                        dim = c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                                dim(betadrawconverged)[2]))
  colnames(betaexchange) <- coefnames
  mpref <- cbind(colMeans(betaexchange), apply(betaexchange, 2, sd))
  comppref <- cbind(mpref[,1], colMeans(out_Bayes$betadraw))
  compWTP <- round(comppref / -matrix(comppref[1,], nrow = 2), digits = 3)
  return(list(betaexchange = betaexchange, mpref = mpref, comppref = comppref, compWTP = compWTP))
}
hilf_order <- hilfana_order(outinput = out, coefnames = coefnames_R, burnin = 1000)

par(mfrow = c(2,2))
plot(density(hilf_priceHB$betaexchange[,33]), main = 'Price'); grid()
plot(density(hilf_priceHB$betaexchange[,34]), main = '50 CB'); grid()
plot(density(hilf_priceHB$betaexchange[,35]), main = '100 CB'); grid()
plot(density(hilf_priceHB$betaexchange[,36]), main = '150 CB'); grid()
plot(density(hilf_order$betaexchange[,1]), main = 'cPrice'); grid()
plot(density(hilf_order$betaexchange[,22]), main = 'c50 CB'); grid()
plot(density(hilf_order$betaexchange[,23]), main = 'c100 CB'); grid()
plot(density(hilf_order$betaexchange[,24]), main = 'c150 CB'); grid()

# 10. Budget Constraint
Rcpp::sourceCpp("cpp/rhierMnlRwMixture_rcpp_loop_Illus_BLP_type.cpp", showOutput = FALSE)
Rcpp::sourceCpp("cpp/Speed++_MS_BC_BLP_Efficient.cpp", showOutput = FALSE)
source("R/rhierMnlRwMixture_main_untuned_BC.R")
for (i in 1:length(Road$lgtdata)) {
  Road_BC$lgtdata[[i]]$X <- Road$lgtdata[[i]]$X[, c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17,
                                                     19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32,
                                                     34, 35, 36, 1:12)]
}
coefnames_BC <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17,
                             19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32,
                             34, 35, 36, 1:12)]
coefnames_BC <- c('log_budget', coefnames_BC)
coefnames_BC[2] <- 'log-alpha'
nvar_c <- 25
Amu <- diag(1/10, nrow = nvar_c)
mustarbarc <- rep(0, nvar_c)
nu <- 15 + nvar_c
V <- nu * diag(nvar_c) * 0.5
pr <- 1
Prior <- list(ncomp = 1, Amu = Amu, mustarbarc = mustarbarc, nu = nu, V = V)
Mcmc <- list(R = 400000, keep = 8)
if (!exists("out_BC")) {
  out_BC <- rhierMnlRwMixture_SR(
    Data = list(p = p, lgtdata = Road_BC$lgtdata),
    Prior = Prior,
    Mcmc = Mcmc,
    nvar_c = nvar_c,
    pr = pr,
    starting_budget = log(11.00)
  )
}
out_postHB <- array(0, dim = c(Mcmc$R / Mcmc$keep, 37, 2))
varlogB <- array(0, dim = c(Mcmc$R / Mcmc$keep, 37, 2))
for (m in 1:(Mcmc$R / Mcmc$keep)) {
  out_postHB[m, , 1] <- out_BC$nmix$compdraw[[m]][[1]]$mustar
  varlogB[m, , 1] <- var(out_BC$betadraw[,1,m])
}
par(mfrow = c(2,2))
matplot(out_postHB[, 1, 1, drop = FALSE], type = 'l', main = 'Log-Budget Mean', xlab = 'Observation', ylab = 'Value')
n_obs <- dim(out_postHB)[1]
matplot(out_postHB[2001:n_obs, 1, 1:2], type = 'l', main = 'Log-Budget Mean (Zoomed)', xlab = 'Observation', ylab = 'Value')
mean_param1 <- rowMeans(out_postHB[, 1, ])
matplot(mean_param1, type = 'l', main = 'Mean of Budget Across Draws', xlab = 'Observation', ylab = 'Mean Value')
sd_param1 <- apply(out_postHB[, 1, ], 1, sd)
matplot(sd_param1, type = 'l', main = 'Std. Dev. of Budget Across Draws', xlab = 'Observation', ylab = 'Standard Deviation')

hilfana_BC <- function(outinput, coefnames, burnin) {
  plot(outinput$loglike)
  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 1, ])
    if (length(x[is.finite(x)]) == 0) next
    plot(x, type = 'l', main = 'budget')
  }
  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 1, (burnin + 1):dim(outinput$betadraw)[3]])
    if (length(x[is.finite(x)]) == 0) next
    plot(density(x), type = 'l', main = 'budget')
  }
  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 2, ])
    if (length(x[is.finite(x)]) == 0) next
    plot(x, type = 'l', main = 'alpha')
  }
  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 2, (burnin + 1):dim(outinput$betadraw)[3]])
    if (length(x[is.finite(x)]) == 0) next
    plot(density(x), type = 'l', main = 'alpha')
  }
  betadrawconverged <- outinput$betadraw[, , (burnin + 1):dim(outinput$betadraw)[3]]
  dimnames(betadrawconverged) <- list(NULL, as.list(coefnames), NULL)
  print(round(cbind(
    rowMeans(betadrawconverged[10, , ]),
    rowMeans(betadrawconverged[11, , ])
  ), digits = 2))
  betaexchange <- array(aperm(betadrawconverged, perm = c(1, 3, 2)),
                        dim = c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                                dim(betadrawconverged)[2]))
  colnames(betaexchange) <- coefnames
  mpref <- cbind(colMeans(betaexchange), apply(betaexchange, 2, sd))
  for (m in 1:nrow(betaexchange)) {
    betaexchange[m, ] <- startobeta(betaexchange[m, ])
  }
  return(list(betaexchange = betaexchange, mpref = mpref))
}
hilf_BC <- hilfana_BC(outinput = out_BC, coefnames = coefnames_BC, burnin = 1000)
x <- hilf_BC$betaexchange[, 1]
pct_below_899 <- mean(x < 8.99) * 100
cat(sprintf("%.1f%% of observations are below 8.99.\n", pct_below_899))
q1 <- quantile(x, 0.25)
q2 <- quantile(x, 0.50)
q3 <- quantile(x, 0.75)
d <- density(x)
op <- par(family = "Times New Roman", cex = 1.0)
on.exit(par(op))
plot(d, main = "", xlab = "Budget (in 100 EUR)", ylab = "Density", type = "n", ylim = c(0, max(d$y)), xlim = c(0, max(d$x)))
polygon(x = c(d$x[d$x <= q1], rev(d$x[d$x <= q1])), y = c(d$y[d$x <= q1], rep(0, sum(d$x <= q1))), col = gray(0.9), border = NA)
polygon(x = c(d$x[d$x >= q1 & d$x <= q2], rev(d$x[d$x >= q1 & d$x <= q2])), y = c(d$y[d$x >= q1 & d$x <= q2], rep(0, sum(d$x >= q1 & d$x <= q2))), col = gray(0.8), border = NA)
polygon(x = c(d$x[d$x >= q2 & d$x <= q3], rev(d$x[d$x >= q2 & d$x <= q3])), y = c(d$y[d$x >= q2 & d$x <= q3], rep(0, sum(d$x >= q2 & d$x <= q3))), col = gray(0.7), border = NA)
polygon(x = c(d$x[d$x >= q3], rev(d$x[d$x >= q3])), y = c(d$y[d$x >= q3], rep(0, sum(d$x >= q3))), col = gray(0.6), border = NA)
lines(d, lwd = 2)
abline(v = 8.99, col = "red", lwd = 2)
title(main = sprintf("Budget in population (%.1f%% below 8.99 EUR)", pct_below_899), cex.main = 1.2, font.main = 1)
summary(hilf_BC$betaexchange)

summarize_hb_model <- function(model_output, coefnames, burnin = 1000, price_var = "Price") {
  ndraws_total <- dim(model_output$betadraw)[3]
  used_draws <- (burnin + 1):ndraws_total
  betadraw_post <- model_output$betadraw[,,used_draws]
  betadraw_flat <- aperm(betadraw_post, c(1, 3, 2))
  dim(betadraw_flat) <- c(dim(betadraw_post)[1] * dim(betadraw_post)[3], dim(betadraw_post)[2])
  colnames(betadraw_flat) <- coefnames
  mean_vals <- colMeans(betadraw_flat)
  sd_vals <- apply(betadraw_flat, 2, sd)
  q025 <- apply(betadraw_flat, 2, quantile, probs = 0.025)
  q50 <- apply(betadraw_flat, 2, quantile, probs = 0.50)
  q975 <- apply(betadraw_flat, 2, quantile, probs = 0.975)
  summary_df <- data.frame(
    Attribute = coefnames,
    Mean = mean_vals,
    SD = sd_vals,
    Q2.5 = q025,
    Q50 = q50,
    Q97.5 = q975,
    row.names = NULL
  )
  if (price_var %in% coefnames) {
    price_mean <- mean_vals[price_var]
    summary_df$WTP <- - (summary_df$Mean / price_mean)
  } else {
    summary_df$WTP <- NA
    warning("Price variable not found in coefnames; WTP column set to NA.")
  }
  return(list(summary_table = summary_df, betadraw_flat = betadraw_flat))
}
res_unconstr <- summarize_hb_model(model_output = out_HB, coefnames = coefnames, burnin = 2500)
print(head(res_unconstr$summary_table, 10))
res_price <- summarize_hb_model(price_HB_constr, coefnames, burnin = 1000)
res_order_const <- summarize_hb_model(model_output = out, coefnames = coefnames_R, burnin = 1500, price_var = "Price")
align_summary_table <- function(reference_summary, target_summary) {
  target_summary[match(reference_summary$Attribute, target_summary$Attribute), ]
}
res_order_const$summary_table <- align_summary_table(res_unconstr$summary_table, res_order_const$summary_table)
compare_means <- data.frame(
  Attribute = res_unconstr$summary_table$Attribute,
  Unconstrained_Mean = res_unconstr$summary_table$Mean,
  PriceConst_Mean = res_price$summary_table$Mean,
  OrderConst_Mean = res_order_const$summary_table$Mean
)
print(compare_means)
compare_wtp <- data.frame(
  Attribute = res_unconstr$summary_table$Attribute,
  Unconstrained_WTP = res_unconstr$summary_table$WTP,
  PriceConst_WTP = res_price$summary_table$WTP,
  OrderConst_WTP = res_order_const$summary_table$WTP
)
print(compare_wtp)
df_price_unconstr <- data.frame(Price_Coefficient = res_unconstr$betadraw_flat[,"Price"], Model = "Unconstrained")
df_price_priceC <- data.frame(Price_Coefficient = res_price$betadraw_flat[,"Price"], Model = "Price-Constrained")
df_price_orderC <- data.frame(Price_Coefficient = res_order_const$betadraw_flat[,"Price"], Model = "Order-Constrained")
df_price_melt <- rbind(df_price_unconstr, df_price_priceC, df_price_orderC)
ggplot(df_price_melt, aes(x = Price_Coefficient, fill = Model)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Model, scales = "free") +
  labs(title = "Comparison of Price Coefficients Across Models", x = "Price Coefficient", fill = "Model") +
  theme_minimal()
burnin <- 1500
betadraw_post_unconstr <- out_HB$betadraw[,,(burnin+1):dim(out_HB$betadraw)[3]]
individual_means_unconstr <- apply(betadraw_post_unconstr, c(1,2), mean)
colnames(individual_means_unconstr) <- coefnames
hist(individual_means_unconstr[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Unconstrained)",
     xlab = "Price Coefficient",
     col = "skyblue", border = "white")
betadraw_post_price_const <- price_HB_constr$betadraw[,,(burnin+1):dim(price_HB_constr$betadraw)[3]]
individual_means_price_const <- apply(betadraw_post_price_const, c(1,2), mean)
colnames(individual_means_price_const) <- coefnames
hist(individual_means_price_const[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Price-Constrained)",
     xlab = "Price Coefficient",
     col = "salmon", border = "white")
betadraw_post_order_const <- out$betadraw[,,(burnin+1):dim(out$betadraw)[3]]
individual_means_order_const <- apply(betadraw_post_order_const, c(1,2), mean)
colnames(individual_means_order_const) <- coefnames_R
hist(individual_means_order_const[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Order-Constrained)",
     xlab = "Price Coefficient",
     col = "orange", border = "white")
price_draws <- selected_betamix[, "Price"]
cb50_draws <- res_order_const$betadraw_flat[, "50 Cash Back"]
cb100_draws <- res_order_const$betadraw_flat[, "100 Cash Back"]
cb150_draws <- res_order_const$betadraw_flat[, "150 Cash Back"]
summary_stats <- function(x) {
  list(mean = mean(x), median = median(x), min = min(x), max = max(x))
}
price_summary <- summary_stats(price_draws)
cb50_summary <- summary_stats(cb50_draws)
cb100_summary <- summary_stats(cb100_draws)
cb150_summary <- summary_stats(cb150_draws)
cat("Price Coefficient Summary:\n"); print(price_summary)
cat("\n50 Cash Back Coefficient Summary:\n"); print(cb50_summary)
cat("\n100 Cash Back Coefficient Summary:\n"); print(cb100_summary)
cat("\n150 Cash Back Coefficient Summary:\n"); print(cb150_summary)
summarize_hb_BC_model <- function(model_output, coefnames, burnin = 1000, price_var = "log-alpha") {
  ndraws_total <- dim(model_output$betadraw)[3]
  used_draws <- (burnin + 1):ndraws_total
  betadraw_post <- model_output$betadraw[,, used_draws]
  betadraw_flat <- aperm(betadraw_post, c(1, 3, 2))
  dim(betadraw_flat) <- c(dim(betadraw_post)[1] * dim(betadraw_post)[3], dim(betadraw_post)[2])
  colnames(betadraw_flat) <- coefnames
  mean_vals <- colMeans(betadraw_flat)
  sd_vals <- apply(betadraw_flat, 2, sd)
  q025 <- apply(betadraw_flat, 2, quantile, probs = 0.025)
  q50 <- apply(betadraw_flat, 2, quantile, probs = 0.50)
  q975 <- apply(betadraw_flat, 2, quantile, probs = 0.975)
  summary_df <- data.frame(
    Attribute = coefnames,
    Mean = mean_vals,
    SD = sd_vals,
    Q2.5 = q025,
    Q50 = q50,
    Q97.5 = q975,
    row.names = NULL
  )
  if (price_var %in% coefnames) {
    price_mean <- mean_vals[price_var]
    summary_df$WTP <- - (summary_df$Mean / price_mean)
  } else {
    summary_df$WTP <- NA
    warning("Price variable not found in coefnames; WTP column set to NA.")
  }
  return(list(summary_table = summary_df, betadraw_flat = betadraw_flat))
}
res_BC <- summarize_hb_BC_model(model_output = out_BC, coefnames = coefnames_BC, burnin = 45000, price_var = "log-alpha")
print(head(res_BC$summary_table, 37))
hilfana_BC <- function(outinput, coefnames, burnin) {
  print(dim(outinput$betadraw))
  betadrawconverged <- outinput$betadraw[, , (burnin + 1):dim(outinput$betadraw)[3]]
  dimnames(betadrawconverged) <- list(NULL, as.list(coefnames), NULL)
  betaexchange <- array(aperm(betadrawconverged, perm = c(1, 3, 2)),
                        dim = c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                                dim(betadrawconverged)[2]))
  colnames(betaexchange) <- coefnames
  mpref <- cbind(colMeans(betaexchange), apply(betaexchange, 2, sd))
  for (m in 1:nrow(betaexchange)) {
    betaexchange[m, ] <- startobeta(betaexchange[m, ])
  }
  return(list(betaexchange = betaexchange, mpref = mpref))
}
hilf_BC <- hilfana_BC(outinput = out_BC, coefnames = coefnames_BC, burnin = 1000)
summarize_model <- function(beta_matrix, model_name) {
  summary_df <- data.frame(
    Attribute = colnames(beta_matrix),
    Mean = apply(beta_matrix, 2, mean),
    Q1 = apply(beta_matrix, 2, quantile, probs = 0.25),
    Median = apply(beta_matrix, 2, quantile, probs = 0.50),
    Q3 = apply(beta_matrix, 2, quantile, probs = 0.75),
    IQR = apply(beta_matrix, 2, IQR)
  )
  summary_df$Model <- model_name
  return(summary_df)
}
summary_unconstrained <- summarize_model(hilf_HB$betaexchange, "Unconstrained")
summary_sign <- summarize_model(hilf_priceHB$betaexchange, "Sign-Constrained")
summary_order <- summarize_model(hilf_order$betaexchange, "Order-Constrained")
summary_budget <- summarize_model(hilf_BC$betaexchange, "Budget-Constrained")
summary_aggregate <- summarize_model(out_Bayes$betadraw, "Aggregate")
all_summaries <- rbind(summary_unconstrained, summary_sign, summary_order, summary_budget, summary_aggregate)
all_summaries <- all_summaries[, c("Model", "Attribute", "Mean", "Q1", "Median", "Q3", "IQR")]
print(all_summaries)
print(summary(out$loglike))
