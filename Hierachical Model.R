####################################################################################################
##   4. Hierarchical Mixed Logit (MXL) Model - new
####################################################################################################

# -------------------------------
# 1. Load Required Packages
# -------------------------------

# Function to load or install required packages
load_or_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "doParallel", "foreach", "parallel", "sf", 
  "stringr", "data.table", "pbapply", "caret", "mlogit", "Rcpp", "RcppArmadillo",
  "bayesm"
)

# Load all packages
lapply(packages, load_or_install)

# -------------------------------
# 2. Load the Estimation Data
# -------------------------------

# Define the path to your estimation data
path_e_data_mod <- "/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/Estimation_Data_tab.Rdata"

# Load the data
load(path_e_data_mod)

# -------------------------------
# 3. Define Attribute Labels
# -------------------------------

# Define attributelabels_tablet with longer, descriptive labels
attributelabels_tablet <- c(
  'Brand 1',                   # Brand1
  'Brand 2',                   # Brand2
  'Brand 3',                   # Brand3
  'Brand 4',                   # Brand4
  'Brand 5',                   # Brand5
  'Brand 6',                   # Brand6
  'Brand 7',                   # Brand7
  'System B',                  # SystemB
  '8 Inches',                  # 8Inches
  '10 Inches',                 # 10Inches
  '12 Inches',                 # 12Inches
  '13 Inches',                 # 13Inches
  'High Resolution (264 ppi)', # HighResolution
  '16 GB',                     # 16GB
  '32 GB',                     # 32GB
  '64 GB',                     # 64GB
  '128GB',                     # 128GB
  'Without SD-Slot',           # WithoutSDSlot
  '1.6 GHz',                   # 1.6GHz
  '2.2 GHz',                   # 2.2GHz
  '8-12 h. Runtime',           # 8-12hRuntime
  'WLAN + UMTS/3G',            # WLANUMTS3G
  'WLAN + LTE/4G',             # WLANLTE4G
  'S-phone Synch.',            # SphoneSynch
  'Value Pack',                # ValuePack
  'Cover',                     # Cover
  'Keyboard',                  # Keyboard
  'Mouse',                     # Mouse
  'Pencil',                    # Pencil
  '32 GB Memory Card',         # 32GBMemoryCard
  'Keyboard + Pencil',         # KeyboardPencil
  'Keyboard + Mouse + Pencil', # KeyboardMousePencil
  'Price',                     # Price
  '50 Cash Back',              # 50CashBack
  '100 Cash Back',             # 100CashBack
  '150 Cash Back'              # 150CashBack
)

# -------------------------------
# 4. Sampling Parameters
# -------------------------------

# Define the total number of individuals
N <- length(E_Data_mod$lgtdata)

# Define sampling parameters
sample_fraction <- 0.1      # Proportion of individuals to sample
set.seed(123)               # Set seed for reproducibility
n_sample <- round(N * sample_fraction)  # Calculate sample size

# Sample 10% of respondents
sample_indices <- sample(1:N, n_sample)
E_Data_mod_sampled <- list(
  lgtdata = E_Data_mod$lgtdata[sample_indices],  # Subset individual-level data
  p = E_Data_mod$p,  # Keep number of alternatives
  t = E_Data_mod$t   # Keep number of tasks
)

# Verify dataset size
cat("Sampled Dataset Size:", length(E_Data_mod_sampled$lgtdata), "\n")  # Should be 30% of N

# Prepare data for modeling
Road <- E_Data_mod_sampled # Change to full dataset for final analysis
yforBayes <- unlist(lapply(Road$lgtdata, function(indiv) indiv$y))
XforBayes <- do.call(rbind, lapply(Road$lgtdata, function(indiv) indiv$X))


# Assign column names to the design matrices in Road$lgtdata
for (i in seq_along(Road$lgtdata)) {
  colnames(Road$lgtdata[[i]]$X) <- attributelabels_tablet
}
# -------------------------------
# 5. Aggregate Bayesian Estimation
# -------------------------------

# Run Aggregate Bayesian Estimation using 'rmnlIndepMetrop'
out_Bayes <- rmnlIndepMetrop(
  Data = list(y = yforBayes, X = as.matrix(XforBayes), p = Road$p),
  Mcmc = list(R = 2000)
)

# Assign column names for 'betadraw' to match 'XforBayes'
colnames(out_Bayes$betadraw) <- colnames(XforBayes)
coefnames <- colnames(XforBayes)  # Extract coefficient names

# Summarize aggregate model results
cat("\nSummary of Betadraw (Aggregate Bayesian Estimation):\n")
print(summary(out_Bayes$betadraw))

# Compute and display means and standard deviations
bayes_summary <- round(
  cbind(
    Mean = colMeans(out_Bayes$betadraw),
    SD = apply(out_Bayes$betadraw, 2, sd)
  ),
  digits = 3
)
print("Bayesian Summary (Mean and SD):")
print(bayes_summary)

# -------------------------------
# 6. Define Sign Restrictions
# -------------------------------

# Initialize all coefficients as unconstrained (0)
SignRes <- rep(0, ncol(Road$lgtdata[[1]]$X))

# Assign sign restrictions based on attribute expectations
SignRes[which(coefnames == "High Resolution (264 ppi)")] <- 1   # High resolution should increase utility
SignRes[which(coefnames == "Without SD-Slot")] <- -1           # Without SD card should decrease utility
SignRes[which(coefnames == "8-12 h. Runtime")] <- 1            # 8-12h runtime should increase utility
SignRes[which(coefnames == "S-phone Synch.")] <- 1             # Smartphone Synchronization should increase utility
SignRes[which(coefnames == "Value Pack")] <- 1                 # Value Pack should increase utility
SignRes[which(coefnames == "Price")] <- -1                     # Price should decrease utility

# -------------------------------
# 7. Hierarchical Mixed Logit Models
# -------------------------------

# Run Unconstrained Hierarchical Mixed Logit Model
out_HB <- rhierMnlRwMixture(
  Data = list(lgtdata = Road$lgtdata, p = Road$p),
  Prior = list(ncomp = 1),
  Mcmc = list(R = 5000, keep = 2)
)

# Run Sign Constrained Hierarchical Mixed Logit Model
out_HB_constr <- rhierMnlRwMixture(
  Data = list(lgtdata = Road$lgtdata, p = Road$p),
  Prior = list(ncomp = 1, SignRes = SignRes),
  Mcmc = list(R = 2000, keep = 2)
)


# Assign dimension names to Hierarchical Models
dimnames(out_HB$betadraw) <- list(NULL, coefnames, NULL)
dimnames(out_HB_constr$betadraw) <- list(NULL, coefnames, NULL)

# -------------------------------
# 8. Define the `hilfana` Function with Corrections
# -------------------------------

# This function analyzes the Hierarchical Mixed Logit Model results
hilfana <- function(outinput, coefnames, burnin, labels_map){
  
  # Assign dimension names before any plotting or indexing
  dimnames(outinput$betadraw) <- list(NULL, coefnames, NULL)
  
  # 1. Plot Log-Likelihood Trace
  plot(outinput$loglike);grid()
  
  # plot Price for first 6 individuals
  par(mfrow=c(3,2))
  for (i in 1:6){
    plot(outinput$betadraw[i,33,],type='l', ylab=coefnames[33], main=i);grid()
  }
  
  
  plot(outinput$loglike, type='l', main="Log-Likelihood Trace", 
       xlab="Iteration", ylab="Log-Likelihood")
  grid()
  
  # 2. Define Coefficients of Interest
  # Based on current focus: Price and Cashback
  coefficients_of_interest <- c(
    "Price", "50 Cash Back", "100 Cash Back", "150 Cash Back"
  )
  
  # Verify that all coefficients exist in coefnames
  missing_coefs <- setdiff(coefficients_of_interest, coefnames)
  if(length(missing_coefs) > 0){
    warning(paste("The following coefficients are missing in coefnames:", 
                  paste(missing_coefs, collapse=", ")))
  }
  
  # Filter out missing coefficients to avoid errors
  coefficients_present <- coefficients_of_interest[coefficients_of_interest %in% coefnames]
  
  # 3. Trace Plots for Coefficients of Interest
  par(mfrow=c(1,2))
  for (coef in coefficients_present){
    plot(outinput$betadraw[, coef, ], type='l',
         ylab=labels_map[coef],
         main=paste("Trace Plot:", labels_map[coef]))
    grid()
  }
  
  # Reset plotting layout
  par(mfrow=c(1,1))
  
  # 4. Check Dimensions (For Debugging)
  print("Dimensions of betadraw:")
  print(dim(outinput$betadraw))  # Dimensions: individuals x coefficients x iterations
  
  # 5. Extract Post-Burn-in Samples
  total_iterations <- dim(outinput$betadraw)[3]
  if(burnin >= total_iterations){
    stop("Burn-in exceeds the number of MCMC iterations.")
  }
  betadrawconverged <- outinput$betadraw[,,(burnin+1):total_iterations]
  
  # 6. Density Plots for Coefficients of Interest
  par(mfrow=c(1,2))
  for (coef in coefficients_present){
    if(all(!is.na(betadrawconverged[, coef, ]))){
      plot(density(betadrawconverged[, coef, ]), 
           ylab=labels_map[coef], 
           main=paste("Density Plot:", labels_map[coef]))
      grid()
    } else {
      plot.new()
      title(main=paste("Density Plot:", labels_map[coef]))
      text(0.5, 0.5, "No Data Available")
    }
  }
  
  # Reset plotting layout
  par(mfrow=c(1,1))
  
  # 7. Posterior Means for Specific Individuals (e.g., Individuals 10 and 11)
  individuals_of_interest <- c(10, 11)
  for (ind in individuals_of_interest){
    if(ind <= dim(betadrawconverged)[1]){
      posterior_mean <- rowMeans(betadrawconverged[ind, , ])
      cat("\nPosterior Mean for Individual", ind, ":\n")
      # Replace coefficient names with labels
      names(posterior_mean) <- labels_map[coefnames]
      print(round(posterior_mean, 2))
    } else {
      cat("\nIndividual", ind, "is out of bounds.\n")
    }
  }
  
  # 8. Reshape Beta Draws for Market Analysis
  betaexchange <- array(aperm(betadrawconverged, perm=c(1,3,2)),
                        dim=c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                              dim(betadrawconverged)[2]))
  
  colnames(betaexchange) <- coefnames
  
  # 9. Average Preferences and Heterogeneity
  mpref <- cbind(Mean = colMeans(betaexchange), SD = apply(betaexchange, 2, sd))
  
  # 10. Comparison to Aggregate Model
  # Ensure that 'out_Bayes$betadraw' has the same column names as 'coefnames'
  if(!all(colnames(out_Bayes$betadraw) == coefnames)){
    warning("Column names of out_Bayes$betadraw do not match coefnames.")
  }
  
  comppref <- cbind(
    Hierarchical_Mean = mpref[, "Mean"],
    Aggregate_Mean = colMeans(out_Bayes$betadraw)
  )
  
  # Replace row names with labels
  rownames(comppref) <- labels_map[rownames(comppref)]
  
  # 11. Calculate Willingness to Pay (WTP)
  if ("Price" %in% rownames(comppref)){
    # Extract Price coefficients for both models
    price_hb <- comppref["Price", "Hierarchical_Mean"]
    price_agg <- comppref["Price", "Aggregate_Mean"]
    
    # Attributes for WTP (excluding Price and Cashback)
    attributes_for_wtp <- setdiff(rownames(comppref), c("Price", "50 Cash Back", "100 Cash Back", "150 Cash Back"))
    
    # Verify that attributes_for_wtp exist in mpref
    missing_wtp_attrs <- setdiff(attributes_for_wtp, rownames(mpref))
    if(length(missing_wtp_attrs) > 0){
      warning(paste("The following attributes for WTP are missing in mpref:", 
                    paste(missing_wtp_attrs, collapse=", ")))
    }
    
    # Exclude any missing attributes
    attributes_for_wtp <- attributes_for_wtp[attributes_for_wtp %in% rownames(mpref)]
    
    # Calculate WTP for each attribute
    wtp_hb <- mpref[attributes_for_wtp, "Mean"] / -price_hb
    wtp_agg <- comppref[attributes_for_wtp, "Aggregate_Mean"] / -price_agg
    
    # Combine into a matrix
    compWTP <- round(cbind(Hierarchical_WTP = wtp_hb, Aggregate_WTP = wtp_agg), digits = 3)
    
    # Print WTP estimates
    cat("\nWillingness to Pay (WTP) Estimates:\n")
    print(compWTP)
  } else {
    warning("Price coefficient not found. WTP cannot be computed.")
    compWTP <- NULL
  }
  
  # 12. Preference Heterogeneity Plots for Specific Coefficients
  # Focus on Price and Cashback
  par(mfcol=c(2,2))
  
  # Density plot for Price
  if("Price" %in% colnames(betaexchange)){
    plot(density(betaexchange[, "Price"]),
         main="Price Distribution",
         xlab="Price")
    grid()
  } else {
    plot.new()
    title(main="Price Distribution")
    text(0.5, 0.5, "No Data Available")
  }
  
  # Density plots for Cashback
  for (cashback_level in c("50 Cash Back", "100 Cash Back", "150 Cash Back")){
    if(cashback_level %in% colnames(betaexchange)){
      plot(density(betaexchange[, cashback_level]),
           main=paste(cashback_level, "Distribution"),
           xlab=cashback_level)
      grid()
    } else {
      plot.new()
      title(main=paste(cashback_level, "Distribution"))
      text(0.5, 0.5, "No Data Available")
    }
  }
  
  # Reset plotting layout
  par(mfcol=c(1,1))
  
  # Smooth Scatter: Price vs. Each Cashback Level
  par(mfcol=c(2,2))
  for (cashback_level in c("50 Cash Back", "100 Cash Back", "150 Cash Back")){
    if(all(cashback_level %in% colnames(betaexchange), "Price" %in% colnames(betaexchange))){
      plot(betaexchange[, "Price"], betaexchange[, cashback_level],
           main=paste("Price vs", cashback_level),
           xlab="Price",
           ylab=cashback_level)
      abline(lm(betaexchange[, cashback_level] ~ betaexchange[, "Price"]), col="red")
      grid()
    } else {
      plot.new()
      title(main=paste("Price vs", cashback_level))
      text(0.5, 0.5, "Insufficient Data")
    }
  }
  
  # Reset plotting layout
  par(mfcol=c(1,1))
  
  # 13. Return Results
  return(list(betaexchange=betaexchange, 
              mpref=mpref, 
              comppref=comppref, 
              compWTP=compWTP))
}

# -------------------------------
# 9. Create `labels_map`
# -------------------------------

# Since 'coefnames' are already descriptive, we'll map them to themselves.
labels_map <- setNames(coefnames, coefnames)

# **Optional:** If you prefer shorter or customized labels, define them here.
# Example:
# labels_map <- c(
#   "High Resolution (264 ppi)" = "High Res (264 ppi)",
#   "Without SD-Slot" = "No SD Slot",
#   "S-phone Synch." = "Phone Sync",
#   # Continue mapping for all coefficients
# )

# -------------------------------
# 10. Run the `hilfana` Function for Both Models
# -------------------------------

# Run the hilfana function for the Unconstrained Model
hilf_HB <- hilfana(
  outinput = out_HB, 
  coefnames = coefnames, 
  burnin = 500, 
  labels_map = labels_map
)

# Run the hilfana function for the Sign-Constrained Model
hilf_constr <- hilfana(
  outinput = out_HB_constr, 
  coefnames = coefnames, 
  burnin = 200, 
  labels_map = labels_map
)




# -------------------------------
# 11. Order Constraints 
# -------------------------------

Road$lgtdata[[1]]$X[1:2,]
Road_R$lgtdata[[1]]$X[1:2,]

###Source cpp function for estimation
Rcpp::sourceCpp("~/Desktop/Master/3. Semester/CSCC/MArket Simulation/rhierMnlRwMixture_rcpp_loop_Sim_modHP.cpp",showOutput = FALSE)
source('~/Desktop/Master/3. Semester/rhierMnlRwMixture_main.R')



unlist(coefnames)

Road_R=Road

for (i in 1:length(Road$lgtdata)){
  Road_R$lgtdata[[i]]$X = Road$lgtdata[[i]]$X[,c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35,36, 1:12)] #change order of variables according to cpp file
}

coefnames_R=coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35,36, 1:12)]


#number of constrained coefficients (24 in total)
nvar_c = 24

###Prior setting
Amu = diag(1/10, nrow = nvar_c, ncol = nvar_c)
mustarbarc = matrix(rep(0, nvar_c), nrow = nvar_c)
nu = 15 + nvar_c
V = nu * diag(nvar_c)*0.5

Prior = list(ncomp=1, Amu = Amu, mustarbarc = mustarbarc, nu = nu, V = V)
Mcmc = list(R=40000,keep=10)

  out_order = rhierMnlRwMixture_SR(Data=list(p=p,lgtdata=Road_R$lgtdata),
                                   Prior=Prior,Mcmc=Mcmc,nvar_c=nvar_c,flag="approx")

out=out_order

orderedbetadraw=out$betadraw

for (i in 1:dim(out$betadraw)[1]){
  for (r in 1:dim(out$betadraw)[3]){
    orderedbetadraw[i,,r]=startobeta(out$betadraw[i,,r])
  }
}


i=1; r=4000; round(cbind(out$betadraw[i,,r], startobeta(out$betadraw[i,,r])), digits = 2)

# Assign dimension names to betadraw
dim(out_order$betadraw)
dimnames(out_order$betadraw) <- list(
  NULL,             
  coefnames_R,      # Names for coefficients
  NULL              
)

hilfana <- function(outinput, coefnames, burnin, labels_map){
  
  # Assign dimension names using the provided coefnames
  dimnames(outinput$betadraw) <- list(NULL, coefnames, NULL)
  
  # 1. Plot Log-Likelihood Trace
  plot(outinput$loglike, type='l', main="Log-Likelihood Trace", 
       xlab="Iteration", ylab="Log-Likelihood")
  grid()
  
  # 2. Plot Specific Coefficient Trace for First 6 Individuals
  # Instead of hard-coding the coefficient index, specify the coefficient name
  # Assuming you want to plot the "Price" coefficient for individuals 1 to 6
  price_coef <- "Price"  # Update this if the coefficient name differs
  if(!price_coef %in% coefnames){
    warning(paste("Coefficient", price_coef, "not found in coefnames."))
  } else {
    par(mfrow=c(3,2))
    for (i in 1:6){
      plot(outinput$betadraw[i, price_coef, ], type='l', 
           ylab=labels_map[price_coef], main=paste("Individual", i))
      grid()
    }
    par(mfrow=c(1,1))  # Reset plotting layout
  }
  
  # 3. Define Coefficients of Interest
  # Update this list based on your model's coefficients
  coefficients_of_interest <- c(
    "Price", "50 Cash Back", "100 Cash Back", "150 Cash Back"
  )
  
  # Verify that all coefficients exist in coefnames
  missing_coefs <- setdiff(coefficients_of_interest, coefnames)
  if(length(missing_coefs) > 0){
    warning(paste("The following coefficients are missing in coefnames:", 
                  paste(missing_coefs, collapse=", ")))
  }
  
  # Filter out missing coefficients to avoid errors
  coefficients_present <- coefficients_of_interest[coefficients_of_interest %in% coefnames]
  
  # 4. Trace Plots for Coefficients of Interest
  par(mfrow=c(1,2))
  for (coef in coefficients_present){
    plot(outinput$betadraw[, coef, ], type='l',
         ylab=labels_map[coef],
         main=paste("Trace Plot:", labels_map[coef]))
    grid()
  }
  par(mfrow=c(1,1))  # Reset plotting layout
  
  # 5. Check Dimensions (For Debugging)
  print("Dimensions of betadraw:")
  print(dim(outinput$betadraw))  # Dimensions: individuals x coefficients x iterations
  
  # 6. Extract Post-Burn-in Samples
  total_iterations <- dim(outinput$betadraw)[3]
  if(burnin >= total_iterations){
    stop("Burn-in exceeds the number of MCMC iterations.")
  }
  betadrawconverged <- outinput$betadraw[,,(burnin+1):total_iterations]
  
  # 7. Density Plots for Coefficients of Interest
  par(mfrow=c(1,2))
  for (coef in coefficients_present){
    if(all(!is.na(betadrawconverged[, coef, ]))){
      plot(density(betadrawconverged[, coef, ]), 
           ylab=labels_map[coef], 
           main=paste("Density Plot:", labels_map[coef]))
      grid()
    } else {
      plot.new()
      title(main=paste("Density Plot:", labels_map[coef]))
      text(0.5, 0.5, "No Data Available")
    }
  }
  par(mfrow=c(1,1))  # Reset plotting layout
  
  # 8. Posterior Means for Specific Individuals (e.g., Individuals 10 and 11)
  individuals_of_interest <- c(10, 11)
  for (ind in individuals_of_interest){
    if(ind <= dim(betadrawconverged)[1]){
      posterior_mean <- rowMeans(betadrawconverged[ind, , ])
      cat("\nPosterior Mean for Individual", ind, ":\n")
      # Replace coefficient names with labels
      names(posterior_mean) <- labels_map[coefnames]
      print(round(posterior_mean, 2))
    } else {
      cat("\nIndividual", ind, "is out of bounds.\n")
    }
  }
  
  # 9. Reshape Beta Draws for Market Analysis
  betaexchange <- array(aperm(betadrawconverged, perm=c(1,3,2)),
                        dim=c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                              dim(betadrawconverged)[2]))
  
  colnames(betaexchange) <- coefnames
  
  # 10. Average Preferences and Heterogeneity
  mpref <- cbind(Mean = colMeans(betaexchange), SD = apply(betaexchange, 2, sd))
  
  # 11. Comparison to Aggregate Model
  # Ensure that 'out_Bayes$betadraw' has the same column names as 'coefnames'
  if(!exists("out_Bayes") || !("betadraw" %in% names(out_Bayes))){
    warning("out_Bayes$betadraw does not exist. Skipping comparison to aggregate model.")
    comppref <- NULL
  } else {
    if(!all(colnames(out_Bayes$betadraw) == coefnames)){
      warning("Column names of out_Bayes$betadraw do not match coefnames.")
    }
    
    comppref <- cbind(
      Hierarchical_Mean = mpref[, "Mean"],
      Aggregate_Mean = colMeans(out_Bayes$betadraw)
    )
    
    # Replace row names with labels
    rownames(comppref) <- labels_map[rownames(comppref)]
  }
  
  # 12. Calculate Willingness to Pay (WTP)
  if (!is.null(comppref) && "Price" %in% rownames(comppref)){
    # Extract Price coefficients for both models
    price_hb <- comppref["Price", "Hierarchical_Mean"]
    price_agg <- comppref["Price", "Aggregate_Mean"]
    
    # Attributes for WTP (excluding Price and Cashback)
    attributes_for_wtp <- setdiff(rownames(comppref), c("Price", "50 Cash Back", "100 Cash Back", "150 Cash Back"))
    
    # Verify that attributes_for_wtp exist in mpref
    missing_wtp_attrs <- setdiff(attributes_for_wtp, rownames(mpref))
    if(length(missing_wtp_attrs) > 0){
      warning(paste("The following attributes for WTP are missing in mpref:", 
                    paste(missing_wtp_attrs, collapse=", ")))
    }
    
    # Exclude any missing attributes
    attributes_for_wtp <- attributes_for_wtp[attributes_for_wtp %in% rownames(mpref)]
    
    # Calculate WTP for each attribute
    wtp_hb <- mpref[attributes_for_wtp, "Mean"] / -price_hb
    wtp_agg <- comppref[attributes_for_wtp, "Aggregate_Mean"] / -price_agg
    
    # Combine into a matrix
    compWTP <- round(cbind(Hierarchical_WTP = wtp_hb, Aggregate_WTP = wtp_agg), digits = 3)
    
    # Print WTP estimates
    cat("\nWillingness to Pay (WTP) Estimates:\n")
    print(compWTP)
  } else {
    warning("Price coefficient not found or comppref is NULL. WTP cannot be computed.")
    compWTP <- NULL
  }
  
  # 13. Preference Heterogeneity Plots for Specific Coefficients
  # Focus on Price and Cashback
  par(mfcol=c(2,2))
  
  # Density plot for Price
  price_coef <- "Price"
  if(price_coef %in% colnames(betaexchange)){
    plot(density(betaexchange[, price_coef]),
         main="Price Distribution",
         xlab="Price")
    grid()
  } else {
    plot.new()
    title(main="Price Distribution")
    text(0.5, 0.5, "No Data Available")
  }
  
  # Density plots for Cashback
  for (cashback_level in c("50 Cash Back", "100 Cash Back", "150 Cash Back")){
    if(cashback_level %in% colnames(betaexchange)){
      plot(density(betaexchange[, cashback_level]),
           main=paste(cashback_level, "Distribution"),
           xlab=cashback_level)
      grid()
    } else {
      plot.new()
      title(main=paste(cashback_level, "Distribution"))
      text(0.5, 0.5, "No Data Available")
    }
  }
  
  par(mfcol=c(1,1))  # Reset plotting layout
  
  # 14. Smooth Scatter: Price vs. Each Cashback Level
  par(mfcol=c(2,2))
  for (cashback_level in c("50 Cash Back", "100 Cash Back", "150 Cash Back")){
    if(all(cashback_level %in% colnames(betaexchange), "Price" %in% colnames(betaexchange))){
      plot(betaexchange[, "Price"], betaexchange[, cashback_level],
           main=paste("Price vs", cashback_level),
           xlab="Price",
           ylab=cashback_level)
      abline(lm(betaexchange[, cashback_level] ~ betaexchange[, "Price"]), col="red")
      grid()
    } else {
      plot.new()
      title(main=paste("Price vs", cashback_level))
      text(0.5, 0.5, "Insufficient Data")
    }
  }
  
  par(mfcol=c(1,1))  # Reset plotting layout
  
  # 15. Return Results
  return(list(betaexchange=betaexchange, 
              mpref=mpref, 
              comppref=comppref, 
              compWTP=compWTP))
}


labels_map_order <- c(
  "Price" = "Product Price",
  "Value Pack" = "Value Pack Offer",
  "S-phone Synch." = "Smartphone Synchronization",
  "8-12 h. Runtime" = "Battery Runtime (8-12 hours)",
  "Without SD-Slot" = "No SD Slot",
  "High Resolution (264 ppi)" = "High Resolution Display (264 ppi)",
  "16 GB" = "16 GB Storage",
  "32 GB" = "32 GB Storage",
  "64 GB" = "64 GB Storage",
  "128GB" = "128 GB Storage",
  "1.6 GHz" = "Processor Speed (1.6 GHz)",
  "2.2 GHz" = "Processor Speed (2.2 GHz)",
  "WLAN + UMTS/3G" = "WLAN + UMTS/3G Connectivity",
  "WLAN + LTE/4G" = "WLAN + LTE/4G Connectivity",
  "Cover" = "Protective Cover",
  "Keyboard" = "Built-in Keyboard",
  "Mouse" = "Built-in Mouse",
  "Pencil" = "Stylus Pencil",
  "32 GB Memory Card" = "32 GB Memory Card Included",
  "Keyboard + Pencil" = "Keyboard and Pencil Bundle",
  "Keyboard + Mouse + Pencil" = "Keyboard, Mouse, and Pencil Bundle",
  "50 Cash Back" = "50 Currency Cashback",
  "100 Cash Back" = "100 Currency Cashback",
  "150 Cash Back" = "150 Currency Cashback",
  "Brand 1" = "Brand 1",
  "Brand 2" = "Brand 2",
  "Brand 3" = "Brand 3",
  "Brand 4" = "Brand 4",
  "Brand 5" = "Brand 5",
  "Brand 6" = "Brand 6",
  "Brand 7" = "Brand 7",
  "System B" = "Operating System B",
  "8 Inches" = "8-Inch Display",
  "10 Inches" = "10-Inch Display",
  "12 Inches" = "12-Inch Display",
  "13 Inches" = "13-Inch Display"
)


hilf_order=hilfana(outinput=out_order,coefnames=coefnames_R,burnin = 1500, labels_map = labels_map_order)

for (m in 1:nrow(hilf_order$betaexchange)){
  hilf_order$betaexchange[m,] = startobeta(hilf_order$betaexchange[m,])
}

par(mfrow=c(2,2))
plot(density(hilf_constr$betaexchange[,33]),main='Price');grid()  ## price
plot(density(hilf_constr$betaexchange[,34]),main='50 CB');grid()  ## 50 CB
plot(density(hilf_constr$betaexchange[,35]),main='100 CB');grid()  ## 100 CB
plot(density(hilf_constr$betaexchange[,36]),main='150 CB');grid()  ## 150 CB

plot(density(hilf_order$betaexchange[,1]),main='cPrice');grid()  ## price
plot(density(hilf_order$betaexchange[,22]),main='c50 CB');grid()    ## 50 CB
plot(density(hilf_order$betaexchange[,23]),main='c100 CB');grid()  ## 100 CB
plot(density(hilf_order$betaexchange[,24]),main='c150 CB');grid()    ## 150 CB


par(mfrow=c(1,2))
smoothScatter(hilf_constr$betaexchange[,34],hilf_constr$betaexchange[,36],
              xlab = "50 CB",ylab = "150 CB");grid()
abline(0,1)
smoothScatter(hilf_order$betaexchange[,22],hilf_order$betaexchange[,24],
              xlab = "50 CB",ylab = "150 CB");grid()
abline(0,1)









# -------------------------------
# 12. Export Results (later)
# -------------------------------

# Save WTP summaries to CSV
write.csv(hilf_HB$compWTP, "WTP_Summary_Hierarchical.csv", row.names = TRUE)
write.csv(hilf_constr$compWTP, "WTP_Summary_Constrained.csv", row.names = TRUE)

# Save summary statistics
write.csv(bayes_summary, "Bayes_Summary.csv", row.names = TRUE)

# -------------------------------
# 13. Save Plots to Files (later)
# -------------------------------

# Example: Save Trace Plots for Unconstrained Model to PDF
pdf("Trace_Plots_Unconstrained.pdf")
par(mfrow=c(3,3))
for (coef in coefficients_present){
  plot(out_HB$betadraw[, coef, ], type='l',
       ylab=labels_map[coef],
       main=paste("Trace Plot:", labels_map[coef]))
  grid()
}
dev.off()

# -------------------------------
# 14. Final Notes
# -------------------------------



# -------------------------------
# End of Script
# -------------------------------




