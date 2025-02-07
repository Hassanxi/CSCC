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
  "bayesm", "pracma", "stats"
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
sample_fraction <- 0.7      # Proportion of individuals to sample
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
cat("Sampled Dataset Size:", length(E_Data_mod_sampled$lgtdata), "\n")  # Should be 70% of N

# Prepare data for modeling
Road <- E_Data_mod_sampled # Change to sample dataset if needed
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
if (!exists("out_Bayes")) {
out_Bayes <- rmnlIndepMetrop(
  Data = list(y = yforBayes, X = as.matrix(XforBayes), p = Road$p),
  Mcmc = list(R = 200000, keep = 40)
)
}

# Assign column names for 'betadraw' to match 'XforBayes'
colnames(out_Bayes$betadraw) <- colnames(XforBayes)
coefnames <- colnames(XforBayes)  # Extract coefficient names

# Summarize aggregate model results
cat("\nSummary of Betadraw (Aggregate Bayesian Estimation):\n")
print(summary(out_Bayes$betadraw))

plot(out_Bayes$loglike)

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

#save(out_Bayes, file = "out_Bayes_Sampled_100k.RData") 
# -------------------------------
# 6. Define Sign Restrictions
# -------------------------------

# Initialize all coeefficients as unconstrained
PriceSignRes <- rep(0, ncol(Road$lgtdata[[1]]$X))

#Assign Price restriction
PriceSignRes[which(coefnames == "Price")] <- -1                     # Price should decrease utility

# Initialize all coefficients as unconstrained
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

# Run Unconstrained Hierarchical Mixed Logit Model 277 min
if (!exists("out_HB")) {
out_HB <- rhierMnlRwMixture(
  Data = list(lgtdata = Road$lgtdata, p = Road$p),
  Prior = list(ncomp = 1),
  Mcmc = list(R = 200000, keep = 40)
)
}
quartz()
plot(out_HB$loglike)

# Run Price Sign Constrained Hierarchical Mixed Logit Model 144min
if (!exists("price_HB_constr")) {
price_HB_constr <- rhierMnlRwMixture(
  Data = list(lgtdata = Road$lgtdata, p = Road$p),
  Prior = list(ncomp = 1, SignRes = PriceSignRes),
  Mcmc = list(R = 100000, keep = 20)
)
}

quartz()
plot(price_HB_constr$loglike)

# Run All Two-Level Factor Sign Constrained Hierarchical Mixed Logit Model
if (!exists("out_HB_constr")) {
out_HB_constr <- rhierMnlRwMixture(
  Data = list(lgtdata = Road$lgtdata, p = Road$p),
  Prior = list(ncomp = 1, SignRes = SignRes),
  Mcmc = list(R = 150000, keep = 30)
)
}

plot(out_HB_constr$loglike)


#save(out_HB, file = "out_HB_700Sampled_200k_keep40.RData") 
#save(price_HB_constr, file = "price_HB_constr_700Sampled_100k_keep20.RData") 
#save(out_HB_constr, file = "out_HB_constr_700Sampled_150k_keep30.RData") 
#load(out_HB_Sampled_50k.RData)
#load(price_HB_constr_Sampled_25k.RData)
#load(out_HB_constr_Sampled_25k.RData)

#save(out_BC, file = "out_BC_700Sampled_40k_keep8.RData") 
# -------------------------------
# 8. Define the `hilfana` Function 
# -------------------------------

hilfana <- function(outinput,coefnames,burnin, labels){
  
  #quartz()
  plot(outinput$loglike);grid()
  
  #quartz()
  par(mfrow=c(3,3))
  for (i in 1:9){
    plot(outinput$betadraw[i,33,],type='l', ylab=coefnames[33], main=i);grid() # 33 -> price 
  }
  
  dim(outinput$betadraw)
  
  betadrawconverged=outinput$betadraw[,,(burnin+1):dim(outinput$betadraw)[3]]
  
  #quartz()
  par(mfrow=c(3,3))
  for (i in 1:9){
    plot(density(outinput$betadraw[i,33,]), ylab=coefnames[33], main=i);grid() # 33 -> price 
  }
  
  
  dimnames(betadrawconverged) = 
    list(NULL,as.list(coefnames),NULL)
  
  #####preferences of example individuals (posterior means) 
  print(round(cbind(rowMeans(betadrawconverged[10,,]), # ind -> 10 
                    rowMeans(betadrawconverged[11,,])),digits = 2)) # ind -> 10 
  
  
  betaexchange <- array(aperm(betadrawconverged, perm=c( 1 , 3 , 2 )),
                        dim=c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                              dim (betadrawconverged)[2]))
  
  
  colnames(betaexchange) <- coefnames
  
  ####average preferences and heterogeneity in the market
  
  mpref=cbind(colMeans(betaexchange),apply(betaexchange,2,sd))
  
  
  ####comparison to aggregate model
  
  comppref=cbind(mpref[,1],colMeans(out_Bayes$betadraw))
  #comppref
  
  
  ####mean utility in mean money utility units
  compWTP=round(comppref/-t(array(comppref[33,],dim=c(2,nrow(comppref)))),digits = 3) # comppref[x,] x always var where price is located 
  
  
  ####preference heterogeneity in the market
  #quartz()
  #par(mfcol=c(1,3))
  #plot(density(betaexchange[,34]),main=coefnames[34],xlab = coefnames[34] )  
  #plot(density(betaexchange[,35]), main=coefnames[35],xlab = coefnames[35])
  #plot(density(betaexchange[,36]), main=coefnames[36],xlab = coefnames[36])
  
  return(list(betaexchange=betaexchange, mpref=mpref, comppref=comppref, compWTP=compWTP))
} # get graphs, betaexchange, mpref, compref, and compWTP objetcs


# -------------------------------
# 9. Create `labels`
# -------------------------------

# if necessary map more descriptive labels for final analysis
labels <- setNames(coefnames, coefnames)

# -------------------------------
# 10. Run the `hilfana` Function for all three Models
# -------------------------------

# Run the hilfana function for the Unconstrained Model
hilf_HB <- hilfana(
  outinput = out_HB, 
  coefnames = coefnames, 
  burnin = 1500, 
  labels = labels
)

# Run the hilfana function for the Price constrained only Model
hilf_priceHB <- hilfana(
  outinput = price_HB_constr, 
  coefnames = coefnames, 
  burnin = 1000, 
  labels = labels
)

# Run the hilfana function for the Sign-Constrained Model
hilf_constr <- hilfana(
  outinput = out_HB_constr, 
  coefnames = coefnames, 
  burnin = 500, 
  labels = labels
)


# -------------------------------
# 11. Order Constraints 
# -------------------------------

# Source the C++ function for estimation
Rcpp::sourceCpp("~/Desktop/Master/3. Semester/CSCC/MArket Simulation/rhierMnlRwMixture_rcpp_loop_Sim_modHP.cpp", showOutput = FALSE)
source('~/Desktop/Master/3. Semester/rhierMnlRwMixture_main.R')

# Check the current order of coefficients
unlist(coefnames)

# Create a copy of the data for reordering
Road_R <- Road

# Display the first two rows of the design matrix for the first individual
Road$lgtdata[[1]]$X[1:2, ]
Road_R$lgtdata[[1]]$X[1:2, ]

# Reorder variables in the design matrix for all individuals
for (i in 1:length(Road$lgtdata)) {
  Road_R$lgtdata[[i]]$X <- Road$lgtdata[[i]]$X[, c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
}

# Reorder coefficient names to match the new variable order
coefnames_R <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]

# Number of constrained coefficients (24 in total)
nvar_c <- 24

# Define prior settings
Amu <- diag(1/10, nrow = nvar_c, ncol = nvar_c)
mustarbarc <- matrix(rep(0, nvar_c), nrow = nvar_c)
nu <- 15 + nvar_c
V <- nu * diag(nvar_c) * 0.5

# Combine prior settings into a list
Prior <- list(ncomp = 1, Amu = Amu, mustarbarc = mustarbarc, nu = nu, V = V)

# Define MCMC settings
Mcmc <- list(R = 300000, keep = 60)

# Run the hierarchical model if `out_order` does not already exist #277 min
if (!exists("out_order")) {
  out_order <- rhierMnlRwMixture_SR_ord(
    Data = list(p = p, lgtdata = Road_R$lgtdata),
    Prior = Prior,
    Mcmc = Mcmc,
    nvar_c = nvar_c,
    flag = "approx"
  )
}

#save(out_order, file = "out_order_700Sampled_300k_keep60.RData") 

# Plot the log-likelihood to check convergence
quartz()
plot(out_order$loglike)


# Assign the output to a variable for further processing
out <- out_order

# Extract the betadraws from out_order
betadraws <- out$betadraw

# Initialize an array to store the transformed betas
orderedbetadraw <- array(NA, dim = dim(betadraws))

# Loop through each draw and apply the `startobeta` transformation
for (i in 1:dim(betadraws)[1]) {
  for (r in 1:dim(betadraws)[3]) {
    orderedbetadraw[i, , r] <- startobeta(betadraws[i, , r])
  }
}

# Assign dimension names to the transformed betadraw
dimnames(orderedbetadraw) <- list(
  NULL,             
  coefnames_R,      # Names for coefficients
  NULL              
)

# Assign the transformed betas back to out_order
out$betadraw <- orderedbetadraw

# Check the transformation for the first individual and first draw
original_beta <- betadraws[1, , 1]
transformed_beta <- orderedbetadraw[1, , 1]

# Print the original and transformed betas
print("Original Beta:")
print(original_beta)

print("Transformed Beta:")
print(transformed_beta)

# Check if price coefficient is negative/the transformation applied. 
if (transformed_beta[which(coefnames_R == "Price")] < 0) {
  print("Price constraint is satisfied.")
} else {
  print("Price constraint is NOT satisfied.")
}



hilfana_order <- function(outinput,coefnames,burnin, labels){
  
  #quartz()
  plot(outinput$loglike);grid()
  
  #quartz()
  par(mfrow=c(3,3))
  for (i in 1:9){
    plot(outinput$betadraw[i,1,],type='l', ylab=coefnames[1], main=i);grid() # 1 -> price 
  }
  
  dim(outinput$betadraw)
  
  betadrawconverged=outinput$betadraw[,,(burnin+1):dim(outinput$betadraw)[3]]
  
  #quartz()
  par(mfrow=c(3,3))
  for (i in 1:9){
    plot(density(outinput$betadraw[i,1,]), ylab=coefnames[1], main=i);grid() # 1 -> price 
  }
  
  
  dimnames(betadrawconverged) = 
    list(NULL,as.list(coefnames),NULL)
  
  #####preferences of example individuals (posterior means) 
  print(round(cbind(rowMeans(betadrawconverged[10,,]), # ind -> 10 
                    rowMeans(betadrawconverged[11,,])),digits = 2)) # ind -> 10 
  
  
  betaexchange <- array(aperm(betadrawconverged, perm=c( 1 , 3 , 2 )),
                        dim=c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                              dim (betadrawconverged)[2]))
  
  
  colnames(betaexchange) <- coefnames
  
  ####average preferences and heterogeneity in the market
  
  mpref=cbind(colMeans(betaexchange),apply(betaexchange,2,sd))
  
  
  ####comparison to aggregate model
  
  comppref=cbind(mpref[,1],colMeans(out_Bayes$betadraw))
  #comppref
  
  
  ####mean utility in mean money utility units
  compWTP=round(comppref/-t(array(comppref[1,],dim=c(2,nrow(comppref)))),digits = 3) # comppref[x,] x always var where price is located 
  
  
  ####preference heterogeneity in the market
  #quartz()
  #par(mfcol=c(1,3))
  #plot(density(betaexchange[,34]),main=coefnames[34],xlab = coefnames[34] )  
  #plot(density(betaexchange[,35]), main=coefnames[35],xlab = coefnames[35])
  #plot(density(betaexchange[,36]), main=coefnames[36],xlab = coefnames[36])
  
  return(list(betaexchange=betaexchange, mpref=mpref, comppref=comppref, compWTP=compWTP))
} # get graphs, betaexchange, mpref, compref, and compWTP objetcs



hilf_order=hilfana_order(outinput=out,coefnames=coefnames_R,burnin = 4000)

#we have already startobeta transformed before
#for (m in 1:nrow(hilf_order$betaexchange)){
#  hilf_order$betaexchange[m,] = startobeta(hilf_order$betaexchange[m,])
#}

par(mfrow=c(2,2))
plot(density(hilf_constr$betaexchange[,33]),main='Price');grid()  ## price
plot(density(hilf_constr$betaexchange[,34]),main='50 CB');grid()  ## 50 CB
plot(density(hilf_constr$betaexchange[,35]),main='100 CB');grid()  ## 100 CB
plot(density(hilf_constr$betaexchange[,36]),main='150 CB');grid()  ## 150 CB

plot(density(hilf_order$betaexchange[,1]),main='cPrice');grid()  ## price
plot(density(hilf_order$betaexchange[,22]),main='c50 CB');grid()    ## 50 CB
plot(density(hilf_order$betaexchange[,23]),main='c100 CB');grid()  ## 100 CB
plot(density(hilf_order$betaexchange[,24]),main='c150 CB');grid()    ## 150 CB




#####################
## Budget Constraint
#####################


#library(devtools)
#library(Rcpp)
#library(RcppArmadillo)
#library(bayesm)
#library(pracma) # required for approximating Jacobian

###Source cpp function for estimation
Rcpp::sourceCpp("/Users/helua1/Desktop/Master/3. Semester/CSCC/Budget Constraint/rhierMnlRwMixture_rcpp_loop_Illus_BLP_type.cpp",showOutput = FALSE)
Rcpp::sourceCpp("/Users/helua1/Desktop/Master/3. Semester/CSCC/Budget Constraint/Speed++_MS_BC_BLP_Efficient.cpp",showOutput = FALSE)
source('/Users/helua1/Desktop/Master/3. Semester/CSCC/Budget Constraint/rhierMnlRwMixture_main_untuned_BC.R')

# Check the current order of coefficients
unlist(coefnames)

# Create a copy of the data for reordering
Road_BC <- Road


# Reorder variables in the design matrix for all individuals
for (i in 1:length(Road$lgtdata)) {
  Road_BC$lgtdata[[i]]$X <- Road$lgtdata[[i]]$X[, c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
}

# Display the first two rows of the design matrix for the first individual
Road$lgtdata[[1]]$X[1:2, ]
Road_BC$lgtdata[[1]]$X[1:2, ]


## Prepare data for BC modeling w BLP (1995) 
coefnames_BC <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
coefnames_BC=c('log_budget', coefnames_BC) # add log_budget to the beginning
coefnames_BC[[2]] <- 'log-alpha' # change price name to log-alpha 




# Number of constrained coefficients (24 in total + budget = 25)
nvar_c <- 25

# Define prior settings
Amu <- diag(1/10, nrow = nvar_c, ncol = nvar_c)
mustarbarc <- matrix(rep(0, nvar_c), nrow = nvar_c)
nu <- 15 + nvar_c
V <- nu * diag(nvar_c) * 0.5

###Prior setting
#position of price coefficient in design matrix
pr=1


# Combine prior settings into a list
Prior = list(ncomp=1, Amu = Amu, mustarbarc = mustarbarc, nu = nu, V = V)

# Define MCMC settings
Mcmc = list(R=400000, keep=8)

# Run the hierarchical model if `out_BC` does not already exist 
if (!exists("out_BC")) {
  out_BC = rhierMnlRwMixture_SR(Data=list(p=p,lgtdata=Road_BC$lgtdata),
                                Prior=Prior,Mcmc=Mcmc,nvar_c=nvar_c,pr=pr,
                                starting_budget = log(11.00))
}


## convergence of log-buget population mean and variance in the population ncomp = 1
## convergence of log-buget population mean and variance in the population ncomp = 1
## convergence of log-buget population mean and variance in the population ncomp = 1
out_postHB=array(double(Mcmc$R/Mcmc$keep*37*2), dim = c(Mcmc$R/Mcmc$keep,37,2))
varlogB=array(double(Mcmc$R/Mcmc$keep*37*2), dim = c(Mcmc$R/Mcmc$keep,37,2))
for (m in 1:(Mcmc$R/Mcmc$keep)){
  out_postHB[m,,1]=out_BC$nmix$compdraw[[m]][[1]]$mustar
  #out_postHB[m,,2]=out_nBC$nmix$compdraw[[m]][[1]]$mustar
  varlogB[m,,1]=var(out_BC$betadraw[,1,m])
  #varlogB[m,,2]=var(out_nBC$betadraw[,1,m])
}




# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot 1: Log-Budget Mean using the first draw for parameter 1 from out_postHB.
# (Assumes out_postHB is a 3D array: [observations, parameters, draws])
matplot(out_postHB[, 1, 1, drop = FALSE],
        type = 'l',
        main = 'Log-Budget Mean',
        xlab = 'Observation',
        ylab = 'Value')


# Plot 2: Zoomed-in Log-Budget Mean.
# Here we plot two columns (e.g., the first two draws) for observations 2001 to N.
n_obs <- dim(out_postHB)[1]
matplot(out_postHB[2001:n_obs, 1, 1:2],
        type = 'l',
        main = 'Log-Budget Mean (Zoomed)',
        xlab = 'Observation (2001 to N)',
        ylab = 'Value')


# Plot 3: Mean of Parameter 1 across all draws.
# Compute the mean across the third dimension (draws) for parameter 1.
mean_param1 <- rowMeans(out_postHB[, 1, ])
matplot(mean_param1,
        type = 'l',
        main = 'Mean of Parameter 1 Across Draws',
        xlab = 'Observation',
        ylab = 'Mean Value')


# Plot 4: Additional Plot: For example, the standard deviation of Parameter 1 across draws.
sd_param1 <- apply(out_postHB[, 1, ], 1, sd)
matplot(sd_param1,
        type = 'l',
        main = 'Std. Dev. of Parameter 1 Across Draws',
        xlab = 'Observation',
        ylab = 'Standard Deviation')





hilfana_BC <- function(outinput, coefnames, burnin) {
  
  
  plot(outinput$loglike)
  
  
  # Plot 1: Budget values for first parameter across draws

  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 1, ])
    if (length(x[is.finite(x)]) == 0) {
      cat("Skipping plot 'budget' for index", i, "\n")
      next
    }
    plot(x, type = 'l', main = 'budget')
    
  }
  
  # Plot 2: Density of budget values after burnin

  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 1, (burnin + 1):dim(outinput$betadraw)[3]])
    if (length(x[is.finite(x)]) == 0) {
      cat("Skipping density plot 'budget' for index", i, "\n")
      next
    }
    plot(density(x), type = 'l', main = 'budget')

  }
  
  # Plot 3: Alpha values for second parameter across draws

  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 2, ])
    if (length(x[is.finite(x)]) == 0) {
      cat("Skipping plot 'alpha' for index", i, "\n")
      next
    }
    plot(x, type = 'l', main = 'alpha')

  }
  
  # Plot 4: Density of alpha values after burnin

  par(mfrow = c(3, 3))
  for (i in 1:9) {
    x <- exp(outinput$betadraw[i, 2, (burnin + 1):dim(outinput$betadraw)[3]])
    if (length(x[is.finite(x)]) == 0) {
      cat("Skipping density plot 'alpha' for index", i, "\n")
      next
    }
    plot(density(x), type = 'l', main = 'alpha')

  }
  
  # Check dimensions and extract converged draws
  print(dim(outinput$betadraw))
  betadrawconverged <- outinput$betadraw[, , (burnin + 1):dim(outinput$betadraw)[3]]
  dimnames(betadrawconverged) <- list(NULL, as.list(coefnames), NULL)
  
  # Print preferences for two example individuals
  print(round(cbind(rowMeans(betadrawconverged[10, , ]), 
                    rowMeans(betadrawconverged[11, , ])), digits = 2))
  
  # Rearrange draws: combine individuals and draws
  betaexchange <- array(aperm(betadrawconverged, perm = c(1, 3, 2)),
                        dim = c(dim(betadrawconverged)[1] * dim(betadrawconverged)[3],
                                dim(betadrawconverged)[2]))
  
  par(mfrow = c(1, 1))
  x2 <- exp(betaexchange[, 1])
  if (length(x2[is.finite(x2)]) == 0) {
    cat("Skipping final density plot because data is non-finite.\n")
  } else {
    # After computing `x2 <- exp(betaexchange[, 1])`:
    par(mfrow = c(1, 1))
    x2 <- exp(betaexchange[, 1])
    if (length(x2[is.finite(x2)]) == 0) {
      cat("Skipping final density plot because data is non-finite.\n")
    } else 
      # 1) Compute density
      d <- density(x2)
      
      # 2) Compute quartiles
      qvals <- quantile(x2, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
      q1 <- qvals[1]
      q2 <- qvals[2]
      q3 <- qvals[3]
      
      # 3) Decide on a zoom range for the x-axis
      #    For example, we can cap the x-axis at 20 or 30 to "zoom in" on the main mass.
      x_max <- 20  # adjust as you like
      
      # 4) Plot the density line first with xlim up to x_max
      plot(d,
           main = paste("budget in population,",
                        round(mean(x2 < 8.99) * 100, 1), 
                        "% below 899 EUR"),
           xlab = "Budget",
           xlim = c(0, x_max),
           ylim = c(0, max(d$y[d$x <= x_max]) * 1.1))  # small headroom above max y
      
      # 5) Helper function to fill a polygon between [xrange[1], xrange[2]] 
      #    under the density curve, using the chosen color.
      fill_area <- function(xrange, dens, col) {
        idx <- dens$x >= xrange[1] & dens$x <= xrange[2]
        if (any(idx)) {
          polygon(
            x = c(dens$x[idx], rev(dens$x[idx])),
            y = c(dens$y[idx], rep(0, sum(idx))),
            col = col,
            border = NA
          )
        }
      }
      
      # 6) Shade each quartile region in a different shade of green (lightest to darkest).
      #    We do this *after* plotting to get the polygons behind the density line:
      
      # min -> Q1
      fill_area(c(min(d$x), min(q1, x_max)), d, col = "palegreen1")
      
      # Q1 -> Q2
      fill_area(c(q1, min(q2, x_max)), d, col = "palegreen2")
      
      # Q2 -> Q3
      fill_area(c(q2, min(q3, x_max)), d, col = "palegreen3")
      
      # Q3 -> x_max
      fill_area(c(q3, x_max), d, col = "palegreen4")
      
      # 7) Re-draw the density line on top so it's visible
      lines(d, col = "black")
      
      # 8) Draw a vertical red line at 8.99
      abline(v = 8.99, col = "red", lwd = 2)
  }
  
  colnames(betaexchange) <- coefnames
  
  # Compute average preferences and heterogeneity in the market
  mpref <- cbind(colMeans(betaexchange), apply(betaexchange, 2, sd))
  
  # Apply transformation using startobeta() to each row (assuming startobeta is defined)
  for (m in 1:nrow(betaexchange)) {
    betaexchange[m, ] <- startobeta(betaexchange[m, ])
  }
  
  return(list(betaexchange = betaexchange, mpref = mpref))
}


hilf_BC=hilfana_BC(outinput=out_BC,coefnames=coefnames_BC,burnin = 1000)



summary(hilf_BC$betaexchange)

#####################


################

summarize_hb_model <- function(model_output,
                               coefnames,
                               burnin = 1000,      # default
                               price_var = "Price" # name of price attribute in coefnames or coefnames_R
) {
  # 1) Extract draws and discard burn-in
  ndraws_total <- dim(model_output$betadraw)[3]  # total MCMC draws
  used_draws <- (burnin+1):ndraws_total
  betadraw_post <- model_output$betadraw[,,used_draws]
  
  # 2) Flatten into a big matrix: rows = individual-draw combos, cols = attributes
  #    Dimensions after flattening: (#Individuals * #Post-burnin draws) x (#Attributes)
  betadraw_flat <- aperm(betadraw_post, c(1, 3, 2))  
  dim(betadraw_flat) <- c(dim(betadraw_post)[1]*dim(betadraw_post)[3],
                          dim(betadraw_post)[2])
  colnames(betadraw_flat) <- coefnames
  
  # 3) Compute summary stats across all flattened draws
  mean_vals <- colMeans(betadraw_flat)
  sd_vals   <- apply(betadraw_flat, 2, sd)
  q025      <- apply(betadraw_flat, 2, quantile, probs = 0.025)
  q50       <- apply(betadraw_flat, 2, quantile, probs = 0.50)
  q975      <- apply(betadraw_flat, 2, quantile, probs = 0.975)
  
  # 4) Put results in a data.frame
  summary_df <- data.frame(
    Attribute = coefnames,
    Mean      = mean_vals,
    SD        = sd_vals,
    Q2.5      = q025,
    Q50       = q50,
    Q97.5     = q975,
    row.names = NULL
  )
  
  # 5) Compute WTP if price_var is in the coefficients
  #    WTP(X) = - (beta_X / beta_price)  (assuming linear price with a negative coefficient)
  if (price_var %in% coefnames) {
    price_mean <- mean_vals[price_var]
    summary_df$WTP <- - (summary_df$Mean / price_mean)
  } else {
    summary_df$WTP <- NA
    warning("Price variable not found in coefnames; WTP column set to NA.")
  }
  
  # 6) Return the summary table + the flattened draws
  return(list(summary_table = summary_df,
              betadraw_flat = betadraw_flat))
}

# for Unconstrained Model
res_unconstr <- summarize_hb_model(
  model_output = out_HB,
  coefnames    = coefnames,
  burnin       = 2500  # or whichever burn-in you choose
)

# The result is a list: res_unconstr$summary_table and res_unconstr$betadraw_flat
head(res_unconstr$summary_table, 10)  # Inspect the first 10 rows

res_price    <- summarize_hb_model(price_HB_constr, coefnames, burnin = 1000)
#res_sign     <- summarize_hb_model(out_HB_constr,   coefnames, burnin = 300)


# Summarize Order-Constrained Model
# Note: coefnames_R has a different order; Price is the first variable
res_order_const <- summarize_hb_model(
  model_output = out,
  coefnames    = coefnames_R,
  burnin       = 1500,
  price_var    = "Price"    # now first variable
)

align_summary_table <- function(reference_summary, target_summary) {
  target_summary[match(reference_summary$Attribute, target_summary$Attribute), ]
}

# Apply this function before comparison
res_order_const$summary_table <- align_summary_table(res_unconstr$summary_table, res_order_const$summary_table)


### --- 2) Quick side-by-side comparison of means for key attributes ---
compare_means <- data.frame(
  Attribute          = res_unconstr$summary_table$Attribute,
  Unconstrained_Mean = res_unconstr$summary_table$Mean,
  PriceConst_Mean    = res_price$summary_table$Mean,
  #SignConst_Mean     = res_sign$summary_table$Mean,
  OrderConst_Mean     = res_order_const$summary_table$Mean
)
print(compare_means)

### --- 3) Compare WTP for those same attributes ---
compare_wtp <- data.frame(
  Attribute           = res_unconstr$summary_table$Attribute,
  Unconstrained_WTP   = res_unconstr$summary_table$WTP,
  PriceConst_WTP      = res_price$summary_table$WTP,
  #SignConst_WTP       = res_sign$summary_table$WTP,
  OrderConst_WTP       = res_order_const$summary_table$WTP
  
)
print(compare_wtp)



# Convert each model’s price draws into a separate data frame
df_price_unconstr <- data.frame(Price_Coefficient = res_unconstr$betadraw_flat[,"Price"], Model = "Unconstrained")
df_price_priceC   <- data.frame(Price_Coefficient = res_price$betadraw_flat[,"Price"], Model = "Price-Constrained")
#df_price_signC    <- data.frame(Price_Coefficient = res_sign$betadraw_flat[,"Price"], Model = "Sign-Constrained")
df_price_orderC   <- data.frame(Price_Coefficient = res_order_const$betadraw_flat[,"Price"], Model = "Order-Constrained")


# Combine into a long format data frame
df_price_melt <- rbind(df_price_unconstr, df_price_priceC, df_price_orderC)

# Plot separate densities for each model
ggplot(df_price_melt, aes(x = Price_Coefficient, fill = Model)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~Model, scales = "free") +  # Separate plots per model
  labs(title = "Comparison of Price Coefficients Across Models",
       x = "Price Coefficient",
       fill = "Model") +
  theme_minimal()



# For Unconstrained Model
burnin <- 1500
betadraw_post_unconstr <- out_HB$betadraw[,,(burnin+1):dim(out_HB$betadraw)[3]]
individual_means_unconstr <- apply(betadraw_post_unconstr, c(1,2), mean)  # dims: (# individuals) x (# attributes)
colnames(individual_means_unconstr) <- coefnames

# Histogram of individual-level Price coefficients
hist(individual_means_unconstr[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Unconstrained)",
     xlab = "Price Coefficient",
     col = "skyblue", border = "white")


# For Price Sign Constrained Model
betadraw_post_price_const <- price_HB_constr$betadraw[,,(burnin+1):dim(price_HB_constr$betadraw)[3]]
individual_means_price_const <- apply(betadraw_post_price_const, c(1,2), mean)
colnames(individual_means_price_const) <- coefnames

hist(individual_means_price_const[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Price-Constrained)",
     xlab = "Price Coefficient",
     col = "salmon", border = "white")

# For Sign Constrained Model
#betadraw_post_sign_const <- out_HB_constr$betadraw[,,(burnin+1):dim(out_HB_constr$betadraw)[3]]
#individual_means_sign_const <- apply(betadraw_post_sign_const, c(1,2), mean)
#colnames(individual_means_sign_const) <- coefnames

#hist(individual_means_sign_const[,"Price"], 
#     main = "Distribution of Individual-Level Price Coefficients (Sign-Constrained)",
#     xlab = "Price Coefficient",
#     col = "violet", border = "white")

# For Order Constrained Model
betadraw_post_order_const <- out$betadraw[,,(burnin+1):dim(out$betadraw)[3]]
individual_means_order_const <- apply(betadraw_post_order_const, c(1,2), mean)
colnames(individual_means_order_const) <- coefnames_R

hist(individual_means_order_const[,"Price"], 
     main = "Distribution of Individual-Level Price Coefficients (Order-Constrained)",
     xlab = "Price Coefficient",
     col = "orange", border = "white")





# Suppose res_order_const$betadraw_flat is the matrix of posterior draws.
# You can select the relevant columns as follows:
price_draws    <- selected_betamix[, "Price"]
cb50_draws     <- res_order_const$betadraw_flat[, "50 Cash Back"]
cb100_draws    <- res_order_const$betadraw_flat[, "100 Cash Back"]
cb150_draws    <- res_order_const$betadraw_flat[, "150 Cash Back"]

# Compute summary statistics:
summary_stats <- function(x) {
  list(
    mean   = mean(x),
    median = median(x),
    min    = min(x),
    max    = max(x)
  )
}

price_summary  <- summary_stats(price_draws)
cb50_summary   <- summary_stats(cb50_draws)
cb100_summary  <- summary_stats(cb100_draws)
cb150_summary  <- summary_stats(cb150_draws)

cat("Price Coefficient Summary:\n")
print(price_summary)
cat("\n50 Cash Back Coefficient Summary:\n")
print(cb50_summary)
cat("\n100 Cash Back Coefficient Summary:\n")
print(cb100_summary)
cat("\n150 Cash Back Coefficient Summary:\n")
print(cb150_summary)


summarize_hb_BC_model <- function(model_output,
                                  coefnames,
                                  burnin = 1000,      # adjust burnin as appropriate
                                  price_var = "log-alpha"  # the name for the price coefficient in your BC model (e.g., 'log-alpha')
) {
  # 1) Extract draws and discard burn-in:
  ndraws_total <- dim(model_output$betadraw)[3]  # total MCMC draws
  used_draws <- (burnin + 1):ndraws_total
  betadraw_post <- model_output$betadraw[,, used_draws]
  
  # 2) Flatten into a big matrix:
  betadraw_flat <- aperm(betadraw_post, c(1, 3, 2))
  dim(betadraw_flat) <- c(dim(betadraw_post)[1] * dim(betadraw_post)[3],
                          dim(betadraw_post)[2])
  colnames(betadraw_flat) <- coefnames
  
  # 3) Compute summary statistics across all flattened draws:
  mean_vals <- colMeans(betadraw_flat)
  sd_vals   <- apply(betadraw_flat, 2, sd)
  q025      <- apply(betadraw_flat, 2, quantile, probs = 0.025)
  q50       <- apply(betadraw_flat, 2, quantile, probs = 0.50)
  q975      <- apply(betadraw_flat, 2, quantile, probs = 0.975)
  
  summary_df <- data.frame(
    Attribute = coefnames,
    Mean      = mean_vals,
    SD        = sd_vals,
    Q2.5      = q025,
    Q50       = q50,
    Q97.5     = q975,
    row.names = NULL
  )
  
  # 4) Compute WTP if price_var is found.
  if (price_var %in% coefnames) {
    price_mean <- mean_vals[price_var]
    summary_df$WTP <- - (summary_df$Mean / price_mean)
  } else {
    summary_df$WTP <- NA
    warning("Price variable not found in coefnames; WTP column set to NA.")
  }
  
  # 5) Return the summary table and flattened draws.
  return(list(summary_table = summary_df,
              betadraw_flat = betadraw_flat))
}

################




summarize_hb_BC_model <- function(model_output,
                                  coefnames,
                                  burnin = 1000,      # adjust as appropriate
                                  price_var = "log-alpha"  # the name for the price coefficient in your BC model
) {
  # 1) Extract draws and discard burn-in:
  ndraws_total <- dim(model_output$betadraw)[3]  # total MCMC draws
  used_draws <- (burnin + 1):ndraws_total
  betadraw_post <- model_output$betadraw[,, used_draws]
  
  # 2) Flatten into a 2D matrix:
  betadraw_flat <- aperm(betadraw_post, c(1, 3, 2))
  dim(betadraw_flat) <- c(dim(betadraw_post)[1] * dim(betadraw_post)[3],
                          dim(betadraw_post)[2])
  colnames(betadraw_flat) <- coefnames
  
  # 3) Apply startobeta transformation to each draw (row) so that the parameters are on the constrained scale.
  #    This assumes that the function startobeta() is available.
  betadraw_transformed <- t(apply(betadraw_flat, 1, startobeta))
  
  # 4) Compute summary statistics:
  mean_vals <- colMeans(betadraw_transformed)
  sd_vals   <- apply(betadraw_transformed, 2, sd)
  q025      <- apply(betadraw_transformed, 2, quantile, probs = 0.025)
  q50       <- apply(betadraw_transformed, 2, quantile, probs = 0.50)
  q975      <- apply(betadraw_transformed, 2, quantile, probs = 0.975)
  
  summary_df <- data.frame(
    Attribute = coefnames,
    Mean      = mean_vals,
    SD        = sd_vals,
    Q2.5      = q025,
    Q50       = q50,
    Q97.5     = q975,
    row.names = NULL
  )
  
  # 5) Compute WTP if the price variable is in the coefficients:
  if (price_var %in% coefnames) {
    price_mean <- mean_vals[price_var]
    summary_df$WTP <- - (summary_df$Mean / price_mean)
  } else {
    summary_df$WTP <- NA
    warning("Price variable not found in coefnames; WTP column set to NA.")
  }
  
  # 6) Return the summary table and the transformed flattened draws.
  return(list(summary_table = summary_df,
              betadraw_flat = betadraw_transformed))
}





res_BC <- summarize_hb_BC_model(
  model_output = out_BC,       # out_BC is your BC model output from rhierMnlRwMixture_SR
  coefnames    = coefnames_BC,  # coefnames_BC is your modified coefficient names vector for BC (with "log_budget" and "log-alpha")
  burnin       = 45000,          # adjust as appropriate
  price_var    = "log-alpha"   # ensure this matches the name in coefnames_BC for the price parameter
)

# Inspect the first few rows of the summary:
head(res_BC$summary_table, 37)

