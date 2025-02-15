###############################################################################
## 2. Define Products for MarketSim (Including an “Outside” Base)
###############################################################################

### 1. Coefficient Names
# (Assume these are defined from your estimation. For example:)
# coefnames <- colnames(E_Data_mod$lgtdata[[1]]$X)
# For our purposes, we assume that coefnames is already defined.

### 2. Define Attribute Matrices
## 2.1 Brand Matrix (Brand_all)
Brand_all <- matrix(c(
  0,0,0,0,0,0,0,         # Outside (Row 1)
  1,0,0,0,0,0,0,         # Brand 1 (Row 2)
  0,1,0,0,0,0,0,         # Brand 2 (Row 3)
  0,0,1,0,0,0,0,         # Brand 3 (Row 4)
  0,0,0,1,0,0,0,         # Brand 4 (Row 5)
  0,0,0,0,1,0,0,         # Brand 5 (Row 6)
  0,0,0,0,0,1,0,         # Brand 6 (Row 7)
  0,0,0,0,0,0,1          # Brand 7 (Row 8)
), nrow = 8, byrow = TRUE)
rownames(Brand_all) <- c("Outside", "Brand 1", "Brand 2", "Brand 3",
                         "Brand 4", "Brand 5", "Brand 6", "Brand 7")
colnames(Brand_all) <- c("Brand 1", "Brand 2", "Brand 3", "Brand 4", "Brand 5", "Brand 6", "Brand 7")

## 2.2 Multi-Level Attributes
# Screen Size
Screen_all <- matrix(c(
  0,0,0,0,             # "7 inches" baseline
  1,0,0,0,             # "8 inches"
  0,1,0,0,             # "10 inches"
  0,0,1,0,             # "12 inches"
  0,0,0,1              # "13 inches"
), nrow = 5, byrow = TRUE)
rownames(Screen_all) <- c("7 inches", "8 inches", "10 inches", "12 inches", "13 inches")
colnames(Screen_all) <- c("8 Inches", "10 Inches", "12 Inches", "13 Inches")

# Storage Memory
Memory_all <- matrix(c(
  0,0,0,0,             # "8 GB" baseline
  1,0,0,0,             # "16 GB"
  0,1,0,0,             # "32 GB"
  0,0,1,0,             # "64 GB"
  0,0,0,1              # "128GB"
), nrow = 5, byrow = TRUE)
rownames(Memory_all) <- c("8 GB", "16 GB", "32 GB", "64 GB", "128GB")
colnames(Memory_all) <- c("16 GB", "32 GB", "64 GB", "128GB")

# Cashback Offers
Cashback_all <- matrix(c(
  0,0,0,               # "No Cashback" baseline
  1,0,0,               # "50 Cash Back"
  0,1,0,               # "100 Cash Back"
  0,0,1                # "150 Cash Back"
), nrow = 4, byrow = TRUE)
rownames(Cashback_all) <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")
colnames(Cashback_all) <- c("50 Cash Back", "100 Cash Back", "150 Cash Back")

# Processor
Processor_all <- matrix(c(
  0,0,                 # "1 GHz" baseline
  1,0,                 # "1.6 GHz"
  0,1                  # "2.2 GHz"
), nrow = 3, byrow = TRUE)
rownames(Processor_all) <- c("1 GHz", "1.6 GHz", "2.2 GHz")
colnames(Processor_all) <- c("1.6 GHz", "2.2 GHz")

# Connectivity
Connectivity_all <- matrix(c(
  0,0,                 # "WLAN" baseline
  1,0,                 # "WLAN + UMTS/3G"
  0,1                  # "WLAN + LTE/4G"
), nrow = 3, byrow = TRUE)
rownames(Connectivity_all) <- c("WLAN", "WLAN + UMTS/3G", "WLAN + LTE/4G")
colnames(Connectivity_all) <- c("WLAN + UMTS/3G", "WLAN + LTE/4G")

# Equipment
Equipment_all <- matrix(c(
  0,0,0,0,0,0,0,       # "None" baseline
  1,0,0,0,0,0,0,       # "Cover"
  0,1,0,0,0,0,0,       # "Keyboard"
  0,0,1,0,0,0,0,       # "Mouse"
  0,0,0,1,0,0,0,       # "Pencil"
  0,0,0,0,1,0,0,       # "32 GB Memory Card"
  0,0,0,0,0,1,0,       # "Keyboard + Pencil"
  0,0,0,0,0,0,1        # "Keyboard + Mouse + Pencil"
), nrow = 8, byrow = TRUE)
rownames(Equipment_all) <- c("None", "Cover", "Keyboard", "Mouse", "Pencil",
                             "32 GB Memory Card", "Keyboard + Pencil", "Keyboard + Mouse + Pencil")
colnames(Equipment_all) <- c("Cover", "Keyboard", "Mouse", "Pencil", "32 GB Memory Card",
                             "Keyboard + Pencil", "Keyboard + Mouse + Pencil")

## 2.3 Binary Attributes
# Operating System
System_B <- matrix(c(0,1), ncol = 1)
rownames(System_B) <- c("OS_A (Base)", "OS_B")
colnames(System_B) <- "System B"
# Display Resolution
Resolution <- matrix(c(0,1), ncol = 1)
rownames(Resolution) <- c("Standard (Base)", "High Resolution (264 ppi)")
colnames(Resolution) <- "High Resolution (264 ppi)"
# SD Slot
SD_Slot <- matrix(c(0,1), ncol = 1)
rownames(SD_Slot) <- c("With SD Slot (Base)", "Without SD-Slot")
colnames(SD_Slot) <- "Without SD-Slot"
# Battery Life
Battery <- matrix(c(0,1), ncol = 1)
rownames(Battery) <- c("4-8 hours (Base)", "8-12 hours")
colnames(Battery) <- "8-12 h. Runtime"
# Smartphone Sync
Sync <- matrix(c(0,1), ncol = 1)
rownames(Sync) <- c("No Sync (Base)", "S-phone Synch.")
colnames(Sync) <- "S-phone Synch."
# Value Pack
ValuePack <- matrix(c(0,1), ncol = 1)
rownames(ValuePack) <- c("No Value Pack (Base)", "Value Pack")
colnames(ValuePack) <- "Value Pack"

###############################################################################
## 5. Price (Continuous)
###############################################################################
# Price is not a matrix; it will be assigned directly (e.g., altX["Price"] <- 399).

###############################################################################
## 1. Helper Functions
###############################################################################
pick_attribute <- function(attribute_matrix, level_name) {
  if (level_name %in% rownames(attribute_matrix)) {
    return(as.numeric(attribute_matrix[level_name, ]))
  } else {
    return(rep(0, ncol(attribute_matrix)))
  }
}

###############################################################################
## 2. Main Function: construct_alternative
###############################################################################
construct_alternative <- function(
    brand, Brand_all,
    screen_level, Screen_all,
    memory_level, Memory_all,
    processor_level, Processor_all,
    connectivity_level, Connectivity_all,
    equipment_level, Equipment_all,
    cashback_level, Cashback_all,
    system_os, resolution, sdslot, battery, sync, valuepack,
    price_value,
    coefnames
) {
  alt <- rep(0, length(coefnames))
  names(alt) <- coefnames
  
  if (brand %in% rownames(Brand_all)) {
    brand_vec <- Brand_all[brand, ]
    alt[names(brand_vec)] <- as.numeric(brand_vec)
  }
  
  scr_vec <- pick_attribute(Screen_all, screen_level)
  names(scr_vec) <- colnames(Screen_all)
  alt[names(scr_vec)] <- scr_vec
  
  mem_vec <- pick_attribute(Memory_all, memory_level)
  names(mem_vec) <- colnames(Memory_all)
  alt[names(mem_vec)] <- mem_vec
  
  proc_vec <- pick_attribute(Processor_all, processor_level)
  names(proc_vec) <- colnames(Processor_all)
  alt[names(proc_vec)] <- proc_vec
  
  conn_vec <- pick_attribute(Connectivity_all, connectivity_level)
  names(conn_vec) <- colnames(Connectivity_all)
  alt[names(conn_vec)] <- conn_vec
  
  equip_vec <- pick_attribute(Equipment_all, equipment_level)
  names(equip_vec) <- colnames(Equipment_all)
  alt[names(equip_vec)] <- equip_vec
  
  cash_vec <- pick_attribute(Cashback_all, cashback_level)
  names(cash_vec) <- colnames(Cashback_all)
  alt[names(cash_vec)] <- cash_vec
  
  if (system_os != "0") { alt["System B"] <- 1 }
  if (resolution != "0") { alt["High Resolution (264 ppi)"] <- 1 }
  if (sdslot != "0") { alt["Without SD-Slot"] <- 1 }
  if (battery != "0") { alt["8-12 h. Runtime"] <- 1 }
  if (sync != "0") { alt["S-phone Synch."] <- 1 }
  if (valuepack != "0") { alt["Value Pack"] <- 1 }
  
  alt["Price"] <- price_value
  
  return(alt)
}


###############################################################################
## Price & Cashback Optimization with Profit Maximisation
## Using Individual Beta Draws & a Full Market Choiceset
###############################################################################

# -------------------------------
# (I) Define the Market Alternatives (Dominant Brands)
# -------------------------------

# 4.1 Outside Option
alt0 <- construct_alternative(
  brand = "Outside", Brand_all = Brand_all,
  
  screen_level = "7 inches", Screen_all = Screen_all,
  memory_level = "8 GB", Memory_all = Memory_all,
  processor_level = "1 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN", Connectivity_all = Connectivity_all,
  equipment_level = "None", Equipment_all = Equipment_all,
  cashback_level = "No Cashback", Cashback_all = Cashback_all,
  
  system_os = "0",           # OS_A
  resolution = "0",          # standard
  sdslot = "0",              # with SD
  battery = "0",             # 4-8 hours
  sync = "0",                # no sync
  valuepack = "0",           # no value pack
  
  price_value = 0,
  coefnames   = coefnames
)

# 4.2 Apple (Brand 1) - iPad Pro-like
alt_apple <- construct_alternative(
  brand = "Brand 1", Brand_all = Brand_all,
  
  screen_level = "12 inches", Screen_all = Screen_all,
  memory_level = "128GB", Memory_all = Memory_all,
  processor_level = "2.2 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + LTE/4G", Connectivity_all = Connectivity_all,
  equipment_level = "Keyboard + Pencil", Equipment_all = Equipment_all,
  cashback_level = "No Cashback", Cashback_all = Cashback_all,
  
  system_os = "0",            # OS_A (exclusive to Apple)
  resolution = "High Resolution (264 ppi)",
  sdslot = "Without SD-Slot", # Apple lacks SD slots
  battery = "8-12 h. Runtime",
  sync = "S-phone Synch.",
  valuepack = "0",            # No extra bundle
  
  price_value = 8.99,
  coefnames = coefnames
)

# 4.3 Samsung (Brand 2) - Galaxy Tab S8-like (Focal Alternative)
alt_samsung <- construct_alternative(
  brand = "Brand 2", Brand_all = Brand_all,
  
  screen_level = "10 inches", Screen_all = Screen_all,
  memory_level = "64 GB", Memory_all = Memory_all,
  processor_level = "2.2 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + LTE/4G", Connectivity_all = Connectivity_all,
  equipment_level = "Pencil", Equipment_all = Equipment_all,
  cashback_level = "100 Cash Back", Cashback_all = Cashback_all,
  
  system_os = "OS_B",
  resolution = "High Resolution (264 ppi)",
  sdslot = "0",
  battery = "8-12 h. Runtime",
  sync = "S-phone Synch.",
  valuepack = "0",   
  
  price_value = 6.99,
  coefnames = coefnames
)

# 4.4 Huawei (Brand 3) - MatePad Pro-like
alt_huawei <- construct_alternative(
  brand = "Brand 3", Brand_all = Brand_all,
  
  screen_level = "9 inches", Screen_all = Screen_all,
  memory_level = "32 GB", Memory_all = Memory_all,
  processor_level = "1.6 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + UMTS/3G", Connectivity_all = Connectivity_all,
  equipment_level = "None", Equipment_all = Equipment_all,
  cashback_level = "50 Cash Back", Cashback_all = Cashback_all,
  
  system_os = "OS_B",
  resolution = "0",           # Standard resolution
  sdslot = "0",               # Includes SD slot
  battery = "8-12 h. Runtime",
  sync = "0",                 # No smartphone sync
  valuepack = "0",
  
  price_value = 3.99,
  coefnames = coefnames
)

# 4.5 Amazon (Brand 4) - Fire HD-like
alt_amazon <- construct_alternative(
  brand = "Brand 4", Brand_all = Brand_all,
  
  screen_level = "7 inches", Screen_all = Screen_all,
  memory_level = "16 GB", Memory_all = Memory_all,
  processor_level = "1 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN", Connectivity_all = Connectivity_all,
  equipment_level = "32 GB Memory Card", Equipment_all = Equipment_all,
  cashback_level = "No Cashback", Cashback_all = Cashback_all,
  
  system_os = "OS_B",
  resolution = "0",           # Standard resolution
  sdslot = "0",               # Includes SD slot
  battery = "4-8 hours (Base)",
  sync = "0",
  valuepack = "0",
  
  price_value = 0.99,
  coefnames = coefnames
)

# 4.6 Asus (Brand 5) - ZenPad 3S-like
alt_asus <- construct_alternative(
  brand = "Brand 5", Brand_all = Brand_all,
  
  screen_level = "10 inches", Screen_all = Screen_all,
  memory_level = "32 GB", Memory_all = Memory_all,
  processor_level = "1 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + UMTS/3G", Connectivity_all = Connectivity_all,
  equipment_level = "0", Equipment_all = Equipment_all,
  cashback_level = "No Cash Back", Cashback_all = Cashback_all,
  
  system_os = "OS_B",
  resolution = "High Resolution (264 ppi)",
  sdslot = "0",
  battery = "4-8 hours (Base)",
  sync = "0",
  valuepack = "Value Pack",
  
  price_value = 2.99,
  coefnames = coefnames
)

# Combine all alternatives into one market choiceset.
# (Rows are ordered as: Outside, Apple, Samsung, Huawei, Amazon, Asus)
choiceset_full <- rbind(alt0, alt_apple, alt_samsung, alt_huawei, alt_amazon, alt_asus)
rownames(choiceset_full) <- c("Outside", "Tablet 1 (Apple)", "Tablet 2 (Samsung)", 
                              "Tablet 3 (Huawei)", "Tablet 4 (Amazon)", "Tablet 5 (Asus)")
print("Market Choiceset:")
print(choiceset_full)

# ------------------------------- 
# (II) Define Functions for Market Simulation and Profit Optimization
# -------------------------------
# Function to compute market shares from individual beta draws
probXy <- function(betamix, X) {
  # betamix: matrix of individual beta draws (n_draws x n_parameters)
  # X: design matrix (p alternatives x n_parameters)
  Xbeta <- X %*% t(betamix)  # resulting in a p x n_draws matrix of utilities
  probs <- apply(Xbeta, 2, function(u) {
    exp(u) / sum(exp(u))
  })
  avg_probs <- rowMeans(probs)
  return(list(ms = avg_probs, individual_probs = probs))
}

# Profit calculation functions:
# profitcalc() updates the focal alternative's price (Price is in column 33),
# computes market shares using the individual draws, and returns profit.
profitcalc <- function(price, betamix, X, cost, focal_index) {
  X_updated <- X
  X_updated[focal_index, 33] <- price  # Set the Price attribute (33rd column)
  share <- probXy(betamix, X_updated)
  profit <- share$ms[focal_index] * (price - cost[focal_index])
  return(list(profits = profit, share = share$ms))
}

# profitcalchelp() returns just the profit (for use with optim())
profitcalchelp <- function(price, betamix, X, cost, focal_index) {
  profitcalc(price, betamix, X, cost, focal_index)$profits
}

# Function to map Cashback level to an extra cost adjustment.
# (Here we multiply by 1.5 to scale the incremental cost; adjust as needed.)
get_cashback_cost <- function(cashback_level) {
  if (cashback_level == "50 Cash Back") return(0.5 * 1)
  else if (cashback_level == "100 Cash Back") return(1.00 * 1)
  else if (cashback_level == "150 Cash Back") return(1.50 * 1)
  else return(0)  # "No Cashback"
}

# Define a fixed base marginal cost for Samsung (if used in fixed cost specification)
base_cost <- 3.00
get_effective_cost <- function(cashback_level) {
  return(base_cost + get_cashback_cost(cashback_level))
}
# New convex cost function using the baseline cost from the vector.
# The effective cost = baseline_cost + penalty + cashback_cost.
# The penalty term applies only when price exceeds a threshold.
get_effective_cost_convex <- function(price, cashback_level, baseline_cost, threshold = 10, lambda = 0.1) {
  penalty <- ifelse(price > threshold, lambda * (price - threshold)^2, 0)
  cashback_cost <- get_cashback_cost(cashback_level)
  return(baseline_cost + penalty + cashback_cost)
}

#############################################
## Modified Profit Calculation Functions (Using Convex Cost Function)
#############################################

# Profit calculation using the convex cost function.
profitcalc_convex <- function(price, betamix, X, cashback_level, baseline_cost, threshold = 10, lambda = 0.1, focal_index) {
  X_updated <- X
  X_updated[focal_index, 33] <- price
  share <- probXy(betamix, X_updated)
  effective_cost <- get_effective_cost_convex(price, cashback_level, baseline_cost, threshold, lambda)
  profit <- share$ms[focal_index] * (price - effective_cost)
  return(list(profits = profit, share = share$ms))
}

# Helper function for optimization that returns just the profit.
profitcalchelp_convex <- function(price, betamix, X, cashback_level, baseline_cost, threshold = 10, lambda = 0.1, focal_index) {
  profitcalc_convex(price, betamix, X, cashback_level, baseline_cost, threshold, lambda, focal_index)$profits
}


# -------------------------------
# (III) Set Up to Allow Selection of the Underlying Beta Draws
# -------------------------------
# Choose which betadraws to use (you can switch among these models)
# To use different models, uncomment one of the following:
# selected_betamix <- hilf_HB$betaexchange       # Unconstrained model
# selected_betamix <- hilf_priceHB$betaexchange    # Price sign–constrained model
# selected_betamix <- hilf_constr$betaexchange     # Sign–constrained model

# If you wish to use the order–constrained model, then use the beta draws stored in res_order_const$betadraw_flat.
# However, these draws are in the transformed order (coefnames_R).
# We need to reorder them back to the original order (coefnames).
selected_betamix <- res_order_const$betadraw_flat

# Define the permutation used in the transformation:
perm <- c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)
# Compute the inverse permutation:
inv_order <- order(perm)  # This gives indices that sort 'perm' in increasing order

# Reorder the columns:
selected_betamix <- selected_betamix[, inv_order]

# (Optional) Check that the column names now match the original ordering:
# Assuming the beta draws have column names corresponding to coefnames_R, you can do:
if(!is.null(colnames(selected_betamix))) {
  cat("Reordered beta draws column names:\n")
  print(colnames(selected_betamix))
  cat("Original coefnames:\n")
  print(coefnames)
}

# Now take a random subset for simulation purposes:
set.seed(123)  # For reproducibility
betamix_subset <- selected_betamix[runif(nrow(selected_betamix)) > 0.7, ]
betamix_subset <- as.matrix(betamix_subset)

# Now betamix_subset is in the original attribute order,
# so that column 33 is "Price", columns 34-36 are "50 Cash Back", "100 Cash Back", and "150 Cash Back", etc.


# -------------------------------
# (IV) Profit Maximisation for the Focal Alternative (Samsung)
# -------------------------------

# For profit maximisation we need a design matrix and a cost vector.
# We use the full market choiceset "choiceset_full" defined earlier.
# Define the cost vector for the alternatives.
# Assume: Outside option cost = 0, competitor (Apple) cost = 4.50, focal (Samsung) cost = effective cost,
# and for the remaining competitors, set some cost (here we use 4.50 for simplicity).
# We will update Samsung's effective cost later depending on its cashback.
cost <- c(0, 4.50, 4.50, 2.90, 0.70, 2.00)
baseline_cost_vec <- c(0, 4.50, 4.50, 2.90, 0.70, 2.00)

# For the profit optimisation function we will focus on Samsung (row index 3).
# Test profit calculation at an example price:
example_price <- 6.99  # Example price (in Euros)
# (Update Samsung’s effective cost based on its current cashback in alt_samsung)
current_cashback <- alt_samsung["100 Cash Back"]
# For simplicity, here we recompute effective cost based on a known cashback string:
effective_cost_example <- get_effective_cost("100 Cash Back")
cost[3] <- effective_cost_example

example_profit <- profitcalc(example_price, betamix_subset, choiceset_full, cost, focal_index = 3)
cat("Example profit and market share for Samsung at price =", example_price, ":\n")
print(example_profit)

# Now optimize using Brent's method.
opt_result <- optim(
  par = 9.99,                     # initial guess
  fn = profitcalchelp,
  betamix = betamix_subset,
  X = choiceset_full,
  cost = cost,
  focal_index = 3,                # Samsung is row 3 in choiceset_full
  hessian = TRUE,
  control = list(fnscale = -1),   # maximize profit
  method = "Brent",
  lower = 0,
  upper = 30.00                   # upper bound on price
)
opt_result_summary <- matrix(c(opt_result$par, opt_result$value, opt_result$hessian), ncol = 3)
colnames(opt_result_summary) <- c("optimal price", "profit", "hessian")
cat("\nOptimisation result for Samsung (using individual beta draws):\n")
print(opt_result_summary)
optimal_out <- profitcalc(opt_result$par, betamix_subset, choiceset_full, cost, focal_index = 3)
cat("\nProfit and market share at the optimal price:\n")
print(optimal_out)

# =============================================================================
# with penalty costs

## For Samsung (assumed to be row 3), extract its current cashback setting:
current_cashback_samsung <- alt_samsung["100 Cash Back"]

# Optimize Samsung's price using the convex cost function:
opt_result_convex <- optim(
  par = 9.99,  # initial guess for Samsung's price
  fn = function(p) {
    profitcalchelp_convex(p, betamix_subset, choiceset_full,
                          cashback_level = current_cashback_samsung,
                          baseline_cost = baseline_cost_vec[3],
                          threshold = 10,
                          lambda = 0.1,
                          focal_index = 3)
  },
  method = "Brent",
  lower = 0,
  upper = 30.00,
  control = list(fnscale = -1),
  hessian = TRUE
)

opt_result_summary_convex <- matrix(c(opt_result_convex$par, opt_result_convex$value, opt_result_convex$hessian), ncol = 3)
colnames(opt_result_summary_convex) <- c("optimal price", "profit", "hessian")
cat("\nOptimization result for Samsung (convex cost):\n")
print(opt_result_summary_convex)

optimal_out_convex <- profitcalc_convex(opt_result_convex$par, betamix_subset, choiceset_full,
                                        cashback_level = current_cashback_samsung,
                                        baseline_cost = baseline_cost_vec[3],
                                        threshold = 10,
                                        lambda = 0.1,
                                        focal_index = 3)
cat("\nProfit and market share at the optimal price (convex cost):\n")
print(optimal_out_convex)

###############################################################################
## (V) Market Simulation: Price-Demand and Profit Surface with Varying Cashback
###############################################################################
compute_choice_probs <- function(choiceset, beta) {
  utilities <- as.vector(choiceset %*% beta)
  exp_util <- exp(utilities - max(utilities))
  probs <- exp_util / sum(exp_util)
  return(probs)
}

price_grid <- seq(0.99, 20.99, by = 0.1)
cashback_grid <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")
market_size <- 1

baseline_samsung_attributes <- list(
  brand = "Brand 2", 
  Brand_all = Brand_all,
  screen_level = "12 inches", Screen_all = Screen_all,
  memory_level = "64 GB",     Memory_all = Memory_all,
  processor_level = "2.2 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + LTE/4G", Connectivity_all = Connectivity_all,
  equipment_level = "Keyboard + Pencil", Equipment_all = Equipment_all,
  cashback_level = "100 Cash Back", Cashback_all = Cashback_all,
  price_value = 6.99,
  system_os = "OS_B",
  resolution = "High Resolution (264 ppi)",
  sdslot = "0",
  battery = "8-12 h. Runtime",
  sync = "S-phone Synch.",
  valuepack = "Value Pack",
  coefnames = coefnames
)

sim_results <- expand.grid(Price = price_grid, Cashback = cashback_grid, stringsAsFactors = FALSE)
sim_results$Samsung_Share <- NA
sim_results$Profit <- NA

for (i in 1:nrow(sim_results)) {
  current_price <- sim_results$Price[i]
  current_cashback <- sim_results$Cashback[i]
  
  alt_samsung_new <- do.call(construct_alternative, c(
    baseline_samsung_attributes[c("brand", "Brand_all",
                                  "screen_level", "Screen_all",
                                  "memory_level", "Memory_all",
                                  "processor_level", "Processor_all",
                                  "connectivity_level", "Connectivity_all",
                                  "equipment_level", "Equipment_all",
                                  "system_os", "resolution", "sdslot",
                                  "battery", "sync", "valuepack",
                                  "coefnames")],
    list(
      cashback_level = current_cashback,
      Cashback_all = Cashback_all,
      price_value = current_price
    )
  ))
  
  choiceset_new <- rbind(
    alt0,
    alt_apple,
    alt_samsung_new,
    alt_huawei,
    alt_amazon,
    alt_asus
  )
  
  beta_mean_used <- colMeans(selected_betamix)
  probs <- compute_choice_probs(choiceset_new, beta_mean_used)
  samsung_share <- probs[3]
  
  sim_results$Samsung_Share[i] <- samsung_share
  
  effective_cost <- get_effective_cost(current_cashback)
  per_unit_margin <- current_price - effective_cost
  sim_results$Profit[i] <- per_unit_margin * (market_size * samsung_share)
}

print("Simulation Results (Price, Cashback, Samsung Share, Profit):")
print(sim_results)

library(ggplot2)
ggplot(sim_results, aes(x = Price, y = Profit, color = Cashback, group = Cashback)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Profit Surface for Samsung: Price & Cashback Options",
       x = "Price (Euros)",
       y = "Total Profit",
       color = "Cashback Level") +
  theme_minimal()

ggplot(sim_results, aes(x = Price, y = Samsung_Share, color = Cashback, group = Cashback)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Price-Demand Curve for Samsung across Cashback Levels",
       x = "Price (Euros)",
       y = "Predicted Market Share",
       color = "Cashback Level") +
  theme_minimal()

optimal <- sim_results[which.max(sim_results$Profit), ]
cat("Optimal Price and Cashback Combination:\n")
print(optimal)


###############################################################################
## (V) Market Simulation: Price-Demand and Profit Surface with Varying Cashback
###############################################################################

# Revised convex cost function using a firm-specific baseline cost.
get_effective_cost_convex <- function(price, cashback_level, baseline_cost, threshold = 10, lambda = 0.1) {
  # Penalty applies only if price exceeds the threshold.
  penalty <- ifelse(price > threshold, lambda * (price - threshold)^2, 0)
  cashback_cost <- get_cashback_cost(cashback_level)
  return(baseline_cost + penalty + cashback_cost)
}

# Function to compute market shares from individual beta draws.
compute_choice_probs <- function(choiceset, beta) {
  utilities <- as.vector(choiceset %*% beta)
  exp_util <- exp(utilities - max(utilities))
  probs <- exp_util / sum(exp_util)
  return(probs)
}

# Define the simulation grid.
price_grid <- seq(0.99, 20.99, by = 0.1)
cashback_grid <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")
market_size <- 1

# Baseline attributes for Samsung (focal alternative).
baseline_samsung_attributes <- list(
  brand = "Brand 2", 
  Brand_all = Brand_all,
  screen_level = "12 inches", Screen_all = Screen_all,
  memory_level = "64 GB",     Memory_all = Memory_all,
  processor_level = "2.2 GHz", Processor_all = Processor_all,
  connectivity_level = "WLAN + LTE/4G", Connectivity_all = Connectivity_all,
  equipment_level = "Keyboard + Pencil", Equipment_all = Equipment_all,
  cashback_level = "100 Cash Back", Cashback_all = Cashback_all,
  price_value = 6.99,
  system_os = "OS_B",
  resolution = "High Resolution (264 ppi)",
  sdslot = "0",
  battery = "8-12 h. Runtime",
  sync = "S-phone Synch.",
  valuepack = "Value Pack",
  coefnames = coefnames
)

# Create the simulation results data frame.
sim_results <- expand.grid(Price = price_grid, Cashback = cashback_grid, stringsAsFactors = FALSE)
sim_results$Samsung_Share <- NA
sim_results$Profit <- NA

# Loop over every combination in the grid.
for (i in 1:nrow(sim_results)) {
  current_price <- sim_results$Price[i]
  current_cashback <- sim_results$Cashback[i]
  
  # Construct Samsung's alternative with the updated price and cashback.
  alt_samsung_new <- do.call(construct_alternative, c(
    baseline_samsung_attributes[c("brand", "Brand_all",
                                  "screen_level", "Screen_all",
                                  "memory_level", "Memory_all",
                                  "processor_level", "Processor_all",
                                  "connectivity_level", "Connectivity_all",
                                  "equipment_level", "Equipment_all",
                                  "system_os", "resolution", "sdslot",
                                  "battery", "sync", "valuepack",
                                  "coefnames")],
    list(
      cashback_level = current_cashback,
      Cashback_all = Cashback_all,
      price_value = current_price
    )
  ))
  
  # Build the complete choiceset with all alternatives.
  choiceset_new <- rbind(
    alt0,
    alt_apple,
    alt_samsung_new,
    alt_huawei,
    alt_amazon,
    alt_asus
  )
  
  # Compute predicted market shares using the average beta vector.
  beta_mean_used <- colMeans(selected_betamix)
  probs <- compute_choice_probs(choiceset_new, beta_mean_used)
  samsung_share <- probs[3]  # Samsung is row 3
  sim_results$Samsung_Share[i] <- samsung_share
  
  # Use the new convex cost function.
  # For Samsung, the baseline cost is baseline_cost_vec[3].
  effective_cost <- get_effective_cost_convex(current_price, current_cashback, baseline_cost = baseline_cost_vec[3],
                                              threshold = 10, lambda = 0.1)
  per_unit_margin <- current_price - effective_cost
  sim_results$Profit[i] <- per_unit_margin * (market_size * samsung_share)
}

print("Simulation Results (Price, Cashback, Samsung Share, Profit):")
print(sim_results)

# Plot the Profit Surface.
library(ggplot2)
ggplot(sim_results, aes(x = Price, y = Profit, color = Cashback, group = Cashback)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Profit Surface for Samsung: Price & Cashback Options (Convex Cost)",
       x = "Price (Euros)",
       y = "Total Profit",
       color = "Cashback Level") +
  theme_minimal()

# Plot the Price-Demand Curve.
ggplot(sim_results, aes(x = Price, y = Samsung_Share, color = Cashback, group = Cashback)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Price-Demand Curve for Samsung across Cashback Levels (Convex Cost)",
       x = "Price (Euros)",
       y = "Predicted Market Share",
       color = "Cashback Level") +
  theme_minimal()

# Identify the optimal price and cashback combination.
optimal <- sim_results[which.max(sim_results$Profit), ]
cat("Optimal Price and Cashback Combination (Convex Cost):\n")
print(optimal)


#!!! calculate costs here

# Extract the optimal combination from the simulation results
optimal_price <- optimal$Price
optimal_cashback <- optimal$Cashback

# Reconstruct Samsung's alternative using the optimal price and cashback
alt_samsung_optimal <- do.call(construct_alternative, c(
  baseline_samsung_attributes[c("brand", "Brand_all",
                                "screen_level", "Screen_all",
                                "memory_level", "Memory_all",
                                "processor_level", "Processor_all",
                                "connectivity_level", "Connectivity_all",
                                "equipment_level", "Equipment_all",
                                "system_os", "resolution", "sdslot",
                                "battery", "sync", "valuepack",
                                "coefnames")],
  list(
    cashback_level = optimal_cashback,
    Cashback_all = Cashback_all,
    price_value = optimal_price
  )
))

# Build the complete choiceset, replacing Samsung's alternative with the optimal version.
choiceset_optimal <- rbind(
  alt0,
  alt_apple,
  alt_samsung_optimal,
  alt_huawei,
  alt_amazon,
  alt_asus
)

# Compute predicted market shares using the average beta vector.
beta_mean_used <- colMeans(selected_betamix)
optimal_shares <- compute_choice_probs(choiceset_optimal, beta_mean_used)

# Report the predicted market shares.
cat("Predicted Market Shares at the Optimal Combination:\n")
print(optimal_shares)


# Extract optimal combination parameters for Samsung from the simulation results
optimal_price <- optimal$Price         # e.g., 12.18
optimal_cashback <- optimal$Cashback    # e.g., "150 Cash Back"

# Reconstruct Samsung’s optimal alternative using the optimal price and cashback.
alt_samsung_optimal <- do.call(construct_alternative, c(
  baseline_samsung_attributes[c("brand", "Brand_all",
                                "screen_level", "Screen_all",
                                "memory_level", "Memory_all",
                                "processor_level", "Processor_all",
                                "connectivity_level", "Connectivity_all",
                                "equipment_level", "Equipment_all",
                                "system_os", "resolution", "sdslot",
                                "battery", "sync", "valuepack",
                                "coefnames")],
  list(
    cashback_level = optimal_cashback,
    Cashback_all = Cashback_all,
    price_value = optimal_price
  )
))

# Build the complete choiceset with all alternatives.
choiceset_optimal <- rbind(
  alt0,
  alt_apple,
  alt_samsung_optimal,
  alt_huawei,
  alt_amazon,
  alt_asus
)

# Compute predicted market shares using the average beta vector.
beta_mean_used <- colMeans(selected_betamix)
optimal_shares <- compute_choice_probs(choiceset_optimal, beta_mean_used)

# Calculate profit for Samsung at the optimal combination using the joint profit function.
# Note: 'pricefull_baseline' is the vector of baseline prices.
optimal_profit_out <- profitcalc_joint(
  price = optimal_price,
  betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.5, ],
  chset = choiceset_BC_noprice,
  cost = cost,
  bindex = 3,  # Samsung is the second row in the choiceset.
  pricefull = pricefull_baseline,
  cashback_option = optimal_cashback
)

# Output the results.
cat("Predicted Market Shares at the Optimal Combination:\n")
print(optimal_shares)
cat("Profit for Samsung at the Optimal Combination:\n")
print(optimal_profit_out$profits)

###############################################################################

###############################################################################
## COMPETITIVE EQUILIBRIUM: Optimal Pricing (Nash Equilibrium)
## Each firm (tablet brand) chooses its price simultaneously to maximize profit.
## The equilibrium is reached when no firm wishes to deviate.
###############################################################################

# =============================================================================
# PRELIMINARIES (Assume these objects are already defined in your workspace):
#   - coefnames, construct_alternative(), and the attribute matrices (Brand_all, etc.)
#   - The full market choiceset "choiceset_full" (rows: 1 = Outside, 2:Apple, 3:Samsung, 4:Huawei, 5:Amazon, 6:Asus)
#   - The cost vector "cost" (with cost for each alternative; row 1 fixed, rows 2:6 decision‐makers)
#   - The profit functions: profitcalc() and profitcalchelp() (with Price in column 33)
#   - Your beta–draws from a hierarchical model (e.g., hilf_HB, hilf_priceHB, hilf_constr)
# =============================================================================

# -----------------------------------------------------------------------------
# (I) For the Order–Constrained Model: Reorder Beta Draws to the Original Order
# -----------------------------------------------------------------------------
# (If you want to use the order–constrained model beta draws, uncomment this section.)
# Here, coefnames_R was obtained by:
    coefnames_R <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
# Let perm denote that permutation:
perm <- c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)
 inv_order <- order(perm)  # inverse permutation
# To use the order–constrained beta draws stored in res_order_const$betadraw_flat:
 selected_betamix <- res_order_const$betadraw_flat[, inv_order]
# -----------------------------------------------------------------------------
# Otherwise, to use another model (e.g., unconstrained or price–constrained), uncomment one of:
# selected_betamix <- hilf_HB$betaexchange       # unconstrained model
# selected_betamix <- hilf_priceHB$betaexchange    # price sign–constrained model
# selected_betamix <- hilf_constr$betaexchange     # sign–constrained model
# -----------------------------------------------------------------------------

# For simulation purposes, select a random subset of beta draws:
set.seed(123)  # for reproducibility
betamix_subset <- selected_betamix[runif(nrow(selected_betamix)) > 0.99, ]
betamix_subset <- as.matrix(betamix_subset)
# Now, betamix_subset is used in all our profit calculations.

# =============================================================================
# (II) Competitive Equilibrium: Best–Response Function for Price
# =============================================================================

# Define a function that, for firm i (row index in choiceset_full), given the current choiceset,
# returns the best–response price that maximizes its profit.
best_response <- function(firm_index, current_choiceset, betamix, cost, lower = 0, upper = 50) {
  # We define an objective function for firm 'firm_index'
  # that takes a candidate price pr, updates row firm_index (column 33),
  # and returns the profit (to be maximized).
  profit_fun <- function(pr) {
    # Make a copy of the current choiceset
    X_temp <- current_choiceset
    X_temp[firm_index, 33] <- pr  # update the price for firm 'firm_index'
    # Use profitcalchelp() defined earlier to compute profit for this firm
    profit_value <- profitcalchelp(pr, betamix, X_temp, cost, firm_index)
    return(profit_value)
  }
  
  # Use optim() with the Brent method to maximize profit_fun for firm 'firm_index'
  res <- optim(
    par = current_choiceset[firm_index, 33],  # current price as starting value
    fn = profit_fun,
    method = "Brent",
    lower = lower,
    upper = upper,
    control = list(fnscale = -1)  # maximize profit (fnscale = -1)
  )
  return(res$par)
}

# =============================================================================
# (III) Iterative Best–Response Algorithm for Nash Equilibrium
# =============================================================================

# We assume that row 1 (Outside) is fixed.
# Firms: rows 2,3,4,5,6 (Apple, Samsung, Huawei, Amazon, Asus) are choosing their prices.

# Initialize the current choiceset from choiceset_full.
# (Make a copy so that we can update it.)
current_choiceset <- choiceset_full

# Extract current prices (from column 33) for firms 2 to 6:
current_prices <- current_choiceset[2:6, 33]

# Set convergence parameters:
tolerance <- 0.01  # convergence tolerance for price changes
max_iter <- 10
convergence <- FALSE
iteration <- 1

# Store the history of prices (for visualization)
price_history <- matrix(NA, nrow = max_iter, ncol = 5)
colnames(price_history) <- rownames(current_choiceset)[2:6]

while (!convergence && iteration <= max_iter) {
  
  # For each firm (rows 2 to 6), compute its best response given competitors' prices.
  new_prices <- current_prices  # initialize new prices as current
  for (i in 1:length(current_prices)) {
    firm_index <- i + 1  # because row 1 is outside
    new_p <- best_response(firm_index, current_choiceset, betamix_subset, cost)
    new_prices[i] <- new_p
    # Update the firm's price in the current choiceset
    current_choiceset[firm_index, 33] <- new_p
  }
  
  # Record the new prices:
  price_history[iteration, ] <- new_prices
  
  # Check convergence: if maximum change across firms is below tolerance.
  price_change <- max(abs(new_prices - current_prices))
  cat("Iteration", iteration, "max price change:", price_change, "\n")
  if (price_change < tolerance) {
    convergence <- TRUE
  } else {
    current_prices <- new_prices
  }
  
  iteration <- iteration + 1
}

# Trim the history matrix to the number of iterations actually used.
price_history <- price_history[1:(iteration - 1), ]

# The Nash equilibrium prices for the 5 firms (rows 2:6) are:
equilibrium_prices <- current_prices
names(equilibrium_prices) <- rownames(current_choiceset)[2:6]

# Compute equilibrium profits and market shares using the final choiceset:
final_profit_shares <- list()
for (i in 2:6) {
  final_profit_shares[[rownames(current_choiceset)[i]]] <-
    profitcalc(current_choiceset[i, 33], betamix_subset, current_choiceset, cost, i)
}

cat("\nNash Equilibrium Prices:\n")
print(equilibrium_prices)
cat("\nEquilibrium Profit and Market Shares for Each Firm:\n")
print(final_profit_shares)

# =============================================================================
# (IV) Visualize the Convergence of Prices
# =============================================================================
library(ggplot2)
library(reshape2)
price_history_df <- as.data.frame(price_history)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_history_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_history_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Convergence of Prices in the Best-Response Iteration",
       x = "Iteration", y = "Price (Euros)") +
  theme_minimal()

# =============================================================================
# (V) Optionally, Compare Equilibrium Outcomes Under Different Beta Draws
# =============================================================================
# For example, you can switch 'selected_betamix' to hilf_HB$betaexchange (unconstrained)
# or hilf_priceHB$betaexchange (price-constrained) and then re-run the above algorithm.
# The above iterative procedure will then yield the corresponding Nash equilibrium prices.




###############################################################################
## COMPETITIVE EQUILIBRIUM: Optimal Pricing ( Nash Equilibrium)
## Each firm (tablet brand) chooses its price simultaneously to maximize profit.
## The equilibrium is reached when no firm wishes to deviate.
###############################################################################

# =============================================================================
# PRELIMINARIES (Assume these objects are already defined in your workspace):
#   - coefnames, construct_alternative(), and the attribute matrices (Brand_all, etc.)
#   - The full market choiceset "choiceset_full" (rows: 1 = Outside, 2:Apple, 3:Samsung, 4:Huawei, 5:Amazon, 6:Asus)
#   - The profit functions: profitcalc() and profitcalchelp() that compute profit given betamix, choiceset, cost, etc.
#   - The order–constrained model draws in res_order_const, or another model if desired.
#   - baseline_cost_vec: A vector of baseline costs for each row, e.g. c(0, 4.50, 3.00, 1.70, 0.70, 1.70).
#   - threshold, lambda: parameters for the quadratic penalty.
# =============================================================================

# -----------------------------------------------------------------------------
# (I) Select & Reorder Beta Draws for the Order–Constrained Model
# -----------------------------------------------------------------------------

coefnames_R <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
perm <- c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)
inv_order <- order(perm)

# Suppose you want to use the order–constrained model:
selected_betamix <- res_order_const$betadraw_flat[, inv_order]

# Take a random subset of draws for simulation:
set.seed(123)
betamix_subset <- selected_betamix[runif(nrow(selected_betamix)) > 0.99, ]
betamix_subset <- as.matrix(betamix_subset)

# -----------------------------------------------------------------------------
# (II) Convex Cost Function
# -----------------------------------------------------------------------------

# This function computes a firm's effective cost as:
# effective_cost = baseline_cost + penalty + (other adjustments if desired).
# The penalty is quadratic if price > threshold.
get_effective_cost_convexCE <- function(price, baseline_cost, threshold = 10, lambda = 0.1) {
  penalty <- ifelse(price > threshold, lambda * (price - threshold)^2, 0)
  return(baseline_cost + penalty)
}

# -----------------------------------------------------------------------------
# (III) Best–Response Function
# -----------------------------------------------------------------------------

# Instead of using a fixed 'cost' vector, we'll dynamically set each firm's cost
# using the new convex function inside the profit function.

best_response <- function(firm_index, current_choiceset, betamix, baseline_cost_vec,
                          threshold = 10, lambda = 0.1, lower = 0, upper = 20) {
  # profit_fun() updates row firm_index with candidate price 'pr' and computes profit for that firm.
  profit_fun <- function(pr) {
    X_temp <- current_choiceset
    X_temp[firm_index, 33] <- pr
    
    # Create a temporary 'cost' vector to pass to profitcalchelp().
    # We'll keep other firms' costs fixed at their baseline. Only the focal firm gets the penalty.
    cost_temp <- baseline_cost_vec  
    cost_temp[firm_index] <- get_effective_cost_convexCE(pr, baseline_cost_vec[firm_index], threshold, lambda)
    
    # Use your existing profitcalchelp() to compute profit for this firm.
    # Be sure that profitcalchelp() calls profitcalc(), which uses cost_temp for profitcalc.
    profit_value <- profitcalchelp(pr, betamix, X_temp, cost_temp, firm_index)
    return(profit_value)
  }
  
  # Use optim() with the Brent method to maximize the profit function for the focal firm.
  res <- optim(
    par = current_choiceset[firm_index, 33],  # current price as starting value
    fn = profit_fun,
    method = "Brent",
    lower = lower,
    upper = upper,
    control = list(fnscale = -1)  # maximize profit
  )
  return(res$par)
}

# -----------------------------------------------------------------------------
# (IV) Iterative Best–Response Algorithm for Nash Equilibrium
# -----------------------------------------------------------------------------

# We'll assume that row 1 is the outside option, which does not optimize price,
# so the actively optimizing firms are rows 2 to 6: Apple, Samsung, Huawei, Amazon, Asus.

# 1) Initialize the choiceset for iteration.
current_choiceset <- choiceset_full

# 2) Extract the starting prices (column 33) for firms 2 to 6.
current_prices <- current_choiceset[2:6, 33]

# 3) Set up your baseline cost vector for all rows.
#    Example: c(0, 4.50, 3.00, 1.70, 0.70, 1.70)
#    i.e., row 2(Apple) = 4.50, row 3(Samsung) = 3.00, etc.
baseline_cost_vec <- c(0, 4.50, 3.00, 1.70, 0.70, 1.70)

# 4) Set threshold & lambda for all firms (shared or firm-specific).
threshold <- 10
lambda <- 0.1

# 5) Convergence parameters
tolerance <- 0.01
max_iter <- 10
convergence <- FALSE
iteration <- 1

# 6) Store the history of prices for visualization
price_history <- matrix(NA, nrow = max_iter, ncol = 5)
colnames(price_history) <- rownames(current_choiceset)[2:6]

while (!convergence && iteration <= max_iter) {
  
  new_prices <- current_prices
  for (i in seq_along(current_prices)) {
    firm_index <- i + 1  # offset because row 1 is outside
    # Call best_response with the convex cost approach
    new_p <- best_response(firm_index, current_choiceset, betamix_subset, baseline_cost_vec,
                           threshold = threshold, lambda = lambda,
                           lower = 0, upper = 50)
    new_prices[i] <- new_p
    current_choiceset[firm_index, 33] <- new_p
  }
  
  price_history[iteration, ] <- new_prices
  
  price_change <- max(abs(new_prices - current_prices))
  cat("Iteration", iteration, "max price change:", price_change, "\n")
  if (price_change < tolerance) {
    convergence <- TRUE
  } else {
    current_prices <- new_prices
  }
  iteration <- iteration + 1
}

# 7) Trim the history if needed
price_history <- price_history[1:(iteration - 1), ]

# 8) The Nash equilibrium prices:
equilibrium_prices <- current_prices
names(equilibrium_prices) <- rownames(current_choiceset)[2:6]

# 9) Compute equilibrium profits & shares
final_profit_shares <- list()
for (i in 2:6) {
  final_profit_shares[[rownames(current_choiceset)[i]]] <- profitcalc(
    price = current_choiceset[i, 33],
    betamix = betamix_subset,
    X = current_choiceset,
    cost = {
      cost_temp <- baseline_cost_vec
      cost_temp[i] <- get_effective_cost_convexCE(current_choiceset[i, 33],
                                                  baseline_cost_vec[i],
                                                  threshold, lambda)
      cost_temp
    },
    focal_index = i
  )
}

cat("\nNash Equilibrium Prices (Convex Costs):\n")
print(equilibrium_prices)
cat("\nEquilibrium Profit and Market Shares for Each Firm:\n")
print(final_profit_shares)

# -----------------------------------------------------------------------------
# (V) Visualize Convergence of Prices
# -----------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
price_history_df <- as.data.frame(price_history)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_history_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_history_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Convergence of Prices (Nash Equilibrium with Convex Costs)",
       x = "Iteration", y = "Price (Euros)") +
  theme_minimal()



###############################################################################
## Joint Price & Cashback Optimization for Competitive Equilibrium
###############################################################################

# --- Helper function to update cashback indicators in a row ---
update_cashback <- function(row, cb) {
  # This function resets the cashback indicators in the row.
  # We assume that the three cashback attributes are named exactly as follows:
  # "50 Cash Back", "100 Cash Back", "150 Cash Back"
  row["50 Cash Back"] <- 0
  row["100 Cash Back"] <- 0
  row["150 Cash Back"] <- 0
  if (cb == "50 Cash Back") {
    row["50 Cash Back"] <- 1
  } else if (cb == "100 Cash Back") {
    row["100 Cash Back"] <- 1
  } else if (cb == "150 Cash Back") {
    row["150 Cash Back"] <- 1
  }
  # If "No Cashback", leave them all at 0.
  return(row)
}

# --- Joint Best–Response Function for a Single Firm ---
best_response_joint <- function(firm_index, current_choiceset, betamix, cost, 
                                price_lower = 0, price_upper = 25, cashback_options) {
  best_profit <- -Inf
  best_price <- NA
  best_cb <- NA
  
  # Loop over each discrete cashback option
  for (cb in cashback_options) {
    # Make a copy of the current choiceset to update the firm's cashback decision.
    X_temp <- current_choiceset
    # Update the row for the firm: set its cashback indicators using update_cashback.
    # (We assume that the baseline row already contains the proper values for other attributes.)
    X_temp[firm_index, ] <- update_cashback(X_temp[firm_index, ], cb)
    
    # Now, given this cashback decision, optimize price.
    profit_fun <- function(pr) {
      X_temp2 <- X_temp
      X_temp2[firm_index, 33] <- pr  # update the Price (assumed to be column 33)
      # Compute the profit for this firm given pr and the fixed cashback option.
      profit_value <- profitcalchelp(pr, betamix, X_temp2, cost, firm_index)
      return(profit_value)
    }
    
    res <- optim(
      par = current_choiceset[firm_index, 33],  # start from current price
      fn = profit_fun,
      method = "Brent",
      lower = price_lower,
      upper = price_upper,
      control = list(fnscale = -1)  # maximize profit
    )
    candidate_profit <- res$value
    candidate_price <- res$par
    # Check if this candidate (price, cashback) gives a higher profit
    if (candidate_profit > best_profit) {
      best_profit <- candidate_profit
      best_price <- candidate_price
      best_cb <- cb
    }
  }
  return(list(price = best_price, cashback = best_cb, profit = best_profit))
}

# --- Set up discrete cashback options ---
cashback_options <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")

# =============================================================================
# --- (I) Ensure Beta Draws are Selected ---
# =============================================================================

# (Assume you can switch among models by choosing one of these:)
# selected_betamix <- hilf_HB$betaexchange       # unconstrained model
# selected_betamix <- hilf_priceHB$betaexchange    # price sign–constrained model
# selected_betamix <- hilf_constr$betaexchange      # sign–constrained model

# If using the order–constrained model, remember to reorder the columns as needed.
perm <- c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)
inv_order <- order(perm)
#selected_betamix <- res_order_const$betadraw_flat[, inv_order]

# Select a random subset of beta draws for simulation:
set.seed(123)
betamix_subset <- selected_betamix[runif(nrow(selected_betamix)) > 0.95, ]
betamix_subset <- as.matrix(betamix_subset)

# =============================================================================
# --- (II) Competitive Equilibrium with Joint (Price, Cashback) Decisions ---
# =============================================================================

# We assume that row 1 (Outside) is fixed.
# Firms: rows 2,3,4,5,6 (e.g., Apple, Samsung, Huawei, Amazon, Asus) choose both price and cashback.
current_choiceset <- choiceset_full  # full market choiceset from earlier

# Initial prices for firms 2 to 6:
current_prices <- current_choiceset[2:6, 33]


baseline_cost_vec <- c(0, 3.50, 3, 1.70, 0.70, 1.70)

# New version of get_effective_cost that uses a firm–specific baseline cost.
get_effective_cost_joint <- function(cashback_level, baseline_cost) {
  return(baseline_cost + get_cashback_cost(cashback_level))
}


# Update the cost vector for the inside options.
# For each firm, cost is the effective cost, which depends on the chosen cashback.
# Helper function to extract the current cashback decision from a row of current_choiceset.
get_current_cashback <- function(alt_row) {
  # Define the names of the cashback attributes.
  cb_names <- c("50 Cash Back", "100 Cash Back", "150 Cash Back")
  # Identify which cashback indicator is set to 1.
  chosen <- cb_names[which(alt_row[cb_names] == 1)]
  if (length(chosen) == 0) {
    return("No Cashback")
  } else {
    return(chosen[1])
  }
}

# Update the cost vector for the inside options using stored cashback decisions.
cost <- numeric(nrow(current_choiceset))
cost[1] <- baseline_cost_vec[1]  # Outside option cost.
for (i in 2:nrow(current_choiceset)) {
  current_cb <- get_current_cashback(current_choiceset[i, ])
  cost[i] <- get_effective_cost_joint(current_cb, baseline_cost_vec[i])
}

# Initialize current_cashbacks by extracting the current cashback decision for firms 2 to 6.
current_cashbacks <- sapply(2:nrow(current_choiceset), function(i) get_current_cashback(current_choiceset[i, ]))

# Convergence parameters:
tolerance <- 0.05
max_iter <- 20
convergence <- FALSE
iteration <- 1

# Store history for both price and cashback decisions.
price_history <- matrix(NA, nrow = max_iter, ncol = 5)
cashback_history <- matrix(NA, nrow = max_iter, ncol = 5)
colnames(price_history) <- rownames(current_choiceset)[2:6]
colnames(cashback_history) <- rownames(current_choiceset)[2:6]

while (!convergence && iteration <= max_iter) {
  new_prices <- current_prices
  new_cashbacks <- current_cashbacks
  
  # For each firm (rows 2 to 6), compute the joint best response.
  for (i in 1:length(current_prices)) {
    firm_index <- i + 1  # Adjust because row 1 is outside.
    # Get the joint best response for firm firm_index:
    best_resp <- best_response_joint(firm_index, current_choiceset, betamix_subset, cost,
                                     price_lower = 0, price_upper = 30,
                                     cashback_options = cashback_options)
    new_prices[i] <- best_resp$price
    new_cashbacks[i] <- best_resp$cashback
    # Update firm's row in the choiceset: update cashback then update price.
    current_choiceset[firm_index, ] <- update_cashback(current_choiceset[firm_index, ], best_resp$cashback)
    current_choiceset[firm_index, 33] <- best_resp$price
    # Update cost for the firm according to its chosen cashback.
    cost[firm_index] <- get_effective_cost_joint(best_resp$cashback, baseline_cost_vec[firm_index])
  }
  
  # Record history:
  price_history[iteration, ] <- new_prices
  cashback_history[iteration, ] <- new_cashbacks
  
  # Check convergence (here we compare prices; you might also consider cashback changes).
  price_change <- max(abs(new_prices - current_prices))
  cat("Iteration", iteration, "max price change:", price_change, "\n")
  if (price_change < tolerance) {
    convergence <- TRUE
  } else {
    current_prices <- new_prices
    current_cashbacks <- new_cashbacks
  }
  
  iteration <- iteration + 1
}


# Trim history matrices:
price_history <- price_history[1:(iteration - 1), ]
cashback_history <- cashback_history[1:(iteration - 1), ]

# Equilibrium outcomes:
equilibrium_prices <- current_prices
equilibrium_cashbacks <- current_cashbacks
names(equilibrium_prices) <- rownames(current_choiceset)[2:6]
names(equilibrium_cashbacks) <- rownames(current_choiceset)[2:6]

# Compute equilibrium profits and market shares:
final_profit_shares <- list()
for (i in 2:6) {
  final_profit_shares[[rownames(current_choiceset)[i]]] <-
    profitcalc(current_choiceset[i, 33], betamix_subset, current_choiceset, cost, i)
}

cat("\nNash Equilibrium (Joint Price & Cashback) for Firms:\n")
print(data.frame(Price = equilibrium_prices, Cashback = equilibrium_cashbacks))
cat("\nEquilibrium Profit and Market Shares for Each Firm:\n")
print(final_profit_shares)

# =============================================================================
# --- (III) Visualization of Convergence ---
# =============================================================================

library(ggplot2)
library(reshape2)
price_history_df <- as.data.frame(price_history)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_history_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_history_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Convergence of Prices (Joint Optimization)",
       x = "Iteration", y = "Price (Euros)") +
  theme_minimal()

# Optionally, print the evolution of cashback decisions:
print("Cashback decision history (each row is the cashback option chosen by firms 2-6):")
print(cashback_history)


###############################################################################
## Joint Price & Cashback Optimization for Competitive Equilibrium (Convex Cost)
###############################################################################

# 1) Helper function to reset cashback indicators in a row.
update_cashback <- function(row, cb) {
  row["50 Cash Back"] <- 0
  row["100 Cash Back"] <- 0
  row["150 Cash Back"] <- 0
  if (cb == "50 Cash Back") {
    row["50 Cash Back"] <- 1
  } else if (cb == "100 Cash Back") {
    row["100 Cash Back"] <- 1
  } else if (cb == "150 Cash Back") {
    row["150 Cash Back"] <- 1
  }
  return(row)
}


get_cashback_cost <- function(cashback_level) {
  if (cashback_level == "50 Cash Back") return(0.5 * 1)
  else if (cashback_level == "100 Cash Back") return(1.00 * 1)
  else if (cashback_level == "150 Cash Back") return(1.50 * 1)
  else return(0)  # "No Cashback"
}

# 3) Convex cost function that “punishes” high prices.
#    effective cost = baseline_cost + penalty_if_above_threshold + cashback_cost
get_effective_cost_convex_joint <- function(price, baseline_cost, threshold = 10, lambda = 0.1, cb_level) {
  penalty <- ifelse(price > threshold, lambda * (price - threshold)^2, 0)
  return(baseline_cost + penalty + get_cashback_cost(cb_level))
}

# 4) Joint best–response function (Price & Cashback).
#    We loop over discrete cashback options, and for each, we optimize price with Brent.
best_response_joint_convex <- function(firm_index, current_choiceset, betamix, baseline_cost_vec,
                                       threshold = 10, lambda = 0.1,
                                       price_lower = 0, price_upper = 25,
                                       cashback_options) {
  best_profit <- -Inf
  best_price <- NA
  best_cb <- NA
  
  # For each discrete cashback option:
  for (cb in cashback_options) {
    # Temporarily update the row to set the chosen cashback indicators.
    X_temp <- current_choiceset
    X_temp[firm_index, ] <- update_cashback(X_temp[firm_index, ], cb)
    
    # For each candidate price, define an objective that computes profit for the focal firm.
    profit_fun <- function(pr) {
      # Make a copy for local modifications:
      X_local <- X_temp
      X_local[firm_index, 33] <- pr
      
      # Create a local cost vector so only the focal firm’s cost is updated with convex penalty.
      cost_local <- baseline_cost_vec
      cost_local[firm_index] <- get_effective_cost_convex_joint(pr,
                                                                baseline_cost_vec[firm_index],
                                                                threshold,
                                                                lambda,
                                                                cb)
      
      # We call profitcalchelp() which uses profitcalc() with cost_local
      profit_value <- profitcalchelp(pr, betamix, X_local, cost_local, firm_index)
      return(profit_value)
    }
    
    # Optimize price via Brent’s method (within [price_lower, price_upper]).
    res <- optim(
      par = current_choiceset[firm_index, 33],  # current price as start
      fn = profit_fun,
      method = "Brent",
      lower = price_lower,
      upper = price_upper,
      control = list(fnscale = -1)
    )
    
    # Check if this combo yields a higher profit for the focal firm.
    candidate_profit <- res$value
    candidate_price  <- res$par
    if (candidate_profit > best_profit) {
      best_profit <- candidate_profit
      best_price  <- candidate_price
      best_cb     <- cb
    }
  }
  
  return(list(price = best_price, cashback = best_cb, profit = best_profit))
}

# 5) Define the discrete cashback levels.
cashback_options <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")

# =============================================================================
# (I) Ensure Beta Draws are Selected & Reordered (Order–Constrained Example)
# =============================================================================

coefnames_R <- coefnames[c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)]
perm <- c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)
inv_order <- order(perm)
selected_betamix <- res_order_const$betadraw_flat[, inv_order]

set.seed(123)
betamix_subset <- selected_betamix[runif(nrow(selected_betamix)) > 0.95, ]
betamix_subset <- as.matrix(betamix_subset)

# =============================================================================
# (II) Set Up the Market & the Baseline Costs
# =============================================================================

current_choiceset <- choiceset_full   # Must be defined previously
current_prices <- current_choiceset[2:6, 33]

# Suppose we define a baseline cost vector for the 6 rows: (Outside, Apple, Samsung, Huawei, Amazon, Asus).
baseline_cost_vec <- c(0, 3.50, 3.00, 1.70, 0.70, 1.70)

# Helper to find initial cashback for each row:
get_current_cashback <- function(alt_row) {
  cb_names <- c("50 Cash Back", "100 Cash Back", "150 Cash Back")
  chosen <- cb_names[which(alt_row[cb_names] == 1)]
  if (length(chosen) == 0) return("No Cashback") else return(chosen[1])
}
current_cashbacks <- sapply(2:nrow(current_choiceset), function(i) get_current_cashback(current_choiceset[i, ]))

# =============================================================================
# (III) Iterative Best–Response Algorithm for Joint (Price & Cashback) with Convex Cost
# =============================================================================

tolerance <- 0.01
max_iter <- 20
convergence <- FALSE
iteration <- 1

# We track the price and cashback decisions for Apple, Samsung, Huawei, Amazon, Asus (rows 2..6).
price_history <- matrix(NA, nrow = max_iter, ncol = 5)
cashback_history <- matrix(NA, nrow = max_iter, ncol = 5)
colnames(price_history) <- rownames(current_choiceset)[2:6]
colnames(cashback_history) <- rownames(current_choiceset)[2:6]

threshold <- 10   # above this price, penalty kicks in
lambda    <- 0.1  # penalty intensity

while (!convergence && iteration <= max_iter) {
  new_prices     <- current_prices
  new_cashbacks  <- current_cashbacks
  
  for (i in seq_along(current_prices)) {
    firm_index <- i + 1
    # Joint best response for firm 'firm_index' using the convex cost approach
    resp <- best_response_joint_convex(
      firm_index = firm_index,
      current_choiceset = current_choiceset,
      betamix = betamix_subset,
      baseline_cost_vec = baseline_cost_vec,
      threshold = threshold,
      lambda = lambda,
      price_lower = 0,
      price_upper = 20,  # adjust as needed
      cashback_options = cashback_options
    )
    new_prices[i]    <- resp$price
    new_cashbacks[i] <- resp$cashback
    
    # Update the firm's row in the current choiceset:
    current_choiceset[firm_index, ] <- update_cashback(current_choiceset[firm_index, ], resp$cashback)
    current_choiceset[firm_index, 33] <- resp$price
    # Also update cost for that row if you want to keep cost[] in sync, though not strictly needed
    # since cost is recomputed inside best_response. But for completeness:
    cost_row_i <- get_effective_cost_convex_joint(resp$price, baseline_cost_vec[firm_index],
                                                  threshold, lambda, resp$cashback)
    # If you have a cost vector:
    # cost[firm_index] <- cost_row_i
  }
  
  # Record iteration results:
  price_history[iteration, ]    <- new_prices
  cashback_history[iteration, ] <- new_cashbacks
  
  # Check convergence based on price changes (you could also check cashback changes).
  price_change <- max(abs(new_prices - current_prices))
  cat("Iteration", iteration, "max price change:", price_change, "\n")
  if (price_change < tolerance) {
    convergence <- TRUE
  } else {
    current_prices    <- new_prices
    current_cashbacks <- new_cashbacks
  }
  iteration <- iteration + 1
}

# Trim the history matrices
price_history <- price_history[1:(iteration-1), ]
cashback_history <- cashback_history[1:(iteration-1), ]

# The final joint equilibrium in price and cashback for rows 2..6:
equilibrium_prices    <- current_prices
equilibrium_cashbacks <- current_cashbacks
names(equilibrium_prices)    <- rownames(current_choiceset)[2:6]
names(equilibrium_cashbacks) <- rownames(current_choiceset)[2:6]

# Compute equilibrium profits & shares for each of the five inside firms.
final_profit_shares <- list()
for (i in 2:6) {
  # get each firm’s final price & chosen cb
  eq_price_i    <- current_choiceset[i, 33]
  eq_cb_i       <- get_current_cashback(current_choiceset[i, ])
  eq_cost_i     <- get_effective_cost_convex_joint(eq_price_i, baseline_cost_vec[i], threshold, lambda, eq_cb_i)
  
  # create a local cost vector if your profitcalc() references cost
  cost_local <- baseline_cost_vec
  cost_local[i] <- eq_cost_i
  
  # call your profitcalc to get final profit & share
  final_profit_shares[[rownames(current_choiceset)[i]]] <-
    profitcalc(eq_price_i, betamix_subset, current_choiceset, cost_local, i)
}

cat("\nNash Equilibrium (Joint Price & Cashback) with Convex Cost:\n")
print(data.frame(Price = equilibrium_prices, Cashback = equilibrium_cashbacks))
cat("\nEquilibrium Profit and Market Shares for Each Firm:\n")
print(final_profit_shares)

# Visualization of the price convergence
library(ggplot2)
library(reshape2)
price_history_df <- as.data.frame(price_history)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_history_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_history_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Convergence of Prices (Joint Optimization, Convex Cost)",
       x = "Iteration", y = "Price (Euros)") +
  theme_minimal()

cat("\nCashback decision history (each row is the cashback option chosen by firms 2-6):\n")
print(cashback_history)


library(reshape2)
library(dplyr)

# 1) Convert price_history into a long data frame
price_df <- as.data.frame(price_history)
price_df$Iteration <- 1:nrow(price_df)
price_melt <- melt(price_df, id.vars = "Iteration", 
                   variable.name = "Firm", value.name = "Price")

# 2) Convert cashback_history into a long data frame
cb_df <- as.data.frame(cashback_history)
cb_df$Iteration <- 1:nrow(cb_df)
cb_melt <- melt(cb_df, id.vars = "Iteration", 
                variable.name = "Firm", value.name = "Cashback")

# 3) Merge them on Iteration + Firm
combined_history <- left_join(price_melt, cb_melt, by = c("Iteration", "Firm"))


library(reshape2)
library(dplyr)

# 1) Convert price_history into a long data frame
price_df <- as.data.frame(price_history)
price_df$Iteration <- 1:nrow(price_df)
price_melt <- melt(price_df, id.vars = "Iteration", 
                   variable.name = "Firm", value.name = "Price")

# 2) Convert cashback_history into a long data frame
cb_df <- as.data.frame(cashback_history)
cb_df$Iteration <- 1:nrow(cb_df)
cb_melt <- melt(cb_df, id.vars = "Iteration", 
                variable.name = "Firm", value.name = "Cashback")

# 3) Merge them on Iteration + Firm
combined_history <- left_join(price_melt, cb_melt, by = c("Iteration", "Firm"))
library(ggplot2)

ggplot(combined_history, aes(x = Iteration, y = Price, 
                             color = Firm, shape = Cashback)) +
  geom_line(aes(group = Firm), linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Convergence of Prices with Cashback Changes",
       x = "Iteration",
       y = "Price (Euros)",
       shape = "Cashback",
       color = "Firm") +
  theme_minimal()





###############################################################################
## Price Maximization: Optimal Pricing (Static Environment)
## (Budget Constrained Approach)
###############################################################################

###'!!!! CORRECT TO BC MODEL !!! ##### -----


# Same Choiceset but with outside good as last row
# (Rows are ordered as: Outside, Apple, Samsung, Huawei, Amazon, Asus)
choiceset_BC <- rbind (alt_apple, alt_samsung, alt_huawei, alt_amazon, alt_asus, alt0)
rownames(choiceset_BC) <- c("Tablet 1 (Apple)", "Tablet 2 (Samsung)", 
                              "Tablet 3 (Huawei)", "Tablet 4 (Amazon)", "Tablet 5 (Asus)", "Outside") 
print("Market Choiceset:")
print(choiceset_BC)
choiceset_BC <- choiceset_BC[, c(33, 25, 24, 21, 18, 13, 14, 15, 16, 17, 19, 20, 22, 23, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 1:12)] # reorder columns to match order of hilf_BC with price in the first column
print(choiceset_BC)

probabilities_BC_BLP_log_cpp(hilf_BC$betaexchange, choiceset_BC, pr)

#Price Grid for price changes of Samsung
price_grid_BC <- seq(0.99, 20.99, by = 0.5)

#pricedemand_BC function to calculate the market shares for all competitors for each of the price points in price_grid_BC
pricedemand_BC <- function(prgrid,beta,chset,pr){
  shares = matrix(double(6*length(prgrid)),ncol=6) 
  for (i in 1:length(prgrid)){
    chset[2,1]=prgrid[i] # update price of Samsung (Row 2)
    shares[i,]=probabilities_BC_BLP_log_cpp(beta, chset, pr)
  }
  shares
}

#market shares for all competitors for each price point in price_grid_BC
demand_BC = pricedemand_BC(prgrid=price_grid_BC,beta=hilf_BC$betaexchange,chset=choiceset_BC,pr=pr)

#Plot price demand curves for ,i competitor. price_grid_BC represents the price of Samsung. 
#The y-axis represents the market share of each competitor for each price point.
for (i in 1:6){
  plot(price_grid_BC,demand_BC[,i],type='l',main=rownames(choiceset_BC)[i]);grid()
  
}


choiceset_BC_noprice <- choiceset_BC[, -1]  # Removes price from choiceset to reduce hassle with adjusting the rest of the code, price now defined in the functions themselves
print(choiceset_BC_noprice)


###optimal monopolist price from call to optimizer
###optimal monopolist price from call to optimizer
###optimal monopolist price from call to optimizer

profitcalc <- function(price,betamix,chset_,cost,bindex,pricefull){
  pricefull[bindex]=price
  chset=cbind(pricefull,chset_)
  #print(colnames(choiceset_BC))
  #print(head(choiceset_BC))
  #print(colnames(cbind(pricefull, choiceset_BC_noprice)))
  #print(head(cbind(pricefull, choiceset_BC_noprice)))
  #print(paste("Updating row:", bindex, "with price:", price))
  #print(pricefull)
  share = probabilities_BC_BLP_log_cpp(betamix, chset, pr=1)
  profits = share * (pricefull - cost) #profitfunction,, marketsize = 1
  return(list(profits=profits, share=share))
}

profitcalc(price=6.99, #starting price
           betamix=hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>.7,], #adjust the .99 later, only for testruns .99
           chset_=choiceset_BC_noprice, #choiceset without prices
           cost=c(4.50, 4.50, 2.90, 0.70, 2.00, 0),
           bindex=2, #Samsung 2nd row
           pricefull=c(8.99, 6.99, 3.99, 0.99, 2.99, 0))

# helper function for optimization
profitcalchelp <- function(price,betamix,chset_,cost,bindex,pricefull){
  profitcalc(price,betamix,chset_,cost,bindex,pricefull)$profits[bindex]}



pr_opt_SE=optim(par=6.99, fn=profitcalchelp, gr=NULL, 
          betamix=hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>.60,],
          chset_=choiceset_BC_noprice,
          cost=c(4.50, 4.50, 2.90, 0.70, 2.00, 0),
          bindex=2,
          pricefull=c(8.99, 6.99, 3.99, 0.99, 2.99, 0),
          hessian = TRUE, control=list(fnscale=-1), method = "Brent", lower = 0, upper = 20)



#profit maximizing price
pr_opt_SEM = matrix(c(pr_opt_SE$par,pr_opt_SE$value,pr_opt_SE$hessian),ncol=3)
colnames(pr_opt_SEM) <- c("optimal price", "profit", "hessian")
pr_opt_SEM


profitcalc(price=pr_opt_SEM[1],
           betamix=hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>.6,],
           chset_=choiceset_BC_noprice,
           cost=c(4.50, 4.50, 2.90, 0.70, 2.00, 0),
           bindex=2,
           pricefull=c(7.99, 6.99, 3.99, 0.99, 2.99, 0))

###############################################################################
## JOINT OPTIMIZATION: Price & Cashback for Samsung (Static Environment)
## (Budget-Constrained Approach with Fixed Costs)
###############################################################################

# --- Assumptions ---
cost <- c(4.50, 4.50, 2.90, 0.70, 2.00, 0)  # [Apple, Samsung, Huawei, Amazon, Asus, Outside]

pricefull_baseline <- c(8.99, 6.99, 3.99, 0.99, 2.99, 0)  # same ordering as above


# --- Helper Function: Update Cashback ---
# Defined above
#update_cashback <- function(row, cb) {
  # Reset the three cashback indicators:
#  row["50 Cash Back"] <- 0
 # row["100 Cash Back"] <- 0
  #row["150 Cash Back"] <- 0
  #if (cb == "50 Cash Back") {
  #  row["50 Cash Back"] <- 1
#  } else if (cb == "100 Cash Back") {
#    row["100 Cash Back"] <- 1
#  } else if (cb == "150 Cash Back") {
#    row["150 Cash Back"] <- 1
#  }
  # "No Cashback" leaves all as 0.
#  return(row)
#}


# --- Joint Profit Calculation Function ---
# This function computes the profit for Samsung given a candidate price and a chosen cashback option.
# It updates the Samsung row in the market choiceset accordingly.
profitcalc_joint <- function(price, betamix, chset, cost, bindex, pricefull, cashback_option) {
  # Update the price vector: set Samsung's price (bindex) to candidate price.
  pricefull[bindex] <- price
  # Make a copy of the choiceset (which does NOT include the price column, i.e. choiceset_BC_noprice)
  chset_updated <- chset
  # Update the cashback indicators for Samsung's row using the helper function.
  chset_updated[bindex, ] <- update_cashback(chset_updated[bindex, ], cashback_option)
  # Reconstruct the full design matrix by column-binding the price vector with the non-price attributes.
  full_design <- cbind(pricefull, chset_updated)
  # Compute market shares using the C++ function (assumed to take parameters: beta draws, design matrix, pr)
  share <- probabilities_BC_BLP_log_cpp(betamix, full_design, pr = 1)
  # Compute profit for Samsung:
  # Profit = (Samsung's price - Samsung's cost) * (Samsung's market share * market_size)
  profit <- share[bindex] * (pricefull[bindex] - cost[bindex])
  return(list(profits = profit, share = share))
}

# --- Helper Wrapper for Optimization ---
# Returns only the profit for Samsung (the bindex-th element).
profitcalchelp_joint <- function(price, betamix, chset, cost, bindex, pricefull, cashback_option) {
  profitcalc_joint(price, betamix, chset, cost, bindex, pricefull, cashback_option)$profits
}

# --- Joint Optimization: Grid Search Over Cashback Options ---
# Define the discrete set of cashback options:
cashback_options <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")

# Initialize a list to store optimization results for each cashback option.
results_joint <- list()

# For each cashback option, optimize Samsung's price (Samsung is row 2).
for (cb in cashback_options) {
  opt_res <- optim(
    par = pricefull_baseline[2],  # starting price for Samsung
    fn = profitcalchelp_joint,
    betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.90, ],  # select a random subset for simulation
    chset = choiceset_BC_noprice,   # choiceset without the price column
    cost = cost,
    bindex = 2,                    # Samsung is row 2 in choiceset_BC_noprice
    pricefull = pricefull_baseline,
    cashback_option = cb,
    hessian = TRUE,
    control = list(fnscale = -1),  # maximize profit
    method = "Brent",
    lower = 0,
    upper = 20
  )
  results_joint[[cb]] <- list(
    optimal_price = opt_res$par,
    profit = opt_res$value,
    hessian = opt_res$hessian
  )
}

# Combine the results into a data frame for easy comparison.
results_df <- do.call(rbind, lapply(names(results_joint), function(cb) {
  data.frame(Cashback = cb,
             optimal_price = results_joint[[cb]]$optimal_price,
             profit = results_joint[[cb]]$profit)
}))
print("Joint Optimization Results for Samsung:")
print(results_df)

# Identify the best (price, cashback) combination (i.e. highest profit)
optimal_result <- results_df[which.max(results_df$profit), ]
cat("Optimal Price and Cashback Combination for Samsung:\n")
print(optimal_result)

# Optionally, compute and display the profit and market share at the optimal combination.
optimal_profit_out <- profitcalc_joint(
  price = optimal_result$optimal_price,
  betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.5, ],
  chset = choiceset_BC_noprice,
  cost = cost,
  bindex = 2,
  pricefull = pricefull_baseline,
  cashback_option = optimal_result$Cashback
)
cat("Profit and Market Share at the Optimal Combination:\n")
print(optimal_profit_out)

###############################################################################
## (VI) COMPETITIVE EQUILIBRIUM: Optimal Pricing (Nash Equilibrium)
## (Budget-Constrained Approach with Fixed Costs)
###############################################################################


# 1) Define a Best-Response function that optimizes brand 'firm_index' price 
#    while other brands' prices remain fixed.

best_response_BC <- function(firm_index,
                             current_pricefull,
                             betamix,
                             chset_noprice,
                             cost_vec,
                             lower = 0,
                             upper = 25) {
  
  # Helper function for optim:
  profit_fun <- function(candidate_price) {
    # Use existing 'profitcalchelp' with brand 'firm_index'
    # to get the profit of this brand at candidate_price.
    profitcalchelp(
      price = candidate_price,
      betamix = betamix,
      chset_ = chset_noprice,
      cost = cost_vec,
      bindex = firm_index,
      pricefull = current_pricefull
    )
  }
  
  # Starting price for brand 'firm_index'
  start_price <- current_pricefull[firm_index]
  
  # Use Brent’s method to maximize profit for brand 'firm_index'
  res <- optim(
    par = start_price,
    fn = profit_fun,
    method = "Brent",
    lower = lower,
    upper = upper,
    control = list(fnscale = -1)
  )
  return(res$par)   # Return the optimal price
}

# 2) Iterative Algorithm to reach a Nash Equilibrium in prices
#    for the five inside firms (rows 1–5). Row 6 = Outside (static).
#    We keep competitor rows (Apple, Huawei, Amazon, Asus) active as well.

# Initialize a working copy of pricefull (use your baseline as a start).
current_pricefull <- pricefull_baseline

# Set parameters for iterative procedure
tolerance <- 0.1
max_iter <- 10
convergence <- FALSE
iteration <- 1

# We store the price history for each iteration to see if it converges
price_history <- matrix(NA, nrow = max_iter, ncol = 5)  # 5 inside firms
inside_firms <- 1:5  # Row indices for Apple=1, Samsung=2, Huawei=3, Amazon=4, Asus=5

colnames(price_history) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus")

while (!convergence && iteration <= max_iter) {
  
  new_prices <- current_pricefull
  
  # For each brand i in 1..5 (outside is row 6, not a decision-maker),
  # compute its best-response price given current prices of the others.
  for (i in inside_firms) {
    new_prices[i] <- best_response_BC(
      firm_index = i,
      current_pricefull = new_prices,  # pass the updated vector
      betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.7, ],  # random subset for simulation
      chset_noprice = choiceset_BC_noprice,
      cost_vec = cost,
      lower = 0,
      upper = 25
    )
  }
  
  # Record the updated prices (excluding outside option)
  price_history[iteration, ] <- new_prices[inside_firms]
  
  # Check if maximum change across the inside firms is below tolerance
  price_change <- max(abs(new_prices[inside_firms] - current_pricefull[inside_firms]))
  cat("Iteration", iteration, "max price change:", price_change, "\n")
  
  if (price_change < tolerance) {
    convergence <- TRUE
  } else {
    current_pricefull <- new_prices
  }
  
  iteration <- iteration + 1
}

# Trim the price_history to the number of actual iterations
price_history <- price_history[1:(iteration - 1), ]

# The final prices in current_pricefull are your Nash Equilibrium
equilibrium_prices <- current_pricefull
names(equilibrium_prices) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus", "Outside")

cat("\nNash Equilibrium Prices (Budget-Constrained, Fixed Costs):\n")
print(equilibrium_prices)

# 3) Compute Equilibrium Profits and Market Shares
final_profit_shares <- list()
for (i in inside_firms) {
  # For each brand i, we compute profit & share using 'profitcalc'
  out_res <- profitcalc(
    price = equilibrium_prices[i],
    betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.7, ],
    chset_ = choiceset_BC_noprice,
    cost = cost,
    bindex = i,
    pricefull = equilibrium_prices
  )
  final_profit_shares[[names(equilibrium_prices)[i]]] <- out_res
}

cat("\nEquilibrium Profit and Market Shares for Each Firm (Rows 1–5):\n")
print(final_profit_shares)

# 4) Visualize Convergence
library(ggplot2)
library(reshape2)
price_history_df <- data.frame(price_history)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_history_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_history_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth = 1.0) +
  geom_point() +
  labs(
    title = "Convergence of Prices (Budget-Constrained Nash Equilibrium)",
    x = "Iteration",
    y = "Price (Euros)"
  ) +
  theme_minimal()


###############################################################################
## JOINT OPTIMIZATION: Price & Cashback for Samsung (Base  Environment)
## (Budget-Constrained Approach with Fixed Costs)
###############################################################################

# --- Assumptions ---
# - The market choiceset "choiceset_BC" (with columns ordered as in hilf_BC; Price in column 1) 
#   is defined and contains the alternatives in the following order:
#   Row 1: Tablet 1 (Apple)
#   Row 2: Tablet 2 (Samsung)
#   Row 3: Tablet 3 (Huawei)
#   Row 4: Tablet 4 (Amazon)
#   Row 5: Tablet 5 (Asus)
#   Row 6: Outside
#
# - choiceset_BC_noprice is the same matrix but with the Price column removed.
#
# - pricefull_baseline is the vector of baseline prices (e.g., c(8.99, 6.99, 3.99, 0.99, 2.99, 0)).
#
# - cost_fixed is the fixed cost vector (which includes the cashback cost for the baseline promotion),
#   for example: cost_fixed <- c(4.50, 4.50, 2.90, 0.70, 2.00, 0)
#
# - For Samsung, the baseline promotion is assumed to be "100 Cash Back".
#
# - The function probabilities_BC_BLP_log_cpp(beta, design_matrix, pr) computes the market share vector.
#
# - profitcalc_joint() and its helper profitcalchelp_joint() below compute profit for Samsung
#   given a candidate price and a candidate cashback option.
#
# --- Define a Revised Cashback Cost Function ---
get_cashback_cost <- function(cashback_level) {
  if (cashback_level == "50 Cash Back") return(0.5 * 1)
  else if (cashback_level == "100 Cash Back") return(1.00 * 1)
  else if (cashback_level == "150 Cash Back") return(1.50 * 1)
  else return(0)  # "No Cashback"
}

# --- Revised Joint Profit Calculation Function ---
# This function computes the profit for Samsung (focal alternative) given a candidate price 
# and candidate cashback option.
profitcalc_joint <- function(price, betamix, chset_joint, cost_fixed, bindex, pricefull, candidate_cb, baseline_cb_current) {
  # bindex: row index for Samsung in the choiceset and price vector.
  # pricefull: full price vector (for all alternatives).
  # baseline_cb_current: the baseline cashback option that is embedded in cost_fixed.
  
  # First, extract Samsung's baseline cost by removing the cost of its current (baseline) cashback.
  baseline_cost_S <- cost_fixed[bindex] - get_cashback_cost(baseline_cb_current)
  
  # Then, compute the effective cost under the candidate cashback:
  effective_cost_S <- baseline_cost_S + get_cashback_cost(candidate_cb)
  
  # Update the price vector with the candidate price for Samsung:
  pricefull_updated <- pricefull
  pricefull_updated[bindex] <- price
  
  # Update the choiceset for Samsung:
  # chset_joint is the non-price portion of the design matrix.
  chset_updated <- chset_joint
  chset_updated[bindex, ] <- update_cashback(chset_updated[bindex, ], candidate_cb)
  
  # Reconstruct the full design matrix by binding the price vector with the non-price attributes.
  design_full <- cbind(pricefull_updated, chset_updated)
  
  # Compute market shares using the C++ function.
  share_vec <- probabilities_BC_BLP_log_cpp(betamix, design_full, pr = 1)
  
  # Profit for Samsung = (price - effective cost) * (market share * market size).
  # (Market size is assumed to be 1 in these calculations; adjust if needed.)
  profit_S <- share_vec[bindex] * (pricefull_updated[bindex] - effective_cost_S)
  
  return(list(profits = profit_S, share = share_vec[bindex]))
}

# --- Helper Wrapper Function for Optimization ---
profitcalchelp_joint <- function(price, betamix, chset_joint, cost_fixed, bindex, pricefull, candidate_cb, baseline_cb_current) {
  profitcalc_joint(price, betamix, chset_joint, cost_fixed, bindex, pricefull, candidate_cb, baseline_cb_current)$profits
}

# --- Define the Set of Candidate Cashback Options ---
candidate_cb_options <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")

# --- Set Up Baseline Vectors for Optimization ---
# Baseline price vector (ordered as in choiceset_BC):
pricefull_baseline <- c(8.99, 6.99, 3.99, 0.99, 2.99, 0)
# Fixed cost vector (including the baseline cashback cost, assumed embedded):
cost_fixed <- c(4.50, 4.50, 2.90, 0.70, 2.00, 0)
# For Samsung (row 2), the baseline cashback is "100 Cash Back".
baseline_cb_Samsung <- "100 Cash Back"

# --- Joint Optimization: Grid Search Over Candidate Cashback Options ---
# For each candidate cashback option, optimize the price for Samsung using Brent's method.
results_joint <- list()

for (cb_opt in candidate_cb_options) {
  opt_res_joint <- optim(
    par = pricefull_baseline[2],  # starting price for Samsung
    fn = function(p) {
      profitcalchelp_joint(p,
                           betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.9, ],
                           chset_joint = choiceset_BC_noprice,
                           cost_fixed = cost_fixed,
                           bindex = 2,
                           pricefull = pricefull_baseline,
                           candidate_cb = cb_opt,
                           baseline_cb_current = baseline_cb_Samsung)
    },
    method = "Brent",
    lower = 0,
    upper = 20,
    control = list(fnscale = -1)
  )
  results_joint[[cb_opt]] <- list(
    optimal_price = opt_res_joint$par,
    profit = opt_res_joint$value,
    hessian = opt_res_joint$hessian,
    candidate_cashback = cb_opt
  )
}

# Combine the results into a data frame for comparison.
results_joint_df <- do.call(rbind, lapply(names(results_joint), function(cb) {
  data.frame(Cashback = cb,
             optimal_price = results_joint[[cb]]$optimal_price,
             profit = results_joint[[cb]]$profit)
}))
print("Joint Optimization Results for Samsung (Price & Cashback):")
print(results_joint_df)

# Identify the best (price, cashback) combination based on profit.
optimal_result_joint <- results_joint_df[which.max(results_joint_df$profit), ]
cat("Optimal Price and Cashback Combination for Samsung:\n")
print(optimal_result_joint)

# Optionally, compute and display the profit and market share at the optimal combination.
optimal_profit_out_joint <- profitcalc_joint(
  price = optimal_result_joint$optimal_price,
  betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange)) > 0.7, ],
  chset_joint = choiceset_BC_noprice,
  cost_fixed = cost_fixed,
  bindex = 2,
  pricefull = pricefull_baseline,
  candidate_cb = optimal_result_joint$Cashback,
  baseline_cb_current = baseline_cb_Samsung
)
cat("Profit and Market Share at the Optimal Joint Combination:\n")
print(optimal_profit_out_joint)



# 5) Evaluate final equilibrium profits & shares for each inside brand
final_outcome <- list()
for (i in 1:5) {
  # effective cost: baseline_cost_vec[i] + get_cashback_cost(equilibrium_cb[i])
  local_cost <- baseline_cost_vec
  local_cost[i] <- baseline_cost_vec[i] + get_cashback_cost(equilibrium_cb[i])
  
  # build design matrix
  local_chset <- choiceset_BC_noprice
  local_chset[i, ] <- update_cashback(local_chset[i, ], equilibrium_cb[i])
  design_equil <- cbind(equilibrium_prices, local_chset)
  
  share_vec <- probabilities_BC_BLP_log_cpp(hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>0.7, ],
                                            design_equil, pr=1)
  brand_profit <- share_vec[i] * (equilibrium_prices[i] - local_cost[i])
  
  final_outcome[[names(equilibrium_prices)[i]]] <- list(
    Share  = share_vec[i],
    Profit = brand_profit
  )
}

cat("\nEquilibrium Market Shares and Profits:\n")
print(final_outcome)

#### Optional: Plot Price Convergence Over Iterations ####
# If you want to see how price evolves across iterations:
library(reshape2)
library(ggplot2)
price_history_df <- as.data.frame(price_history_joint)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth=1.0) +
  geom_point() +
  labs(title = "Convergence of Prices (Joint Price+Cashback Nash Equilibrium)",
       x = "Iteration",
       y = "Price (Euros)") +
  theme_minimal()

# Also possible to track CB changes over time in cb_history_joint.

###############################################################################
## (VII) JOINT COMPETITIVE OPTIMIZATION OF PRICE & CASHBACK (Nash Equilibrium)
## (Budget-Constrained Approach with Per-Brand Baseline Cost + Cashback)
###############################################################################

########### 1. Cost Function for Cashbacks ###########
# Example: For demonstration, we define the incremental cost for each 
# distinct cashback level. We multiply the nominal cashback amount by 1.1 
# to reflect overhead or partial redemption.
get_cashback_cost <- function(cashback_level) {
  if (cashback_level == "50 Cash Back")   return(0.50 * 1)  # 55 euros
  else if (cashback_level == "100 Cash Back") return(1.00 * 1) # 110 euros
  else if (cashback_level == "150 Cash Back") return(1.50 * 1) # 165 euros
  else return(0)  # "No Cashback"
}

# You could also scale them differently if your currency units are in hundreds, 
# or if partial redemption is expected. 
# For instance:
# if (cashback_level == "50 Cash Back")   return(0.5) 
# etc.
# Admittedly, 50 Cash Back = 0.5 units depends on your model’s scale.

########### 2. Helper to Update a Brand's Row with the Chosen Cashback ###########
# You presumably have something like:
update_cashback <- function(row, cb) {
  # reset indicators
  row["50 Cash Back"] <- 0
  row["100 Cash Back"] <- 0
  row["150 Cash Back"] <- 0
  if (cb == "50 Cash Back") {
    row["50 Cash Back"] <- 1
  } else if (cb == "100 Cash Back") {
    row["100 Cash Back"] <- 1
  } else if (cb == "150 Cash Back") {
    row["150 Cash Back"] <- 1
  }
  # 'No Cashback' => all zero.
  return(row)
}

########### 3. Best-Response with Joint (Price, Cashback) for One Firm ###########
# Each firm enumerates its discrete cashback options, 
# then for each option, uses Brent's method to find the best price. 
# It picks whichever (price, cb) yields the highest profit.

best_response_BC_joint <- function(firm_index,
                                   current_pricefull,
                                   current_cb_choices,
                                   betamix,
                                   chset_noprice,
                                   baseline_cost_vec,
                                   discrete_cashback_options = c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back"),
                                   lower = 1,
                                   upper = 14) {
  
  best_profit <- -Inf
  best_price <- NA
  best_cb <- NA
  
  # For each candidate cashback option:
  for (cb in discrete_cashback_options) {
    
    # define the objective function to pass to optim
    profit_fun <- function(candidate_price) {
      # Build a local copy of the price vector
      local_pricefull <- current_pricefull
      # brand i sets candidate_price
      local_pricefull[firm_index] <- candidate_price
      
      # cost = baseline_cost + get_cashback_cost
      # we modify brand i's cost specifically:
      local_cost <- baseline_cost_vec
      local_cost[firm_index] <- baseline_cost_vec[firm_index] + get_cashback_cost(cb)
      
      # We'll adapt your profitcalchelp to accept a brand's chosen cb:
      # But it might be simpler to do the logic in-line:
      # Construct a local design matrix with the chosen CB for brand i
      # We'll do that in one step after optimization. 
      
      # For share, we need to combine local_pricefull with chset_noprice
      # and also update the brand i's row with the chosen CB.
      local_chset <- chset_noprice
      local_chset[firm_index, ] <- update_cashback(local_chset[firm_index, ], cb)
      design_full <- cbind(local_pricefull, local_chset)
      
      # compute market shares
      share <- probabilities_BC_BLP_log_cpp(betamix, design_full, pr = 1)
      # brand i's profit
      brand_profit_i <- share[firm_index] * (local_pricefull[firm_index] - local_cost[firm_index])
      
      return(brand_profit_i)
    }
    
    # initial guess
    start_price <- current_pricefull[firm_index]
    
    # run Brent's method
    res <- optim(
      par = start_price,
      fn = profit_fun,
      method = "Brent",
      lower = lower,
      upper = upper,
      control = list(fnscale = -1)  # maximize
    )
    
    # Check the resulting profit
    candidate_opt_price <- res$par
    candidate_opt_profit <- res$value
    
    if (candidate_opt_profit > best_profit) {
      best_profit <- candidate_opt_profit
      best_price <- candidate_opt_price
      best_cb <- cb
    }
  }
  
  # Return brand i's best (price, cb, profit).
  return(list(price = best_price, cb = best_cb, profit = best_profit))
}

########### 4. Iterative Algorithm for Joint Price & Cashback Equilibrium ###########
# We have 5 inside brands: Apple(1), Samsung(2), Huawei(3), Amazon(4), Asus(5).
# Outside is row(6) with zero "price," not an optimizer.

# Baseline price vector for the 6 rows:
current_pricefull_joint <- c(8.99, 6.99, 3.99, 0.99, 2.99, 0)

# Baseline cost for each brand (outside=0). 
# This is the "no cashback" baseline cost. 
# Then we add get_cashback_cost(...) for the brand's chosen CB.
baseline_cost_vec <- c(4.50, 3.00, 1.70, 0.70, 2.00, 0)

# Start iterative approach
tolerance <- 0.1
max_iter <- 20
convergence <- FALSE
iteration <- 1

# We'll store (price, cb) for each brand. Let’s store CB in a separate vector:
cb_choices <- rep("No Cashback", 6)  # row 1..5, row6=Outside?

# For tracking iteration history:
price_history_joint <- matrix(NA, nrow = max_iter, ncol = 5)
cb_history_joint <- matrix(NA, nrow = max_iter, ncol = 5)
colnames(price_history_joint) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus")
colnames(cb_history_joint) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus")

while (!convergence && iteration <= max_iter) {
  
  old_prices <- current_pricefull_joint
  old_cb     <- cb_choices
  
  # For each brand i in 1..5:
  for (i in 1:5) {
    
    # brand i picks best (price, cb)
    res_br <- best_response_BC_joint(
      firm_index = i,
      current_pricefull = current_pricefull_joint,
      current_cb_choices = cb_choices,
      betamix = hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>0.9,],
      chset_noprice = choiceset_BC_noprice,
      baseline_cost_vec = baseline_cost_vec,
      discrete_cashback_options = c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back"),
      lower = 1, upper = 14
    )
    
    # update brand i's chosen price & cb
    current_pricefull_joint[i] <- res_br$price
    cb_choices[i] <- res_br$cb
  }
  
  # record iteration
  price_history_joint[iteration, ] <- current_pricefull_joint[1:5]
  cb_history_joint[iteration, ] <- cb_choices[1:5]
  
  # check if changes < tolerance
  price_change <- max(abs(current_pricefull_joint[1:5] - old_prices[1:5]))
  # if you want to consider CB changes as well, treat them as "any changed" => or define a small function that sets numeric code for each CB option
  cb_changed <- any(cb_choices[1:5] != old_cb[1:5])
  
  cat(sprintf("Iteration %d, max price change= %.4f\n", iteration, price_change))
  
  if (price_change < tolerance && !cb_changed) {
    convergence <- TRUE
  }
  
  iteration <- iteration + 1
}

price_history_joint <- price_history_joint[1:(iteration-1), ]
cb_history_joint    <- cb_history_joint[1:(iteration-1), ]

cat("\nJoint Price & Cashback Equilibrium Found:\n")
equilibrium_prices <- current_pricefull_joint
names(equilibrium_prices) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus", "Outside")
equilibrium_cb <- cb_choices
names(equilibrium_cb) <- c("Apple", "Samsung", "Huawei", "Amazon", "Asus", "Outside")

print(list(Prices = equilibrium_prices, Cashbacks = equilibrium_cb))

# 5) Evaluate final equilibrium profits & shares for each inside brand
final_outcome <- list()
for (i in 1:5) {
  # effective cost: baseline_cost_vec[i] + get_cashback_cost(equilibrium_cb[i])
  local_cost <- baseline_cost_vec
  local_cost[i] <- baseline_cost_vec[i] + get_cashback_cost(equilibrium_cb[i])
  
  # build design matrix
  local_chset <- choiceset_BC_noprice
  local_chset[i, ] <- update_cashback(local_chset[i, ], equilibrium_cb[i])
  design_equil <- cbind(equilibrium_prices, local_chset)
  
  share_vec <- probabilities_BC_BLP_log_cpp(hilf_BC$betaexchange[runif(nrow(hilf_BC$betaexchange))>0.7, ],
                                            design_equil, pr=1)
  brand_profit <- share_vec[i] * (equilibrium_prices[i] - local_cost[i])
  
  final_outcome[[names(equilibrium_prices)[i]]] <- list(
    Share  = share_vec[i],
    Profit = brand_profit
  )
}

cat("\nEquilibrium Market Shares and Profits:\n")
print(final_outcome)

#### Optional: Plot Price Convergence Over Iterations ####
# If you want to see how price evolves across iterations:
library(reshape2)
library(ggplot2)
price_history_df <- as.data.frame(price_history_joint)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_melt <- melt(price_history_df, id.vars = "Iteration", variable.name = "Firm", value.name = "Price")

ggplot(price_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(linewidth=1.0) +
  geom_point() +
  labs(title = "Convergence of Prices (Joint Price+Cashback Nash Equilibrium)",
       x = "Iteration",
       y = "Price (Euros)") +
  theme_minimal()

# Also possible to track CB changes over time in cb_history_joint.


# Load necessary libraries
library(ggplot2)
library(reshape2)

# ---------------------------
# Plot 1: Convergence of Price Decisions
# ---------------------------
# price_history_joint is a matrix (iterations x 5) with columns for Apple, Samsung, Huawei, Amazon, and Asus.
price_history_df <- as.data.frame(price_history_joint)
price_history_df$Iteration <- 1:nrow(price_history_df)
price_melt <- melt(price_history_df, id.vars = "Iteration", 
                   variable.name = "Firm", value.name = "Price")

ggplot(price_melt, aes(x = Iteration, y = Price, color = Firm)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Convergence of Price Decisions",
       x = "Iteration", y = "Price (EUR)") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(hjust = 0.5))

# ---------------------------
# Plot 2: Convergence of Cashback Decisions
# ---------------------------
# cb_history_joint is a matrix (iterations x 5) of discrete cashback choices.
# Convert these to a factor and then assign numeric codes for plotting.
cb_history_df <- as.data.frame(cb_history_joint)
cb_history_df$Iteration <- 1:nrow(cb_history_df)
cb_melt <- melt(cb_history_df, id.vars = "Iteration", 
                variable.name = "Firm", value.name = "Cashback")
# Define the desired order for cashback options.
cashback_levels <- c("No Cashback", "50 Cash Back", "100 Cash Back", "150 Cash Back")
cb_melt$Cashback <- factor(cb_melt$Cashback, levels = cashback_levels)
# Create a numeric version for plotting
cb_melt$Cashback_num <- as.numeric(cb_melt$Cashback)

ggplot(cb_melt, aes(x = Iteration, y = Cashback_num, color = Firm)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = 1:4, labels = cashback_levels) +
  labs(title = "Convergence of Cashback Decisions",
       x = "Iteration", y = "Cashback Level") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(hjust = 0.5))


# Load required packages
library(reshape2)
library(ggplot2)

# Assume price_history_joint and cb_history_joint are matrices 
# (iterations x 5) for the five inside brands (Apple, Samsung, Huawei, Amazon, Asus).

# Convert price history to a data frame.
price_df <- as.data.frame(price_history_joint)
price_df$Iteration <- 1:nrow(price_df)
price_melt <- melt(price_df, id.vars = "Iteration", 
                   variable.name = "Brand", value.name = "Price")

# Convert cashback history to a data frame.
cb_df <- as.data.frame(cb_history_joint)
cb_df$Iteration <- 1:nrow(cb_df)
cb_melt <- melt(cb_df, id.vars = "Iteration", 
                variable.name = "Brand", value.name = "Cashback")

# Merge the two data frames by Iteration and Brand.
combined_df <- merge(price_melt, cb_melt, by = c("Iteration", "Brand"))

# Create the plot.
ggplot(combined_df, aes(x = Iteration, y = Price, group = Brand, color = Brand)) +
  geom_line(size = 1) +
  geom_point(aes(shape = Cashback), size = 3) +
  scale_shape_manual(values = c(16, 17, 15, 8),
                     name = "Cashback Level") +
  labs(title = "Convergence of Price and Cashback Decisions",
       x = "Iteration",
       y = "Price (EUR)") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(hjust = 0.5))


# Load required packages
library(reshape2)
library(ggplot2)

# Assume price_history_joint and cb_history_joint are matrices 
# (iterations x 5) for the five inside brands (Apple, Samsung, Huawei, Amazon, Asus).

# Convert price history to a data frame.
price_df <- as.data.frame(price_history_joint)
price_df$Iteration <- 1:nrow(price_df)
price_melt <- melt(price_df, id.vars = "Iteration", 
                   variable.name = "Brand", value.name = "Price")

# Convert cashback history to a data frame.
cb_df <- as.data.frame(cb_history_joint)
cb_df$Iteration <- 1:nrow(cb_df)
cb_melt <- melt(cb_df, id.vars = "Iteration", 
                variable.name = "Brand", value.name = "Cashback")

# Merge the two data frames by Iteration and Brand.
combined_df <- merge(price_melt, cb_melt, by = c("Iteration", "Brand"))

# Create the APA-style plot.
ggplot(combined_df, aes(x = Iteration, y = Price, group = Brand, color = Brand)) +
  geom_line(size = 1.2) +  # Slightly thicker lines
  geom_point(aes(shape = as.factor(Cashback)), size = 3) +  # Shape based on cashback
  scale_shape_manual(values = c(16, 17, 15, 8), name = "Cashback Level") +
  scale_color_manual(values = c("black", "darkgray", "dimgray", "gray", "lightgray")) +  # APA grayscale
  labs(
       x = "Iteration",
       y = "Price (in 100 EUR)") +
  theme_classic() +  # APA prefers simple, clean styles
  theme(
    text = element_text(family = "Times New Roman", size = 12),  # APA font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered, bold title
    axis.title = element_text(face = "bold", size = 12),  # Bold axis labels
    axis.text = element_text(size = 11),  # Readable axis text
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.position = "right",  # Keep legend clear
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Subtle APA grid
    panel.grid.minor = element_blank()  # No minor grid lines
  )



# Load required packages
library(reshape2)
library(ggplot2)

# Assume price_history_joint and cb_history_joint are matrices 
# (iterations x 5) for the five inside brands (Apple, Samsung, Huawei, Amazon, Asus).

# Convert price history to a data frame.
price_df <- as.data.frame(price_history_joint)
price_df$Iteration <- 1:nrow(price_df)
price_melt <- melt(price_df, id.vars = "Iteration", 
                   variable.name = "Brand", value.name = "Price")

# Convert cashback history to a data frame.
cb_df <- as.data.frame(cb_history_joint)
cb_df$Iteration <- 1:nrow(cb_df)
cb_melt <- melt(cb_df, id.vars = "Iteration", 
                variable.name = "Brand", value.name = "Cashback")

# Merge the two data frames by Iteration and Brand.
combined_df <- merge(price_melt, cb_melt, by = c("Iteration", "Brand"))

# Create the APA-style plot.
ggplot(combined_df, aes(x = Iteration, y = Price, group = Brand, color = Brand)) +
  geom_line(size = 1.2) +  # Thicker lines
  geom_point(aes(shape = as.factor(Cashback)), size = 3) +  # Shape based on cashback
  scale_shape_manual(values = c(16, 17, 15, 8), name = "Cashback Level") +
  scale_color_manual(values = c("black", "darkgray", "dimgray", "gray", "lightgray")) +  # APA grayscale
  labs(title = "Convergence of Price and Cashback Decisions",
       x = "Iteration",
       y = "Price (EUR)") +
  theme_classic() +  # APA prefers simple, clean styles
  theme(
    text = element_text(family = "Times New Roman", size = 12),  # APA font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered, bold title
    axis.title = element_text(face = "bold", size = 12),  # Bold axis labels
    axis.text = element_text(size = 11),  # Readable axis text
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.position = "bottom",  # Legend below
    legend.box = "horizontal",  # Arrange legend horizontally
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Subtle APA grid
    panel.grid.minor = element_blank()  # No minor grid lines
  )
