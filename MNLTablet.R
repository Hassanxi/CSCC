################################################################################
##   0. Setup & Data Preparation
##   1. Model-Free Analysis (Frequencies, Cross-Tabs, Visuals)
##   2. Base MNL Model
##   3. Interaction MNL Model
################################################################################
# Variables:
# "RespID"                 = Identifies each unique survey respondent
# "TaskID"                 = Identifies each unique choice task for a respondent
# "AltID"                  = Identifies the alternative within each choice task (1 to 4)
# "Chosen"                 = 1 if the alternative is chosen, 0 otherwise
# "chid"                   = Combined identifier for unique choice situations (i.e.,"RespID_TaskID")

# Attribute Variables:
# "System_B"               = Indicates the operating system (0 = OS A, 1 = OS B)
# "Price"                  = Purchase price in euros (e.g., 99, 299, ..., 899)
# "Brand"                  = Indicates the brand (A, B, C, D, E, F, G)
# "Resolution"             = Display resolution ("Standard" or "High")
# "Memory"                 = Storage memory in GB (8GB, 16GB, 32GB, 64GB, 128GB)
# "SD_Slot"                = Indicates whether the device has an SD slot ("With", "Without")
# "Performance"            = Processor speed ("1 GHz", "1.6 GHz", "2.2 GHz")
# "Battery_Run_Time"       = Battery runtime ("4-8 hours", "8-12 hours")
# "Connections"            = Type of network connectivity ("WLAN", "WLAN + UMTS/3G", "WLAN + LTE/4G")
# "Sync_to_Smartphone"     = Indicates synchronization to a smartphone ("No", "Yes")
# "Value_Pack"             = Indicates availability of a value pack ("No", "Yes")
# "Equipment"              = Additional accessories ("None", "Cover", "Keyboard", "Mouse", "Pencil", etc.)
# "Cash_Back"              = Cashback offers ("No Cash Back", "50 EUR", "100 EUR", "150 EUR")
# "Display_Size"           = Screen size in inches (7, 8, 10, 12, 13)
##########################################################################################################
#############################################
## 0. Setup and Data Preparation
#############################################

# Function to load or install required packages
load_or_install <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required packages
packages <- c("readr", "dplyr", "tidyr", "ggplot2", "parallel", "sf", 
              "stringr", "data.table", "pbapply", "caret", "mlogit", 
              "bayesm", "rstanarm")
lapply(packages, load_or_install)

# STEP 1: Load the raw data
# Load tablet attribute data for reference (not used for modeling)
tablet_data_unf <- fread("/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/tablets.csv")

# STEP 2: Load the respondent estimation data
path_e_data_mod <- "/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/Estimation_Data_tab.Rdata"
load(path_e_data_mod)

# Inspect the structure of the loaded estimation data
str(E_Data_mod)

# Function to prepare data for all respondents
prepare_data <- function(E_Data_mod) {
  data_list <- lapply(seq_along(E_Data_mod$lgtdata), function(i) {
    # Extract respondent data
    respondent <- E_Data_mod$lgtdata[[i]]
    y <- respondent$y  # Choices made
    X <- respondent$X  # Design matrix
    
    # Validate number of alternatives per task
    num_tasks <- length(y)
    num_alts <- nrow(X) / num_tasks
    if (num_alts != 4) {
      stop("Number of alternatives per task is not 4.")
    }
    
    # Create identifiers
    TaskID <- rep(1:num_tasks, each = num_alts)
    AltID  <- rep(1:num_alts, times = num_tasks)
    RespID <- rep(i, nrow(X))
    
    # Binary chosen indicator
    Chosen <- as.integer(AltID == rep(y, each = num_alts))
    
    # Combine into data.frame
    df <- data.frame(
      RespID = RespID,
      TaskID = TaskID,
      AltID  = AltID,
      Chosen = Chosen
    )
    df <- cbind(df, X)
    return(df)
  })
  
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

# Prepare the combined data
combined_data <- prepare_data(E_Data_mod)

# Data cleaning and attribute transformation
combined_data <- combined_data %>%
  rename_with(~ gsub("\\s+", "_", .)) %>%
  rename_with(~ gsub("\\(|\\)", "", .)) %>%
  rename_with(~ gsub("-", "", .)) %>%
  mutate(chid = paste0(RespID, "_", TaskID)) %>%
  mutate(
    Brand = case_when(
      Brand_1 == 1 ~ "A",
      Brand_2 == 1 ~ "B",
      Brand_3 == 1 ~ "C",
      Brand_4 == 1 ~ "D",
      Brand_5 == 1 ~ "E",
      Brand_6 == 1 ~ "F",
      Brand_7 == 1 ~ "G",
      TRUE ~ "Outside Option"
    ),
    Resolution = ifelse(High_Resolution_264_ppi == 1, "High", "Standard"),
    Memory = case_when(
      `16_GB` == 1 ~ "16GB",
      `32_GB` == 1 ~ "32GB",
      `64_GB` == 1 ~ "64GB",
      `128GB` == 1 ~ "128GB",
      TRUE ~ "8GB"
    ),
    SD_Slot = ifelse(Without_SDSlot == 1, "Without", "With"),
    Performance = case_when(
      `1.6_GHz` == 1 ~ "1.6 GHz",
      `2.2_GHz` == 1 ~ "2.2 GHz",
      TRUE ~ "1 GHz"
    ),
    Battery_Run_Time = ifelse(`812_h._Runtime` == 1, "8-12 hours", "4-8 hours"),
    Connections = case_when(
      `WLAN_+_UMTS/3G` == 1 ~ "WLAN + UMTS (3G)",
      `WLAN_+_LTE/4G` == 1 ~ "WLAN + LTE (4G)",
      TRUE ~ "WLAN"
    ),
    Sync_to_Smartphone = ifelse(Sphone_Synch. == 1, "Yes", "No"),
    Value_Pack = ifelse(Value_Pack == 1, "Yes", "No"),
    Equipment = case_when(
      Cover == 1 ~ "Cover",
      Keyboard == 1 ~ "Keyboard",
      Mouse == 1 ~ "Mouse",
      Pencil == 1 ~ "Pencil",
      `32_GB_Memory_Card` == 1 ~ "32GB Memory Card",
      `Keyboard_+_Pencil` == 1 ~ "Keyboard + Pencil",
      `Keyboard_+_Mouse_+_Pencil` == 1 ~ "Keyboard + Mouse + Pencil",
      TRUE ~ "None"
    ),
    Cash_Back = case_when(
      `50_Cash_Back` == 1 ~ "50 EUR",
      `100_Cash_Back` == 1 ~ "100 EUR",
      `150_Cash_Back` == 1 ~ "150 EUR",
      TRUE ~ "No Cash Back"
    ),
    Display_Size = case_when(
      `8_Inches` == 1 ~ "8 Inches",
      `10_Inches` == 1 ~ "10 Inches",
      `12_Inches` == 1 ~ "12 Inches",
      `13_Inches` == 1 ~ "13 Inches",
      TRUE ~ "7 Inches"
    )
  ) %>%
  select(-starts_with("Brand_"), -High_Resolution_264_ppi, -`16_GB`, -`32_GB`, -`64_GB`, -`128GB`, 
         -Without_SDSlot, -`1.6_GHz`, -`2.2_GHz`, -`812_h._Runtime`, -`WLAN_+_UMTS/3G`, -`WLAN_+_LTE/4G`, 
         -Sphone_Synch., -Cover, -Keyboard, -Mouse, -Pencil, -`32_GB_Memory_Card`, 
         -`Keyboard_+_Pencil`, -`Keyboard_+_Mouse_+_Pencil`, -`50_Cash_Back`, -`100_Cash_Back`, 
         -`150_Cash_Back`, -`8_Inches`, -`10_Inches`, -`12_Inches`, -`13_Inches`)

# Relevel factors
combined_data <- combined_data %>%
  mutate(
    Brand = relevel(factor(Brand), ref = "Outside Option"),
    Resolution = relevel(factor(Resolution), ref = "Standard"),
    Memory = relevel(factor(Memory), ref = "8GB"),
    SD_Slot = relevel(factor(SD_Slot), ref = "With"),
    Performance = relevel(factor(Performance), ref = "1 GHz"),
    Battery_Run_Time = relevel(factor(Battery_Run_Time), ref = "4-8 hours"),
    Connections = relevel(factor(Connections), ref = "WLAN"),
    Sync_to_Smartphone = relevel(factor(Sync_to_Smartphone), ref = "No"),
    Value_Pack = relevel(factor(Value_Pack), ref = "No"),
    Equipment = relevel(factor(Equipment), ref = "None"),
    Cash_Back = relevel(factor(Cash_Back), ref = "No Cash Back"),
    Display_Size = relevel(factor(Display_Size), ref = "7 Inches")
  )

# Convert to mlogit format
mlogit_data <- mlogit.data(
  combined_data,
  choice = "Chosen",
  shape  = "long",
  alt.var = "AltID",
  chid.var = "chid"
)


#############################################
## 1 Model-Free Analysis
#############################################
# We'll examine frequencies, cross-tabulations, and chosen analysis 
# for a thorough model-free exploration.
#############################################
## 1.1 : Frequencies & Tables
#############################################
# List of key attributes to explore
attributes <- c(
  "Brand", "Cash_Back", "Resolution", "Memory", "SD_Slot", 
  "Performance", "Battery_Run_Time", "Connections", 
  "Sync_to_Smartphone", "Value_Pack", "Equipment", "Display_Size"
)

cat("\n===========================================\n")
cat("    FREQUENCIES & PROPORTIONS OF ATTRIBUTES\n")
cat("===========================================\n")

# Basic Frequency and Proportion for each attribute
for (attr in attributes) {
  cat("\n----------------------------\n")
  cat("Attribute:", attr, "\n")
  cat("----------------------------\n")
  
  freq_table <- table(combined_data[[attr]])
  prop_table <- prop.table(freq_table)
  
  cat("Frequency:\n")
  print(freq_table)
  
  cat("\nProportion:\n")
  print(prop_table)
}

#############################################
## 1.2 Cross-Tabulations
#############################################


cat("\n===========================================\n")
cat("      CROSS-TABULATIONS OF ATTRIBUTES\n")
cat("===========================================\n")

# 1) Cash_Back x Brand
cat("\n--- Cross-tabulation: Cash_Back x Brand ---\n")
table_brand_cb <- table(combined_data$Brand, combined_data$Cash_Back)
print(table_brand_cb)
cat("\nRow-wise Proportions:\n")
print(prop.table(table_brand_cb, margin = 1))
cat("\nColumn-wise Proportions:\n")
print(prop.table(table_brand_cb, margin = 2))

# 2) Price Binning
cat("\n--- Cross-tabulation: Price_Bin x Cash_Back ---\n")
combined_data$Price_Bin <- cut(
  combined_data$Price,
  breaks = c(-Inf, 0, 2, 4, 6, 8, Inf),
  labels = c("0", "0-2", "2-4", "4-6", "6-8", ">8")
)

table_price_cb <- table(combined_data$Price_Bin, combined_data$Cash_Back)
cat("\nFrequency:\n")
print(table_price_cb)
cat("\nRow-wise Proportions (Price_Bin basis):\n")
print(prop.table(table_price_cb, margin = 1))
cat("\nColumn-wise Proportions (Cash_Back basis):\n")
print(prop.table(table_price_cb, margin = 2))

# 3) Brand x Memory
cat("\n--- Cross-tabulation: Brand x Memory ---\n")
table_brand_memory <- table(combined_data$Brand, combined_data$Memory)
print(table_brand_memory)
cat("\nRow-wise Proportions:\n")
print(prop.table(table_brand_memory, margin = 1))
cat("\nColumn-wise Proportions:\n")
print(prop.table(table_brand_memory, margin = 2))

# 4) Brand x Display_Size
cat("\n--- Cross-tabulation: Brand x Display_Size ---\n")
table_brand_display <- table(combined_data$Brand, combined_data$Display_Size)
print(table_brand_display)
cat("\nRow-wise Proportions:\n")
print(prop.table(table_brand_display, margin = 1))
cat("\nColumn-wise Proportions:\n")
print(prop.table(table_brand_display, margin = 2))

#############################################
## 1.3 Analysis of Chosen Alternatives
#############################################

cat("\n===========================================\n")
cat("        CHOSEN ALTERNATIVES ANALYSIS\n")
cat("===========================================\n")

# Filter rows where the alternative was chosen
chosen_rows <- combined_data %>% filter(Chosen == 1)

cat("\n--- Frequency and Proportion of Chosen Cash_Back Levels ---\n")
chosen_cb_freq <- table(chosen_rows$Cash_Back)
print(chosen_cb_freq)

cat("\nProportion among chosen:\n")
print(prop.table(chosen_cb_freq))

cat("\nProportion of Cash_Back in Full Dataset:\n")
print(prop.table(table(combined_data$Cash_Back)))

# Price statistics for chosen rows
cat("\n--- Price Statistics for Chosen Rows ---\n")
chosen_price_stats <- chosen_rows %>%
  summarise(
    mean_price   = mean(Price),
    median_price = median(Price),
    min_price    = min(Price),
    max_price    = max(Price)
  )
print(chosen_price_stats)

# Summaries by Brand and Cash_Back
cat("\n--- Summary by Brand and Cash_Back ---\n")
combined_data %>%
  group_by(Brand, Cash_Back) %>%
  summarise(
    count        = n(),
    mean_price   = mean(Price),
    mean_chosen  = mean(Chosen),
    .groups = "drop"
  ) %>%
  arrange(Cash_Back, Brand) %>%
  print()


#############################################
## 1.4 Additional Summaries and Checks
#############################################

cat("\n===========================================\n")
cat("      ADDITIONAL SUMMARIES AND CHECKS\n")
cat("===========================================\n")

# Brand-level Summaries
cat("\n--- Brand-level Summaries ---\n")
brand_summaries <- combined_data %>%
  group_by(Brand) %>%
  summarise(
    count          = n(),
    mean_price     = mean(Price),
    median_price   = median(Price),
    chosen_rate    = mean(Chosen),
    top_memory     = names(sort(table(Memory), decreasing = TRUE))[1],
    top_display    = names(sort(table(Display_Size), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(count))
print(brand_summaries)

# Unique (Brand, Cash_Back, Price) combos
cat("\n--- Number of Unique (Brand, Cash_Back, Price) Combinations ---\n")
unique_combos <- combined_data %>%
  distinct(Brand, Cash_Back, Price) %>%
  nrow()
cat("Unique combinations =", unique_combos, "\n")


#############################################
## 1.5 Visualizations
#############################################


cat("\n===========================================\n")
cat("      VISUALIZATIONS (MODEL-FREE)\n")
cat("===========================================\n")

# 2.1 Bar plot for Cash_Back frequencies
cat("\n--- Bar Plot of Cash_Back Frequencies ---\n")
ggplot(as.data.frame(table(combined_data$Cash_Back)), aes(Var1, Freq)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Frequency of Cashback Levels",
    x = "Cash_Back",
    y = "Frequency"
  ) +
  theme_minimal()

# 2.2 Boxplot of Price by Cash_Back
cat("\n--- Boxplot of Price by Cash_Back ---\n")
ggplot(combined_data, aes(x = Cash_Back, y = Price)) +
  geom_boxplot() +
  labs(
    title = "Price Distribution by Cashback Level",
    x = "Cash_Back",
    y = "Price"
  ) +
  theme_minimal()

# 2.3 Stacked bar of Brand across Price_Bin
cat("\n--- Proportion of Brands Across Price Bins ---\n")
ggplot(combined_data, aes(x = Price_Bin, fill = Brand)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Brands Across Price Bins",
    x = "Price Bin",
    y = "Proportion"
  ) +
  theme_minimal()

# 2.4 Price vs. Probability of Being Chosen (line plot)
cat("\n--- Price vs. Probability of Being Chosen ---\n")
combined_data %>%
  group_by(Price) %>%
  summarise(mean_chosen = mean(Chosen)) %>%
  ggplot(aes(x = Price, y = mean_chosen)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Price vs. Probability of Being Chosen",
    x = "Price",
    y = "Mean Probability of Choice"
  ) +
  theme_minimal()

# 2.5 Nonlinear Effects: Price Bin vs. Mean Chosen
cat("\n--- Nonlinear Effects: Price Bin vs. Mean Chosen ---\n")
combined_data %>%
  group_by(Price_Bin) %>%
  summarise(mean_chosen = mean(Chosen)) %>%
  ggplot(aes(x = Price_Bin, y = mean_chosen)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Nonlinear Effects: Price Bin vs. Mean Chosen",
    x = "Price Bin",
    y = "Mean Probability of Choice"
  ) +
  theme_minimal()

# 2.6 Example: Distribution of Memory within Brand
cat("\n--- Bar Plot: Distribution of Memory within each Brand ---\n")
ggplot(combined_data, aes(x = Memory, fill = Brand)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Memory within each Brand",
    x = "Memory",
    y = "Count"
  ) +
  theme_minimal()

cat("\n\n======== END OF MODEL-FREE ANALYSIS ========\n")

#############################################
## 2. Model Specification and Estimation (BASE MODEL)
#############################################

# 2.1 Base formula (no interactions)
mnl_formula <- Chosen ~ 
  Price + 
  System_B + 
  Brand + 
  Resolution + 
  Memory +
  SD_Slot + 
  Performance + 
  Battery_Run_Time + 
  Connections +
  Sync_to_Smartphone + 
  Value_Pack + 
  Equipment + 
  Cash_Back +
  Display_Size | 0

# 2.2 Estimate the BASE MNL model
mnl_model <- mlogit(
  formula = mnl_formula,
  data    = mlogit_data
)

# 2.3 View summary
summary(mnl_model)


#############################################
## 2.4 Compute Model Fit Metrics (BASE MODEL)
#############################################

# 2.4.1 Log-likelihood
log_likelihood <- logLik(mnl_model)

# 2.4.2 Null log-likelihood (uniform distribution among 4 alts)
num_tasks <- nrow(mlogit_data) / 4
null_loglik_uniform <- num_tasks * log(1/4)

# 2.4.3 Number of parameters (k) & observations (n)
k <- length(coef(mnl_model))
n <- nrow(mlogit_data)

# 2.4.4 McFadden's R^2
mcfadden_r2 <- 1 - (as.numeric(log_likelihood) / null_loglik_uniform)

# 2.4.5 Likelihood Ratio (LR) test
lr_test <- -2 * (null_loglik_uniform - as.numeric(log_likelihood))
lr_p_value <- pchisq(lr_test, df = k, lower.tail = FALSE)

# 2.4.6 AIC & BIC
aic <- AIC(mnl_model) 
bic <- -2 * as.numeric(log_likelihood) + k * log(n)

# 2.4.7 Predictive accuracy
mnl_predictions   <- predict(mnl_model, newdata = mlogit_data)
predicted_choice  <- apply(mnl_predictions, 1, which.max)
actual_choice     <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy          <- mean(predicted_choice == actual_choice)
accuracy_percent  <- accuracy * 100

#############################################
## 2.5 Print Model Fit Results (BASE MODEL)
#############################################

cat("\nMODEL FIT METRICS (Base Model)\n")
cat("----------------------------------------\n")
cat("Log-Likelihood (Model):  ", as.numeric(log_likelihood), "\n")
cat("Log-Likelihood (Null):   ", null_loglik_uniform, "\n")
cat("McFadden's R-squared:    ", mcfadden_r2, "\n")
cat("Likelihood Ratio (LR):   ", lr_test, "\n")
cat("LR Test p-value:         ", lr_p_value, "\n")
cat("AIC:                     ", aic, "\n")
cat("BIC:                     ", bic, "\n")
cat("Predictive Accuracy (%): ", accuracy_percent, "\n")

#############################################
## 2.6 Diagnostics (BASE MODEL)
#############################################

# 2.6.1 1st-order condition
#  => summary(mnl_model) typically shows gradient info
#  => "gradient close to zero" means solution is good

# 2.6.2 2nd-order condition: Hessian negative definite
hessian_eigs <- eigen(mnl_model$hessian)$values
cat("\nHessian Eigenvalues (Base Model):\n")
print(hessian_eigs)
cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")


#############################################
## 3. Model Specification and Estimation (INTERACTION MODEL)
#############################################

# 3.1 Define formula with interactions
#     We add Price:Cash_Back, Price:Brand, Battery_Run_Time:Price, Memory:SD_Slot

mnl_formula_interactions <- Chosen ~ 
  # main effects
  Price + 
  System_B + 
  Brand + 
  Resolution + 
  Memory +
  SD_Slot + 
  Performance + 
  Battery_Run_Time + 
  Connections +
  Sync_to_Smartphone + 
  Value_Pack + 
  Equipment + 
  Cash_Back +
  Display_Size +
  
  # interaction effects
  Price:Cash_Back +
  # Price:Brand +
  # Price:Performance +
  Memory:SD_Slot |
  0

# 3.2 Estimate the MNL model with interactions
mnl_model_int <- mlogit(
  formula = mnl_formula_interactions,
  data    = mlogit_data
)

# 3.3 View summary
summary(mnl_model_int)


#############################################
## 3.4 Compute Model Fit Metrics (INTERACTION MODEL)
#############################################

# 3.4.1 Log-likelihood
log_likelihood_int <- logLik(mnl_model_int)

# 3.4.2 Same null log-likelihood as before
#     (still uniform across 4 alts)
null_loglik_int <- null_loglik_uniform

# 3.4.3 Number of parameters (k2) & observations (n2)
k2 <- length(coef(mnl_model_int))
n2 <- nrow(mlogit_data)

# 3.4.4 McFadden's R^2
mcfadden_r2_int <- 1 - (as.numeric(log_likelihood_int) / null_loglik_int)

# 3.4.5 Likelihood Ratio (LR) test
lr_test_int <- -2 * (null_loglik_int - as.numeric(log_likelihood_int))
lr_p_value_int <- pchisq(lr_test_int, df = k2, lower.tail = FALSE)

# 3.4.6 AIC & BIC
aic_int <- AIC(mnl_model_int) 
bic_int <- -2 * as.numeric(log_likelihood_int) + k2 * log(n2)

# 3.4.7 Predictive accuracy
mnl_predictions_int <- predict(mnl_model_int, newdata = mlogit_data)
predicted_choice_int <- apply(mnl_predictions_int, 1, which.max)
actual_choice_int    <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy_int         <- mean(predicted_choice_int == actual_choice_int)
accuracy_percent_int <- accuracy_int * 100

#############################################
## 3.5 Print Model Fit Results (INTERACTION MODEL)
#############################################

cat("\nMODEL FIT METRICS (Interaction Model)\n")
cat("----------------------------------------\n")
cat("Log-Likelihood (Model):  ", as.numeric(log_likelihood_int), "\n")
cat("Log-Likelihood (Null):   ", null_loglik_int, "\n")
cat("McFadden's R-squared:    ", mcfadden_r2_int, "\n")
cat("Likelihood Ratio (LR):   ", lr_test_int, "\n")
cat("LR Test p-value:         ", lr_p_value_int, "\n")
cat("AIC:                     ", aic_int, "\n")
cat("BIC:                     ", bic_int, "\n")
cat("Predictive Accuracy (%): ", accuracy_percent_int, "\n")


#############################################
## 3.6 Diagnostics (INTERACTION MODEL)
#############################################

# 3.6.1 1st-order condition
#     Check summary(mnl_model_int) for gradient info


## 3.6.2 2nd-order condition: Hessian negative definite
hessian_eigs_int <- eigen(mnl_model_int$hessian)$values
cat("\nHessian Eigenvalues (Interaction Model):\n")
print(hessian_eigs_int)
cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")



