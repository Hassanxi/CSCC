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
## 1. Model Specification and Estimation (BASE MODEL)
#############################################

# 1.1 Base formula (no interactions)
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

# 1.2 Estimate the BASE MNL model
mnl_model <- mlogit(
  formula = mnl_formula,
  data    = mlogit_data
)

# 1.3 View summary
summary(mnl_model)


#############################################
## 2. Compute Model Fit Metrics (BASE MODEL)
#############################################

# 2.1 Log-likelihood
log_likelihood <- logLik(mnl_model)

# 2.2 Null log-likelihood (uniform distribution among 4 alts)
num_tasks <- nrow(mlogit_data) / 4
null_loglik_uniform <- num_tasks * log(1/4)

# 2.3 Number of parameters (k) & observations (n)
k <- length(coef(mnl_model))
n <- nrow(mlogit_data)

# 2.4 McFadden's R^2
mcfadden_r2 <- 1 - (as.numeric(log_likelihood) / null_loglik_uniform)

# 2.5 Likelihood Ratio (LR) test
lr_test <- -2 * (null_loglik_uniform - as.numeric(log_likelihood))
lr_p_value <- pchisq(lr_test, df = k, lower.tail = FALSE)

# 2.6 AIC & BIC
aic <- AIC(mnl_model) 
bic <- -2 * as.numeric(log_likelihood) + k * log(n)

# 2.7 Predictive accuracy
mnl_predictions   <- predict(mnl_model, newdata = mlogit_data)
predicted_choice  <- apply(mnl_predictions, 1, which.max)
actual_choice     <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy          <- mean(predicted_choice == actual_choice)
accuracy_percent  <- accuracy * 100

#############################################
## 3. Print Model Fit Results (BASE MODEL)
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
## 4. Diagnostics (BASE MODEL)
#############################################

# 4.1 1st-order condition
#  => summary(mnl_model) typically shows gradient info
#  => "gradient close to zero" means solution is good

# 4.2 2nd-order condition: Hessian negative definite
hessian_eigs <- eigen(mnl_model$hessian)$values
cat("\nHessian Eigenvalues (Base Model):\n")
print(hessian_eigs)
cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")


#############################################
## 5. Model Specification and Estimation (INTERACTION MODEL)
#############################################

# 5.1 Define formula with interactions
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

# 5.2 Estimate the MNL model with interactions
mnl_model_int <- mlogit(
  formula = mnl_formula_interactions,
  data    = mlogit_data
)

# 5.3 View summary
summary(mnl_model_int)


#############################################
## 6. Compute Model Fit Metrics (INTERACTION MODEL)
#############################################

# 6.1 Log-likelihood
log_likelihood_int <- logLik(mnl_model_int)

# 6.2 Same null log-likelihood as before
#     (still uniform across 4 alts)
null_loglik_int <- null_loglik_uniform

# 6.3 Number of parameters (k2) & observations (n2)
k2 <- length(coef(mnl_model_int))
n2 <- nrow(mlogit_data)

# 6.4 McFadden's R^2
mcfadden_r2_int <- 1 - (as.numeric(log_likelihood_int) / null_loglik_int)

# 6.5 Likelihood Ratio (LR) test
lr_test_int <- -2 * (null_loglik_int - as.numeric(log_likelihood_int))
lr_p_value_int <- pchisq(lr_test_int, df = k2, lower.tail = FALSE)

# 6.6 AIC & BIC
aic_int <- AIC(mnl_model_int) 
bic_int <- -2 * as.numeric(log_likelihood_int) + k2 * log(n2)

# 6.7 Predictive accuracy
mnl_predictions_int <- predict(mnl_model_int, newdata = mlogit_data)
predicted_choice_int <- apply(mnl_predictions_int, 1, which.max)
actual_choice_int    <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy_int         <- mean(predicted_choice_int == actual_choice_int)
accuracy_percent_int <- accuracy_int * 100

#############################################
## 7. Print Model Fit Results (INTERACTION MODEL)
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
## 8. Diagnostics (INTERACTION MODEL)
#############################################

# 8.1 1st-order condition
#     Check summary(mnl_model_int) for gradient info

# 8.2 2nd-order condition: Hessian negative definite
hessian_eigs_int <- eigen(mnl_model_int$hessian)$values
cat("\nHessian Eigenvalues (Interaction Model):\n")
print(hessian_eigs_int)
cat("\nIf all these eigenvalues are negative, we have a local maximum.\n")


