####################################################################################################
## 0. Setup & Data Preparation
####################################################################################################

# Load or install required packages
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
packages <- c("readr", "dplyr", "tidyr", "ggplot2", "doParallel", "foreach", "parallel", "sf", 
              "stringr", "data.table", "pbapply", "caret", "mlogit", "Rcpp", "RcppArmadillo", "bayesm")
invisible(lapply(packages, load_or_install))

# Load raw tablet data (for reference)
tablet_data_unf <- fread("PATH/TO/tablets.csv")  # replace with your relative path

# Load respondent estimation data
path_e_data_mod <- "PATH/TO/Estimation_Data_tab.Rdata"  # replace with your relative path
load(path_e_data_mod)

# Inspect estimation data
str(E_Data_mod)
head(E_Data_mod, 1)
summary(E_Data_mod)

# Prepare data for all respondents
prepare_data <- function(E_Data_mod) {
  data_list <- lapply(seq_along(E_Data_mod$lgtdata), function(i) {
    respondent <- E_Data_mod$lgtdata[[i]]
    y <- respondent$y
    X <- respondent$X
    num_tasks <- length(y)
    num_alts <- nrow(X) / num_tasks
    if (num_alts != 4) stop("Number of alternatives per task is not 4.")
    TaskID <- rep(1:num_tasks, each = num_alts)
    AltID  <- rep(1:num_alts, times = num_tasks)
    RespID <- rep(i, nrow(X))
    Chosen <- as.integer(AltID == rep(y, each = num_alts))
    df <- data.frame(RespID = RespID, TaskID = TaskID, AltID = AltID, Chosen = Chosen)
    cbind(df, X)
  })
  bind_rows(data_list)
}

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
    System = case_when(
      System_B == 1  ~ "B",
      TRUE ~ "A"
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
         -`150_Cash_Back`, -`8_Inches`, -`10_Inches`, -`12_Inches`, -`13_Inches`, -`System_B`)

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
    Display_Size = relevel(factor(Display_Size), ref = "7 Inches"),
    System = relevel(factor(System), ref = "B")
  )

mlogit_data <- mlogit.data(
  combined_data,
  choice = "Chosen",
  shape  = "long",
  alt.var = "AltID",
  chid.var = "chid"
)

####################################################################################################
## 1. Model-Free Analysis
####################################################################################################

# Frequencies & Tables for key attributes
attributes <- c("Price", "Brand", "Cash_Back", "Resolution", "Memory", "SD_Slot", 
                "Performance", "Battery_Run_Time", "Connections", "Sync_to_Smartphone", 
                "Value_Pack", "Equipment", "Display_Size")

for (attr in attributes) {
  cat("\n----------------------------\n")
  cat("Attribute:", attr, "\n")
  freq_table <- table(combined_data[[attr]])
  prop_table <- prop.table(freq_table)
  cat("Frequency:\n")
  print(freq_table)
  cat("Proportion:\n")
  print(prop_table)
}

# Cross-Tabulations
cat("\n--- Cash_Back x Brand ---\n")
table_brand_cb <- table(combined_data$Brand, combined_data$Cash_Back)
print(table_brand_cb)
print(prop.table(table_brand_cb, margin = 1))
print(prop.table(table_brand_cb, margin = 2))

combined_data$Price_Bin <- cut(
  combined_data$Price,
  breaks = c(-Inf, 0, 2, 4, 6, 8, Inf),
  labels = c("0", "0-2", "2-4", "4-6", "6-8", ">8")
)
cat("\n--- Price_Bin x Cash_Back ---\n")
table_price_cb <- table(combined_data$Price_Bin, combined_data$Cash_Back)
print(table_price_cb)
print(prop.table(table_price_cb, margin = 1))
print(prop.table(table_price_cb, margin = 2))

cat("\n--- Brand x Memory ---\n")
table_brand_memory <- table(combined_data$Brand, combined_data$Memory)
print(table_brand_memory)
print(prop.table(table_brand_memory, margin = 1))
print(prop.table(table_brand_memory, margin = 2))

# Chosen Alternatives Analysis
chosen_rows <- combined_data %>% filter(Chosen == 1)
cat("\n--- Chosen Cash_Back Frequency ---\n")
print(table(chosen_rows$Cash_Back))
print(prop.table(table(chosen_rows$Cash_Back)))
print(prop.table(table(combined_data$Cash_Back)))

cat("\n--- Chosen Price Statistics ---\n")
chosen_price_stats <- chosen_rows %>%
  summarise(mean_price = mean(Price),
            median_price = median(Price),
            min_price = min(Price),
            max_price = max(Price))
print(chosen_price_stats)

cat("\n--- Summary by Brand and Cash_Back ---\n")
combined_data %>%
  group_by(Brand, Cash_Back) %>%
  summarise(count = n(),
            mean_price = mean(Price),
            mean_chosen = mean(Chosen),
            .groups = "drop") %>%
  arrange(Cash_Back, Brand) %>%
  print(n = 29)

# Additional Brand-Level Summaries
cat("\n--- Brand-Level Summaries ---\n")
brand_summaries <- combined_data %>%
  group_by(Brand) %>%
  summarise(count = n(),
            mean_price = mean(Price),
            median_price = median(Price),
            chosen_rate = mean(Chosen),
            .groups = "drop") %>%
  arrange(desc(count))
print(brand_summaries)

cat("\n--- Unique (Brand, Cash_Back, Price) Combinations ---\n")
unique_combos <- combined_data %>% distinct(Brand, Cash_Back, Price) %>% nrow()
cat("Unique combinations =", unique_combos, "\n")

# Visualizations (example plots)
inside_data <- combined_data %>% filter(Brand != "Outside Option")
ggplot(as.data.frame(table(combined_data$Cash_Back)), aes(Var1, Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequency of Cashback Levels", x = "Cash_Back", y = "Frequency") +
  theme_minimal()

ggplot(combined_data, aes(x = Cash_Back, y = Price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Cash_Back", x = "Cash_Back", y = "Price") +
  theme_minimal()

ggplot(inside_data, aes(x = Price_Bin, fill = Brand)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Brands Across Price Bins", x = "Price_Bin", y = "Proportion") +
  theme_minimal()

combined_data %>%
  group_by(Price) %>%
  summarise(mean_chosen = mean(Chosen)) %>%
  ggplot(aes(x = Price, y = mean_chosen)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  labs(title = "Price vs. Mean Choice Probability", x = "Price", y = "Mean Choice") +
  theme_minimal()

####################################################################################################
## 2. Base MNL Model
####################################################################################################

mnl_formula <- Chosen ~ 
  Price + System + Brand + Resolution + Memory +
  SD_Slot + Performance + Battery_Run_Time + Connections +
  Sync_to_Smartphone + Value_Pack + Equipment + Cash_Back + Display_Size | 0

mnl_model <- mlogit(formula = mnl_formula, data = mlogit_data)
summary(mnl_model)

# Model Fit Metrics
log_likelihood <- logLik(mnl_model)
num_tasks <- nrow(mlogit_data) / 4
null_loglik_uniform <- num_tasks * log(1/4)
k <- length(coef(mnl_model))
n <- nrow(mlogit_data)
mcfadden_r2 <- 1 - (as.numeric(log_likelihood) / null_loglik_uniform)
lr_test <- -2 * (null_loglik_uniform - as.numeric(log_likelihood))
lr_p_value <- pchisq(lr_test, df = k, lower.tail = FALSE)
aic <- AIC(mnl_model)
bic <- -2 * as.numeric(log_likelihood) + k * log(n)
mnl_predictions <- predict(mnl_model, newdata = mlogit_data)
predicted_choice <- apply(mnl_predictions, 1, which.max)
actual_choice <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy <- mean(predicted_choice == actual_choice)
accuracy_percent <- accuracy * 100

cat("\nMODEL FIT METRICS (Base Model)\n")
cat("Log-Likelihood (Model): ", as.numeric(log_likelihood), "\n")
cat("Log-Likelihood (Null): ", null_loglik_uniform, "\n")
cat("McFadden's R-squared: ", mcfadden_r2, "\n")
cat("Likelihood Ratio (LR): ", lr_test, "\n")
cat("LR Test p-value: ", lr_p_value, "\n")
cat("AIC: ", aic, "\n")
cat("BIC: ", bic, "\n")
cat("Predictive Accuracy (%): ", accuracy_percent, "\n")

hessian_eigs <- eigen(mnl_model$hessian)$values
cat("\nHessian Eigenvalues (Base Model):\n")
print(hessian_eigs)

####################################################################################################
## 3. Interaction MNL Model
####################################################################################################

mnl_formula_interactions <- Chosen ~ 
  Price + System + Brand + Resolution + Memory + SD_Slot + Performance + Battery_Run_Time + 
  Connections + Sync_to_Smartphone + Value_Pack + Equipment + Cash_Back + Display_Size +
  Price:Cash_Back + Memory:SD_Slot | 0

mnl_model_int <- mlogit(formula = mnl_formula_interactions, data = mlogit_data)
summary(mnl_model_int)

log_likelihood_int <- logLik(mnl_model_int)
null_loglik_int <- null_loglik_uniform
k2 <- length(coef(mnl_model_int))
n2 <- nrow(mlogit_data)
mcfadden_r2_int <- 1 - (as.numeric(log_likelihood_int) / null_loglik_int)
lr_test_int <- -2 * (null_loglik_int - as.numeric(log_likelihood_int))
lr_p_value_int <- pchisq(lr_test_int, df = k2, lower.tail = FALSE)
aic_int <- AIC(mnl_model_int)
bic_int <- -2 * as.numeric(log_likelihood_int) + k2 * log(n2)
mnl_predictions_int <- predict(mnl_model_int, newdata = mlogit_data)
predicted_choice_int <- apply(mnl_predictions_int, 1, which.max)
actual_choice_int <- mlogit_data$AltID[mlogit_data$Chosen == 1]
accuracy_int <- mean(predicted_choice_int == actual_choice_int)
accuracy_percent_int <- accuracy_int * 100

cat("\nMODEL FIT METRICS (Interaction Model)\n")
cat("Log-Likelihood (Model): ", as.numeric(log_likelihood_int), "\n")
cat("Log-Likelihood (Null): ", null_loglik_int, "\n")
cat("McFadden's R-squared: ", mcfadden_r2_int, "\n")
cat("Likelihood Ratio (LR): ", lr_test_int, "\n")
cat("LR Test p-value: ", lr_p_value_int, "\n")
cat("AIC: ", aic_int, "\n")
cat("BIC: ", bic_int, "\n")
cat("Predictive Accuracy (%): ", accuracy_percent_int, "\n")

hessian_eigs_int <- eigen(mnl_model_int$hessian)$values
cat("\nHessian Eigenvalues (Interaction Model):\n")
print(hessian_eigs_int)
