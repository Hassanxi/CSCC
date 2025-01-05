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


# STEP 1: Load the raw data (tablet attributes)
# NOTE: This step is just for inspecting the attributes and for reference if needed
tablet_data_unf <- fread("/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/tablets.csv")

# View column names to identify attribute variables (optional step)
# colnames(tablet_data_unf)

# Check unique levels for each attribute
# attribute_levels <- lapply(tablet_data_unf, function(column) {
#   list(Unique_Levels = unique(column))
# })
# for (attribute in names(attribute_levels)) {
#   cat("\nAttribute:", attribute, "\n")
#   print(attribute_levels[[attribute]])
# }

# Load the respondent estimation data
path_e_data_mod <- "/Users/helua1/Desktop/Master/3. Semester/CSCC/Tablet Computers/Estimation_Data_tab.Rdata"
load(path_e_data_mod)

# Inspect the structure of the loaded data
str(E_Data_mod)

# Example: Check the structure of the first respondent's design matrix
#head(E_Data_mod$lgtdata[[1]]$X)  # Preview the design matrix
#dim(E_Data_mod$lgtdata[[1]]$X)   # Get dimensions of the design matrix
#colnames(E_Data_mod$lgtdata[[1]]$X)  # List column names

# Summarize unique values in each column for the first respondent
#sapply(as.data.frame(E_Data_mod$lgtdata[[1]]$X), unique)

# Preview the first respondent's response vector
#E_Data_mod$lgtdata[[1]]$y

# Check the unique values in `y` for all respondents
#responses <- sapply(E_Data_mod$lgtdata, function(x) x$y)
#unique(unlist(responses))

# Summarize specific columns (e.g., "Price")
#price_column <- grep("Price", colnames(E_Data_mod$lgtdata[[1]]$X), value = TRUE)
#summary(E_Data_mod$lgtdata[[1]]$X[, price_column])

# Count non-zero values for each column
#non_zero_counts <- colSums(E_Data_mod$lgtdata[[1]]$X != 0)
#non_zero_counts

# Create a train-test split
#trainIndex <- createDataPartition(y = E_Data_mod$lgtdata[[1]]$y, p = 0.8, list = FALSE)
#train_data <- E_Data_mod$lgtdata[[1]]$X[trainIndex, ]
#test_data <- E_Data_mod$lgtdata[[1]]$X[-trainIndex, ]

# Prepare data for all respondents
prepare_data <- function(E_Data_mod) {
  data_list <- lapply(seq_along(E_Data_mod$lgtdata), function(i) {
    # Extract data for respondent `i`
    respondent <- E_Data_mod$lgtdata[[i]]
    y <- respondent$y  # Chosen alternatives
    X <- respondent$X  # Design matrix
    
    # Number of choice tasks (each task has `p` alternatives)
    num_tasks <- length(y)  # Total choice tasks per respondent
    num_alts <- nrow(X) / num_tasks  # Alternatives per task (4 here)
    
    if (num_alts != 4) {
      stop("Number of alternatives per task is not 4.")
    }
    
    # Create identifiers for Task ID and Alternative ID
    TaskID <- rep(1:num_tasks, each = num_alts)
    AltID <- rep(1:num_alts, times = num_tasks)
    
    # Create respondent identifier
    RespID <- rep(i, nrow(X))
    
    # Determine chosen alternative (binary: 1 if chosen, 0 otherwise)
    Chosen <- as.integer(AltID == rep(y, each = num_alts))
    
    # Combine into a data frame
    df <- data.frame(
      RespID = RespID,       # Respondent ID
      TaskID = TaskID,       # Choice task ID
      AltID = AltID,         # Alternative ID within task
      Chosen = Chosen        # Binary choice indicator
    )
    
    # Add attribute data
    df <- cbind(df, X)
    return(df)
  })
  
  # Combine all respondents' data into one data frame
  combined_data <- bind_rows(data_list)
  
  # Return combined data
  return(combined_data)
}

# Apply the function
combined_data <- prepare_data(E_Data_mod)

# Inspect the structure
str(combined_data)

# Check the distribution of choices
table(combined_data$Chosen)

# View a subset of the data
head(combined_data)

table(combined_data$TaskID, combined_data$Chosen)


mlogit_data <- mlogit.data(
  combined_data,
  choice = "Chosen",       # Binary choice variable
  shape = "long",          # Long format
  alt.var = "AltID",       # Alternative ID
  id.var = "RespID",       # Respondent ID
  chid.var = "TaskID"      # Choice task ID
)

str(combined_data)


# Check number of alternatives per task
alt_counts <- combined_data %>%
  group_by(RespID, TaskID) %>%
  summarise(num_alternatives = n_distinct(AltID), .groups = "drop")

# Identify tasks with incorrect alternative counts
invalid_tasks <- alt_counts %>% filter(num_alternatives != 4)
if (nrow(invalid_tasks) > 0) {
  cat("Tasks with incorrect alternative counts found:\n")
  print(invalid_tasks)
} else {
  cat("All tasks have the correct number of alternatives.\n")
}

# Check unique values for key columns
cat("Unique AltID:", length(unique(combined_data$AltID)), "\n")
cat("Unique RespID:", length(unique(combined_data$RespID)), "\n")
cat("Unique TaskID:", length(unique(combined_data$TaskID)), "\n")
cat("Unique Chosen values:", unique(combined_data$Chosen), "\n")

# Check if required columns are present
required_columns <- c("Chosen", "AltID", "RespID", "TaskID")
missing_columns <- setdiff(required_columns, colnames(combined_data))
if (length(missing_columns) > 0) {
  cat("Missing required columns:\n", missing_columns, "\n")
} else {
  cat("All required columns are present.\n")
}

# Inspect the first few rows to ensure structure
head(combined_data)


# Ensure correct types
combined_data$Chosen <- as.integer(combined_data$Chosen)  # Binary (0 or 1)
combined_data$AltID <- as.factor(combined_data$AltID)     # Categorical
combined_data$TaskID <- as.factor(combined_data$TaskID)   # Categorical
combined_data$RespID <- as.factor(combined_data$RespID)   # Categorical




