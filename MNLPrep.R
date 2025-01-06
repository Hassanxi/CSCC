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

# Optional: Inspect column names and unique levels in tablet data
# colnames(tablet_data_unf)
# attribute_levels <- lapply(tablet_data_unf, unique)
# print(attribute_levels)

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
    num_tasks <- length(y)  # Total number of tasks
    num_alts <- nrow(X) / num_tasks  # Alternatives per task
    if (num_alts != 4) {
      stop("Number of alternatives per task is not 4.")
    }
    
    # Create identifiers for respondent, tasks, and alternatives
    TaskID <- rep(1:num_tasks, each = num_alts)  # Task ID
    AltID <- rep(1:num_alts, times = num_tasks)  # Alternative ID
    RespID <- rep(i, nrow(X))  # Respondent ID
    
    # Determine chosen alternative (binary)
    Chosen <- as.integer(AltID == rep(y, each = num_alts))
    
    # Combine identifiers and attributes into a data frame
    df <- data.frame(
      RespID = RespID,
      TaskID = TaskID,
      AltID = AltID,
      Chosen = Chosen
    )
    df <- cbind(df, X)  # Add attribute data
    return(df)
  })
  
  # Combine data for all respondents
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

# STEP 3: Prepare the combined data
combined_data <- prepare_data(E_Data_mod)

# STEP 4: Data cleaning
# Rename columns to remove spaces, parentheses, and dashes
combined_data <- combined_data %>%
  rename_with(~ gsub("\\s+", "_", .)) %>%
  rename_with(~ gsub("\\(|\\)", "", .)) %>%
  rename_with(~ gsub("-", "", .))

# Create a globally unique identifier for choice situations
combined_data <- combined_data %>%
  mutate(chid = paste0(RespID, "_", TaskID))

# STEP 5: Transform attributes for interpretation
combined_data <- combined_data %>%
  mutate(
    # Map Brand binary columns to labels, with "Outside Option" as base level
    Brand = case_when(
      Brand_1 == 1 ~ "A",
      Brand_2 == 1 ~ "B",
      Brand_3 == 1 ~ "C",
      Brand_4 == 1 ~ "D",
      Brand_5 == 1 ~ "E",
      Brand_6 == 1 ~ "F",
      Brand_7 == 1 ~ "G",
      TRUE ~ "Outside Option"  # Base level
    ),
    
    # Map Resolution
    Resolution = ifelse(High_Resolution_264_ppi == 1, "High", "Standard"),
    
    # Map Memory attributes
    Memory = case_when(
      `16_GB` == 1 ~ "16GB",
      `32_GB` == 1 ~ "32GB",
      `64_GB` == 1 ~ "64GB",
      `128GB` == 1 ~ "128GB",
      TRUE ~ "8GB"  # Base level
    ),
    
    # Map SD-Slot
    SD_Slot = ifelse(Without_SDSlot == 1, "Without", "With"),  # Base: With
    
    # Map Performance
    Performance = case_when(
      `1.6_GHz` == 1 ~ "1.6 GHz",
      `2.2_GHz` == 1 ~ "2.2 GHz",
      TRUE ~ "1 GHz"  # Base level
    ),
    
    # Map Battery Run Time
    Battery_Run_Time = ifelse(`812_h._Runtime` == 1, "8-12 hours", "4-8 hours"),  # Base: 4-8 hours
    
    # Map Connections
    Connections = case_when(
      `WLAN_+_UMTS/3G` == 1 ~ "WLAN + UMTS (3G)",
      `WLAN_+_LTE/4G` == 1 ~ "WLAN + LTE (4G)",
      TRUE ~ "WLAN"  # Base level
    ),
    
    # Map Synchronization
    Sync_to_Smartphone = ifelse(Sphone_Synch. == 1, "Yes", "No"),  # Base: No
    
    # Map Value Pack
    Value_Pack = ifelse(Value_Pack == 1, "Yes", "No"),  # Base: No
    
    # Map Equipment
    Equipment = case_when(
      Cover == 1 ~ "Cover",
      Keyboard == 1 ~ "Keyboard",
      Mouse == 1 ~ "Mouse",
      Pencil == 1 ~ "Pencil",
      `32_GB_Memory_Card` == 1 ~ "32GB Memory Card",
      `Keyboard_+_Pencil` == 1 ~ "Keyboard + Pencil",
      `Keyboard_+_Mouse_+_Pencil` == 1 ~ "Keyboard + Mouse + Pencil",
      TRUE ~ "None"  # Base level
    ),
    
    # Map Cash Back
    Cash_Back = case_when(
      `50_Cash_Back` == 1 ~ "50 EUR",
      `100_Cash_Back` == 1 ~ "100 EUR",
      `150_Cash_Back` == 1 ~ "150 EUR",
      TRUE ~ "No Cash Back"  # Base level
    ),
    
    # Map Display Size
    Display_Size = case_when(
      `8_Inches` == 1 ~ "8 Inches",
      `10_Inches` == 1 ~ "10 Inches",
      `12_Inches` == 1 ~ "12 Inches",
      `13_Inches` == 1 ~ "13 Inches",
      TRUE ~ "7 Inches"  # Base level
    )
  ) %>%
  # Drop original binary columns after mapping
  select(-starts_with("Brand_"), -High_Resolution_264_ppi, -`16_GB`, -`32_GB`, -`64_GB`, -`128GB`, 
         -Without_SDSlot, -`1.6_GHz`, -`2.2_GHz`, -`812_h._Runtime`, -`WLAN_+_UMTS/3G`, -`WLAN_+_LTE/4G`, 
         -Sphone_Synch., -Value_Pack, -Cover, -Keyboard, -Mouse, -Pencil, -`32_GB_Memory_Card`, 
         -`Keyboard_+_Pencil`, -`Keyboard_+_Mouse_+_Pencil`, -`50_Cash_Back`, -`100_Cash_Back`, 
         -`150_Cash_Back`, -`8_Inches`, -`10_Inches`, -`12_Inches`, -`13_Inches`)

# STEP 6: Transform data into mlogit-compatible format
mlogit_data <- mlogit.data(
  combined_data,
  choice = "Chosen",       # Binary choice variable
  shape = "long",          # Long format
  alt.var = "AltID",       # Alternative identifier
  chid.var = "chid"        # Unique choice situation identifier
)

# STEP 7: Inspect the prepared data
str(mlogit_data)   # Check structure
head(mlogit_data)  # Preview the first few rows
summary(mlogit_data)  # Summarize key columns

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































