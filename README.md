# CSCC
Hassan El Uadguiri, ..., ...
Code + Script for Analysis

# About the Dataset:
	Respondents: 1,046 individuals.
	Choice Tasks: 13 tasks per respondent.
	Alternatives per Task: 4 alternatives in each choice task (including one outside option).
	Attributes: 17 attributes with varying levels.
	Brand (7 levels), 
	Operation system (2 levels), 
	Display size (5 levels), 
	Memory (5 levels), 
	Performance (3 levels), 
	Connections (3 Levels), 
	Value pack (2 levels), 
	Price (8 levels from 99 to 899 EUR), 
	Display size small (2 small), 
	Display resolution (2 levels), 
	SD Slot (2 levels), 
	Battery (2 Levels), 
	Synchronisation to smartphone (5 Levels), 
	Equipment (8 levels), 
	Cash Back I ( 2 Levels), 
	Cash Back II (3 Levels), 
	Cash Back III (4 Levels). 

Demographic Data: None available; budget constraints inferred from observed data.
1,046×13=13,598 total choice observations

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




