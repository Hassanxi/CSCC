# CSCC - Tablet Choice Analysis  
**Authors**: Hassan El Uadguiri, ..., ...

This repository contains code and scripts for the (re-)analysis of a discrete choice experiment dataset focusing on consumer preferences for tablet computers for the course Customer Satisfaction and Consumer Choice (CSCC) by Prof. Otter. The project includes data preparation, exploratory analysis, and the estimation of a baseline multinomial logit (MNL) model, and acts as a reference in our seminar paper.

---

## About the Dataset  

- **Respondents**: 1,046 individuals.  
- **Choice Tasks**: Each respondent completed 13 choice tasks.  
- **Alternatives per Task**: Each task presented 4 alternatives (including one outside option).  
- **Total Choice Observations**: (1,046 times 13 = 13,598).  
- **Demographic Data**: None available; budget constraints are inferred from observed data.

---

## Variables  

### Key Identifiers:  
- **`RespID`**: Identifies each unique survey respondent.  
- **`TaskID`**: Identifies each unique choice task for a respondent.  
- **`AltID`**: Identifies the alternative within each choice task (1 to 4).  
- **`Chosen`**: Binary variable; 1 if the alternative was chosen, 0 otherwise.  
- **`chid`**: Combined identifier for unique choice situations (i.e., `"RespID_TaskID"`).  

### Attribute Variables:  
- **`System_B`**: Indicates the operating system (0 = OS A, 1 = OS B).  
- **`Price`**: Purchase price in euros (e.g., 99, 299, ..., 899).  
- **`Brand`**: Indicates the brand (A, B, C, D, E, F, G).  
- **`Resolution`**: Display resolution ("Standard" or "High").  
- **`Memory`**: Storage memory in GB (8GB, 16GB, 32GB, 64GB, 128GB).  
- **`SD_Slot`**: Indicates whether the device has an SD slot ("With", "Without").  
- **`Performance`**: Processor speed ("1 GHz", "1.6 GHz", "2.2 GHz").  
- **`Battery_Run_Time`**: Battery runtime ("4-8 hours", "8-12 hours").  
- **`Connections`**: Network connectivity options ("WLAN", "WLAN + UMTS/3G", "WLAN + LTE/4G").  
- **`Sync_to_Smartphone`**: Synchronization to a smartphone ("No", "Yes").  
- **`Value_Pack`**: Availability of a value pack ("No", "Yes").  
- **`Equipment`**: Additional accessories ("None", "Cover", "Keyboard", "Mouse", "Pencil", etc.).  
- **`Cash_Back`**: Cashback offers ("No Cash Back", "50 EUR", "100 EUR", "150 EUR").  
- **`Display_Size`**: Screen size in inches (7, 8, 10, 12, 13).  

---

## Scripts:
- **`MNL Prep`**: Transforming the data to mlogit ready format
- ... 

### Key Files:  ...


