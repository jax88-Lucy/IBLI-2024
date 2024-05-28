# The Impact of Index Based Livestock Insurance (IBLI) on Child Nutrition in Marsabit County, Kenya
# Jackson Kadyampakeni
# Data 
# Source: https://data.ilri.org/portal/dataset/ibli-marsabit-r1 
#______________________________________________________________________________________________________
#HH Education 
library(readr)
library(dplyr)

S3_Education <- read_csv("raw_data/S3_Education.csv")
S3_Education <- S3_Education %>%
  select(hhid, round, memberid, hv_s3q24, s3q6, s3q7, s3q7b, s3q10, s3q11, s3q12, s3q13) %>%
  rename(edu_status = hv_s3q24, highest_edu = s3q6, current_enrollment = s3q7, 
         sch_feeding = s3q10, sch_absent = s3q11, sch_absent_reason = s3q12, 
         sch_absent_days = s3q13, sch_fees = s3q7b)

#merge hh rosters + edu
hhdata2 <- hhdata %>%
  left_join(hhedu %>% select(hhid, round, memberid, edu_status, highest_edu, current_enrollment, sch_feeding, sch_absent, sch_absent_reason, sch_absent_days, sch_fees),
            by = c("hhid", "round", "memberid"))
write.csv(hhdata2, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/processed/hhdata2.csv", row.names = FALSE)

#____________________________________________________________________________________________________________

# IBLI Insurance Data 

#__________________________________________________________________________

#cleaning S15A_Groups_IBLI_HSNP 
library(readr)
S15A_Groups_IBLI_HSNP <- read_csv("raw_data/S15A Groups, IBLI, HSNP.csv")
View(S15A_Groups_IBLI_HSNP)
S15A_Groups_IBLI_HSNP <- S15A_Groups_IBLI_HSNP %>%
  select(hhid, round, s15q12, s15q13, s15q17a, s15q17c, s15q18c, s15q18d, s15q21, s15q21a, s15q21b, s15q21c) %>%
  rename(
    HSNP_part = s15q12, 
    hsnp_payout = s15q13, 
    disc_coupjf = s15q17a, 
    disc_coupas = s15q17c, 
    ibli_insure_jf = s15q18c, 
    ibli_insure_as = s15q18d, 
    ibli_payout_m = s15q21, 
    pm_amount = s15q21a, 
    ibli_payout_a = s15q21b, 
    pa_amount = s15q21c
  )
# Converting Yes = 1 and No = 0 
S15A_Groups_IBLI_HSNP <- S15A_Groups_IBLI_HSNP %>%
  mutate(across(c(HSNP_part, disc_coupjf, disc_coupas, ibli_insure_jf, ibli_insure_as, ibli_payout_m, ibli_payout_a),
                ~ ifelse(is.na(.), 0, ifelse(. == "Yes", 1, 0))))
# Converting figures to integer 
S15A_Groups_IBLI_HSNP <- S15A_Groups_IBLI_HSNP %>%
  mutate(
    hsnp_payout = as.integer(hsnp_payout),
    pm_amount = as.integer(pm_amount),
    pa_amount = as.integer(pa_amount)
  )
#Replacing -ve payouts with 0
S15A_Groups_IBLI_HSNP <- S15A_Groups_IBLI_HSNP %>%
  mutate(
    across(c(pm_amount, pa_amount), ~ if_else(. < 0, 0, .))
  )
#Create an insured and payout var 
S15A_Groups_IBLI_HSNP <- S15A_Groups_IBLI_HSNP %>%
  group_by(round) %>%  # Group data by round
  mutate(
    Insured = if_else(ibli_insure_jf == 1 | ibli_insure_as == 1, 1, 0),  # Set Insured to 1 if either condition is met
    Payout = if_else(ibli_payout_m == 1 | ibli_payout_a == 1, 1, 0)     # Set Payout to 1 if either condition is met
  ) %>%
  ungroup()
write.csv(S15A_Groups_IBLI_HSNP, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S15A_Groups_IBLI_HSNP.csv", row.names = FALSE)

#________________________________________________________________________________________________________________

#Cleaning S15E_Game_and_Discount_Coupon 

#consolidating the IBLI discounts Coupons 
library(readr)
S15E_Game_and_Discount_Coupon <- read_csv("raw_data/S15E Game and Discount Coupon.csv")
View(S15E_Game_and_Discount_Coupon)
S15E_Game_and_Discount_Coupon <- S15E_Game_and_Discount_Coupon %>% 
  mutate(
    ibli_discount = case_when(
      round == 1 ~ NA_real_,  # No discount data for 2009
      round == 2 ~ discount_jan_2010,  # Only Jan 2010 data available
      round == 3 ~ max(discount_jan_2011, discount_aug_2011, na.rm = TRUE),  # Max of Jan and Aug 2011
      round == 4 ~ discount_aug_2012,  # Only Aug 2012 data available
      round == 5 ~ max(discount_jan_2013, discount_aug_2013, na.rm = TRUE),  # Max of Jan and Aug 2013
      round == 6 ~ NA_real_,  # No discount data for 2020
      TRUE ~ NA_real_  # Default case for any unhandled round
    )
  )
S15E_Game_and_Discount_Coupon <- S15E_Game_and_Discount_Coupon %>% 
  select(hhid, round, ibli_discount)
S15E_Game_and_Discount_Coupon <- S15E_Game_and_Discount_Coupon %>% 
  mutate(ibli_disc_y = if_else(ibli_discount > 0, 1, 0))

write.csv(S15E_Game_and_Discount_Coupon, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S15E_Game_and_Discount_Coupon.csv", row.names = FALSE)

#_____________________________________________________________________________________________________________
# Merging with ibli_indexa
ibli_indexarea <- left_join(S15A_Groups_IBLI_HSNP, S15E_Game_and_Discount_Coupon, by = c("hhid", "round"))

write.csv(ibli_indexarea, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/ibli_index_area.csv", row.names = FALSE)

#_____________________________________________________________________________________________________________________

# Cleaning IBLI Contracts Data: S15B_IBLI_Contracts
library(readr)
S15B_IBLI_Contracts <- read_csv("raw_data/S15B IBLI Contracts.csv")
View(S15B_IBLI_Contracts)
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>% 
  select(hhid, round, contract_id, s15q19, s15q39, s15q39_r6, s15q39_r7, s15q39_a)
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  select(hhid, round, contract_id, s15q19, s15q39, s15q39_r6, s15q39_r7, s15q39_a) %>%
  rename(
    contr_sp = s15q19, 
    index_area = s15q39, 
    index_area_r6 = s15q39_r6, 
    index_area_r7 = s15q39_r7, 
    index_strikep = s15q39_a, 
  )
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  arrange(hhid, round)
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  group_by(hhid) %>%
  fill(index_area, .direction = "downup") %>%
  ungroup()
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(
    index_area = case_when(
      is.na(index_area) & (index_area_r7 == "Central Marsabit" | index_area_r7 == "Gadamoji") ~ "Central and Gadamoji",
      is.na(index_area) & index_area_r6 == "Maikona" ~ "Maikona",
      is.na(index_area) & index_area_r6 == "Gadamoji" ~ "Central and Gadamoji",
      is.na(index_area) & index_area_r7 == "Maikona" ~ "Maikona",
      is.na(index_area) & index_area_r7 == "Turbi" ~ "Maikona",
      is.na(index_area) & index_area_r7 == "Laisamis" ~ "Laisamis",
      is.na(index_area) & index_area_r6 == "Laisamis" ~ "Laisamis",
      is.na(index_area) & index_area_r7 == "Loiyangalani" ~ "Loiyangalani",
      is.na(index_area) & index_area_r6 == "Loiyangalani" ~ "Loiyangalani",
      is.na(index_area) & index_area_r6 == "Kargi" ~ "Loiyangalani",
      is.na(index_area) & index_area_r7 == "Kargi" ~ "Loiyangalani",
      TRUE ~ index_area
    )
  )
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  select(hhid, round, index_area, index_strikep) 
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  group_by(hhid) %>%
  fill(index_area, .direction = "downup") %>%
  ungroup()
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(index_area = if_else(str_detect(index_area, "\\.b$"), "Other", index_area)) %>%
  select(hhid, round, index_area, index_strikep)

S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(index_area = case_when(
    index_area == "Central and Gadamoji" ~ 1,
    index_area == "Laisamis" ~ 2,
    index_area == "Loiyangalani" ~ 3,
    index_area == "Maikona" ~ 4,
    index_area == "North Horr" ~ 5,
    index_area == "Other" ~ 6,
    TRUE ~ NA_integer_  # This handles any unexpected or NA values
  ))

S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(index_strikep = case_when(
    grepl("%", index_strikep) ~ 1,      # Check if the value contains a percentage symbol
    is.na(index_strikep) ~ NA_real_,   # Check if the value is NA and keep it as NA
    TRUE ~ 0                           # All other values that do not meet the above conditions
  ))
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(index_strikep = case_when(
    round %in% c(2, 3) & is.na(index_strikep) ~ 0,  # Replace NA with 0 in rounds 2 and 3
    TRUE ~ index_strikep  # Keep other values as they are
  ))
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  group_by(hhid) %>%  # Group data by household ID
  mutate(
    index_area = ifelse(is.na(index_area), max(index_area, na.rm = TRUE), index_area)
  ) %>%
  ungroup() 
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  group_by(index_area, round) %>%
  mutate(index_strikep = ifelse(all(is.na(index_strikep)), NA_real_,
                                coalesce(index_strikep, first(index_strikep[!is.na(index_strikep)])))) %>%
  
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  group_by(hhid, round) %>%
  mutate(index_strikep = ifelse(is.na(index_strikep) & any(!is.na(index_strikep)),
                                max(index_strikep, na.rm = TRUE),
                                index_strikep)) %>%
  ungroup() %>%  # Ungroup to apply other transformations without group constraints
  mutate(index_strikep = ifelse(round == 7, 0, index_strikep)) %>%
  mutate(index_strikep = ifelse(is.na(index_strikep) & (round == 4 | round == 6), 1, index_strikep))
  
write.csv(S15B_IBLI_Contracts, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S15B_IBLI_Contracts.csv", row.names = FALSE)
#____________________________________________________________________________________________________________________

#Merging IBLI Insurance and Index area 
S15B_IBLI_Contracts <- S15B_IBLI_Contracts %>%
  mutate(round = as.numeric(round))

# Perform the left join
ibli_indexarea <- left_join(ibli_indexarea, S15B_IBLI_Contracts, by = c("hhid", "round"))
#_____________________________________________________________________________________________
ibli_indexarea <- ibli_indexarea %>%
  mutate(index_strikep = case_when(
    round %in% c(2, 3) & is.na(index_strikep) ~ 0,  # Replace NA with 0 in rounds 2 and 3
    TRUE ~ index_strikep  # Keep other values as they are
  ))
ibli_indexarea <- ibli_indexarea %>%
  group_by(index_area, round) %>%
  mutate(index_strikep = ifelse(all(is.na(index_strikep)), NA_real_,
                                coalesce(index_strikep, first(index_strikep[!is.na(index_strikep)])))) %>%
  ibli_indexarea <- ibli_indexarea %>%
  group_by(hhid, round) %>%  # Group data by hhid and round
  mutate(index_strikep = ifelse(is.na(index_strikep) & any(!is.na(index_strikep)), 
                                max(index_strikep, na.rm = TRUE), 
                                index_strikep)) %>% 

ibli_indexarea <- ibli_indexarea %>%
  group_by(hhid, round) %>%
  fill(index_area, .direction = "downup") %>%
  ungroup()

# Standardizing NDVI strike points across index areas 
ibli_indexarea <- ibli_indexarea %>%
  group_by(index_area, round) %>%
  mutate(index_strikep = if (any(index_strikep == 1, na.rm = TRUE)) 1 else if (any(index_strikep == 0, na.rm = TRUE)) 0 else index_strikep) %>%
  ungroup()

write.csv(ibli_indexarea, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/ibli_index_area.csv", row.names = FALSE)

#___________________________________________________________________________________________________________________________

# Child Data Cleaning 

#Cleaning S2_Household_Rosters

library(readr)
S2_Household_Rosters <- read_csv("raw_data/S2 Household Rosters.csv")
View(S2_Household_Rosters)
S2_Household_Rosters <- S2_Household_Rosters %>%
  select(hhid, round, memberid, s2q2, s2q3, s2q5, s2q4b, s2q4a) %>%
  rename(
    gender = s2q2,
    age = s2q3,
    hh_role = s2q5,
    u5_yob = s2q4b,
    u5_mob = s2q4a,
  )
#Recoding gender 
library(dplyr)
library(forcats)
#Recoding gender, creating hh_head age and gender
S2_Household_Rosters <- S2_Household_Rosters %>%
  mutate(
    gender = case_when(
      gender == "Male" ~ 1,
      gender == "Female" ~ 0,
      TRUE ~ NA_integer_
    ),
    hh_head_age = ifelse(hh_role == "Head", age, NA),
    hh_head_gender = ifelse(hh_role == "Head", gender, NA)
  ) %>%
  group_by(hhid) %>%
  mutate(
    hh_head_age = first(na.omit(hh_head_age)),
    hh_head_gender = first(na.omit(hh_head_gender))
  ) %>%
  ungroup() %>%
  mutate(
    gender = fct_recode(as.factor(gender), Male = "1", Female = "0"),
    hh_head_gender = fct_recode(as.factor(hh_head_gender), Male = "1", Female = "0")
  )
#Filling hh_head obs. 
# Define survey dates for each round
round_to_date <- as.Date(c("2009-11-01", "2010-11-01", "2011-11-01", "2012-11-01", "2013-11-01", "2015-11-01", "2020-11-01"))

S2_Household_Rosters <- S2_Household_Rosters %>%
  mutate(
    survey_date = round_to_date[round]
  )

# Check for invalid rounds
if(any(S2_Household_Rosters$round > length(round_to_date) | S2_Household_Rosters$round < 1)) {
  stop("Round index out of bounds")
}

# Convert child age to days based on survey date
S2_Household_Rosters$u5_mob <- iconv(S2_Household_Rosters$u5_mob, to = "UTF-8")
S2_Household_Rosters <- S2_Household_Rosters %>%
  mutate(
    u5_mob_num = match(tolower(u5_mob), tolower(month.name)),
    dob = make_date(u5_yob, u5_mob_num, 1), # Assuming u5_yob is correctly formatted as year
    u5doa = as.integer(difftime(survey_date, dob, units = "days"))
  )

#Filter under 5 years of age. 
S2_Household_Rosters <- S2_Household_Rosters %>%
  filter(!is.na(u5doa) & u5doa > 0)

write.csv(S2_Household_Rosters, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S2_Household_Rosters.csv", row.names = FALSE)

#_______________________________________________________________________________________________________________________

# Cleaning Child Health data 

library(readr)
S5B_Child_Health <- read_csv("raw_data/S5B Child Health.csv")
View(S5B_Child_Health)
S5B_Child_Health <- S5B_Child_Health %>%
  select(hhid, round, memberid, s5q11, s5q12, s5q14, s5q15, s5q16) %>%
  rename(
    sup_food = s5q11, 
    bcg_vaccine = s5q12, 
    muac = s5q14, 
    height = s5q15, 
    weight = s5q16
  )
#Converting sup_food and bcg vaccine to 1 and 0 
S5B_Child_Health <- S5B_Child_Health %>%
  mutate(
    sup_food = case_when(
      sup_food == "Yes" ~ 1,
      sup_food == "No" ~ 0,
      TRUE ~ NA_integer_  # Handle other cases as NA or specify as needed
    ),
    bcg_vaccine = case_when(
      bcg_vaccine == "Yes" ~ 1,
      bcg_vaccine == "No" ~ 0,
      TRUE ~ NA_integer_  # Handle other cases as NA or specify as needed
    )
  )
write.csv(S5B_Child_Health, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S5B_Child_Health.csv", row.names = FALSE)

#______________________________________________________________________

# Merging Child Health with Children Under 5 datasets (S2_Household_Rosters_U5)

S2_Household_Rosters_U5 <- inner_join(S2_Household_Rosters, S5B_Child_Health, by = c("hhid", "round", "memberid"))

#Replace Missing muac with average muac for kids in Marsabit county (13cm)
S2_Household_Rosters_U5 <- S2_Household_Rosters_U5 %>%
  mutate(
    muac = as.character(muac),  # Ensure muac is treated as a character for replacement
    muac = ifelse(muac == ".b" | is.na(muac), "13", muac),  # Replace '.b' and NA with '13'
    muac = as.numeric(muac)  # Convert back to numeric if necessary
  )

#Calculating muac for age z score (mfaz) using WHO Growth standard (2006)
#Data preping 
if(!require(remotes)) install.packages("remotes")
if(!require(zscorer)) install.packages("zscorer")
library(zscorer)

S2_Household_Rosters_U5 <- S2_Household_Rosters_U5 %>%
  rename(sex = gender) %>%  # Rename 'gender' column to 'sex'
  rename(birth_days = u5doa) %>%
  mutate(
    # Recode 'sex' values: 1 for Male, 2 for Female
    sex = case_when(
      sex == "Male" ~ 1,
      sex == "Female" ~ 2,
      TRUE ~ NA_integer_  # Set other or missing values to NA
    ),
    # Round 'muac' to one decimal point
    muac = round(as.numeric(muac), 1)
  )

#_______________________________________________________________________________________________________________________
#merging with insurance data and child data and cleaning for analysis 
mfaz_data <- addWGSR(data = mfaz_data, sex = "sex", firstPart = "muac",    
                    secondPart = "birth_days", index = "mfa")
#Using only calculated z scores for children above 0 months as required. 
mfaz_data <- filter(mfaz_data, !is.na(mfaz))

mfaz_data <- mfaz_data %>%
  mutate(ibli_discount = replace_na(ibli_discount, 0))

mfaz_data <- mfaz_data %>% 
  mutate(ibli_disc_y = if_else(ibli_discount > 0, 1, 0))

mfaz_data <- mfaz_data %>% 
  mutate(sup_food = if_else(sup_food > 0, 1, 0))

S2_Household_Rosters_U5 <- left_join(S2_Household_Rosters, S5B_Child_Health, by = c("hhid", "round", "memberid"))
write.csv(mfaz_data, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/mfaz_data.csv", row.names = FALSE)

#__________________________________________________________________________________________________________________________

# Weekly food consumption frequency 
# Aggregating individual food groups into nutrient rich food groups
library(readr)
S11A_Weekly_Food_Consumption <- read_csv("raw_data/S11A Weekly Food Consumption.csv")
View(S11A_Weekly_Food_Consumption)

# Processing data: renaming, selecting relevant columns, and filtering
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  select(hhid, round, foodclass, foodtype, s11q2, s11q4a, s11q5, s11q6a) %>%
  rename(
    consumed = s11q2, 
    qnty_unit = s11q4a, 
    total_value = s11q5, 
    means_aquire = s11q6a
  )
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  mutate(
    consumed = case_when(
      consumed == "Yes" ~ 1,
      consumed == "No" ~ 0,
      TRUE ~ NA_integer_  # Handles any other values with NA
    )
  )
  
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  filter(consumed != ".b") 

S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  mutate(
    Vitamin_A = if_else(foodclass %in% c("MILK AND MILK PRODUCTS", "MEAT POULTRY OFFAL", "EGGS", "VEGETABLES", "FRUITS"), consumed, 0),
    Protein = if_else(foodclass %in% c("PULSES LEGUMES NUTS", "MILK AND MILK PRODUCTS", "MEAT POULTRY OFFAL", "FISH", "EGGS"), consumed, 0),
    Hem_iron = if_else(foodclass %in% c("MEAT POULTRY OFFAL", "FISH"), consumed, 0),
    Oil_and_fats = if_else(foodclass == "OILS AND FATS", consumed, 0),
    Staples = if_else(foodclass %in% c("CEREALS", "ROOTS AND TUBERS"), consumed, 0),
    Fruits_and_vegetables = if_else(foodclass %in% c("VEGETABLES", "FRUITS"), consumed, 0)
  )
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  group_by(hhid, round) %>%
  summarize(
    Total_Vitamin_A = sum(Vitamin_A),
    Total_Protein = sum(Protein),
    Total_Hem_iron = sum(Hem_iron),
    Total_Oil_and_fats = sum(Oil_and_fats),
    Total_Staples = sum(Staples),
    Total_Fruits_and_vegetables = sum(Fruits_and_vegetables),
    .groups = "drop"
  ) 

S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  mutate(
    Vitamin_A_Score = case_when(
      Total_Vitamin_A == 0 ~ 1,
      Total_Vitamin_A >= 1 & Total_Vitamin_A <= 6 ~ 2,
      Total_Vitamin_A >= 7 ~ 3
    ),
    Protein_Score = case_when(
      Total_Protein == 0 ~ 1,
      Total_Protein >= 1 & Total_Protein <= 6 ~ 2,
      Total_Protein >= 7 ~ 3
    ),
    Hem_iron_Score = case_when(
      Total_Hem_iron == 0 ~ 1,
      Total_Hem_iron >= 1 & Total_Hem_iron <= 6 ~ 2,
      Total_Hem_iron >= 7 ~ 3
    ),
    Oil_and_fats_Score = case_when(
      Total_Oil_and_fats == 0 ~ 1,
      Total_Oil_and_fats >= 1 & Total_Oil_and_fats <= 6 ~ 2,
      Total_Oil_and_fats >= 7 ~ 3
    ),
    Staples_Score = case_when(
      Total_Staples == 0 ~ 1,
      Total_Staples >= 1 & Total_Staples <= 6 ~ 2,
      Total_Staples >= 7 ~ 3
    ),
    Fruits_and_vegetables_Score = case_when(
      Total_Fruits_and_vegetables == 0 ~ 1,
      Total_Fruits_and_vegetables >= 1 & Total_Fruits_and_vegetables <= 6 ~ 2,
      Total_Fruits_and_vegetables >= 7 ~ 3
    )
  )
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  select(
    hhid, 
    round, 
    Vitamin_A_Score, 
    Protein_Score, 
    Hem_iron_Score, 
    Oil_and_fats_Score, 
    Staples_Score, 
    Fruits_and_vegetables_Score
  ) %>%
  rename(
    Vitamin_A = Vitamin_A_Score, 
    Protein = Protein_Score, 
    Hem_iron = Hem_iron_Score, 
    Oil_fats = Oil_and_fats_Score, 
    Staples = Staples_Score, 
    Fruits_veg = Fruits_and_vegetables_Score
  )
  
#labling the scores: Never: 0 day (Poor food consumption) - Sometimes : 1-6 days (Borderline food consumption)- At least daily : 7 (and / or more) days (Acceptable food consumption)
S11A_Weekly_Food_Consumption <- S11A_Weekly_Food_Consumption %>%
  mutate(
    Vitamin_A_Label = case_when(
      Vitamin_A == 1 ~ "Poor food consumption",
      Vitamin_A == 2 ~ "Borderline food consumption",
      Vitamin_A == 3 ~ "Acceptable food consumption"
    ),
    Protein_Label = case_when(
      Protein == 1 ~ "Poor food consumption",
      Protein == 2 ~ "Borderline food consumption",
      Protein == 3 ~ "Acceptable food consumption"
    ),
    Hem_iron_Label = case_when(
      Hem_iron == 1 ~ "Poor food consumption",
      Hem_iron == 2 ~ "Borderline food consumption",
      Hem_iron == 3 ~ "Acceptable food consumption"
    ),
    Oil_fats_Label = case_when(
      Oil_fats == 1 ~ "Poor food consumption",
      Oil_fats == 2 ~ "Borderline food consumption",
      Oil_fats == 3 ~ "Acceptable food consumption"
    ),
    Staples_Label = case_when(
      Staples == 1 ~ "Poor food consumption",
      Staples == 2 ~ "Borderline food consumption",
      Staples == 3 ~ "Acceptable food consumption"
    ),
    Fruits_veg_Label = case_when(
      Fruits_veg == 1 ~ "Poor food consumption",
      Fruits_veg == 2 ~ "Borderline food consumption",
      Fruits_veg == 3 ~ "Acceptable food consumption"
    )
  )
write.csv(S11A_Weekly_Food_Consumption, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/S11A_Weekly_Food_Consumption.csv", row.names = FALSE)


mfaz_data <- left_join(mfaz_data, S11A_Weekly_Food_Consumption, by = c("hhid", "round")) 

#_________________________________________________________________________________________
#HH identification - TLU Classes 
library(readr)
S0A_Household_Identification_information <- read_csv("raw_data/S0A Household Identification information.csv")
View(S0A_Household_Identification_information)

S0A_Household_Identification_information <- S0A_Household_Identification_information %>%
  select(hhid, round, TLU_class)

mfaz_data <- left_join(mfaz_data, S0A_Household_Identification_information, by = c("hhid", "round"))

mfaz_data <- mfaz_data %>%
  mutate(age = as.numeric(as.character(age)))

#replacing ibli discount to 0 for baseline year and round 6 (no data available)
mfaz_data <- mfaz_data %>%
  mutate(ibli_discount = if_else(is.na(ibli_discount), 0, ibli_discount))
mfaz_data <- mfaz_data %>%
  mutate(sup_food = if_else(is.na(sup_food), 0, sup_food))
mfaz_data <- mfaz_data %>%
  mutate(TLU_class = if_else(is.na(TLU_class), "1", TLU_class))

mfaz_data <- left_join(mfaz_data, S15B_IBLI_Contracts, by = c("hhid", "round"))
mfaz_data <- mfaz_data %>%
  mutate(index_strikep = replace_na(index_strikep, 0))


mfaz_data <- mfaz_data %>%
  mutate(hh_head_gender = case_when(
    hh_head_gender == "Female" ~ 1,
    hh_head_gender == "Male" ~ 2,
    TRUE ~ NA_integer_  # Handle any other unexpected values as NA
  ))

write.csv(mfaz_data, "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/master_data/mfaz_data.csv", row.names = FALSE)

#____________________________________________________________________________________________________


# Regressions 
#1. Insurance on mfaz 
#OLS with FE 
iv_reg1 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg, data = mfaz_data)

iv_reg2 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg | hhid, data = mfaz_data)

iv_reg2.2 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg | hhid + round, data = mfaz_data)

iv_reg3 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg + age + sex + sup_food + TLU_class, data = mfaz_data)

iv_reg3.3 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg + age + sex + sup_food + TLU_class | hhid, data = mfaz_data)

iv_reg4 <- feols(mfaz ~ Insured * Vitamin_A + Insured * Protein + Insured * Hem_iron + Insured * Oil_fats + Insured * Staples + Insured * Fruits_veg + age + sex + sup_food + TLU_class | hhid + round, data = mfaz_data)

modelsummary(
  list(iv_reg5, iv_reg6, iv_reg7, iv_reg8, iv_reg9),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/insu_table.docx"
)

#IV 
iv_reg5 <- fixest::feols (mfaz ~ 1 | Insured ~ ibli_discount, data = mfaz_data) 
summary(iv_reg5)

iv_reg6 <- fixest::feols (mfaz ~ 1 | hhid | Insured ~ ibli_discount , data = mfaz_data) 
summary(iv_reg6) 

iv_reg7 <- fixest::feols (mfaz ~ 1 | hhid + round | Insured ~ ibli_discount , data = mfaz_data) 

iv_reg8 <- fixest::feols (mfaz ~ 1 + age + sex + sup_food + TLU_class | hhid | Insured ~ ibli_discount, data = mfaz_data) 

iv_reg9 <- fixest::feols (mfaz ~ 1 + age + sex + sup_food + TLU_class | hhid + round | Insured ~ ibli_discount, data = mfaz_data) 


modelsummary(
  list(iv_reg5, iv_reg6, iv_reg7, iv_reg8, iv_reg9),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/insu_table.docx"
)

lm_reg5 <- fixest::feols (mfaz ~ Insured, data = mfaz_data) 

lm_reg6 <- fixest::feols (mfaz ~ Insured| hhid, data = mfaz_data) 

lm_reg7 <- fixest::feols (mfaz ~ Insured | hhid + round, data = mfaz_data) 

lm_reg8 <- fixest::feols (mfaz ~ Insured + age + sex + sup_food + TLU_class | hhid, data = mfaz_data) 

lm_reg9 <- fixest::feols (mfaz ~ Insured + age + sex + sup_food + TLU_class | hhid + round, data = mfaz_data) 

modelsummary(
  list(lm_reg5, lm_reg6, lm_reg7, lm_reg8, lm_reg9),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/lminsu_table.docx"
)


#------------------------------------------------------------------------------------------------------

#OLS with FE 
iv_regp1 <- feols(mfaz ~ Payout, data = mfaz_data)
iv_regp2 <- feols(mfaz ~ Payout |hhid, data = mfaz_data)
iv_regp3 <- feols(mfaz ~ Payout |hhid + round, data = mfaz_data)
iv_regp4 <- feols(mfaz ~ Payout + age + sex + sup_food + TLU_class |hhid, data = mfaz_data)
iv_regp5 <- feols(mfaz ~ Payout + age + sex + sup_food + TLU_class |hhid + round, data = mfaz_data)

modelsummary(
  list(iv_regp1, iv_regp2, iv_regp3, iv_regp4, iv_regp5),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/ols_payout.docx"
) 


iv_regv <- fixest::feols (Vitamin_A ~ 1 + hh_head_gender + TLU_class |hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_regv2 <- fixest::feols (Vitamin_A ~ 1 + hh_head_gender + TLU_class|hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 
summary(iv_reghi2)
iv_regpr <- fixest::feols (Protein ~ 1 + hh_head_gender + TLU_class|hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_regpr2 <- fixest::feols (Protein ~ 1 + hh_head_gender + TLU_class|hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 

iv_reghi <- fixest::feols (Hem_iron ~ 1 + hh_head_gender + TLU_class|hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_reghi2 <- fixest::feols (Hem_iron ~ 1 + hh_head_gender + TLU_class|hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 

iv_regof <- fixest::feols (Oil_fats ~ 1 + hh_head_gender + TLU_class|hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_regof2 <- fixest::feols (Oil_fats ~ 1 |hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 

iv_regs <- fixest::feols (Staples ~ 1 + hh_head_gender + TLU_class|hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_regs2 <- fixest::feols (Staples ~ 1 + hh_head_gender + TLU_class|hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 

iv_regfv <- fixest::feols (Fruits_veg ~ 1 + hh_head_gender + TLU_class|hhid| Insured ~ ibli_discount, data = mfaz_dataf) 
iv_regfv2 <- fixest::feols (Fruits_veg ~ 1 + hh_head_gender + TLU_class|hhid + round| Insured ~ ibli_discount, data = mfaz_dataf) 

modelsummary(
  list(iv_regv,iv_regv2, iv_regpr,iv_regpr2, iv_reghi,iv_reghi2, iv_regof,iv_regof2, iv_regs,iv_regs2, iv_regfv,iv_regfv2),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/inutri_table.docx"
)

#Predicting 3 
lm_regv1 <- feols(Vitamin_AV ~ 1|Insured ~ ibli_discount, data = mfaz_data)
lm_regv5 <- feols(StaplesV ~ 1|Insured ~ ibli_discount, data = mfaz_data)
lm_regv6 <- feols(Fruits_vegV ~ 1|Insured ~ ibli_discount, data = mfaz_data)
modelsummary(
  list(lm_regv1, lm_regv5,lm_regv6),
  estimate = "{estimate}{stars}",
  output = "/Users/jacksonkadyampakeni/Desktop/IBLI-2024/IBLI_analysis24/documentation/inutriv_table.docx"
)

#Graphs 

#MUAC z-score vs Food Scores by insurance and Payout status 
library(dplyr)
library(tidyr)
library(ggplot2)

# Reshape the data for analysis and plotting
mfaz_long <- pivot_longer(mfaz_data, cols = c(Vitamin_A, Protein, Hem_iron, Oil_fats, Staples, Fruits_veg), 
                          names_to = "Nutrient", values_to = "Score")

# Sample data might include:
# mfaz_long <- data.frame(hhid = 1:100,
#                         mfaz = rnorm(100, 0, 1),
#                         Insured = sample(0:1, 100, replace = TRUE),
#                         Payout = sample(0:1, 100, replace = TRUE),
#                         Nutrient = sample(c("Vitamin_A", "Protein"), 100, replace = TRUE),
#                         Score = runif(100, 1, 10))

#plot
plot <- ggplot(mfaz_long, aes(x = Score, y = mfaz, color = factor(Insured), shape = factor(Payout))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Nutrient, scales = "free_x") +
  labs(x = "Food Consumption Score (FCS)", 
       y = "Child MUAC z-score",
       color = "Insured",
       shape = "Payout") +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Uninsured", "Insured")) +
  scale_shape_manual(values = c(16, 17), 
                     labels = c("No Payout", "Payout")) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

print(plot)

#Graph: Comparing insured and payout receipt hh
# Summary of IBLI insurance and payouts : Graph hh that insured and received payouts 
mfaz_data$round <- as.numeric(as.character(mfaz_data$round))

# data for percentages
percentage_data <- mfaz_data %>%
  group_by(round) %>%
  summarise(
    total_hh = n(),  # Total households per round
    hh_insure_jf = sum(ibli_insure_jf, na.rm = TRUE),
    hh_insure_as = sum(ibli_insure_as, na.rm = TRUE),
    hh_payout_m = sum(ibli_payout_m, na.rm = TRUE),
    hh_payout_a = sum(ibli_payout_a, na.rm = TRUE),
    .groups = 'drop'  # To ensure data is not grouped in further steps
  ) %>%
  mutate(
    perc_insure_jf = ifelse(total_hh > 0, hh_insure_jf / total_hh * 100, 0),
    perc_insure_as = ifelse(total_hh > 0, hh_insure_as / total_hh * 100, 0),
    perc_payout_m = ifelse(total_hh > 0, hh_payout_m / total_hh * 100, 0),
    perc_payout_a = ifelse(total_hh > 0, hh_payout_a / total_hh * 100, 0)
  ) %>%
  complete(round = full_seq(round, 1), fill = list(perc_insure_jf = 0, perc_insure_as = 0, perc_payout_m = 0, perc_payout_a = 0))

# sum percentages for insured and received payouts
summary_data <- percentage_data %>%
  group_by(round) %>%
  summarise(
    sum_insure = sum(perc_insure_jf, perc_insure_as),
    sum_payout = sum(perc_payout_m, perc_payout_a),
    .groups = 'drop'
  )
p <- ggplot(percentage_data, aes(x = round)) +
  geom_line(aes(y = perc_insure_jf, color = "Jan & Feb"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = perc_insure_as, color = "Aug & Sept"), size = 1.2, linetype = "dotted") +
  geom_line(aes(y = perc_payout_m, color = "Payout March"), size = 1.2) +
  geom_line(aes(y = perc_payout_a, color = "Payout August"), size = 1.2) +
  geom_text(data = summary_data, aes(y = sum_insure, label = ifelse(sum_insure > 0, sprintf("%.1f%%", sum_insure), "")),
            hjust = -0.1, vjust = 1.1, color = "blue", size = 4, check_overlap = TRUE) +
  geom_text(data = summary_data, aes(y = sum_payout, label = ifelse(sum_payout > 0, sprintf("%.1f%%", sum_payout), "")),
            hjust = 1.1, vjust = -0.1, color = "red", size = 4, check_overlap = TRUE) +
  scale_y_continuous(name = "Households (%)") +
  labs(
    x = "Round"
  ) +
  scale_color_manual(values = c("Jan & Feb" = "blue", "Aug & Sept" = "green", "Payout March" = "red", "Payout August" = "purple")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(color = "gray20"),
    axis.title = element_text(size = 12, face = "bold")
  )
print(p)

# Graph mfaz 
total_children <- nrow(mfaz_data)

# histogram where y is the percentage of total observations
p_insu <- ggplot(mfaz_data, aes(x = mfaz, y = ..count../total_children*100)) +  # Use ..count.. to get bin counts and convert to percentages
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", stat="bin") +
  labs(
    x = "MUAC Z-Score",
    y = "Children Under 5 Years of Age (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(color = "grey20", size = 10),
    axis.text.y = element_text(color = "grey20", size = 10)
  )
print(p_insu)

#Age vs mfaz 
p_age <- ggplot(mfaz_data, aes(x = age, y = mfaz)) +
  geom_point(alpha = 0.6, color = "black") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  facet_wrap(~ round) +  
  labs(x = "Age (years)",
       y = "Child MUAC z-score"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5), 
    strip.text.x = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )
print(p_age)


# Food Consumption Score 
# Transforming data and calculating percentages
food_consumption_summary_by_round2 <- Sjoiner %>%
  select(index_area, round, Vitamin_A, Hem_iron, Protein) %>%
  pivot_longer(cols = c(Vitamin_A, Hem_iron, Protein), 
               names_to = "Nutrient_Category", 
               values_to = "Consumption_Level") %>%
  mutate(Consumption_Level = as.factor(Consumption_Level)) %>%  # Convert to factor if it's numeric
  group_by(index_area, Nutrient_Category, Consumption_Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(index_area, Nutrient_Category) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  arrange(index_area, Nutrient_Category, Consumption_Level)

# Plotting food consumption as percentages
ggplot(food_consumption_summary_by_round2, aes(x = index_area, y = Percentage, fill = Consumption_Level)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Nutrient_Category, scales = "free_y") +
  labs(x = "District", y = "Percentage of Households (%)", title = "Household Food Consumption by Percentage") +
  scale_fill_brewer(palette = "Pastel1", name = "Consumption Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

