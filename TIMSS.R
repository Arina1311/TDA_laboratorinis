library(tidyverse)
library(dplyr)
library(haven)

# Apie duomenys
# Mokyklos
# IDSCHOOL -	School ID
# BCBG06A	- GEN\INSTRUCTIONAL DAYS PER YEAR
# BCBG07 -	GEN\TOTAL NUMBER COMPUTERS
# BCBG13A	- GEN\AGREEMENT\PROVIDE INFORMATION (1: Agree a lot; 2: Agree a little; 3: Disagree a little; 4: Disagree a lot; 9: Omitted or invalid; Sysmis: Not administered)
# BCBG13B	- GEN\AGREEMENT\PROMOTE INTEREST (1: Agree a lot; 2: Agree a little; 3: Disagree a little; 4: Disagree a lot; 9: Omitted or invalid; Sysmis: Not administered)
# BCBG13C	- GEN\AGREEMENT\PROVIDE SPECIAL ACTIVITIES (1: Agree a lot; 2: Agree a little; 3: Disagree a little; 4: Disagree a lot; 9: Omitted or invalid; Sysmis: Not administered)
# BCBG13D	- GEN\AGREEMENT\EMPHASIZE IMPORTANCE (1: Agree a lot; 2: Agree a little; 3: Disagree a little; 4: Disagree a lot; 9: Omitted or invalid; Sysmis: Not administered)
# BCBG05A	- GEN\HOW MANY PEOPLE LIVE IN AREA (1: More than 500,000 people; 2: 100,001 to 500,000 people; 3: 50,001 to 100,000 people; 4: 30,001 to 50,000 people; 5: 15,001 to 30,000 people; 6: 3,001 to 15,000 people; 7: 3,000 people or fewer; 99: Omitted or invalid; Sysmis: Not administered)
# BCBG03A	- GEN\STUDENTS BACKGROUND\ECONOMIC DISADVA (1: 0 to 10%; 2: 11 to 25%; 3: 26 to 50%; 4: More than 50%)

# Mokyniui
# IDSCHOOL -	School ID
# IDSTUD -	Student ID
# ITSEX -	Sex of Students (1: Girl; 2: Boy)
# BSDAGE	- Students Age
# BSDGSLM	- Students Like Learning Mathematics/IDX (1: Very Much Like Learning Mathematics; 2: Somewhat Like Learning Mathematics; 3: Do Not Like Learning Mathematics)
# BSDGICM	- Instructional Clarity in Mathematics Lessons/IDX (1: High Clarity of Instruction; 2: Moderate Clarity of Instruction; 3: Low Clarity of Instruction)
# BSDGSCM	- Students Confident in Mathematics/IDX (1: Very Confident in Mathematics; 2: Somewhat Confident in Mathematics; 3: Not Confident in Mathematics)\
# BSDGSVM	- Students Value Mathematics/IDX (1: Strongly Value Mathematics; 2: Somewhat Value Mathematics; 3: Do Not Value Mathematics)
# BSMMAT01 -	1ST PLAUSIBLE VALUE MATHEMATICS (PROGNOZUOJAMAS kintamasis)


load('/Users/arinaperzu/Desktop/TDA laboratorinis/bcgltum8.rdata')
load('/Users/arinaperzu/Desktop/TDA laboratorinis/bsgltum8.rdata')

LT_schools <- select(BCGLTUM8, IDSCHOOL, BCBG07, BCBG13A, BCBG13B, BCBG13C, BCBG13D, BCBG03A, BCBG05A)

LT_students <- select(BSGLTUM8, IDSCHOOL, IDSTUD, ITSEX, BSDAGE, BSDGSLM, BSDGICM, BSDGSCM, BSDGSVM, BSMMAT01)

LT_data <- inner_join(LT_students, LT_schools, by = "IDSCHOOL")
LT_data <- na.omit(LT_data)

# Reikia pasalinti 9/99/999 nevalidus data
targets   <- c(9, 99, 999)
cols_skip <- c("IDSCHOOL","IDSTUD","ITSEX")

mask_bad <- LT_data %>%
  mutate(across(-all_of(cols_skip),
                ~ suppressWarnings(as.numeric(haven::zap_missing(haven::zap_labels(.)))))) %>%
  # ar bent viename stulpelyje yra 9/99/999
  transmute(bad = if_any(-all_of(cols_skip), ~ . %in% targets)) %>%
  pull(bad)

# filtruojam originalią lentelę pagal kaukę
LT_data <- LT_data[ !replace_na(mask_bad, FALSE), ]
colnames(LT_data)

# Factorius darome
# Gal kur BCBG05A nereikia factoriaus nes 7 lygiai? galima pabandyti taip ir taip
cols_to_factor <- c("ITSEX", "BSDGSLM","BSDGICM","BSDGSCM","BSDGSVM",
                    "BCBG13A","BCBG13B", "BCBG13C", "BCBG13D", "BCBG03A", "BCBG05A")

LT_data <- LT_data %>%
  mutate(across(all_of(cols_to_factor), ~ factor(.)))

is.factor(LT_data$ITSEX)

