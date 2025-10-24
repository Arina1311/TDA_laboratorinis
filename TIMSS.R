library(tidyverse)
library(dplyr)
library(haven)

# Apie duomenys
# Mokyklos
# IDSCHOOL -	School ID
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
cols_to_factor <- c("IDSCHOOL", "IDSTUD", "ITSEX", "BSDGSLM","BSDGICM","BSDGSCM","BSDGSVM",
                    "BCBG13A","BCBG13B", "BCBG13C", "BCBG13D", "BCBG03A", "BCBG05A")

LT_data <- LT_data %>%
  mutate(across(all_of(cols_to_factor), ~ factor(.)))

is.factor(LT_data$ITSEX)

library(dplyr)
library(foreign)
library(knitr)
library(afex)
library(nlme)
library(msm)
library(car)

data.hlm <- read.spss("hsbdataset.sav",
                      to.data.frame = TRUE,
                      use.value.labels = TRUE,
                      strings.as.factors = TRUE)

lapply(data.hlm, FUN = class)
data.hlm$school<-factor(data.hlm$school)
data.hlm$student<-factor(data.hlm$student)
data.hlm$female<-factor(data.hlm$female)
data.hlm$sector<-factor(data.hlm$sector)

#1)
model.0 <- lme(BSMMAT01 ~ 1, data=LT_data, random = ~1|IDSCHOOL)
summary(model.0)

#Y00 hipoteze
gamma_00 <- data.frame(fixef(model.0))[1,1]
gamma_00
Anova(model.0, type = "III", test = "Chisq")

#t00 ir sigma hipotezes
var <-model.0$apVar
par<-attr(var, "Pars")
vc<-exp(par)^2
int.var.st.err <- deltamethod (~ exp(x1)^2, par, var)
resid.var.st.err <- deltamethod (~ exp(x2)^2, par, var)
wald.z.r <- vc[2]/resid.var.st.err
wald.z.int <- vc[1]/int.var.st.err
p.r <- 1- pnorm(wald.z.r)
p.int <- 1- pnorm(wald.z.int)
p.r
p.int

ICC <- (vc[1])/(vc[1]+vc[2])
ICC

#2)
#Apjungtas modelis: y00 + y01*meanses + y02*sector + u0 + y10*ses +
#y11(ses*sector) + u1*ses + y20*female + e
model.1<-lme(BSMMAT01 ~ 1 + meanses+sector+ses+ses:sector+female, data=LT_data, random = ~1+ses|IDSCHOOL)
summary(model.1)

gamma_00 <- data.frame(fixef(model.1)[1])[1,1]
gamma_01 <- data.frame(fixef(model.1)[2])[1,1]
gamma_02 <- data.frame(fixef(model.1)[3])[1,1]
gamma_10 <- data.frame(fixef(model.1)[4])[1,1]
gamma_11 <- data.frame(fixef(model.1)[6])[1,1]
gamma_20 <- data.frame(fixef(model.1)[5])[1,1]
gamma_00
gamma_01
gamma_02
gamma_10
gamma_11
gamma_20

anova(model.1)

#sigmaNaujas = 6.05^2 = 36.6025
#sigmaSenas 36.6^2 = 1339.56
#3)
#1339.56 - 36.6025 / 1339.56 = 0.9727
anova(model.1, model.0)

#4)
resid1 <- residuals(model.1, level = 1)
qqnorm(resid1); qqline(resid1)

#5)
#mathach = y00 + y01*-0.4 + y02*1 + y10*0.4 +
#y11(0.4*1) + y20*1 + e + u0 + u1*0.4
