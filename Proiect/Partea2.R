setwd("D:/R/Proiect")
getwd()


# Pachete -----------------------------------------------------------------

pacheteBDSA <- c("knitr", "rmarkdown", "RColorBrewer", "scales", "tidyverse", 
                 "plyr", "dplyr", "ggplot2", "gganimate", "Rmisc", "Hmisc", 
                 "randomForest", "rattle", "rpart.plot", "caret", "gmodels", 
                 "PresenceAbsence", "ROCR", "ca", "plotROC", "ROSE", "devtools", 
                 "rpart", "readxl", "ellipsis", "gridExtra", "ca","wordcloud", 
                 "radarchart", "tidytext", "tidyr", "textdata", "evaluate") # vectorul pacheteBDSA cu toate functiile de care avem nevoie
# Instalare
install.packages(pacheteBDSA, dependencies = T)

# Apelare
sapply(pacheteBDSA, library, character.only = T)

# Citire date ---------------------------------------------------------
dateProiect <- read_csv("survey_results_public.csv") # dataframe
dim(dateProiect) # dimensiunea efectiva a setului de date: 64461 randuri; 61 coloane

# Importare fisier cu descrierea variabilelor
descriereVariabileProiect <- read_csv("survey_results_schema.csv")
dim(descriereVariabileProiect) # dimensiune: 61 de randuri (variabilele din set); 2 coloane
View(descriereVariabileProiect) # vizualizare format tabelar


# Vizualizarea setului de date ----------------------------------------
head(dateProiect) # afiseaza in consola
View(dateProiect) # afiseaza formatul tabelar (ca in Excel); NA <-> null
variable.names(dateProiect) # afiseaza o lista cu denumirea tuturor variabilelor 
# install.packages("dplyr")
#library(dplyr)
glimpse(dateProiect) # <-> head


# Selectare variabile de interes ----------------------------------
dateProiect <- dateProiect %>% 
  select(c(Respondent, MainBranch, Hobbyist, Age, Age1stCode, CompFreq, CompTotal, ConvertedComp, 
           Country, CurrencyDesc, CurrencySymbol, DevType, EdLevel, Employment, Gender, JobSat, 
           JobSeek, NEWDevOps, NEWDevOpsImpt, NEWEdImpt, NEWLearn, NEWOvertime, NEWPurpleLink, 
           OpSys, OrgSize, PurchaseWhat, SOAccount, SOComm, SOPartFreq, SOVisitFreq, SurveyEase, 
           SurveyLength, UndergradMajor, WelcomeChange, WorkWeekHrs, YearsCode, YearsCodePro)) # select -> tidyverse
dim(dateProiect) # 37 variabile pastrate in analiza ! poate mai modific
View(dateProiect)

# copie
dateProiectCopie <- dateProiect

# Regresie Logistica --------------------------------------- curatam + target

# Vom folosi in modelul de regresie liniara variabilele urmatoare:
# Age1, YearsCodePf1, EdLevel1, WorkWeekHrs1, DevType1, Gender1

# densitate pe varsta
#sursa <- "Stackoverflow"

#dateProiectCopie %>%
#  filter(!is.na(Age)) %>% # & Age<75 & Age>25)%>%
#  ggplot(aes(Age)) +
# geom_density(color="white", fill="steelblue", adjust=5) +
#  labs(x = "Age",
#      y = "Frecventa",
#     caption = sursa) +
#  ggtitle("Varsta") + theme_gray(base_size = 12)



# grupare varsta Age -> Age1

table(dateProiectCopie$Age)

dateProiectCopie <- dateProiectCopie %>% 
  filter(!is.na(YearsCodePro)) %>%
  mutate(Age1 = as.factor(case_when(Age <= 25 ~ "18-25",
                                    Age <= 30 ~ "26-30",
                                    Age <= 35 ~ "31-35",
                                    Age <= 40 ~ "36-40",
                                    Age <= 45 ~ "41-45",
                                    Age > 45  ~ "45+")))
table(dateProiectCopie$Age)

# grupare varsta YearsCodePro -> YearsCodePro1
table(dateProiectCopie$YearsCodePro)

dateProiectCopie <- dateProiectCopie %>% 
  filter(!is.na(YearsCodePro)) %>%
  mutate(YearsCodePro1 = as.factor(case_when(YearsCodePro <= 5  ~ "0-5",
                                           YearsCodePro <= 10 ~ "6-10",
                                           YearsCodePro <= 15 ~ "11-15",
                                           YearsCodePro <= 20 ~ "16-20",
                                           YearsCodePro > 20  ~ "21+")))
table(dateProiectCopie$YearsCodePro1)
table(dateProiectCopie$YearsCodePro, dateProiectCopie$YearsCodePro1)


# grupare nivel de studii EdLevel -> EdLevel1
table(dateProiectCopie$EdLevel)

# Transformam valorile de pe variabila categoriala EdLevel folosind urmatoarele codificari:

dateProiectCopie$EdLevel1 <- factor(dateProiectCopie$EdLevel, labels = c("UNI", "UNI","NE","UNI","UNI", "SCH","UNI","SCH","SCH"))
table(dateProiectCopie$EdLevel1)
table(dateProiectCopie$EdLevel, dateProiectCopie$EdLevel1)


# grupare ore lucrate WorkWeekHrs -> WorkWeekHrs1
table(Data2020t$WorkWeekHrs)


