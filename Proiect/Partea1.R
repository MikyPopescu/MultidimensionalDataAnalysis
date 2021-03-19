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

# copie pt date + filtru
dateProiectCopie <- dateProiect %>% 
  filter(!is.na(Country)) %>% 
  filter(!is.na(Employment))
dim(dateProiectCopie)


# Selectare tari
View(table(dateProiectCopi$Country))
tariProiect <- c("United States", "India", "Romania","France","United Kingdom","Spain","Sweden")
length(tariProiect) #7


dateProiectFiltrate <- dateProiectCopie %>% 
  filter (Country %in% tariProiect)
dim(dateProiectFiltrate)


# Analiza corespondentelor ---------

table(dateProiectFiltrate$Country)
attach(dateProiectFiltrate)



#install.packages("ggplot2")
#library(ggplot2)
dateProiectFiltrate  %>%
  filter(!is.na(Employment)) %>% 
  group_by(Employment, Country) %>% 
  dplyr::summarise(n=n())  %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(Country,n),y=n, fill=Employment, position="fill"))+
  scale_fill_brewer(palette="Dark2")


modelAnalizaCorespondente <- table(Employment, Country)
modelAnalizaCorspondente #tabelul de contingenta



#Valididate model
chisq.test(modelAnalizaCorespondente)



#Aplic functia ca pentru analiza corespondentelor
# install.packages("ca")
# library(ca)
corespondenteEmployment <- ca(modelAnalizaCorespondente)
summary(corespondenteEmployment)
windows()
plot(corespondenteEmployment, lines=c(FALSE,F))
