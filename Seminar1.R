# Pachete -----------------------------------------------------------------

pacheteBDSA <- c("knitr", "rmarkdown", "RColorBrewer", "scales", "tidyverse", 
                 "plyr", "dplyr", "ggplot2", "gganimate", "Rmisc", "Hmisc", 
                 "randomForest", "rattle", "rpart.plot", "caret", "gmodels", 
                 "PresenceAbsence", "ROCR", "ca", "plotROC", "ROSE", "devtools", 
                 "rpart", "readxl", "ellipsis", "gridExtra", "ca","wordcloud", 
                 "radarchart", "tidytext", "tidyr", "textdata", "evaluate")


# Instalare pachete
install.packages(pacheteBDSA, dependencies = T)

# Incarcare pachete
sapply(pacheteBDSA, library, character.only =T)


Data2020 <- read_csv("survey_results_public.csv")
dim(Data2020)


# Import fisierul cu descriera variabilelor
vb_desc <- read_csv("survey_results_schema.csv")
dim(vb_desc)
View(vb_desc)

head(Data2020)
View(Data2020)
variable.names(Data2020)


# Pastram doar variabilele de interes
Data2020 <- Data2020 %>%
  select(c(Respondent, MainBranch, Hobbyist, Age, Age1stCode, CompFreq, CompTotal, ConvertedComp, 
           Country, CurrencyDesc, CurrencySymbol, DevType, EdLevel, Employment, Gender, JobSat, 
           JobSeek, NEWDevOps, NEWDevOpsImpt, NEWEdImpt, NEWLearn, NEWOvertime, NEWPurpleLink, 
           OpSys, OrgSize, PurchaseWhat, SOAccount, SOComm, SOPartFreq, SOVisitFreq, SurveyEase, 
           SurveyLength, UndergradMajor, WelcomeChange, WorkWeekHrs, YearsCode, YearsCodePro))
dim(Data2020)
# 37 variabile pastrate in analiza
View(Data2020)


table(Data2020$MainBranch)

#gasire descriere vb
vb_desc$Description[vb_desc$Variable=="MainBranch"]



table(Data2020$CompFreq)
table(Data2020$JobSat)
View(table(Data2020$Country))

table(Data2020$NEWOvertime)
table(Data2020$Hobbyist)  #do you code as a hobby?
round(prop.table(table(Data2020$Hobbyist)),3)*100
table(Data2020$ConvertedComp)

prop.table()

# Filtre - transformari
#aplicam cateva filtre- exclud inregistrarile care au na(null) pe convertedcomp(salariul anual)
Data2020t <- Data2020 %>%
  filter(!is.na(ConvertedComp)) %>% #ctrl+shift+m
dim(Data2020t)
  
#creem o noua variabila cu mutate
table(Data2020t$YearsCode)


Data2020t <- Data2020t %>%
  mutate(YearsCode = as.numeric(case_when(YearsCode == "Less than 1 year"  ~ "0.5",
                                          YearsCode == "More than 50 years"  ~ "51",
                                          is.na(YearsCode) == TRUE ~ "0",
                                          TRUE ~ YearsCode)))# branch de else ("other")
table(Data2020t$YearsCode)

Data2020t <- Data2020t %>%
  mutate(YearsCodePro = as.numeric(case_when(YearsCodePro == "Less than 1 year"  ~ "0.5",
                                          YearsCodePro == "More than 50 years"  ~ "51",
                                          is.na(YearsCodePro) == TRUE ~ "0",
                                          TRUE ~ YearsCodePro)))# branch de else ("other")
table(Data2020t$YearsCodePro)

# analiza corespondentelor
#filttrare tari

View(table(Data2020t$Country))

countries<- c("United States", "United Kingdom", "Germany", "India", "Canada", 
              "France", "Italy", "Romania")

length(countries)

Data2020filtre <- Data2020t %>%
  filter(Country %in% countries)
dim(Data2020filtre)

table(Data2020filtre$Country)


attach(Data2020filtre) #in loc sa facem refererinta catre fiecare variabila cu $
table(Hobbyist) # este echivalent cu table(Data2020t$Hobbyist)

#JobSatisfaction si Country
#Vrem sa vedem gradul de satisfactie la job pe cele 8 tari selectate
m1_ac <- table(JobSat, Country)

Data2020filtre  %>%
  filter(!is.na(JobSat)) %>% 
  group_by(JobSat, Country) %>% 
  dplyr::summarise(n=n())  %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(Country,n),y=n, fill=JobSat, position="fill"))+
  scale_fill_brewer(palette="Dark2")


m1_ac
#de aici obtinem tabelul de contingenta pentru ac
#urmatorul pas este sa aplicam functia ca() pentru ac
corespondente_JobSat <- ca(m1_ac)
summary(corespondente_JobSat)

windows()
plot(corespondente_JobSat, lines=c(FALSE,F))


#vrem sa urmarim daca nivelul de educatie influenteaza lucrul peste program
m2_ac <- table(NEWOvertime,NEWEdImpt)
m2_ac

table(NEWOvertime)
table(NEWEdImpt)

corespondente_Overtime <- ca(m2_ac)
summary(corespondente_Overtime)
#calitatea rerezentarii este buna
#contributia la inertia totala este data de critically important
#pe randuri este never si often

windows()
plot(corespondente_Overtime, lines=c(FALSE,F))

Data2020t <- Data2020t %>% 
  filter(Age>=18 & Age <=70)

dim(Data2020t)

sursa <- "Stackoverflow"

#recomandam sa reprezentam grafic o histograma sau densitate pentru a urmai distributia variabilei
##Density function pt Age 
Data2020t %>%
  filter(!is.na(Age)) %>% # & Age<75 & Age>25)%>%
  ggplot(aes(Age)) +
  geom_density(color="white", fill="steelblue", adjust=5) +
  labs(x = "Age",
       y = "Frecventa",
       caption = sursa) +
  ggtitle("Varsta") + theme_gray(base_size = 12)


#gruparea varstei(age) in segmente
Data2020t <- Data2020t %>% 
  mutate(Age1 = as.factor(case_when(Age <= 25 ~ "18-25",
                                    Age <= 30 ~ "26-30",
                                    Age <= 35 ~ "31-35",
                                    Age <= 40 ~ "36-40",
                                    Age <= 45 ~ "41-45",
                                    Age > 45  ~ "45+")))
table(Data2020t$Age)


# Distributia pentru YearsCodePro

table(Data2020t$YearsCodePro)

Data2020t %>%
  filter(!is.na(YearsCodePro)) %>%
  ggplot(aes(YearsCodePro)) +
  geom_density(color="white", fill="steelblue", adjust=5) +
  labs(x = "YearsCodePro",
       y = "Frecventa",
       caption = sursa) +
  ggtitle("YearsCodePro") + theme_gray(base_size = 12)

# Cream o noua variabila categoriala in care stabilim segmente pentru numarul de ani
# de cand respondentii programeazaca profesionisti
Data2020t <- Data2020t %>% 
  mutate(YearsCodePf = as.factor(case_when(YearsCodePro <= 5  ~ "0-5",
                                           YearsCodePro <= 10 ~ "6-10",
                                           YearsCodePro <= 15 ~ "11-15",
                                           YearsCodePro <= 20 ~ "16-20",
                                           YearsCodePro > 20  ~ "21+")))
table(Data2020t$YearsCodePf)
table(Data2020t$YearsCodePro, Data2020t$YearsCodePf)