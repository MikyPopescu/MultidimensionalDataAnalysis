# Seminar 2 ---------------------------------------------------------------


# Regresie logisica -------------------------------------------------------

# Vom folosi in modelul de regresie liniara variabilele urmatoare:
# Age1, YearsCodePf, Employment1, WorkWeekHrs1, JobSeek1, JobSat1

table(Data2020t$Employment)
# Transformam valorile de pe variabila categoriala Employment folosind urmatoarele codificari:
Data2020t$Employment1 <- factor(Data2020t$Employment, labels = c("FT", "PT", "IND"))
table(Data2020t$Employment1)
table(Data2020t$Employment, Data2020t$Employment1)

table(Data2020t$WorkWeekHrs)

Data2020t <- Data2020t %>%
  filter(!is.na(WorkWeekHrs))
dim(Data2020t)
# 30012    40

windows()
Data2020t %>%
  filter(!is.na(WorkWeekHrs) & WorkWeekHrs>25 & WorkWeekHrs<75) %>%
  ggplot(aes(WorkWeekHrs)) +
  geom_density(color="white", fill="aquamarine3", adjust=5, alpha=.6) +
  labs(x = "WorkWeekHrs",
       y = "Densitate",
       caption = sursa) +
  ggtitle("Numarul de ore la calculator pe saptamana")

summary(Data2020t$WorkWeekHrs)

# Transformam variabila WorkWeekHrs in vb. categoriala
Data2020t <- Data2020t %>%
  mutate(WorkWeekHrs1 = as.factor(case_when(WorkWeekHrs <= 35 ~ "0-35",
                                            WorkWeekHrs <= 40 ~ "36-40",
                                            WorkWeekHrs <= 45 ~ "41-45",
                                            WorkWeekHrs  > 45 ~ "45+")))
table(Data2020t$WorkWeekHrs1)
table(Data2020t$WorkWeekHrs, Data2020t$WorkWeekHrs1)

#Continuam cu transformarea variabilelor

# JobSeek
vb_desc$Description[vb_desc$Variable == "JobSeek"]

table(Data2020t$JobSeek)
Data2020t$JobSeek1 <- factor(Data2020t$JobSeek, labels = c("Open", "Active", "Not"))
table(Data2020t$JobSeek1)
table(Data2020t$JobSeek, Data2020t$JobSeek1)

# JobSatisfaction
table(Data2020t$JobSat)
Data2020t$JobSat1 <- factor(Data2020t$JobSat, labels = c("NSD", "SD", "SS", "VD", "VS"))
table(Data2020t$JobSat1)
table(Data2020t$JobSat, Data2020t$JobSat1)



# Definim variabila target (output):
# WellPaid - Urmarim sa identificam probabilitatea ca un programator sa fie in clasa celor 
# Bine platit (1) / Nu (0), in functie de valoarea salariului anual din variabila ConvertedComp

vb_desc$Description[vb_desc$Variable == "ConvertedComp"]

summary(Data2020t$ConvertedComp)
Data2020t 


# Verificam densitatea de repartitie pentru variabila ConvertedComp (Salariul anual) si decidem sa
# eliminam apoi valorile outliers + eliminare NAs
windows()
Data2020t %>%
  filter(!is.na(ConvertedComp) & ConvertedComp<300000) %>%  #& WorkWeekHrs>25 & WorkWeekHrs<75) %>%
  ggplot(aes(ConvertedComp)) +
  geom_density(color="white", fill="aquamarine3", adjust=5, alpha=.6) +
  labs(x = "ConvertedComp",
       y = "Densitate",
       caption = sursa) +
  ggtitle("Salariul anual in dolari US")

dim(Data2020t)
# [1] 30012    43
Data2020t <- Data2020t %>%
  filter(!is.na(ConvertedComp) & ConvertedComp<300000)
dim(Data2020t)
# [1] 28619    43

Data2020t <- Data2020t %>% 
  mutate(WellPaid = as.factor(case_when(ConvertedComp <  median(ConvertedComp) ~ 0,
                                        ConvertedComp >= median(ConvertedComp) ~ 1)))
table(Data2020t$WellPaid)
prop.table(table(Data2020t$WellPaid))

table(Data2020t$Employment1, Data2020t$WellPaid)

temp_FT <- Data2020t %>%
  filter(Employment1 == "FT")
round(prop.table(table(temp_FT$WellPaid))*100,2)

temp_PT <- Data2020t %>%
  filter(Employment1 == "PT")
round(prop.table(table(temp_PT$WellPaid))*100,2)



# Estimarea modelului de Regresie Logistica -------------------------------

summary(Data2020t$Employment1)
# Age1, YearsCodePf, Employment1, WorkWeekHrs1, JobSeek1, JobSat1

summary(Data2020t %>% 
          select(c(WellPaid, Age1, YearsCodePf, Employment1, WorkWeekHrs1, JobSeek1, JobSat1)))
attach(Data2020t)

dim(Data2020t)
# [1] 28619    44
Data2020t_model1 <- Data2020t %>% 
  filter(!is.na(Age1)) %>% 
  filter(!is.na(YearsCodePf)) %>% 
  filter(!is.na(Employment1)) %>% 
  filter(!is.na(WorkWeekHrs1)) %>% 
  filter(!is.na(JobSeek1)) %>% 
  filter(!is.na(JobSat1))
dim(Data2020t_model1)
# [1] 28565    44


# Filtre ------------------------------------------------------------------

# Filtre
# Aplicam cateva filtre - exclud inregistrarile care au NA (NULL) pe ConvertedComp (Salariu anual)
Data2020t <- Data2020 %>% 
  filter(!is.na(ConvertedComp))
dim(Data2020t)
# 34756    37

Data2020t <- Data2020t %>% 
  filter(Age>=18 & Age<=70)
dim(Data2020t)
# 30522    37


Data2020t <- Data2020t %>%
  filter(!is.na(WorkWeekHrs))
dim(Data2020t)
# 30012    40

dim(Data2020t)
# [1] 30012    43
Data2020t <- Data2020t %>%
  filter(!is.na(ConvertedComp) & ConvertedComp<300000)
dim(Data2020t)
# [1] 28619    43

Data2020t_model1 <- Data2020t %>% 
  filter(!is.na(Age1)) %>% 
  filter(!is.na(YearsCodePf)) %>% 
  filter(!is.na(Employment1)) %>% 
  filter(!is.na(WorkWeekHrs1)) %>% 
  filter(!is.na(JobSeek1)) %>% 
  filter(!is.na(JobSat1))
dim(Data2020t_model1)
# [1] 28565    44


# Model 1 - regresie logistica --------------------------------------------

?glm
options(scipen=999)
model1 <- glm(WellPaid ~ Age1 + YearsCodePf + Employment1 + WorkWeekHrs1 + JobSeek1 + JobSat1,
              data = Data2020t_model1,
              family = "binomial")
summary(model1)
# Urmarim in output Pr(>|z|) daca parametrii estimati sunt semnificativi din punct de vedere statistic
# Observam ca valorile asociate testelor, acele p-values au valori mai mici decat nivelul de semnificatie
# alpha = 0.05, deci concluzionam ca toti parametrii estimati sunt semnificativi din punct de vedere statistic.

# AIC - Akaike criterion, daca ar fi sa comparam doua modele, atunci cel mai bun este cel cu valoarea cea mai mica.


# Interpretarea modelului -------------------------------------------------

exp(coef(model1))


# Calculam predictiiile modelului -----------------------------------------
?predict
pred1 <- predict(model1, type = "response")
pred1

length(pred1)
dim(Data2020t_model1)

# Codificam predictiile in cele doua clase:
# 1 = WellPaid
# 0 = Not WellPaid (mai putin bine platiti)
summary(pred1)
y_pred_num1 <- ifelse(pred1 > 0.5, 1, 0)
table(y_pred_num1)

y_pred1 <- factor(y_pred_num1, labels = c(0, 1))
y_act1  <- Data2020t_model1$WellPaid

mean(y_pred1 == y_act1, na.rm = T)

?performance

pred <- prediction(pred1, y_act1)
class(pred)
class(Data2020t_model1)

pred
pred %>% 
  performance(measure = "tpr", x.measure = "fpr") %>% 
  plot(colorize = T, lwd = 7)

# Determinam valoarea A.U.C., adica Area Under the Curve (ROC)
AUCLog1 <- performance(pred, measure = "auc")@y.values[[1]]
AUCLog1
cat("Valoarea AUC pentru Model 1 Regresie Logistica:", AUCLog1)
# Valoarea AUC pentru Model 1 Regresie Logistica: 0.747085 (74.7%)

# Determinam matricea de confuzie a clasificarii pentru modelul estimat 1 (R.Log.)
confusionMatrix(data = y_pred1, as.factor(y_act1))
# Facem proba pentru indicatorului de acuratete a modelului:
# Suma elementelor de pe diagonala principala / suma tuturor observatiilor
(9724 + 9775)/(9724 + 9775 + 4506 + 4560)
# Concluzie, modelul are o acuratete de 68.26%