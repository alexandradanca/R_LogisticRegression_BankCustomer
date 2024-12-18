##################################################################
########### 1.Prezentarea setului de date ########################
# setam directorul si importam baza de date
setwd("D:/OneDrive/Desktop/Master/An 2/Regresie logistica/Proiect")
library(readr)
library(psych)
library(moments)
library(car)


Bank_Customer_Churn <- read_csv("Bank Customer Churn Prediction.csv")

#### 1.1. Operatii preliminare ####
# verificam daca avem observatii lipsa
colSums(is.na(Bank_Customer_Churn))

# clasele variabilelor
sapply(Bank_Customer_Churn,class)

# transformam din char/numeric in factor
Bank_Customer_Churn$gender<-as.factor(Bank_Customer_Churn$gender)
Bank_Customer_Churn$products_number<-as.factor(Bank_Customer_Churn$products_number)
Bank_Customer_Churn$credit_card<-as.factor(Bank_Customer_Churn$credit_card)
Bank_Customer_Churn$active_member<-as.factor(Bank_Customer_Churn$active_member)
Bank_Customer_Churn$churn<-as.factor(Bank_Customer_Churn$churn)

# verificam daca s-au transformat
sapply(Bank_Customer_Churn,class)

# transformam din 0 in nu, din 1 in da
levels(Bank_Customer_Churn$churn)<-c("nu","da")

#### 1.2. Analiza descripriva a variabilelor numerice si nenumerice ####
## 1.2.1. Pentru variabile numerice
df_numeric <- Bank_Customer_Churn %>%
  select (credit_score, age, tenure, balance, estimated_salary)
summary(df_numeric)

#
hist(Bank_Customer_Churn$credit_score,
     main="Scorul de credit",
     xlab="Scorul de credit", 
     col="darkmagenta")
hist(Bank_Customer_Churn$age,
     main="Varsta",
     xlab="Varsta", 
     col="orange")
hist(Bank_Customer_Churn$tenure,
     main="Vechime in ani la banca",
     xlab="Vechime in ani la banca", 
     col="blue")
hist(Bank_Customer_Churn$balance,
     main="Sold cont",
     xlab="Sold cont", 
     col="pink")
hist(Bank_Customer_Churn$estimated_salary,
     main="Salariul estimat",
     xlab="Salariul estimat", 
     col="green")


skewness(df_numeric)
kurtosis(df_numeric)

## 1.2.1. Pentru variabile nenumerice
table(Bank_Customer_Churn$country)
table(Bank_Customer_Churn$gender)
table(Bank_Customer_Churn$products_number)
table(Bank_Customer_Churn$credit_card)
table(Bank_Customer_Churn$active_member)
table(Bank_Customer_Churn$churn)

barplot(table(Bank_Customer_Churn$country), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown", legend.text=T)
barplot(table(Bank_Customer_Churn$gender), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="blue")
barplot(table(Bank_Customer_Churn$products_number), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="red")
barplot(table(Bank_Customer_Churn$credit_card), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="green")
barplot(table(Bank_Customer_Churn$active_member), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="pink")
barplot(table(Bank_Customer_Churn$churn), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="yellow")

### Oulieri
# vedem ce dimensiune are baza noastra de date inainte de eliminarea outlierilor
dim(Bank_Customer_Churn)
# vizualizam boxplot
boxplot(Bank_Customer_Churn$credit_score) 
boxplot(Bank_Customer_Churn$age)
boxplot(Bank_Customer_Churn$tenure)
boxplot(Bank_Customer_Churn$balance)
boxplot(Bank_Customer_Churn$estimated_salary)
# eliminam outlierii pentru credit_score
quartiles <- quantile(Bank_Customer_Churn$credit_score, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(Bank_Customer_Churn$credit_score)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data_no_outlier <- subset(Bank_Customer_Churn, Bank_Customer_Churn$credit_score > Lower & Bank_Customer_Churn$credit_score < Upper)
# eliminam outlierii pentru age
quartiles <- quantile(data_no_outlier$age, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data_no_outlier$age)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
Bank_Customer_Churn <- subset(data_no_outlier, data_no_outlier$age > Lower & data_no_outlier$age < Upper)
# vizualizam boxplot pentru cele 2 modificate
boxplot(Bank_Customer_Churn$credit_score) 
boxplot(Bank_Customer_Churn$age)
# vedem ce dimensiune are baza noastra de date dupa eliminarea outlierilor
dim(Bank_Customer_Churn)


#### impart baza in training si test
#70% training si 30% test 
sample <- sample(c(TRUE, FALSE), nrow(Bank_Customer_Churn), replace=TRUE, prob=c(0.7,0.3))
train  <- Bank_Customer_Churn[sample, ]
test   <- Bank_Customer_Churn[!sample, ]
dim(train)
dim(test)

#####################################################################################################
########### 2.Selectarea variabilelor prin aplicarea procedeului Purposeful ########################
############
#regresie logistica simpla (univariata) pentru var numerice
model1 <- glm(churn ~ credit_score, data = train, family = 'binomial')
summary(model1)
model2 <- glm(churn ~ age, data = train, family = 'binomial')
summary(model2)
model3 <- glm(churn ~ tenure, data = train, family = 'binomial')
summary(model3)
model4 <- glm(churn ~ balance, data = train, family = 'binomial')
summary(model4)
model5 <- glm(churn ~ estimated_salary , data = train, family = 'binomial')
summary(model5)

# tabel de contingenta pentru var categoriale
### GOF ###
# Gender
test_gender <- chisq.test(table(train$gender), p = c(1/2, 1/2))
test_gender

#Products_number
test_products_number <- chisq.test(table(train$products_number), 
                                   p = c(1/4, 1/4, 1/4, 1/4))
test_products_number

#Credit_card
test_credit_card <- chisq.test(table(train$credit_card), 
                                   p = c(1/2, 1/2))
test_credit_card

#Active_member
test_active_member <- chisq.test(table(train$active_member), 
                               p = c(1/2, 1/2))
test_active_member

#Country
test_country <- chisq.test(table(train$country), 
                                 p = c(1/3, 1/3, 1/3))
test_country

# Regresia mare
model_all_1 <- glm(churn ~ credit_score+age+tenure+balance+gender+products_number+credit_card+country, data = train, family = 'binomial')
summary(model_all_1)
model_all_2 <- glm(churn ~ age+balance+gender, data = train, family = 'binomial')
summary(model_all_2)

#testul raportul de verosimilitate
#H0: Modelul M2 este mai bun
#H1: Modelul M1 este mai bun
library(lmtest)
(A <- logLik(model_all_2))
(B <- logLik(model_all_1))
(teststat <- -2 * (as.numeric(A)-as.numeric(B)))
#df = 12 - 4 = 8
(p.val <- pchisq(teststat, df = 8, lower.tail = FALSE))
lrtest(model_all_2, model_all_1)

### sau testul chi-square
#Apoi, putem rula testul chi-patrat cu functia anova în R pentru a compara primul ??i al doilea model. Vom vedea ce model explica mai bine variabila noastra de raspuns
anova( model_all_1 , model_all_2 , test="Chisq") 


#####################################################################################################
########### 3.Selectarea variabilelor prin aplicarea procedeului Stepwise ########################
train$churn <-ifelse(train$churn=="da", 1, 0)
both <-step(lm(churn~.,data=train),direction="both")
both$anova


#####################################################################################################
########### 4. Evaluarea ajustarii modelului cu ajutorul Testului Omnibus ########################
library(rms)
logit2.res <- lrm(churn ~ credit_score+country+gender+age+tenure+
                    balance+products_number+credit_card+active_member+
                    estimated_salary, data = train, y = TRUE, x = TRUE)
residuals(logit2.res, type = "gof")


#####################################################################################################
########### 5.Evaluarea clasificarii prin matricea de clasificare ########################
library(caret)
library(ISLR)

#fit logistic regression model
model <- glm(churn ~ credit_score+country+gender+age+tenure+
               balance+products_number+credit_card+active_member+
               estimated_salary, family="binomial", data=train)
#ufolosim modelul pentru a prezice probabilitatea pentru churn
glm.probs <- predict(model, test, type="response")
test$pred_glm = ifelse(glm.probs > 0.5, "1", "0")
test$churn <- ifelse(test$churn=="da", 1, 0)
test$pred_glm = as.factor(test$pred_glm)
test$churn = as.factor(test$churn)
levels(test$pred_glm)
levels(test$churn)
confusionMatrix(test$churn,test$pred_glm)
#calculate sensitivity
sensitivity(test$churn, test$pred_glm)
#calculate specificity
specificity(test$churn, test$pred_glm)
Accuracy = (2168 + 265) / (2168 + 320 + 265 + 101)
Accuracy
Error_rate = (101 + 320) / (2168 + 320 + 265 + 101)
Error_rate 

#####################################################################################################
#### 6.Compararea celor doua procedee de selectare a variabilelor- Coeficientul Mallows' Cq ########
library(olsrr)

#fit full model
full_model <- lm(churn ~ ., data = train)

# modelul din procedeului Purposeful
model1 <- lm(churn ~ credit_score + age + tenure + balance + gender + products_number 
             + credit_card + country, data = train)
# modelul din procedeului Stepwise
model2 <- lm(churn ~ credit_score + estimated_salary + credit_card + tenure, data = train)

#calculate Mallows' Cp for each model
ols_mallows_cp(model1, full_model)

ols_mallows_cp(model2, full_model)


#####################################################################################################
########### 7. Interpretarea modelului final ########################
coef(model1)
exp(coef(model1))

#sau
e1.OR <- exp(model1$coefficients)
round(e1.OR,3)
