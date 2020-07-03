library(moments) #For Skewness & Kurtosis
library(ggplot2) #For Visualization
library(ggcorrplot) #For Correlation matrix
library(corrplot) #For Correlation
library(dplyr)  #For data manipulation/recode data
library(tidyverse) # handy utility functions
library(outliers) # library for identifying outliers

X= UCI_Credit_Card
dim(X)
colnames(X)


#Performing Descriptive Stats on the data
summary(X)
skewness(X)
kurtosis(X)
unique(X$SEX)
unique(X$MARRIAGE)
unique(X$EDUCATION)
unique(X$default.payment.next.month)

# Outiler Treatment
## 1. LIMIT_BAL
median(X$LIMIT_BAL)
ggplot(X, aes(x = "Limit", y = LIMIT_BAL)) + geom_boxplot()
summary(X$LIMIT_BAL)
library(outliers)  

X$LIMIT_BAL[X$LIMIT_BAL > 500000] <- 140000
summary(X$LIMIT_BAL)
ggplot(X, aes(x = "Limit", y = LIMIT_BAL)) + geom_boxplot()


## 2. AGE
ggplot(X, aes(x = "AGE", y = AGE)) + geom_boxplot()
summary(X$AGE)

## 3. PAY_0
ggplot(X, aes(x = "PAY_0", y = PAY_0)) + geom_boxplot()
summary(X$PAY_0)

## 4. PAY_2
ggplot(X, aes(x = "PAY_2", y = PAY_2)) + geom_boxplot()
summary(X$PAY_2)

## 5. PAY_3
ggplot(X, aes(x = "PAY_3", y = PAY_3)) + geom_boxplot()
summary(X$PAY_3)

## 6. PAY_4
ggplot(X, aes(x = "PAY_4", y = PAY_4)) + geom_boxplot()
summary(X$PAY_4)

## 7. PAY_5
ggplot(X, aes(x = "PAY_5", y = PAY_5)) + geom_boxplot()
summary(X$PAY_5)

## 8. PAY_6
ggplot(X, aes(x = "PAY_6", y = PAY_6)) + geom_boxplot()
summary(X$PAY_6)

## 9.Bill Amt_1
summary(X$BILL_AMT1)
ggplot(X, aes(x = "Limit", y = BILL_AMT1)) + geom_boxplot()
X$BILL_AMT1[X$BILL_AMT1 > 109611] <- 22382
X$BILL_AMT1[X$BILL_AMT1 < 0] <- 22382
summary(X$BILL_AMT1)
ggplot(X, aes(x = "Limit", y = BILL_AMT1)) + geom_boxplot()

## 10.Bill Amt_2
summary(X$BILL_AMT2)
ggplot(X, aes(x = "Limit", y = BILL_AMT2)) + geom_boxplot()
X$BILL_AMT2[X$BILL_AMT2 > 80000] <- 21200
X$BILL_AMT2[X$BILL_AMT2 < -40000] <- 21200
summary(X$BILL_AMT2)
ggplot(X, aes(x = "Limit", y = BILL_AMT2)) + geom_boxplot()

## 11.Bill Amt_3
summary(X$BILL_AMT3)
ggplot(X, aes(x = "Limit", y = BILL_AMT3)) + geom_boxplot()
X$BILL_AMT3[X$BILL_AMT3 > 100000] <- 20089
X$BILL_AMT3[X$BILL_AMT3 < -70000] <- 20089
summary(X$BILL_AMT3)
ggplot(X, aes(x = "Limit", y = BILL_AMT3)) + geom_boxplot()

## 11.Bill Amt_4
summary(X$BILL_AMT4)
ggplot(X, aes(x = "Limit", y = BILL_AMT4)) + geom_boxplot()
X$BILL_AMT4[X$BILL_AMT4 > 70000] <- 19052
X$BILL_AMT4[X$BILL_AMT4 < -50000] <- 19052
summary(X$BILL_AMT4)
ggplot(X, aes(x = "Limit", y = BILL_AMT4)) + geom_boxplot()

## 12.Bill Amt_5
summary(X$BILL_AMT5)
ggplot(X, aes(x = "Limit", y = BILL_AMT5)) + geom_boxplot()
X$BILL_AMT5[X$BILL_AMT5 > 90000] <- 18105
X$BILL_AMT5[X$BILL_AMT5 < -50000] <- 18105
summary(X$BILL_AMT5)
ggplot(X, aes(x = "Limit", y = BILL_AMT5)) + geom_boxplot()

## 13.Bill Amt_6
summary(X$BILL_AMT6)
ggplot(X, aes(x = "Limit", y = BILL_AMT6)) + geom_boxplot()
X$BILL_AMT6[X$BILL_AMT6 > 100000] <- 17071
X$BILL_AMT6[X$BILL_AMT6 < -50000] <- 17071
summary(X$BILL_AMT6)
ggplot(X, aes(x = "Limit", y = BILL_AMT6)) + geom_boxplot()

## 14.PAY Amt_1
summary(X$PAY_AMT1)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT1)) + geom_boxplot()
X$PAY_AMT1[X$PAY_AMT1 > 10000] <- 2100
summary(X$PAY_AMT1)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT1)) + geom_boxplot()

## 15.PAY Amt_2
summary(X$PAY_AMT2)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT2)) + geom_boxplot()
X$PAY_AMT2[X$PAY_AMT2 > 10000] <- 2009
summary(X$PAY_AMT2)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT2)) + geom_boxplot()

## 16.PAY Amt_3
summary(X$PAY_AMT3)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT3)) + geom_boxplot()
X$PAY_AMT3[X$PAY_AMT3 > 10000] <- 1800
summary(X$PAY_AMT3)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT3)) + geom_boxplot()

## 17.PAY Amt_4
summary(X$PAY_AMT4)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT4)) + geom_boxplot()
X$PAY_AMT4[X$PAY_AMT4 > 10000] <- 1500
summary(X$PAY_AMT4)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT4)) + geom_boxplot()

## 18.PAY Amt_5
summary(X$PAY_AMT5)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT5)) + geom_boxplot()
X$PAY_AMT5[X$PAY_AMT5 > 10000] <- 1500
summary(X$PAY_AMT5)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT5)) + geom_boxplot()

## 19.PAY Amt_6
summary(X$PAY_AMT6)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT6)) + geom_boxplot()
X$PAY_AMT6[X$PAY_AMT6 > 10000] <- 1500
summary(X$PAY_AMT6)
ggplot(X, aes(x = "PAY_AMT", y = PAY_AMT6)) + geom_boxplot()


# Recode Data
X1= X
## 1.SEX
X1$SEX=as.factor(recode(X1$SEX,'1'='Male','2'='Female'))

## 2.EDUCATION
X1$EDUCATION=as.factor(recode(X1$EDUCATION,'0'='Unknown1','1'='Graduate School','2'='University',
                             '3'='High School','4'='Others','5'='Unknown2','6'='Unknown3'))

## 3. MARRIAGE
X1$MARRIAGE=as.factor(recode(X1$MARRIAGE,'0'='Unknown','1'='Married','2'='Single','3'='Other'))

## 4.Age group
X1$AGE<-cut(X1$AGE, c(20,30,40,50,60,70,80),labels = c("20-29","30-39","40-49","50-59","60-69","70-79") )
View(X1)
summary(X1)

# Data Visualization
## 1. Age vs Limit Bal
ggplot(data = X1,aes(x=AGE,y=LIMIT_BAL,fill=default.payment.next.month))+ geom_boxplot()
## Limit Balance is high for age between 60-69 and 40-49.


## 2.Edu vs Limit Bal 
g1=ggplot(data = X1,aes(x = EDUCATION, y = LIMIT_BAL, fill = EDUCATION)) + 
  geom_boxplot(alpha = 0.3) +
  labs(title = "Plot Credit Limit by Education",x ="Education",y="Credit Limit",fill = "Education") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
plot(g1)
## Graduate School people have high Limit Balance.


## 3. Dist. of gender
g2=ggplot(data= X1, aes(x=SEX, fill=SEX)) + geom_bar() +
  labs(title = "Distribution by Gender", x ="Gender",fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
plot(g2)
## Female use more Credit card as compare to male.


## 4.Dist. of Marital Status
g3=ggplot(data= X1, aes(x=MARRIAGE, fil=SEX)) + geom_bar() +
  labs(title = "Distribution by Marital Status and Gender", x ="Marital Status",fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))
plot(g3)
## Single Person use more Credit card as compare to Married Person.


## 5.Credit Limit by Marital Status
g5=ggplot(data = X1,aes(x = MARRIAGE, y =LIMIT_BAL, fill = MARRIAGE)) + 
  geom_boxplot(alpha = 0.3) +
  labs(title = "Plot Credit Limit by Marital Status (incl Mean)",x ="Marital Status",y="Credit Limit",fill = "Marital Status")
plot(g5)
## Credit card Limit is maximum for married people.


## 6.Credit Limit by Gender
g6=ggplot(data = X1,aes(x = SEX, y = LIMIT_BAL, fill = SEX)) + 
  geom_boxplot(alpha = 1) +
  labs(title = "Plot Credit Limit by Gender",x ="Education",y="Credit Limit",fill = "Gender")
plot(g6)
## Credit Limit is high for male people.


## 7.Credit Limit by Age Group
g7=ggplot(data = X1,aes(x = AGE, y = LIMIT_BAL, fill = AGE)) + 
  geom_boxplot(alpha = 0.3) +
  labs(title = "Plot Credit Limit by Age Group",x ="Education",y="Credit Limit",fill = "Age Group")
plot(g7)
## Credit Limit is high for age between 60-69.

## 8.Credit Limit by Marital Status and Gender
g8=ggplot(data = X1,aes(x = MARRIAGE, y = LIMIT_BAL, fill = SEX)) + 
  geom_boxplot(alpha = 0.3) +
  labs(title = "Plot Credit Limit by Marital Status and Gender",x ="Marital Status",y="Credit Limit",fill = "Gender")
plot(g8)
## Married Male People has maximum Credit Limit.

library(gridExtra)
grid.arrange(g1,g5,g6,g7,ncol=2)

#Correlation Matrix for given variables
b=cor(X)
b
# Visualizing Corelation Matrix
corrplot(b)

# Split into Train & Test Data-
train <- sample(1:nrow(X),size = ceiling(0.80*nrow(X)),replace = FALSE)
# training set
cc_train <- X[train,]
View(cc_train)
# test set
cc_test <- X[-train,]
View(cc_test)

# Model Building
Z = glm(default.payment.next.month ~.-ID, data = cc_train, family = 'binomial')
summary(Z)

D=predict(Z,cc_test) #response=compute the prediction in probabilities
View(D)
y_pred<-ifelse( D > 0.5,1,0)
View(y_pred)

# Confusion Matrix
cm<-table(y_pred,cc_test$default.payment.next.month)
print(cm)

# Accuracy
Accuracy<-sum(diag(cm))/sum(cm)
print(Accuracy*100)
## Accuracy of Logistic Regression is 79.25%

diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
precision
recall
f1

# ROC Curve
library(ROCR)
predmodel2 <- prediction(D, cc_test$default.payment.next.month)
perf1 <- performance(predmodel2,"tpr","fpr")
plot(perf1)


