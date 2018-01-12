# import library
library(PerformanceAnalytics)
library(Amelia)
library(nnet)

# read data from IS590Model.txt
d <- read.delim("C:/WANG_DU_TANG_IS590BAO_Final_Project/Wang_IS590BAO_Modelling/Wang_IS590BAO_Modelling.txt", header = TRUE, sep = "\t")
summary(d)

# construct&format nomial variables
age <- ifelse(d$AGE > 50, 1, 0) #if aged over 50,1; else, 0
gender<- ifelse(d$GENDER=='Female', 1, 0) #if is female,1; else, 0
married <- d$MARRIED #if married,1; else, 0
state <- ifelse(d$STATE =='Illinois',1,0) #if in ILLINOIS,1; else, 0
family <- ifelse(d$COUNT.F.FAMILY_ID. > 0, 1, 0) #if have family members,1; else, 0
event <- ifelse(d$COUNT.E.EVENT_ATTENDEE_ID. > 0, 1, 0) #if ever attend events, 1; else,0
volunteer <- ifelse(d$COUNT.V.VOLUNTEER_ID. > 1, 1, 0) #if ever volunteer, 1; else,0
amount1 = ifelse(as.numeric(as.character(d$sum.T.AMOUNT.))[d$sum.T.AMOUNT.] > 0,1,0)
amount1[is.na(amount1)]<- 0 # if ever donate, 1;else, 0

# overview of all variables
df <- data.frame(age,gender,married,state,family,event,volunteer,amount1)
chart.Correlation(df, method="pearson", histogram=TRUE,pch="+")

# build logistic regression models based on hypothesis
asGlm <- glm(amount1~age+gender+married+family+state+event+volunteer, family ='binomial')
summary(asGlm)

# eliminate the state variable with largest p-value 
asGlm1 <- glm(amount1~age+gender+married+family+event+volunteer, family ='binomial')
summary(asGlm1)

# eliminate the gender variable with second largest p-value 
asGlm2 <- glm(amount1~age+married+family+event+volunteer, family ='binomial')
summary(asGlm2)

# eliminate the family variable with third largest p-value
asGlm3 <- glm(amount1~age+married+event+volunteer, family ='binomial')
summary(asGlm3)
# all rest variables fit the model at the significance level of 95%

# plot of standardized residuals
plot(fitted(asGlm3), rstandard(asGlm3))

# try to level age and states into more levels in logistic regression 
## reconstruct variables age and state
age1 <- cut(d$AGE,seq(0,110,10)) # cut age range by 10
age1 <- relevel(age1, ref = '(50,60]') # set age (50,60] as reference
state_num <- factor(as.numeric(d$STATE)) # convert state values to number factors
state_num <-relevel(state_num,ref = '16')   # set Illinois as reference

## build&test new models with multiple levels
asGlm4 <- glm(amount1~age1+gender+married+family+state_num+event+volunteer, family ='binomial')
summary(asGlm4)

asGlm5 <- glm(amount1~age1+married+family+event+volunteer, family ='binomial')
summary(asGlm5)

asGlm6 <- glm(amount1~age1+married+event+volunteer, family ='binomial')
summary(asGlm6)

# find mission values 
## there are missing values in age and state raw data represented by 0 and 'no state', respectively
d$AGE[d$AGE==0]<- NA
d$STATE[d$STATE=='No State']<-NA
df_constituent <- data.frame(d$CONSTITUENT_ID,d$AGE,d$STATE,d$GENDER,d$MARRIED)
missmap(df_constituent, main = "Missing values")
