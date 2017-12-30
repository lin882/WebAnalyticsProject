library(caret)

#import dataset
data1 = read.csv("droplist-advancecapital_12061.csv",sep=",")

#check data type
str(data1)

#statisitc summry
summary(data1$columnName)

attach(data1)

#convert categorical data to factors
data1$M_Q3_2017 = as.numeric(data1$M_Q3_2017)

data1$AddDrop = as.factor()

#or create dummies for add and drop
data1$AddDropDummy4[data1$AddDrop=="Add"]=1
data1$AddDropDummy4[!data1$AddDrop=="Add"]=0
data1$AddDropDummy4=factor(data1$AddDropDummy4)

levels(data1$AddDropDummy3) <- c("Drop", "Add")

#logit model
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
model1 <- glm(AddDrop~NIQ3Q2+NIQ2Q1+NIQ1Q4+ReQ3Q2+ReQ2Q1+ReQ1Q4+TAQ3Q2+TAQ2Q1+TAQ1Q4+SPQ3Q2+SPQ2Q1+SPQ1Q4+
                S_Q4_2017+S_Q3_2017+S_Q2_2017+S_Q1_2017+M_Q4_2017+M_Q3_2017+M_Q2_2017+M_Q1_2017,
              family = "binomial",
              data = data1)
summary(model1)

hist(S_Q3_2017)





#ANOVA
anova(model, test="Chisq")
