#########
# 1
#########
install.packages("caret")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("lattice")
install.packages("rpart")
install.packages("dplyr")
install.packages("ROCR")
install.packages("pROC")

library("dplyr")
library("caret")
library("ggplot2")
library("lattice")
library("rpart.plot")
library("rpart")
library("randomForest")
library("ROCR")
library("pROC")

setwd(choose.dir())

# Data importation
readmission = read.csv("readmission.csv")

# Splitting into a training set and a test set
set.seed(144)
split = createDataPartition(readmission$readmission, p = 0.75, list = FALSE)
readm.train <- readmission[split,]
readm.test <- readmission[-split,]

summary(readmission)
count(readmission, vars = "gender")

  readmission %>%
  group_by(gender) %>%
  summarise(gender_count=n())
  
  readmission %>%
    group_by(race) %>%
    summarise(gender_count=n())

###1b.
cost.matrix <-matrix(c(0, 1200, 35000, 0),2,2,byrow=T)
L_FP <- 1200
# Hint:
# The cost of a false negative, as compared to a true positive, is 25% chance of a $35,000 cost,
# minus the upfront $1200
cost <- (35000 * .25)
cost1<- 35000 - cost - 1200
L_FN <- cost1
loss.matrix <- matrix(c(0,L_FP,L_FN,0),2,2,byrow=T)
loss.matrix
L_FN

###1c.

cv.trees = train(y = readm.train$readmission,
                 x = subset(readm.train, parms =list(loss=loss.matrix)),
                 method = "class",
                 trControl = trainControl(method = "cv", number = 25),
                 tuneGrid = data.frame(cp = .001))




trees = rpart(readmission~.,readm.train, method="class", minbucket=10)

mod.cart = 

prp(mod.cart)
cv.trees = train(y = readm.train$readmission,
                 
                 x = subset(readm.train,  parms =list(loss=loss.matrix)),
                 
                 method = "rpart",
                 
                 trControl = trainControl(method = "cv", number = 10),
                 
                 tuneGrid = data.frame(cp = .001))


prp(cv.trees$finalModel)


cv.tree <- rpart(readmission~., data= readm.train, method="cv", 
                parms =list(loss=loss.matrix),

                
                                minbucket=25, cp=.001)

#mod.cart = cv.trees$finalModel
set.seed(121)
tree <- rpart(readmission ~ .,data = readm.train,
              parms =list(loss=loss.matrix),
              control = rpart.control(cp = 0.001))
prp(tree)




###1d.

pred.cart = predict(tree, newdata=readm.test)

confusion.matrix <- table(readm.test$readmission,pred.cart)
confusion.matrix
number.interventions <- confusion.matrix[1,2]+confusion.matrix[2,2]
prevented.readmissions <- confusion.matrix[2,2]*.25

number.interventions

prevented.readmissions

accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
TPR <- confusion.matrix[2,2]/sum(confusion.matrix[2,])
FPR <- confusion.matrix[1,2]/sum(confusion.matrix[1,])
cost <- sum(cost.matrix*confusion.matrix)

accuracy
TPR
FPR 


baseline.accuracy <- sum(!readm.test$readmission)/nrow(readm.test)
baseline.cost <- (confusion.matrix[2,1]+confusion.matrix[2,2])*35000
absolute.savings <- (baseline.cost-accuracy)/baseline.cost 
relative.savings <- (baseline.cost-cost)/baseline.cost


baseline.accuracy
baseline.cost
absolute.savings
relative.savings

#########
# 2
#########

property = read.csv("Residential_Property_Assessments.csv")

plot(property$Assessment, property$Livable.Area, main="Property Tax Scatterplot",
     xlab="Livable Area ", ylab=" Assessment ", pch=19)




equ <- lm(Assessment~., property)
summary(equ)


new <- data.frame(
  Livable.Area = c(2500)
)

this <- predict(equ, newdata = new, interval = "confidence")

this


this1<- mean(property$Assessment)
var()

# Compute the standard deviation =
# square root of th variance
sd(my_data$Sepal.Length)


#########
# 3
#########
bc.temp = read.csv("breastcancer.csv")
response = bc.temp$response
train.indicator = bc.temp$train.indicator
train.indicator
bc = bc.temp

bc$train.indicator = NULL
x.new = as.matrix(bc[,-1])


bc.test = bc[!train.indicator,]
bc.train = bc[train.indicator,]
y.test = response[!train.indicator]
y.train = response[train.indicator]



###3a
mult.logit.bc = glm(response~., data = bc.train, family = "binomial")

is.na(mult.logit.bc)


summary(mult.logit.bc$df.null)

mult.logit.bc  
  
#illustration of is.na

tvec = c(1, 2, NA)
tvec
is.na(tvec)
!is.na(mult.logit.bc)
confusion.matrix = table( bc.train$response, pred.mult.train)
confusion.matrix



###3b
pred.mult.train = predict(mult.logit.bc, bc.train, type ="response")
round <- round(pred.mult.train, digits = 10)

hist(pred.mult.train, breaks=5, main="3B Predict")


truePos = confusion.matrix[2,2]/sum(confusion.matrix[2,])
truePos
# 0
falsePos = confusion.matrix[1,2]/sum(confusion.matrix[1,])
falsePos
# 0.03571429
rocr.pred = prediction(pred.mult.train, bc.train$response)
roc.perf = plot(performance(rocr.pred, "tpr", "fpr"))

AUC = as.numeric(performance(rocr.pred, "auc")@y.values)
AUC
# 1
xabline(0,1)







# Execute the following code before problem 3e

x.scale = scale(x.new)
x.test = as.matrix(x.scale[!train.indicator,])
x.train = as.matrix(x.scale[train.indicator,])

library(glmnet)
lasso.logit = cv.glmnet(x.train, y.train, family = "binomial")
lasso.logit

###3e
#this gives you the coefficients
this <- coef(lasso.logit, s = "lambda.min")
summary(this)

###3f
pred.lasso.train = as.vector(predict(lasso.logit, newx = x.train, s = "lambda.min", type = "response"))

hist(pred.lasso.train, breaks=5, main="3F Predict")


###3g

pred.mult.test = predict(mult.logit.bc, newdata = bc.test, type = "response")
pred.lasso.test = as.vector(predict(lasso.logit, newx = x.test, s = "lambda.min", type = "response"))


prediction.mult.test= prediction(pred.mult.test, bc.test$response)
perf.mult.test = performance(prediction.mult.test, measure = "tpr", x.measure = "fpr")

prediction.lasso.test= prediction(pred.lasso.test,bc.test$response)
perf.lasso.test = performance(prediction.lasso.test, measure = "tpr", x.measure = "fpr")

roc.analysis.lasso = roc(bc.test$response ~perf.lasso.test, bc.test)
roc.analysis.lasso$response


roc.analysis.mult = roc(bc.test$response~pred.mult.test, bc.test)
roc.analysis.mult$response

plot(perf.mult.test, xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Out-of-Sample ROC Comparison")
plot(perf.lasso.test, add= T, col = "orange", lty = 4, lwd = 2)
legend("bottomright", c("Multiple Logistic",  "L1 Penalized"), lty = c(1,  4), col = c("black", "orange"), bty="n")

#########
# 4
#########

#No helper code provided

wine = read.csv("redwine.csv")
plot(wine)
summary(wine)




split = createDataPartition(wine$fixed.acidity, p = 0.70, list = FALSE)
wine.train <- wine[split,]
wine.test <- wine[-split,]

x.train = wine.train[,-1]
x.test = wine.test[,-1]
y.train = wine.train[,1]
y.test = wine.test[,1]


fix <- lm(fixed.acidity~., wine.train)
summary(fix)

  vola <- lm(volatile.acidity~., wine.train)
summary(vola)

cit <- lm(citric.acid~., wine.train)
summary(cit)

sugar <- lm(residual.sugar~., wine.train)
summary(sugar)

chlor <- lm(chlorides~., wine.train)
summary(chlor)

free <- lm(free.sulfur.dioxide~., wine.train)
summary(free)

total <- lm(total.sulfur.dioxide~., wine.train)
summary(total)

den <- lm(density~., wine.train)
summary(den)

ph <- lm(pH~., wine.train)
summary(ph)

sul <- lm(sulphates~., wine.train)
summary(sul)

alch <- lm(alcohol~., wine.train)
summary(alch)

qual <- lm(quality ~., wine.train)
summary(qual)


den1 <- lm(density~.-citric.acid -quality, wine.train)
fix <- lm(fixed.acidity~., wine.train)

den.test <- lm(density~.-citric.acid -quality, wine.test)
fix.test <- lm(fixed.acidity~.- quality , wine.test)

summary(fix.test)
summary(den.test)
pred.lm = predict(fix, newdata=wine.test)
r2.lm = 1-sum((y.test -pred.lm)^2)/sum((y.test-mean(y.train))^2)



summary(den.test)
summary(fix.test)


# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)

model1 <- lm(fixed.acidity~., wine.train)
summary(model1)

model1 <- train(fixed.acidity ~.-quality, data = wine.train, method = "lm",
                trControl = train.control)

model2 <- train(fixed.acidity ~.-volatile.acidity- free.sulfur.dioxide -quality , data = wine.train, method = "lm",
               trControl = train.control)



pred = predict(model1, newdata=wine.test)
r2 = 1-sum((y.test - pred)^2)/sum((y.test - mean(y.train))^2)
r2
#[1] 0.867495
pred = predict(model2, newdata=wine.test)
r2 = 1-sum((y.test - pred)^2)/sum((y.test - mean(y.train))^2)
r2
#0.8676707
