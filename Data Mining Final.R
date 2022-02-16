colnames(data)
summary(data)
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)

plot(agg, xaxt="n", ylab="CPI", xlab="this")

#draw x-axis
axis(1, labels = paste(agg$Group.1 ,agg$PHAF, seq=agg$Group.1), las=1)

plot(data$TNOS,data$MaNHA, xlab = "Total number of students", ylab =" Not High Ability Males")
abline(lm(X~Y), col = "blue")

par(mar=c(3.75,4,3.75,4))

plot(agg$PHAF,agg$Group.1, xlab = "AGG ", ylab ="Percent High Ability Female")
abline(lm(X~Y), col = "blue")


plot(data$TNOS,data$FeNHA, xlab = "Total number of students", ylab =" Not High Ability Females")
abline(lm(X~Y), col = "blue")

par(mar=c(3.75,4,3.75,4))

plot(data$TNOS,data$FeHA, xlab = "Total number of students ", ylab ="High Ability Female")
abline(lm(X~Y), col = "blue")



#predict(x.lm, data.frame(x=0:3000))



counts <- boxplot(agg$Group.1,agg$PHAF)

barplot(agg$PHAF[1:10] ~ agg$Group.1[1:10] , main="High Ability Students by Counties",
        xlab="Counites")

data_class <- data

# put outcome in its own object
mjob_outcome <- data_class %>% select(GHA)

# remove original variable from the data set
data_class <- data_class %>% select(-GHA)


smp_size <- floor(0.75 * nrow(data_class))

train_ind <- sample(seq_len(nrow(data_class)), size = smp_size)

# creating test and training sets that contain all of the predictors
class_pred_train <- data_class[train_ind, ]
class_pred_test <- data_class[-train_ind, ]

mjob_outcome_train <- mjob_outcome[train_ind, ]
mjob_outcome_test <- mjob_outcome[-train_ind, ]

mjob_pred_knn <- knn(train = class_pred_train, test = class_pred_test, cl = mjob_outcome_train, k=17)






















CA10 <- data %>% group_by(County) %>% summarise(number = n())

CA10 <- CA10[CA10$number > 5,]
  


str(CA10)

View(CA10)

boxplot(data$PHAF[1:20] ~ data$County[list(CA10)],main="High Ability Students by Counties",
        xlab="Counites")

boxplot(boxplotData$PHAF ~ boxplotData$County,main="High Ability Students by Counties",
        xlab="Counites")

boxplot(boxplotData$PHAM ~ boxplotData$County,main="High Ability Students by Counties",
        xlab="Counites")

boxplotData <- data[data$County %in% c("Hamilton", "Lake", "Marion","Porter","Elkhart","Allen","Delaware","Hendricks","Howard","Johnson","LaPorte", "Madison","Wayne"),]

View(boxplotData)

setwd(choose.dir())
data <- read.csv("highAbilityByGenderFL1.csv")

install.packages("rpart.plot")
install.packages("TH.data")
install.packages("dplyr")

library(dplyr)
library(rpart)
library(rpart.plot)


#agg = aggregate(data,by=list(data$County),FUN = mean(data,na.rm=TRUE))

agg <-aggregate(data, by=list(data$County), FUN=mean, na.rm=TRUE)

View(agg)


X <- data$PHAF
Y <- data$PHAM


row.names(data$County)

View(agg)



data
str(data)
View(data)
attributes(data)


#Set a training set and test set
set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
ha.train <- data[ind==1,]
ha.test <- data[ind==2,]



formula <- data$GHA ~ data$FeHA + data$MaHA

#Put the training set into rpat
data_rpart <- rpart(formula, data = ha.train, control = rpart.control(minsplit = 10))

# check the summary of the decision tree
summary(data_rpart)
print(data_rpart)

#Check the cp table
print(data_rpart$cptable)

#Plot the decision tree before it is pruned
plot(data_rpart)
text(data_rpart, use.n=T, cex=.3)

#Make it look good
rpart.plot::rpart.plot(data_rpart)
#data_rpart, type = 5, extra = 100, branch.lty = 2, box.palette = "#ACDBF8"





#Prune the tree
opt <- which.min(data_rpart$cptable[,"xerror"])
cp <- data_rpart$cptable[opt, "CP"]
data_prune <- prune(data_rpart, cp = cp)
print(data_prune)

#Plot the pruned tree
plot(data_prune)
text(data_prune, use.n=T, cex = .2)
rpart.plot::rpart.plot(data_prune)



#Prediction
data_pred <- predict(data_prune, newdata = data.frame(data$PHAF))
print(data_pred)
