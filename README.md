# LOSH-Occupancy-and-Habitat-Associations
Using machine learning to predict Loggerhead Shrike (LOSH) occupancy in the Mississippi Delta based on measured habitat characteristics
____________________________________________
### R Code

#Check dataset formatting and contents


head(LOSH_occupancy) # check proper column formatting of loaded data

length(LOSH_occupancy) # number of predictors (subtract 5 for point identification columns to get 14)

nrow(LOSH_occupancy) # number of observations

#Convert Use to factor

LOSH_occupancy$Use<- as.factor(LOSH_occupancy$Use)

str(LOSH_occupancy)



#Check correlation of habitat variables

library(psych)

corPlot(LOSH_corsubset, cex = 0.5, xlas = 2, main = "Pairwise Correlation Plot of Habitat Variables")





#Create Train and Test Sets

set.seed(2) #same results for random results
sample <- sample.int(n = nrow(LOSH_occupancy), size = floor(.70*nrow(LOSH_occupancy)), replace = F)
train <- LOSH_occupancy[sample, ]
test  <- LOSH_occupancy[-sample, ]
str(train)
nrow(train)
nrow(test)

#Convert Use to factor

train$Use<- as.factor(train$Use)
str(train)

test$Use<- as.factor(test$Use)
str(test)





###########################

### Logistic Regression

#############################

################Logistic Regression Training Fit###############

glm.shrike <- glm(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                    Ditches + DitchW + GrassW + GrassH, data = train, family = binomial)

summary(glm.shrike)

confint(glm.shrike) #95% CI of beta coeff

#Model Error Rate Prediction Test

glm.probs <- predict(glm.shrike, test, type = "response")
glm.probs[1:10]
contrasts(test$Use)

glm.pred <- rep("No", 109)  #Last number (72) is number of records in test set (so 20% of data - 360*0.20)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred , test$Use)

#set seed 2 (70/30 split)

(82+2) / 109  #correct prediction rate equals 0.7706422
(21+4) / 109 #test error rate 0.2293578


##############################

### LDA Analysis

############ LDA Analysis Training Fit #############


#Create Train and Test Sets

set.seed(2) #same results for random results
sample <- sample.int(n = nrow(LOSH_occupancy), size = floor(.70*nrow(LOSH_occupancy)), replace = F)
train <- LOSH_occupancy[sample, ]
test  <- LOSH_occupancy[-sample, ]
str(train)
nrow(train)
nrow(test)

#Convert Use to factor

train$Use<- as.factor(train$Use)
str(train)

test$Use<- as.factor(test$Use)
str(test)


library(MASS)

lda.shrike <- lda(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                    Ditches + DitchW + GrassW + GrassH, data = train)

lda.shrike


#Model Error Rate Prediction Test (original formatting)

lda.pred <- predict(lda.shrike, test)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, test$Use)

(82+1) / 109  #correct prediction rate
(22+3) / 109 #test error rate


#Model Error Rate Prediction Test (formatted as for LR)

lda.probs <- predict(lda.shrike, test, type = "response")
contrasts(test$Use)

lda.pred <- rep("No", 109)  #Last number (72) is number of records in test set (so 20% of data - 360*0.20)
lda.pred[lda.probs = 1] <- "Yes"
table(lda.pred , test$Use)

nrow(test)

(86+1) / 109  #correct prediction rate
(22+0) / 109 #test error rate








##############################

### GAM Analysis

############ GAM Training Fit #############


library(gam)

###LOGISTIC REGRESSION GAM


gam.shrike <- gam(Use ~ s(WireL, df=3) + 
                    s(NatPerch, df=1) + 
                    s(NatPerchH, df=2) +
                    s(AnthroPerch, df=6) + 
                    s(AnthroPerchH, df=4) + 
                    s(Ditches, df=3) + 
                    s(DitchW, df=3) + 
                    s(GrassW, df=2) + 
                    s(GrassH, df=2), 
                  family = binomial, data = train)

summary(gam.shrike)

##############################################################
#ANOVA Test for linear variables

##Totperches100##
#NatPerches not significant for np effects, could be linear?

M1 <- gam(Use ~ s(WireL, df=3) + 
            s(NatPerchH, df=3) +
            s(AnthroPerch, df=3) + 
            s(AnthroPerchH, df=3) + 
            s(Ditches, df=3) + 
            s(DitchW, df=3) + 
            s(GrassW, df=3) + 
            s(GrassH, df=3), 
          family = binomial, data = train)  ##Excluded

M2 <- gam(Use ~ s(WireL, df=3) + 
            NatPerch + 
            s(NatPerchH, df=3) + 
            s(AnthroPerch, df=3) + 
            s(AnthroPerchH, df=3) + 
            s(Ditches, df=3) + 
            s(DitchW, df=3) + 
            s(GrassW, df=3) + 
            s(GrassH, df=3), 
          family = binomial, data = train)  ##Linear

M3 <- gam(Use ~ s(WireL, df=3) + 
                      s(NatPerch, df=1) + 
                      s(NatPerchH, df=3) +
                      s(AnthroPerch, df=3) + 
                      s(AnthroPerchH, df=3) + 
                      s(Ditches, df=3) + 
                      s(DitchW, df=3) + 
                      s(GrassW, df=3) + 
                      s(GrassH, df=3), 
                    family = binomial, data = train)  ##Spline

anova(M1, M2, M3, test = "F")

#Significant improvement with NatPerches as linear variable

#####################

#Create Train and Test Sets

set.seed(2) #same results for random results
sample <- sample.int(n = nrow(LOSH_occupancy), size = floor(.70*nrow(LOSH_occupancy)), replace = F)
train <- LOSH_occupancy[sample, ]
test  <- LOSH_occupancy[-sample, ]
str(train)
nrow(train)
nrow(test)

#Convert Use to factor

train$Use<- as.factor(train$Use)
str(train)

test$Use<- as.factor(test$Use)
str(test)


##GAM Training Fit:

gam2.shrike <- gam(Use ~ s(WireL, df=3) + 
                     s(NatPerch, df=1) + 
                     s(NatPerchH, df=2) +
                     s(AnthroPerch, df=6) + 
                     s(AnthroPerchH, df=4) + 
                     s(Ditches, df=3) + 
                     s(DitchW, df=3) + 
                     s(GrassW, df=2) + 
                     s(GrassH, df=2), 
                   family = binomial, data = train)

summary(gam2.shrike)

####Model Error Rate Prediction Test

gam.probs <- predict(gam2.shrike, test, type = "response")
gam.probs[1:10]
contrasts(test$Use)

gam.pred <- rep("No", 109)
gam.pred[gam.probs > .5] <- "Yes"
table(gam.pred , test$Use)

(80+4)/109  #Prediction success rate
(19+6)/109 #test error rate

###Plot all variables

par(mfrow = c(4, 4))

plot(gam2.shrike, se = TRUE , col = "blue")




##############################

### Decision Tree

############ Decision Tree Training Fit #############

library(tree)

tree.shrike <- tree(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                      Ditches + DitchW + GrassW + GrassH, data = train)

summary(tree.shrike)

tree.pred <- predict(tree.shrike, test, type="class")

table(tree.pred, test$Use)


#Tree pruning - cf.tree determines optimal branching of the trees and performs cross validation

set.seed(2)

#cross validation results
cv.shrike <- cv.tree(tree.shrike, FUN = prune.misclass)  #specify to prune based on mis-classification rates not something like deviance

names(cv.shrike) #k cost parameter used to tune, do not worry about that
cv.shrike

plot(cv.shrike$size, cv.shrike$dev, type="b") #can see deviance is lowest at optimal tree size
plot(cv.shrike$k, cv.shrike$dev, type = "b")


#prune tree based on optimal size

prune.shrike <- prune.misclass(tree.shrike, best=3)

plot(prune.shrike)

summary(prune.shrike)

## Tree Misclassification Rate

tree.pred <- predict(prune.shrike, test, type="class")

table(tree.pred, test$Use)

(81+1)/109  #Successful prediction rate
1 - (81+1)/109 #test error/misclassification rate


#### Random Forest

library(randomForest)

set.seed (2)
rf.shrike <- randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                            Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, data=train, importance = TRUE)

tree.pred <- predict(rf.shrike, test, type="class")

table(tree.pred, test$Use)

(81+2)/109  #Successful prediction rate
1 - (81+2)/109 #test error/misclassification rate

tree.probs <- predict(rf.shrike, test, type = "response")
tree.probs[1:10]
contrasts(test$Use)

tree.pred <- rep("No", 109)
tree.pred[tree.probs = 1] <- "Yes"
table(tree.pred , test$Use)



#Importance Plot

imp<-importance(rf.shrike)

Gini<-imp[,"MeanDecreaseGini"]

par(mar=c(5,8,4,1)+.1)
barplot(sort(Gini,decreasing=TRUE), horiz=TRUE, las=1, xlim=c(0,100), col = "chocolate2", xlab="Importance (Gini Index Decrease)")



## Tree Visualization

library(rpart)
library(rpart.plot)
fit <- rpart(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
               Ditches + DitchW + GrassW + GrassH, data = train, method = 'class')
rpart.plot(fit, extra = 106)


