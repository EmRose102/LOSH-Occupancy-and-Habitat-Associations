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


##Variable Prediction Plots

library(ggeffects)

plot(ggpredict(glm.shrike, terms = "WireL"))

plot(ggpredict(gam2.shrike, terms = "WireL"))

plot(ggpredict(gam2.shrike, terms = "AnthroPerch"))

plot(ggpredict(gam2.shrike, terms = "AnthroPerchH"))




##############################

### Random Forest (RF)

############ Random Forest Training Fit #############

library(randomForest)

set.seed (87)
rf.shrike <- randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                            Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, data=train, importance = TRUE)

## RF Misclassification Rate

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


imp<-importance(prune.shrike)

Gini<-imp[,"MeanDecreaseGini"]

par(mar=c(5,8,4,1)+.1)
barplot(sort(Gini,decreasing=TRUE), horiz=TRUE, las=1, xlim=c(0,100), col = "chocolate2", xlab="Importance (Gini Index Decrease)")



#Extract tree for visualization

library(randomForest)
library(reprtree)

install.packages("devtools")
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)

single.tree <- getTree(rf.shrike, k=1, labelVar=TRUE)

reprtree:::plot.getTree(rf.shrike)

#8 Nodes max with wireL as top variable

set.seed (8)
rf.shrike <- randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                            Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, maxnodes=8, data=train, importance = TRUE)

reprtree:::plot.getTree(rf.shrike)

#12 Nodes max with wireL as top variable

set.seed (8)
rf.shrike <- randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                            Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, 
                          maxnodes=12, data=train, importance = TRUE)

reprtree:::plot.getTree(rf.shrike)


#12 Nodes max with wireL as top variable

set.seed (27)
rf.shrike <- randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
                            Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, 
                          maxnodes=12, data=train, importance = TRUE)

reprtree:::plot.getTree(rf.shrike)

plot.getTree(rf.shrike, node.info=TRUE)


rpart.plot(plot.getTree(rf.shrike), extra = 106)

## Tree Visualization

library(rpart)
library(rpart.plot)
fit <- rpart(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
               Ditches + DitchW + GrassW + GrassH, data = train, method = 'class')

fit <- rpart(randomForest(Use ~ WireL + NatPerch + NatPerchH + AnthroPerch + AnthroPerchH + 
               Ditches + DitchW + GrassW + GrassH, na.action=na.roughfix, data=train, importance = TRUE))


rpart.plot(fit, extra = 106)

rpart.plot(single.tree, extra = 106)


plot(tree.shrike)
text(tree.shrike, pretty=0)


