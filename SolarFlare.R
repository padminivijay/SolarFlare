library(dplyr)

setwd("C:\\Users\\Padmini\\Google Drive\\Predict 422\\Project\\ExtraCreditProject")
flare2 <- read.csv("flare2.data2", skip = 1, header = FALSE, sep = " ")
flare_names <- c("zurich_class", "spot_size", "spot_distrib", "activity",
                 "evolution", "act_code", "hist_complex", "become_complex",
                 "area", "area_largest", "c_class", "m_class", "x_class")
colnames(flare2) <- flare_names
summary(flare2)
str(flare2)

# Make columns factors
flare2[, 4:9] <- lapply(flare2[, 4:9], FUN = as.factor)
str(flare2)

# Histograms of target variables
hist(flare2$c_class)
hist(flare2$m_class)
hist(flare2$x_class)

table(flare2$c_class)
table(flare2$m_class)
table(flare2$x_class)

### add field to indicate if flare occurred or not
flare2$c_flare = flare2$c_class > 0
flare2$m_flare = flare2$m_class > 0
flare2$x_flare = flare2$x_class > 0

names(flare2)

flare2[, 14:16] <- lapply(flare2[, 14:16], FUN = as.factor)
str(flare2)


###Create test and training sets
n <- dim(flare2)[1]
set.seed(1)
test <- sample(n, round(n/4)) # randomly sample 25% test
data.train <- flare2[-test, ]
data.test <- flare2[test, ]

###Logistic regression
glm.fit = glm( c_flare ~ zurich_class + spot_size + spot_distrib + activity + evolution 
                 + act_code + hist_complex + become_complex + area + area_largest , data=data.train, family = binomial )
glm.sum = summary(glm.fit)
glm.sum
glm.probs = predict(glm.fit,type="response")
glm.pred=rep ("FALSE" ,800)
glm.pred[glm.probs > .5]="TRUE"
table(glm.pred,data.train$c_flare)
mean(glm.pred != data.train$c_flare)


glm.fit = glm( c_flare ~ zurich_class + spot_size + spot_distrib + activity + evolution 
               + act_code + hist_complex + become_complex + area + area_largest , data=data.train, family = binomial )
glm.probs2 = predict.glm(glm.fit,data.test,type="response")
glm.pred=rep("FALSE" ,266)
glm.pred[glm.probs2 > .5]="TRUE"
table(glm.pred,data.test$c_flare)
mean(glm.pred == data.test$c_flare)
