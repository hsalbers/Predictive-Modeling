#Hope Albers and Arshan Tarapore
rm(list=ls())

az <- read.csv("az2000.csv")
azdf <- az[c(5,8,11,14)]
azdf$so2_mean01 <- ifelse(azdf$so2_mean > median(azdf$so2_mean),1,0)

samplesize <- floor(0.80 * nrow(azdf))
train_ind <- sample(seq_len(nrow(azdf)), size = samplesize)
trn <- azdf[train_ind, ]
tst <- azdf[-train_ind, ]
trn_so2_01 <- trn$so2_mean01 
trnso2 <- trn$so2_mean

logfit <- glm(so2_mean01~no2_mean+o3_mean+co_mean,data=azdf,family="binomial")
summary(logfit)
coef(logfit)

glm.prob <- predict(logfit,type="response")
glm.pred <- rep("0",length(glm.prob))
glm.pred[glm.prob>0.5] <- "1"
sum(as.numeric(glm.pred))/length(glm.pred)

confmat<- table(azdf$so2_mean01,glm.pred)
confmat
require(boot)

lfit <- lm(so2_mean~no2_mean+o3_mean+co_mean,data=azdf)
summary(lfit)


lpred <- predict(lfit)

summary(lpred)


plot(lpred,col='red',ylim=c(0,20),ylab="SO2 [parts per billion]",xlab="Data Collection ID",main="Predicted SO2 v. Actual SO2")
points(azdf$so2_mean,col="black")
legend(2500,18,c("Predictions","Actuals"),col=c("red","black"),pch=c(1,1),title="Legend")


print(paste('Success rate:',(mean(glm.pred==azdf$so2_mean01))))
print(paste('Error rate:',(mean(glm.pred!=azdf$so2_mean01))))
print(paste('Type 1 Error Rate:',confmat['0','1']/sum(confmat['0',])))
print(paste('Type 2 Error Rate:',confmat['1','0']/sum(confmat['1',])))
print(paste('Power:',confmat['1','1']/sum(confmat['1',])))
print(paste('Precision:',confmat['1','1']/(confmat['0','1']+confmat['1','1'])))


require(MASS)
lda.fit <- lda(so2_mean01~o3_mean+no2_mean+co_mean,data=trn,family="binomial")
lda.fit

lda.pred <- predict(lda.fit,tst)
cm02 <- table(tst$so2_mean01,lda.pred$class)
cm02
print(paste('Success rate:',(mean(lda.pred$class==tst$so2_mean01))))
print(paste('Error rate:',(mean(lda.pred$class!=tst$so2_mean01))))
print(paste('Type 1 Error Rate:',cm02['0','1']/sum(cm02['0',])))
print(paste('Type 2 Error Rate:',cm02['1','0']/sum(cm02['1',])))
print(paste('Power:',cm02['1','1']/sum(cm02['1',])))
print(paste('Precision:',cm02['1','1']/(cm02['0','1']+cm02['1','1'])))


qda.fit <- qda(so2_mean01~o3_mean+no2_mean+co_mean,data=trn,family="binomial")
qda.fit

qda.pred <- predict(qda.fit,tst)
cm2 <- table(tst$so2_mean01,qda.pred$class)
cm2
print(paste('Success rate:',(mean(qda.pred$class==tst$so2_mean01))))
print(paste('Error rate:',(mean(qda.pred$class!=tst$so2_mean01))))
print(paste('Type 1 Error Rate:',cm2['0','1']/sum(cm2['0',])))
print(paste('Type 2 Error Rate:',cm2['1','0']/sum(cm2['1',])))
print(paste('Power:',cm2['1','1']/sum(cm2['1',])))
print(paste('Precision:',cm2['1','1']/(cm2['0','1']+cm2['1','1'])))

