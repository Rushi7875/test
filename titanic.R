tit <- read.csv("titanic.csv",na.strings = c("","NA"))
the file
View(tit)
str(tit)
summary(tit)
tit <- tit[,-1]
tit <- tit[!colnames(tit) %in% c("name","home.dest")]
colSums(is.na(tit))
apply(is.na(tit),c(2),sum)/nrow(tit)*100
tit <- tit[!colnames(tit) %in% c("body","cabin")]
tit <- tit[!colnames(tit) %in% "boat"]


tit$survived <- ifelse(tit$survived==1,"Yes","No")

tit$ticket <- as.numeric(tit$ticket)
tit$pclass <- as.factor(tit$pclass)
tit$survived <- as.factor(tit$survived)
tit$embarked <- as.factor(tit$embarked)
tit.num <- tit[sapply(tit, is.numeric)]
tit.fac <- tit[sapply(tit,is.factor)]

for(i in 1:ncol(tit.num)){
  tit.num[,i][is.na(tit.num[,i])] <- mean(tit.num[,i],na.rm = T)
}

getmode <- function(x){
  uni <- unique(x)
  uni[which.max(tabulate(match(x,uni)))]
}

for(i in 1:ncol(tit.fac)){
  tit.fac[,i][is.na(tit.fac[,i])] <- getmode(tit.fac[,i])
}

any(is.na(tit.num))
any(is.na(tit.fac))

library(ggplot2)
library(gridExtra)

factplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_bar(fill = "blue", color = "black", alpha= 0.2)+
    xlab(column)
}

fp <- lapply(colnames(tit.fac), factplot, df=tit.fac)
do.call("grid.arrange", fp)

numplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
    geom_density(fill='blue', alpha=0.2)+
    xlab(column)
}


np <- lapply(colnames(tit.num), numplot, df=tit.num)
do.call("grid.arrange", np)


tit <- cbind(tit.num,tit.fac)

View(tit)

set.seed(100)
s=sample(2,nrow(tit),replace = T,prob = c(0.8,0.2))
train <- tit[s==1,]
test <- tit[s==2,]
test1 <- test[!colnames(test) %in% "survived"]
colnames(test1)


model <- glm(survived~., data = train,family = binomial("logit"))
summary(model)

pred <- predict(model,test1,type = "response")
pred1 <- ifelse(pred>=0.5,"Yes","No")
table(test$survived,pred1)
table(test$survived)
table(pred1)

acc <- (127+64)/(127+64+19+21)

tpr <- (64)/(64+19)
fpr <- (21)/(127+27)
tnr <- (127)/(127+21)

#feature selection'
nullmodel <- glm(survived ~1,data = train,family = binomial("logit"))
fullmodel <- glm(survived~., data = train,family = binomial("logit"))
bestfit <- step(nullmodel,scope = list(lower=nullmodel,upper=fullmodel),direction = "both")

model1 <- glm(formula = survived ~ sex + pclass + age + sibsp + embarked, 
              family = binomial("logit"), data = train)
summary(model1)
pred2 <- predict(model,test1,type = "response")
pred3 <- ifelse(pred>=0.4,"Yes","No")
table(test$survived,pred3)
acc1 <- (111+66)/(111+66+17+37)

tpr1 <- (66)/(66+17)
fpr1 <- (37)/(37+111)
tnr1 <- (111)/(111+17)
