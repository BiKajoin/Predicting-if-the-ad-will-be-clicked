library('tidyverse')
library('rpart')
library('rpart.plot')
library('caret')
library('ROCR')
library('lift')

#========================================================
#======================= Exploring ======================
#========================================================



###For finding duplicated words===========================
#Separate words by space
words <- ads %>%
  select(Ad.Topic.Line) %>%
  separate(Ad.Topic.Line,into = c('A','B','C','D','E','F'),sep = ' ')
allwords <- c(words$A,words$B,words$C,words$D,words$E,words$F)
allwordsdf <- data.frame(allwords)
#Output
allwordsdf %>%
  mutate_if(is_character,as.factor) %>%
  na.omit(allwordsdf) %>%
  group_by(allwords) %>%
  count() %>%
  arrange(desc(n))

#Other data exploration
hist(ads$Daily.Time.Spent.on.Site)
hist(ads$Age)
hist(ads$Area.Income)
hist(ads$Daily.Internet.Usage)

cor(ads$Age,ads$Daily.Time.Spent.on.Site)
ggplot(ads,aes(Age,Daily.Time.Spent.on.Site)) + geom_point()
###==========================================================

#========================================================
#======================= Modeling =======================
#========================================================

###Data preparation
#Change time format and deselect unimportant data
ads <- read.csv('advertising.csv')
adsf <- ads %>%
  separate(Timestamp, c('date','time'),sep = ' ') %>%
  separate(time, c('hr','min','sec'),sep = ':') %>%
  select(-Ad.Topic.Line,-City,-Country, -date,-min,-sec)
  
#Convert variable to appropriate data type
adsf <- adsf %>% mutate_if(is_character, as.factor)
adsf$hr <- as.numeric(adsf$hr)
adsf$Clicked.on.Ad <- as.factor(adsf$Clicked.on.Ad)

###Create train and test set
set.seed(123)
adsf_testindex <- sample(nrow(adsf),0.3*nrow(adsf))
adsf_training <- adsf[-adsf_testindex,]
adsf_testing <- adsf[adsf_testindex,]
#Output
summary(adsf_training)
summary(adsf_testing)


#====================================================
###Create Model (Holdout method)
##Create model
adsf_tree <- rpart(Clicked.on.Ad ~ ., data = adsf_training)
#output
rpart.plot(adsf_tree)
#for PDF file
pdf('ads.pdf')
dev.off()

##Predict
adsf_res <- predict(adsf_tree, adsf_testing, type = 'class')
adsf_res_p <- predict(adsf_tree, adsf_testing)[,1]
#output
adsf_res
adsf_res_p

##Confusion Matrix
confusionMatrix(adsf_res,adsf_testing$Clicked.on.Ad,positive = '1')
confusionMatrix(adsf_res,adsf_testing$Clicked.on.Ad,positive = '1',
                mode = 'prec_recall')
#====================================================
###Create Model (K-Fold method)
##Create model
train_ctrl <- trainControl(method = 'cv',number = 5)
model_kfold <- train(Clicked.on.Ad ~ ., data = adsf_training,
                     trControl = train_ctrl,
                     method = 'rpart')
#output
model_kfold
model_kfold$finalModel
rpart.plot(model_kfold$finalModel)
#for PDF file
pdf('ads_kfold.pdf')
dev.off()

##Predict
adsf_kfold_res <- predict(model_kfold$finalModel,adsf_testing,type = 'class')
adsf_kfold_res_p <- predict(model_kfold$finalModel,adsf_testing)[,'1']
#output
summary(adsf_kfold_res_p)

##Confusion Matrix
confusionMatrix(adsf_kfold_res,adsf_testing$Clicked.on.Ad,positive = '1')
confusionMatrix(adsf_kfold_res,adsf_testing$Clicked.on.Ad,positive = '1',
                mode = 'prec_recall')

##Lift analysis
lift_result <- data.frame(prob = adsf_kfold_res_p, y = adsf_testing$Clicked.on.Ad)
lift_obj <- lift(y~prob, data=lift_result, class='1')
plot(lift_obj)
lift_result %>%
  arrange(desc(prob)) %>%
  slice_head(n=30) %>%
  count(y) -> yncount1

adsf_testing %>%
  count(Clicked.on.Ad) -> yncount2

yncount1
yncount2

(yncount1$n[yncount1$y==1]/(0.1*nrow(adsf_testing))) / 
  (yncount2$n[yncount2$Clicked.on.Ad==1]/nrow(adsf_testing))

#Use lift library
TopDecileLift(adsf_kfold_res_p,adsf_testing$Clicked.on.Ad)

#====================================================
###Create Model (K-Fold method with grid search)
##Create model
train_ctrl <- trainControl(method = 'cv',number = 5)
cplist <- seq(0.0001,0.05,0.0001)
min_split <- c(1:100)
tunegrid <- expand.grid(cp=cplist)
model_kfold <- train(Clicked.on.Ad ~ ., data = adsf_training,
                     method = 'rpart',
                     trControl = train_ctrl,
                     tuneGrid = tunegrid,
                     minsplit = min_split)
#output
model_kfold
model_kfold$finalModel
rpart.plot(model_kfold$finalModel)
#for PDF file
pdf('ads_kfold.pdf')
dev.off()

##Predict
adsf_kfold_res <- predict(model_kfold$finalModel,adsf_testing,type = 'class')
adsf_kfold_res_p <- predict(model_kfold$finalModel,adsf_testing)[,'1']
#output
summary(adsf_kfold_res)
summary(adsf_kfold_res_p)
head(adsf_kfold_res_p)
adsf_kfold_res_p

##Confusion Matrix
confusionMatrix(adsf_kfold_res,adsf_testing$Clicked.on.Ad,
                positive = '1',
                mode = 'prec_recall')

confusionMatrix(adsf_kfold_res,adsf_testing$Clicked.on.Ad,positive = '1')


##Lift analysis
lift_result <- data.frame(prob = adsf_kfold_res_p, 
                          y = adsf_testing$Clicked.on.Ad)
lift_obj <- lift(y~prob, data=lift_result, class='1')
plot(lift_obj)


lift_result %>%
  arrange(desc(prob)) %>%
  slice_head(n=0.1*nrow(adsf_testing)) %>%
  count(y) -> yncount1

adsf_testing %>%
  count(Clicked.on.Ad) -> yncount2

yncount1
yncount2

(yncount1$n[yncount1$y==1]/(0.1*nrow(adsf_testing))) / 
  (yncount2$n[yncount2$Clicked.on.Ad==1]/nrow(adsf_testing))

#Use lift library
TopDecileLift(adsf_kfold_res_p,adsf_testing$Clicked.on.Ad)
