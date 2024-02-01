
library("readxl")
install.packages("purrr")
install.packages("ggplot2")

#Create new dataset excluding casual and registered variables
bike_df <- read_excel("C:/Users/Admin/Downloads/1657875746_day.xlsx")
View(bike_df)

bike_df<-subset(bike_df,select=-c(casual,registered))
head(bike_df,5)
dim(bike_df)

#Summary of the dataset
summary(bike_df)

#Structure of dataset
str(bike_df)

#Rename the columns
names(bike_df)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')

#Read the data
head(bike_df,5)

#Typecasting the datetime and numerical attributes to category

bike_df$datetime<- as.Date(bike_df$datetime)
bike_df$year<-as.factor(bike_df$year)
bike_df$month<-as.factor(bike_df$month)
bike_df$season <- as.factor(bike_df$season)
bike_df$holiday<- as.factor(bike_df$holiday)
bike_df$weekday<- as.factor(bike_df$weekday)
bike_df$workingday<- as.factor(bike_df$workingday)
bike_df$weather_condition<- as.factor(bike_df$weather_condition)

#Missing values in dataset
missing_val<-data.frame(apply(bike_df,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val

library(ggplot2)
#column plot for season wise monthly distribution of counts
ggplot(bike_df,aes(x=month,y=total_count,fill=season))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Season wise monthly distribution of counts')

#column plot for weekday wise monthly distribution of counts
ggplot(bike_df,aes(x=month,y=total_count,fill=weekday))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Weekday wise monthly distribution of counts')

#boxplot for total_count_outliers
par(mfrow=c(1, 1))#divide graph area in 1 columns and 1 rows
boxplot(bike_df$total_count,main='Total_count',sub=paste(boxplot.stats(bike_df$total_count)$out))


#Violin plot for Yearly wise distribution of counts
ggplot(bike_df,aes(x=year,y=total_count,fill=year))+geom_violin()+theme_bw()+
  labs(x='Year',y='Total_Count',title='Yearly wise distribution of counts')

#load the purrr library for functions and vectors
library(purrr)
#Split the dataset based on simple random resampling
train_index<-sample(1:nrow(bike_df),0.7*nrow(bike_df))
train_data<-bike_df[train_index,]
test_data<-bike_df[-train_index,]
dim(train_data)
dim(test_data)

#Read the train and test data
head(train_data,5)
head(test_data,5)

#Create a new subset for train attributes 
train<-subset(train_data,select=c('season','year','month','holiday', 'weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
#Create a new subset for test attributes
test<-subset(test_data,select=c('season','year','month','holiday','weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
head(train,5)
head(test,5)

#create a new subset for train categorical attributes
train_cat_attributes<-subset(train,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for test categorical attributes
test_cat_attributes<-subset(test,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for train numerical attributes
train_num_attributes<-subset(train,select=c('weekday','month','temp','humidity','windspeed','total_count'))
#create a new subset for test numerical attributes
test_num_attributes<-subset(test,select=c('weekday','month','temp', 'humidity','windspeed','total_count'))

install.packages("caret")
#load the caret library
library(caret)
#other variables along with target variable to get dummy variables
othervars<-c('month','weekday','temp','humidity','windspeed','total_count')
set.seed(2626)
#Categorical variables
vars<-setdiff(colnames(train),c(train$total_count,othervars))
#formula pass through encoder to get dummy variables
f <- paste('~', paste(vars, collapse = ' + '))
#encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f), train)
#Predicting the encode attributes
encode_attributes<-predict(encoder,train)
#Binding the train_num_attributes and encode_attributes
train_encoded_attributes<-cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)


set.seed(5662)
#Categorical variables
vars<-setdiff(colnames(test),c(test$total_count,othervars))
#formula pass through encoder to get dummy variables
f<- paste('~',paste(vars,collapse='+'))
#Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f),test)
#Predicting the encoder attributes
encode_attributes<-predict(encoder,test)
#Binding the test_num_attributes and encode_attributes
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)


#Set seed to reproduce the results of random sampling
set.seed(672)
#training the lr_model
lr_model<-lm(train_encoded_attributes$total_count~.,train_encoded_attributes[,-c(6)])
#Summary of the model
summary(lr_model)


#Cross validation prediction
#To ignore warning messages
options(warn=-1)
#Set seed to reproduce results of random sampling
set.seed(623)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
CV_predict<-train(total_count~.,data=train_encoded_attributes,method='lm',trControl=train.control)
#Summary of cross validation prediction
summary(CV_predict)

#Cross validation prediction plot
residuals<-resid(CV_predict)
y_train<-train_encoded_attributes$total_count
plot(y_train,residuals,ylab=('Residuals'),xlab=('Observed'),main=('Cross validation prediction plot'))
abline(0,0)

set.seed(6872)
options(warn=-1)
#predict the lr_model
lm_predict<- predict(lr_model,test_encoded_attributes[,-c(6)])
head(lm_predict,5)

set.seed(688)
#Root mean squared error
rmse<-RMSE(lm_predict, test_encoded_attributes$total_count)
print(rmse)
#Mean squared error
mae<-MAE(lm_predict, test_encoded_attributes$total_count)
print(mae)

#Residual plot
y_test<-test_encoded_attributes$total_count
residuals<-y_test-lm_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)


set.seed(6788271)
#load the randomForest library
install.packages("randomForest")
library(randomForest)
#training the model
rf_model<-randomForest(total_count~.,train_encoded_attributes,importance=TRUE,ntree=200)
rf_model

#Cross validation prediction for Random Forest
options(warn=-1)
set.seed(6772)
library(randomForest)
install.packages("ranger")
#load the ranger library for random forest CV
library(ranger)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
rf_CV_predict<-train(total_count~.,train_encoded_attributes,method='ranger',trControl=train.control)
rf_CV_predict

#Cross validation prediction plot
residuals<-resid(rf_CV_predict)
plot(y_train,residuals,xlab='Observed',ylab='Residuals',main='Cross validation prediction plot')
abline(0,0)

set.seed(7889)
#Predicting the model
rf_predict<-predict(rf_model,test_encoded_attributes[,-c(6)])
head(rf_predict,5)

set.seed(667)
#Root mean squared error
rmse<-RMSE(y_test,rf_predict)
print(rmse)
mae<-MAE(y_test,rf_predict)
print(mae)

#Residual plot
residuals<-y_test-rf_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

Bike_predictions=data.frame(y_test,rf_predict)
write.csv(Bike_predictions,'Bike_Renting_R.CSV',row.names=F)
Bike_predictions