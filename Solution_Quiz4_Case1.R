rm(list=ls()); graphics.off()

if(!require(tidyverse)) install.packages('tidyverse')
if(!require(text2vec)) install.packages('text2vec')
if(!require(glmnet)) install.packages('glmnet')
if(!require(MLmetrics)) install.packages('MLmetrics')
if(!require(MLmetrics)) install.packages('MLmetrics')
if(!require(tm)) install.packages('tm')
if(!require(SnowballC)) install.packages('SnowballC')
if(!require(e1071)) install.packages('e1071')
if(!require(caret)) install.packages('caret')
if(!require(rpart)) install.packages('rpart')


library(rpart)
library(e1071)
library(tidyverse)
library(text2vec) # text vectorization
library(glmnet) # building model cv.gmlnet()
library(tm)
library(SnowballC)
library(caret)

#cargando la data Train
Data_Yelp_train = read.csv("https://raw.githubusercontent.com/capajaro/Quiz--4/main/Train_Cleaned.csv")
Data_Yelp_Cleaned_train = Data_Yelp_train[c("clean_txt", "target")]
colnames(Data_Yelp_Cleaned_train) = c("text", "target")
id = 1:nrow(Data_Yelp_Cleaned_train)
Data_Yelp_Cleaned_train = cbind(id,Data_Yelp_Cleaned_train)
Data_Yelp_Cleaned_train$target = as.factor(Data_Yelp_Cleaned_train$target)

# Creando la matriz de palabras
corpus = Corpus(VectorSource(Data_Yelp_Cleaned_train$text))
frequencies = DocumentTermMatrix(corpus)

Matrix_train = as.data.frame(as.matrix(frequencies))
colnames(Matrix_train)= make.names(colnames(frequencies))
Matrix_train$target = as.factor(Data_Yelp_Cleaned_train$target)


#cargando la data Test
Data_Yelp_test = read.csv("https://raw.githubusercontent.com/capajaro/Quiz--4/main/Test_Cleaned.csv")
Data_Yelp_Cleaned_test = cbind(Data_Yelp_test["clean_txt"],Data_Yelp_test["target"])
colnames(Data_Yelp_Cleaned_test) = c("text", "target")
id = 1:nrow(Data_Yelp_Cleaned_test)
Data_Yelp_Cleaned_test = as.data.frame(cbind(id,Data_Yelp_Cleaned_test))

# Creando la matriz de palabras
corpus = Corpus(VectorSource(Data_Yelp_Cleaned_test$text))
frequencies = DocumentTermMatrix(corpus)

Matrix_test = as.data.frame(as.matrix(frequencies))
colnames(Matrix_test)= make.names(colnames(frequencies))
Matrix_test$target = as.factor(Data_Yelp_Cleaned_test$target)

##################################################################

Matrix_train$next. = NULL
Matrix_train$else. = NULL

#convert training data to matrix format
x <- model.matrix(target~.,Matrix_train)
#convert class to numerical variable
y <- Matrix_train$target
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x,y,alpha=1,family='binomial',type.measure = 'mse' )
#plot result
plot(cv.out)

#get test data
x_test <- model.matrix(target~.,Matrix_test)

#best value of lambda
lambda_1se <- cv.out$lambda.1se

#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type='response')

lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type='response')

#translate probabilities to predictions
lasso_predict <- rep("neg",nrow(testset))
lasso_predict[lasso_prob>.5] <- "pos"
#confusion matrix
table(pred=lasso_predict,true=testset$diabetes)


predict_Lasso=predict(modelo_Lasso,newdata=Matrix_test)

TP_Sp = as.data.frame(table(Matrix_test$target, predict_Lasso))$Freq[4]
TF_Sp = as.data.frame(table(Matrix_test$target, predict_Lasso))$Freq[1]
paste0("Accuracy Lasso = " ,": ",signif(100*(TP_Sp+TF_Sp)/nrow(Matrix_test), digits = 4), " %")

