#***********Decision tree using C 5.0 algorithm*****************

credit<-read.csv(file.choose(),header = T)
View(credit)
str(credit)
table(credit$checking_status)
table(credit$savings_status)
table(credit$class)
set.seed(123)
train_sample<-sample(1000,900)
View(train_sample)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$class))
prop.table(table(credit_test$class))
install.packages("C50")
library(C50)
?C5.0Control #how to finally tune the algorithm
credit_model <- C5.0(credit_train[-17], credit_train$class, trials= 10)
credit_model
summary(credit_model)
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$class, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
