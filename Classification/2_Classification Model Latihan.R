library(tidyverse)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(skimr)
library(e1071)
library(tree)


# 1. import data
df <- read_csv("../racademy/Classification/HR-Employee-Attrition.csv")

# 2. ringkasan data
dim(df)
head(df, 10)
skim(df)


# 3. data preprocessing
set.seed(123)
# konversi data
df0 <- df %>% mutate_if(is.character, as.factor)

df1 <- df0[, -c(27,22, 9)]

#Membangi Data Ke Training dan Testing (70:30)
index_train <- sample(1:nrow(df), 0.7 * nrow(df))
train <- df1[index_train, ]
test <- df1[-index_train, ]

# 4. model
#Membuat Model Decison Tree Untuk Mengklasifikasi Apakah Seseorang akan klaim Asuransi atau tidak. 
tree <- rpart(Attrition ~., train, method = "class")
summary(tree)

#Visualisasikan Decison Tree
fancyRpartPlot(tree)

# 5. validasi
#Menggunakan Untuk Melakukan Prediksi Pada Data Testing
prediction <- predict(tree, test, type = "class")

#Validasi Menggunakan Confussion Matrix
conf <- table(test$Attrition, prediction)
conf

TP <- conf[1, 1] 
FN <- conf[1, 2] 
FP <- conf[2, 1] 
TN <- conf[2, 2]


#Menghitung Nilai Akurasi
acc <- (TP + TN)/(TP + FN + FP + TN)
accdt <- acc
acc

#Menghitung Nilai Precision
prec <- TP / (TP + FP)
prec

#Menghitung Nilai Recall
rec <- TP / (TP + FN)
rec

# atau
confusionMatrix(prediction, test$Attrition)

