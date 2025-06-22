bankloan <- read.csv("D:/1. KULIAH PER SEMESTER/Kuliah Semester 
6/Data Mining/SETELAH UTS/3. bankloan.csv", header=TRUE) 
head(bankloan) 
#set.seed(20) 
acak <- sample(1:nrow(bankloan), 450, replace=FALSE) 
bankloan.training <- bankloan[acak,]  
bankloan.testing <- bankloan[-acak,] 
library(rpart) 
model.pohon <- rpart(as.factor(default) ~ age + ed + employ + 
                       address 
                     + income + debtinc + creddebt + othdebt, 
                     data=bankloan.training) 
prob.prediksi <- predict(model.pohon, bankloan.testing) 
prediksi <- ifelse(prob.prediksi[,2] > 0.5, 1, 0) 
tabel <- table(bankloan.testing$default, prediksi)  
akurasi <- (tabel[1,1] + tabel[2,2])/sum(tabel)  
akurasi 

library(randomForest)  
set.seed(100) 
model.forest <- randomForest(as.factor(default) ~ age + ed + employ 
                             + address 
                             + income + debtinc + creddebt + 
                               othdebt,  
                             data=bankloan.training, 
                             importance=TRUE, ntree=2000, mtry=3) 
prediksi.rf <- predict(model.forest, bankloan.testing) 
tabel.rf <- table(bankloan.testing$default, prediksi.rf)  
akurasi.rf <- (tabel.rf[1,1] + tabel.rf[2,2])/sum(tabel.rf) 
akurasi.rf 
#importance(model.forest)  
varImpPlot(model.forest)  
#getTree(model.forest, labelVar=TRUE, k=2) 
for(i in 1:100){ 
  acak <- sample(1:nrow(bankloan), 450, replace=FALSE)  
  bankloan.training <- bankloan[acak,] 
  bankloan.testing <- bankloan[-acak,] 
  model.pohon<- rpart(as.factor(default) ~ age + ed + employ + 
                        address 
                      + income + debtinc + creddebt + othdebt,  
                      data=bankloan.training) 
  prob.prediksi <- predict(model.pohon, bankloan.testing) 
  prediksi <- ifelse(prob.prediksi[,2] > 0.5, 1, 0) 
  tabel <- table(bankloan.testing$default, prediksi) 
  akurasi[i] <- (tabel[1,1] + tabel[2,2])/sum(tabel) 
  model.forest <- randomForest(as.factor(default) ~ age + ed + 
                                 employ + address 
                               + income+debtinc + creddebt + 
                                 othdebt, 
                               data=bankloan.training, 
                               importance=TRUE, ntree=2000, mtry=3)  
  prediksi.rf <- predict(model.forest, bankloan.testing) 
  tabel.rf <- table(bankloan.testing$default, prediksi.rf) 
  akurasi.rf[i] <- (tabel.rf[1,1] + tabel.rf[2,2])/sum(tabel.rf) 
} 
boxplot(cbind(akurasi, akurasi.rf))  
plot(akurasi, akurasi.rf)  
points(akurasi, akurasi, type="l")



# Syntax yang digunakan dalam melakukan penanganan missing-values pada data 
bankloan <- read.csv("D:/1. KULIAH PER SEMESTER/Kuliah Semester 
6/Data Mining/SETELAH UTS/3. bankloan.csv", header=TRUE) 
head(bankloan) 
#PRE PROCESSING (ADRESS = MEAN & EMPLOY = MEAN) 
# Menghitung mean untuk kolom employ dan address 
mean_employ <- mean(bankloan$employ[bankloan$employ != 0], na.rm = 
                      TRUE) 
mean_address <- mean(bankloan$address[bankloan$address != 0], na.rm 
                     = TRUE) 

# Mengganti nilai 0 dengan mean 
bankloan$employ[bankloan$employ == 0] <- mean_employ 
bankloan$address[bankloan$address == 0] <- mean_address 


# Menampilkan hasil untuk memverifikasi perubahan 
head(bankloan)