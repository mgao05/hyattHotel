View(out_201402)
myOut_201402 <- data.frame(myOut_201402)
myOut_201402 <- out_201402[, c(1:50, 230:237)]
View(myOut_201402)
summary(myOut_201402)
#chisq.test(myOut_201402$MARKET_CODE_C, myOut_201402$NPS_Type)
##remove all NAs from the data set
myOut <-  na.omit(myOut_201402)
## Will test the correlation between NPS_Type and the variables below
#1-50
# MARKET_CODE_C
# MARKET_GROUP_C
# RATE_PLAN_C
# ROOM_TYPE_CODE_C
# ROOM_TYPE_DESCRIPTION_C
# LENGTH_OF_STAY_C
# POV_CODE_C
# DIRECT_NIGHTS_NUM_R
# ROOM_TYPE_CODE_R
# ROOM_TYPE_DESCRIPTION_R
##correlation 
chisq.test(myOut$MARKET_CODE_C, myOut$NPS_Type) #p-value = 0.4325
chisq.test(myOut$MARKET_GROUP_C, myOut$NPS_Type) #p-value = 0.03644 **
chisq.test(myOut$RATE_PLAN_C, myOut$NPS_Type) #p-value = 0.903
chisq.test(myOut$ROOM_TYPE_CODE_C, myOut$NPS_Type) #p-value = 0.003403 ***
chisq.test(myOut$ROOM_TYPE_DESCRIPTION_C, myOut$NPS_Type) #p-value = 0.006716 ***
chisq.test(myOut$LENGTH_OF_STAY_C, myOut$NPS_Type) #p-value = 0.06735
chisq.test(myOut$POV_CODE_C, myOut$NPS_Type) #p-value = 0.03327 **
chisq.test(myOut$DIRECT_NIGHTS_NUM_R, myOut$NPS_Type) #p-value = 0.05924
chisq.test(myOut$ROOM_TYPE_CODE_R, myOut$NPS_Type) # p-value = 3.362e-07 ***
chisq.test(myOut$ROOM_TYPE_DESCRIPTION_R, myOut$NPS_Type) #p-value = 0.003869 ***
## 51-100
# NT_RATE_R **
# LENGTH_OF_STAY_CATEGORY_R ***
# GROUPS_VS_FIT_R ***
# GOLDPASSPORT_FLG_R ***
# e_country_I **


########Prediction Model
summary(na.open201402$NPS_Type)
perc02 <- c(length(na.open201402$NPS_Type[na.open201402$NPS_Type =="Detractor"]), length(na.open201402$NPS_Type[na.open201402$NPS_Type =="Passive"]),length(na.open201402$NPS_Type[na.open201402$NPS_Type =="Promoter"])  )
summary(na.open201405$NPS_Type)
perc05 <- c(length(na.open201405$NPS_Type[na.open201405$NPS_Type =="Detractor"]), length(na.open201402$NPS_Type[na.open201405$NPS_Type =="Passive"]),length(na.open201405$NPS_Type[na.open201405$NPS_Type =="Promoter"])  )
summary(na.open201408$NPS_Type)
perc08 <- c(length(na.open201408$NPS_Type[na.open201408$NPS_Type =="Detractor"]), length(na.open201408$NPS_Type[na.open201408$NPS_Type =="Passive"]),length(na.open201408$NPS_Type[na.open201408$NPS_Type =="Promoter"])  )
summary(na.open201411$NPS_Type)
perc11 <- c(length(na.open201411$NPS_Type[na.open201411$NPS_Type =="Detractor"]), length(na.open201411$NPS_Type[na.open201411$NPS_Type =="Passive"]),length(na.open201411$NPS_Type[na.open201411$NPS_Type =="Promoter"])  )
labels = c("Detractor", "Passive", "Promoter") 
#get the distribution of NPS_Type(s) in different months
P02 <- pie(perc02, labels)
P05 <- pie(perc05, labels)
P08 <- pie(perc08, labels)
P11 <- pie(perc11, labels)

########build models
library(kernlab)
randIndex <- sample(1:dim(na.open201402_l_us)[1])
cutPoint2_3 <- floor(2 * dim(na.open201402_l_us)[1]/3)
###read 201402 for business v.s leisure
na.open201402_b_us <- na.open201402_b[na.open201402_b$Country_PL == "United States",]
###2/3 in Feb for training data, and the rest 1/3 in Feb for test data 
##business
trainData_201402_b_us <-na.open201402_b_us[randIndex[1:cutPoint2_3],]
testData_201402_b_us <- na.open201402_b_us[randIndex[(cutPoint2_3+1):dim(na.open201402_b_us)[1]],]
#hotel + customer variables
svmOutput_201402_b_us <- ksvm(NPS_Type ~ MARKET_GROUP_C + ROOM_TYPE_DESCRIPTION_C + ROOM_TYPE_DESCRIPTION_R + LENGTH_OF_STAY_CATEGORY_R + GROUPS_VS_FIT_R + GOLDPASSPORT_FLG_R, data = trainData_201402_b_us, kernel = "rbfdot", kpar = "automatic",C = 5,cross = 3, prob.model = TRUE)
pred_201402_b_us <- predict(svmOutput_201402_b_us, testData_201402_b_us)
testData_201402_b_us_na <- na.omit(testData_201402_b_us$NPS_Type)
results_201402_b_us <- table(testData_201402_b_us_na, pred_201402_b_us)
accuracy_201402_b_us <- (results_201402_b_us[1,1] + results_201402_b_us[2,2] + results_201402_b_us[3,3])/length(pred_201402_b_us)
accuracy_201402_b_us #0.685

##leisure
trainData_201402_l_us <-na.open201402_l_us[randIndex[1:cutPoint2_3],]
testData_201402_l_us <- na.open201402_l_us[randIndex[(cutPoint2_3+1):dim(na.open201402_l_us)[1]],]
#hotel + customer variables
svmOutput_201402_l_us <- ksvm(NPS_Type ~ MARKET_GROUP_C + ROOM_TYPE_DESCRIPTION_C + ROOM_TYPE_DESCRIPTION_R + LENGTH_OF_STAY_CATEGORY_R + GROUPS_VS_FIT_R + GOLDPASSPORT_FLG_R, data = trainData_201402_l_us, kernel = "rbfdot", kpar = "automatic",C = 5,cross = 3, prob.model = TRUE)
pred_201402_l_us <- predict(svmOutput_201402_l_us, testData_201402_l_us)
results_201402_l_us <- table(testData_201402_l_us$NPS_Type, pred_201402_l_us)
accuracy_201402_l_us <- (results_201402_l_us[1,1] + results_201402_l_us[2,2] + results_201402_l_us[3,3])/length(pred_201402_l_us)
accuracy_201402_l_us #0.675

###business & leisure in different seasons/months

##read data in 4 different months: Feb, May, Aug, and Nov
##2/10 in Feb for training data, and 1/10 in Feb, May, Aug, and Nov for test data
#02
TrainData_201402_us <- na.open201402[na.open201402$Country_PL == "United States",]
TrainData_201402_us_na <- TrainData_201402_us[TrainData_201402_us$NPS_Type != "",]
#05
testData_201405_us <- na.open201405[na.open201405$Country_PL == "United States",]
testData_201405_us_na <- testData_201405_us[testData_201405_us$NPS_Type != "",]
randIndex <- sample(1:dim(testData_201405_us_na)[1])
cutPoint1_10 <- floor(dim(testData_201405_us_na)[1]/10)
testData_201405_us_na <- testData_201405_us_na[randIndex[1:cutPoint1_10],]
#08
testData_201408_us <- na.open201408[na.open201408$Country_PL == "United States",]
testData_201408_us_na <- testData_201408_us[testData_201408_us$NPS_Type != "",]
randIndex <- sample(1:dim(testData_201408_us_na)[1])
cutPoint1_10 <- floor(dim(testData_201408_us_na)[1]/10)
testData_201408_us_na <- testData_201408_us_na[randIndex[1:cutPoint1_10],]
#11
testData_201411_us <- na.open201411[na.open201411$Country_PL == "United States",]
testData_201411_us_na <- testData_201411_us[testData_201411_us$NPS_Type != "",]
randIndex <- sample(1:dim(testData_201411_us_na)[1])
cutPoint1_10 <- floor(dim(testData_201411_us_na)[1]/10)
testData_201411_us_na <- testData_201411_us_na[randIndex[1:cutPoint1_10],]

#ksvm models
library(kernlab)
randIndex <- sample(1:dim(TrainData_201402_us_na)[1])
cutPoint2_10 <- floor(2*dim(TrainData_201402_us_na)[1]/10)
cutPoint3_10 <- floor(3*dim(TrainData_201402_us_na)[1]/10)
trainData_201402_us_na <-TrainData_201402_us_na[randIndex[1:cutPoint2_10],]
testData_201402_us_na <- TrainData_201402_us_na[randIndex[(cutPoint2_10+1):cutPoint3_10],]
svmOutput_201402_us <- ksvm(NPS_Type ~ MARKET_GROUP_C + ROOM_TYPE_DESCRIPTION_C + ROOM_TYPE_DESCRIPTION_R + POV_CODE_C + LENGTH_OF_STAY_CATEGORY_R + GROUPS_VS_FIT_R + GOLDPASSPORT_FLG_R, data = trainData_201402_us_na, kernel = "rbfdot", kpar = "automatic",C = 5,cross = 3, prob.model = TRUE)
#02
pred_201402_us <- predict(svmOutput_201402_us, testData_201402_us_na)
results_201402_us <- table(testData_201402_us_na$NPS_Type, pred_201402_us)
accuracy_201402_us <- (results_201402_us[1,1] + results_201402_us[2,2] + results_201402_us[3,3])/length(pred_201402_us)
accuracy_201402_us #0.678
#05
pred_201405_us <- predict(svmOutput_201402_us, testData_201405_us_na)
results_201405_us <- table(testData_201405_us_na, pred_201405_us)
accuracy_201405_us <- (results_201405_us[1,1] + results_201405_us[2,2] + results_201405_us[3,3])/length(pred_201405_us)
accuracy_201405_us #0.634
#08
pred_201408_us <- predict(svmOutput_201402_us, testData_201408_us_na)
results_201408_us <- table(testData_201408_us, pred_201408_us)
accuracy_201408_us <- (results_201408_us[1,1] + results_201408_us[2,2] + results_201408_us[3,3])/length(pred_201408_us)
accuracy_201408_us #0.656
#11
pred_201411_us <- predict(svmOutput_201402_us, testData_201411_us_na)
results_201411_us <- table(testData_201411_us, pred_201411_us)
accuracy_201411_us <- (results_201411_us[1,1] + results_201411_us[2,2] + results_201411_us[3,3])/length(pred_201411_us)
accuracy_201411_us #0.644

##NB models
library(e1071)
nb.model_201402_us <- naiveBayes(NPS_Type ~ MARKET_GROUP_C + ROOM_TYPE_DESCRIPTION_C + ROOM_TYPE_DESCRIPTION_R + POV_CODE_C + LENGTH_OF_STAY_CATEGORY_R + GROUPS_VS_FIT_R + GOLDPASSPORT_FLG_R, data = trainData_201402_us_na)
#02
nb.pred_201402_us <-predict(nb.model_201402_us, testData_201402_us_na)
results_201402_us_nb <- table(testData_201402_us_na$NPS_Type, nb.pred_201402_us)
accuracy_201402_us <- (results_201402_us_nb[1,1] + results_201402_us_nb[2,2] + results_201402_us_nb[3,3])/length(pred_201402_us)
accuracy_201402_us #0.633
#05
nb.pred_201405_us <-predict(nb.model_201402_us, testData_201405_us_na)
results_201405_us_nb <- table(testData_201405_us_na$NPS_Type, nb.pred_201402_us)
accuracy_201405_us <- (results_201405_us_nb[1,1] + results_201405_us_nb[2,2] + results_201405_us_nb[3,3])/length(pred_201405_us)
accuracy_201405_us #0.623
#08
nb.pred_201408_us <-predict(nb.model_201402_us, testData_201408_us_na)
results_201408_us_nb <- table(testData_201408_us_na$NPS_Type, nb.pred_201408_us)
accuracy_201408_us <- (results_201408_us_nb[1,1] + results_201408_us_nb[2,2] + results_201408_us_nb[3,3])/length(pred_201408_us)
accuracy_201408_us #0.634
#11
nb.pred_201411_us <-predict(nb.model_201402_us, testData_201411_us_na)
results_201411_us_nb <- table(testData_201411_us_na$NPS_Type, nb.pred_201411_us)
accuracy_201411_us <- (results_201411_us_nb[1,1] + results_201411_us_nb[2,2] + results_201411_us_nb[3,3])/length(pred_201411_us)
accuracy_201411_us #0.625
