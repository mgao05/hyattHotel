#mengran 
#group project

library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(lattice)

library(e1071)
library(kernlab)
library(gridExtra)
library(caret)


library(arules)
library(arulesViz)
library(mclust)
library(Matrix)


dat02 <- read.csv( "C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\na.open201402.csv")


barchart(dat02$POV_CODE_C, col = "orange")

hotel.name <- unique(dat02$Hotel.Name.Long_PL)
hotel.lat <- unique(dat02$Property.Latitude_PL)
hotel.lon <- unique(dat02$Property.Longitude_PL)
geo.dat <- data.frame(hotel.name, hotel.lat, hotel.lon)





#barplot for different subregion in different years
dat02.ori <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\complete_data\\out-201402.csv")
dat02.ori <- dat02.ori[!(is.na(dat02.ori$Likelihood_Recommend_H) | dat02.ori$Likelihood_Recommend_H==""), ]
dat02.ori <- dat02.ori[!(is.na(dat02.ori$Sub.Continent_PL) | dat02.ori$Sub.Continent_PL==""), ]
dat02.ori <- dat02.ori[!(is.na(dat02.ori$Brand_PL) | dat02.ori$Brand_PL==""), ]
units.by.reg <- aggregate(dat02.ori$Likelihood_Recommend_H, 
                          by = list(region = dat02.ori$Sub.Continent_PL), 
                          FUN= mean)
##plot average likelihood to recommend for reach sub continent
barchart(units.by.reg$x~units.by.reg$region, data = units.by.reg, col= rainbow(16),ylab= "Likelihood to Reccomend (Scale from 1 to 10) by sub-continent in February 2014")
#it vary much between each subcontinent, we need to consider different culture and background of the region 
#so it is more logical to focus on one subregion so we can see the pattern and learn from it.
#while this learning process can be transferred to other regions as well.




dat05.ori <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\complete_data\\out-201405.csv")
dat05.ori <- dat02.ori[!(is.na(dat05.ori$Likelihood_Recommend_H) | dat05.ori$Likelihood_Recommend_H==""), ]
dat05.ori <- dat02.ori[!(is.na(dat05.ori$Sub.Continent_PL) | dat05.ori$Sub.Continent_PL==""), ]
dat05.ori <- dat02.ori[!(is.na(dat05.ori$Brand_PL) | dat05.ori$Brand_PL==""), ]
units.by.reg05 <- aggregate(dat05.ori$Likelihood_Recommend_H, 
                          by = list(region = dat05.ori$Sub.Continent_PL), 
                          FUN= mean)
##plot average likelihood to recommend for reach sub continent
barchart(units.by.reg05$x~units.by.reg05$region, data = units.by.reg05, col= rainbow(16), ylab= "Likelihood to Reccomend (Scale from 1 to 10) by sub-continent in May 2014")




dat08.ori <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\complete_data\\out-201408.csv")
dat08.ori <- dat08.ori[!(is.na(dat08.ori$Likelihood_Recommend_H) | dat08.ori$Likelihood_Recommend_H==""), ]
dat08.ori <- dat08.ori[!(is.na(dat08.ori$Sub.Continent_PL) | dat08.ori$Sub.Continent_PL==""), ]
dat08.ori <- dat08.ori[!(is.na(dat08.ori$Brand_PL) | dat08.ori$Brand_PL==""), ]
units.by.reg08 <- aggregate(dat08.ori$Likelihood_Recommend_H, 
                            by = list(region = dat08.ori$Sub.Continent_PL), 
                            FUN= mean)
##plot average likelihood to recommend for reach sub continent
barchart(units.by.reg08$x~units.by.reg08$region, data = units.by.reg08, col= rainbow(16), ylab= "Likelihood to Reccomend (Scale from 1 to 10) by sub-continent in August 2014")


##november
dat11 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\complete_data\\out-201411.csv")
dat11.ori <- dat11
dat11.ori <- dat11.ori[!(is.na(dat11.ori$Likelihood_Recommend_H) | dat11.ori$Likelihood_Recommend_H==""), ]
dat11.ori <- dat11.ori[!(is.na(dat11.ori$Sub.Continent_PL) | dat11.ori$Sub.Continent_PL==""), ]
dat11.ori <- dat11.ori[!(is.na(dat11.ori$Brand_PL) | dat11.ori$Brand_PL==""), ]
units.by.reg11 <- aggregate(dat11.ori$Likelihood_Recommend_H, 
                            by = list(region = dat11.ori$Sub.Continent_PL), 
                            FUN= mean)
##plot average likelihood to recommend for reach sub continent
barchart(units.by.reg11$x~units.by.reg11$region, data = units.by.reg11, col= rainbow(16), ylab= "Likelihood to Reccomend (Scale from 1 to 10) by sub-continent in November 2014")


#check north america by brand 
dat11.north <- dat11.ori[dat11.ori$Sub.Continent_PL == "North America",]
scale.by.brand11 <- aggregate(dat11.north$Likelihood_Recommend_H, 
                            by = list(brand = dat11.north$Brand_PL), 
                            FUN= mean)
##plot average likelihood to recommend for reach sub continent
barchart(scale.by.brand11$x~scale.by.brand11$brand, data = scale.by.brand11, col= rainbow(8), ylab= "Likelihood to Reccomend (Scale from 1 to 10) by Brands in North America")



dat11.ori <- dat11.ori[!(is.na(dat11.ori$Overall_Sat_H) | dat11.ori$Overall_Sat_H ==""), ]
boxplot((dat11.ori$Overall_Sat_H) ~ dat11.ori$Brand_PL, main = "Overall Satisfaction rate for Different Brands of November 2014 for All Regions", col = rainbow(9))

boxplot((dat11.north$Overall_Sat_H) ~ dat11.north$Brand_PL, main = "Overall Satisfaction rate for Different Brands of November 2014 for North America", col = rainbow(9))



#association rule for business in us
#choose.files()
business.02 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\business_and_leisure\\Business\\dat.business.1402.csv")
business.02.us <- business.02[business.02$Country_PL == "United States", ]
#subset several variables of service that are relevant to NPS score
busi.service <- business.02.us[,c(201:224,228,233)]
business02 <- as(busi.service, "transactions")
summary(itemFrequency(business02))
itemFrequencyPlot(business02, support=0.05, cex.names=1.1)

rules1 <- apriori(business02, parameter = list(supp = 0.35, conf = 0.7),
                  appearance = list(rhs=c("NPS_Type=Promoter"), default="lhs"),
                  control = list(verbose=F))
#plot(rules1, method = "paracoord") too many rules, not feasible for processing

#top10 support
top.support <- sort(rules1, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10)) 
plot(head(top.support, 10))
plot(head(top.support, 10), method = "paracoord")


#top10 confidence
top.confidence <- sort(rules1, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))
plot(head(top.confidence, 10))
plot(head(top.confidence, 10), method = "paracoord")



##########201405 business rules
business.05 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\business_and_leisure\\Business\\dat.business.1405.csv")
business.05.us <- business.05[business.05$Country_PL == "United States", ]
#subset several variables of service that are relevant to NPS score
busi.service <- business.05.us[,c(201:224,228,233)]
business05 <- as(busi.service, "transactions")
summary(itemFrequency(business05))
itemFrequencyPlot(business05, support=0.05, cex.names=1.1)

rules1 <- apriori(business05, parameter = list(supp = 0.35, conf = 0.7),
                  appearance = list(rhs=c("NPS_Type=Promoter"), default="lhs"),
                  control = list(verbose=F))
#plot(rules1, method = "paracoord") too many rules, not feasible for processing

#top10 support
top.support <- sort(rules1, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10)) 
plot(head(top.support, 10))
plot(head(top.support, 10), method = "paracoord")


#top10 confidence
top.confidence <- sort(rules1, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))
plot(head(top.confidence, 10))
plot(head(top.confidence, 10), method = "paracoord")


##########201408 business rules
business.08 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\business_and_leisure\\Business\\dat.business.1408.csv")
business.08.us <- business.08[business.08$Country_PL == "United States", ]
#subset several variables of service that are relevant to NPS score
busi.service <- business.08.us[,c(201:224,228,233)]
business08 <- as(busi.service, "transactions")
summary(itemFrequency(business08))
#itemFrequencyPlot(business08, support=0.05, cex.names=1.1)

rules1 <- apriori(business08, parameter = list(supp = 0.35, conf = 0.7),
                  appearance = list(rhs=c("NPS_Type=Promoter"), default="lhs"),
                  control = list(verbose=F))
#plot(rules1, method = "paracoord") too many rules, not feasible for processing

#top10 support
top.support <- sort(rules1, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10)) 
plot(head(top.support, 10))
plot(head(top.support, 10), method = "paracoord")


#top10 confidence
top.confidence <- sort(rules1, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))
plot(head(top.confidence, 10))
plot(head(top.confidence, 10), method = "paracoord")



##########201411 business rules
business.11 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\business_and_leisure\\Business\\dat.business.1411.csv")
business.11.us <- business.11[business.11$Country_PL == "United States", ]
#subset several variables of service that are relevant to NPS score
busi.service <- business.11.us[,c(201:224,228,233)]
business11 <- as(busi.service, "transactions")
summary(itemFrequency(business11))
#itemFrequencyPlot(business11, support=0.05, cex.names=1.1)

rules1 <- apriori(business11, parameter = list(supp = 0.35, conf = 0.7),
                  appearance = list(rhs=c("NPS_Type=Promoter"), default="lhs"),
                  control = list(verbose=F))


#plot(rules1, method = "paracoord") too many rules, not feasible for processing
#plot all rules

#top10 support
top.support <- sort(rules1, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10)) 
plot(head(top.support, 10))
plot(head(top.support, 10), method = "paracoord")


#top10 confidence
top.confidence <- sort(rules1, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence,10))
plot(head(top.confidence, 10))
plot(head(top.confidence, 10), method = "paracoord")








#####model for lm between Likelihood_Recommend_H and ratings:
# Overall_Sat_H
# Guest_Room_H
# Tranquility_H
# Condition_Hotel_H
# Customer_SVC_H
# Staff_Cared_H
# Internet_Sat_H
# Check_In_H
# F&B_Overall_Experience_H

#subset several variables of service that are relevant to NPS score
busi.11.likelihood <- business.11.us[,c(143,144,146,138)]

#get rid of NAs
#change NAs into mean of the column
busi.11.likelihood$Likelihood_Recommend_H[is.na(busi.11.likelihood$Likelihood_Recommend_H)] <- mean(na.omit(busi.11.likelihood$Likelihood_Recommend_H))
busi.11.likelihood$Customer_SVC_H[is.na(busi.11.likelihood$Customer_SVC_H)] <- mean(na.omit(busi.11.likelihood$Customer_SVC_H))
busi.11.likelihood$Staff_Cared_H[is.na(busi.11.likelihood$Staff_Cared_H)] <- mean(na.omit(busi.11.likelihood$Staff_Cared_H))
busi.11.likelihood$Check_In_H[is.na(busi.11.likelihood$Check_In_H)] <- mean(na.omit(busi.11.likelihood$Check_In_H))

nrows <- nrow(busi.11.likelihood)
# Using techniques discussed in class, create two datasets - one for training and one for testing.
randIndex <- sample(1:dim(busi.11.likelihood)[1])
#or can sample this way
randIndex <- sample(1:nrows)
head(randIndex)
summary(randIndex)
length(randIndex)
cutPoint2_3 <- floor(2*dim(busi.11.likelihood)[1]/3)
cutPoint2_3
nrows
trainData<- busi.11.likelihood[randIndex[1:cutPoint2_3],]
testData <- busi.11.likelihood[randIndex[(cutPoint2_3+1):dim(busi.11.likelihood)[1]],]

# Build a Model using KSVM & visualize the results
# 1)Build a model (using the 'ksvm' function, trying to predict likelihood to recommend). You can use all the possible attributes, 
# #or select the attributes that you think would be the most 
# model.ksvm <- ksvm(Likelihood_Recommend_H ~ ., data=trainData,kernel="rbfdot",
#                    kpar="automatic",C=5,cross=3,prob.model=FALSE)
# 
# model.ksvm
# #model outcome
# # SV type: eps-svr  (regression) 
# # parameter : epsilon = 0.1  cost C = 5 
# # 
# # Gaussian Radial Basis kernel function. 
# # Hyperparameter : sigma =  0.27376831779705 
# # 
# # Number of Support Vectors : 4394 
# # 
# # Objective Function Value : -4552.454 
# # Training error : 0.068883 
# # Cross validation error : 0.744913 
# 
# pred.ksvm <- predict(model.ksvm, testData)
# results <- table(pred.ksvm, testData$Ozone)
# RMSE.ksvm <- sqrt(mean((testData$Ozone-pred.ksvm)^2))
# RMSE.ksvm
# 
# # 3)Plot the results. Use a scatter plot.
# dat.ksvm <- testData
# #define a new column called error to store the absolute value of ozone level minus the predicted ozone level
# dat.ksvm$error.ksvm <- abs(as.numeric(dat.ksvm$Likelihood_Recommend_H-pred.ksvm))
# #make a plot using ggplot
# p.ksvm <- ggplot(dat.ksvm, aes(x = dat.ksvm$Overall_Sat_H, y = dat.ksvm$Guest_Room_H)) + 
#   geom_point(aes(color = dat.ksvm$error.ksvm, size= dat.ksvm$error.ksvm))+
#   ggtitle("Scatter Plot")+labs(x="Over all rating",y="Guest room rating") 
# p.ksvm



#lm model########################################################
model.lm <- lm(Likelihood_Recommend_H ~ ., trainData)
# make a prediction for testdata
pred.lm <- predict(model.lm, testData)
RMSE.lm <- sqrt(mean((testData$Likelihood_Recommend_H-pred.lm)^2))
RMSE.lm

#creat a new dataset that includes the predicted value as a column
dat.lm <- testData
#define a new column called error to store the absolute value of ozone level minus the predicted ozone level
dat.lm$error.lm <- abs(as.numeric(dat.lm$Likelihood_Recommend_H-pred.lm))
#make a plot using ggplot
p.lm <- ggplot(dat.lm, aes(x = dat.lm$Check_In_H, y = dat.lm$Customer_SVC_H)) + 
  geom_point(aes(color = dat.lm$error.lm, size= dat.lm$error.lm))+
  ggtitle("Scatter Plot")+labs(x="Chech-in Service",y="Customer service") 
p.lm




business.11.us <- business.11.us[ business.11.us$State_PL == "New York",]

busi.11.likelihood <- business.11.us[,c(143,144,146,138)]


#get rid of NAs
#change NAs into mean of the column
busi.11.likelihood$Likelihood_Recommend_H[is.na(busi.11.likelihood$Likelihood_Recommend_H)] <- mean(na.omit(busi.11.likelihood$Likelihood_Recommend_H))
busi.11.likelihood$Customer_SVC_H[is.na(busi.11.likelihood$Customer_SVC_H)] <- mean(na.omit(busi.11.likelihood$Customer_SVC_H))
busi.11.likelihood$Staff_Cared_H[is.na(busi.11.likelihood$Staff_Cared_H)] <- mean(na.omit(busi.11.likelihood$Staff_Cared_H))
busi.11.likelihood$Check_In_H[is.na(busi.11.likelihood$Check_In_H)] <- mean(na.omit(busi.11.likelihood$Check_In_H))

nrows <- nrow(busi.11.likelihood)
# Using techniques discussed in class, create two datasets - one for training and one for testing.
randIndex <- sample(1:dim(busi.11.likelihood)[1])
#or can sample this way
randIndex <- sample(1:nrows)
head(randIndex)
summary(randIndex)
length(randIndex)
cutPoint2_3 <- floor(2*dim(busi.11.likelihood)[1]/3)
cutPoint2_3
nrows
trainData<- busi.11.likelihood[randIndex[1:cutPoint2_3],]
testData <- busi.11.likelihood[randIndex[(cutPoint2_3+1):dim(busi.11.likelihood)[1]],]

# Build a Model using KSVM & visualize the results
# 1)Build a model (using the 'ksvm' function, trying to predict likelihood to recommend). You can use all the possible attributes, 
# #or select the attributes that you think would be the most 
# model.ksvm <- ksvm(Likelihood_Recommend_H ~ ., data=trainData,kernel="rbfdot",
#                    kpar="automatic",C=5,cross=3,prob.model=FALSE)
# 
# model.ksvm
# #model outcome
# # SV type: eps-svr  (regression) 
# # parameter : epsilon = 0.1  cost C = 5 
# # 
# # Gaussian Radial Basis kernel function. 
# # Hyperparameter : sigma =  0.27376831779705 
# # 
# # Number of Support Vectors : 4394 
# # 
# # Objective Function Value : -4552.454 
# # Training error : 0.068883 
# # Cross validation error : 0.744913 
# 
# pred.ksvm <- predict(model.ksvm, testData)
# results <- table(pred.ksvm, testData$Ozone)
# RMSE.ksvm <- sqrt(mean((testData$Ozone-pred.ksvm)^2))
# RMSE.ksvm
# 
# # 3)Plot the results. Use a scatter plot.
# dat.ksvm <- testData
# #define a new column called error to store the absolute value of ozone level minus the predicted ozone level
# dat.ksvm$error.ksvm <- abs(as.numeric(dat.ksvm$Likelihood_Recommend_H-pred.ksvm))
# #make a plot using ggplot
# p.ksvm <- ggplot(dat.ksvm, aes(x = dat.ksvm$Overall_Sat_H, y = dat.ksvm$Guest_Room_H)) + 
#   geom_point(aes(color = dat.ksvm$error.ksvm, size= dat.ksvm$error.ksvm))+
#   ggtitle("Scatter Plot")+labs(x="Over all rating",y="Guest room rating") 
# p.ksvm



#lm model########################################################
model.lm <- lm(Likelihood_Recommend_H ~ ., trainData)
# make a prediction for testdata
pred.lm <- predict(model.lm, testData)
RMSE.lm <- sqrt(mean((testData$Likelihood_Recommend_H-pred.lm)^2))
RMSE.lm

#creat a new dataset that includes the predicted value as a column
dat.lm <- testData
#define a new column called error to store the absolute value of ozone level minus the predicted ozone level
dat.lm$error.lm <- abs(as.numeric(dat.lm$Likelihood_Recommend_H-pred.lm))
#make a plot using ggplot
p.lm <- ggplot(dat.lm, aes(x = dat.lm$Check_In_H, y = dat.lm$Customer_SVC_H)) + 
  geom_point(aes(color = dat.lm$error.lm, size= dat.lm$error.lm))+
  ggtitle("Scatter Plot")+labs(x="Chech-in Service",y="Customer service") 
p.lm



library(stringr)
library(wordcloud)
library(plotrix)

wordcloud(string, scale = c(4,.5), min.freq = 1, max.words =100
          , random.order = FALSE, random.color = FALSE, rot.per = 0.4, colors = rainbow(5))


string <- c("Dry_cleaning","Laundry","Pool_outdoor",
            "Restaurant", 
            "Laundry", 
            "Valet-parking", 
            "Mini_bar","Boutique_pl", 
            "Fitness_center",
            "Elevator",
            "Dry_cleaning",
            "Business_center",
            "Restaurant",
            "Bell_staff",
            "Valet_parking",
            "Pool_outdoor", 
            "Limo", 
            "Fitness_trainer",
            "Laundry",
            "restaurant",
            "Bell_staff",
            "Valet_parking",
            "Pool_outdoor",
            "Self_parking",
            "Limo_service")

