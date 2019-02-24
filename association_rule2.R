#mengran 
#group project

library(ggmap)
library(maptools)
library(maps)
library(ggplot2)
library(lattice)

dat02 <- read.csv( "C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\na.open201402.csv")





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



library(arules)
library(arulesViz)
library(mclust)
library(Matrix)
#association rule for business in us
#choose.files()
business.02 <- read.csv("C:\\Users\\mgao05\\Desktop\\ist 687 applied data science\\data\\business_and_leisure\\Business\\dat.business.1402.csv")
business.02.us <- business.02[business.02$Country_PL == "United States", ]
#subset several variables of service that are relevant to NPS score
busi.service <- business.02.us[,c(201:224,228,233)]
business02 <- as(busi.service, "transactions")
summary(itemFrequency(business02))
itemFrequencyPlot(business02, support=0.05, cex.names=1.1)

rules1 <- apriori(business02, parameter = list(supp = 0.005, conf = 0.7),
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

rules1 <- apriori(business05, parameter = list(supp = 0.005, conf = 0.7),
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
itemFrequencyPlot(business08, support=0.05, cex.names=1.1)

rules1 <- apriori(business08, parameter = list(supp = 0.005, conf = 0.7),
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
itemFrequencyPlot(business11, support=0.05, cex.names=1.1)

rules1 <- apriori(business11, parameter = list(supp = 0.005, conf = 0.7),
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

