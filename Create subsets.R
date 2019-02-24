
# Create subset that contains data of opening hotels that are located in North America region
# Use this file to generate subset for Feb, May, August, and Nov. 
# Use new subset to do further analysis (choose variabels, calcualte p-values, etc.)
# 3 files for each month: integrated, leisure, and business.
# Upload all 3 files to google drive and save 

#load data201402######
file201402.ori <- read.csv("\\\\hd.ad.syr.edu\\01\\877dcb\\Documents\\Desktop\\IST 687\\Project\\complete_data\\out-201402.csv")

#subset only north america
na.open201402 <-  file201402.ori[file201402.ori$Sub.Continent_PL == "North America",]

#subset - opening hotels
na.open201402 <-  na.open201402[na.open201402$Status_PL== "Open",]

#subset to remove rows that have emplty NPS_Type cells
na.open201402 <- na.open201402[!(is.na(na.open201402$NPS_Type) | na.open201402$NPS_Type==""), ]

#export data201402
write.csv(na.open201402, file = "na.open201402.csv")


#seperate leisure and business data 
dat.leisure.1402<- na.open201402[na.open201402$POV_CODE_C == "LEISURE",]
dat.business.1402 <- na.open201402[na.open201402$POV_CODE_C == "BUSINESS",]

##export data

write.csv(dat.leisure.1402, file = "dat.leisure.1402.csv")
write.csv(dat.business.1402, file = "dat.business.1402.csv")



#load data201405######
file201405.ori <- read.csv("\\\\hd.ad.syr.edu\\01\\877dcb\\Documents\\Desktop\\IST 687\\Project\\complete_data\\out-201405.csv")

#subset only north america
na.open201405 <-  file201405.ori[file201405.ori$Sub.Continent_PL == "North America",]

#subset - opening hotels
na.open201405 <-  na.open201405[na.open201405$Status_PL== "Open",]

#subset to remove rows that have emplty NPS_Type cells
na.open201405 <- na.open201405[!(is.na(na.open201405$NPS_Type) | na.open201405$NPS_Type==""), ]

#export data201405
write.csv(na.open201405, file = "na.open201405.csv")


#seperate leisure and business data 
dat.leisure.1405<- na.open201405[na.open201405$POV_CODE_C == "LEISURE",]
dat.business.1405 <- na.open201405[na.open201405$POV_CODE_C == "BUSINESS",]

##export data

write.csv(dat.leisure.1405, file = "dat.leisure.1405.csv")
write.csv(dat.business.1405, file = "dat.business.1405.csv")


#load data201408######
file201408.ori <- read.csv("\\\\hd.ad.syr.edu\\01\\877dcb\\Documents\\Desktop\\IST 687\\Project\\complete_data\\out-201408.csv")

#subset only north america
na.open201408 <-  file201408.ori[file201408.ori$Sub.Continent_PL == "North America",]

#subset - opening hotels
na.open201408 <-  na.open201408[na.open201408$Status_PL== "Open",]

#subset to remove rows that have emplty NPS_Type cells
na.open201408 <- na.open201408[!(is.na(na.open201408$NPS_Type) | na.open201408$NPS_Type==""), ]

#export data201408
write.csv(na.open201408, file = "na.open201408.csv")


#seperate leisure and business data 
dat.leisure.1408 <- na.open201408[na.open201408$POV_CODE_C == "LEISURE",]
dat.business.1408 <- na.open201408[na.open201408$POV_CODE_C == "BUSINESS",]

##export data

write.csv(dat.leisure.1408, file = "dat.leisure.1408.csv")
write.csv(dat.business.1408, file = "dat.business.1408.csv")



#load data201411######
file201411.ori <- read.csv("\\\\hd.ad.syr.edu\\01\\877dcb\\Documents\\Desktop\\IST 687\\Project\\complete_data\\out-201411.csv")

#subset only north america
na.open201411 <-  file201411.ori[file201411.ori$Sub.Continent_PL == "North America",]

#subset - opening hotels
na.open201411 <-  na.open2014011[na.open201411$Status_PL== "Open",]

#subset to remove rows that have emplty NPS_Type cells
na.open201411 <- na.open201411[!(is.na(na.open201411$NPS_Type) | na.open201411$NPS_Type==""), ]

#export data201411
write.csv(na.open201411, file = "na.open201411.csv")


#seperate leisure and business data 
dat.leisure.1411 <- na.open201411[na.open201411$POV_CODE_C == "LEISURE",]
dat.business.1411 <- na.open201411[na.open201411$POV_CODE_C == "BUSINESS",]

##export data

write.csv(dat.leisure.1411, file = "dat.leisure.1411.csv")
write.csv(dat.business.1411, file = "dat.business.1411.csv")
