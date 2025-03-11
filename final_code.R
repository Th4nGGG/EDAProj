#Loading libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(ggpubr)
install.packages("DMwR2")
library(DMwR2)
install.packages("car")
library(car)
library(modelr)
#Load and review the general attributes of the training file
#Import file
house <- read.csv(file.choose())
test <- read.csv(file.choose())
#Overview of the dataset
View(house)
dim(house)
names(house)
head(house)
unique(house)

#checking variable type of the dataset
for (i in 1:length(house)){
  print(typeof(house[,i]))
}

#Summary key statistics of each variable
summary(house)

#Part 2
#Data preprocessing
#Check how many NAs in each column
colSums(is.na(house)) 
#Columns with NAs 
names(which(colSums(is.na(house))>0))
house_missing <- house %>% select(names(which(colSums(is.na(house))>0)))

#Impute missing values 
#Categorical 

#Replace with "Not applicable"
house$Alley[is.na(house$Alley)] = "Not applicable"
house$MasVnrType[is.na(house$MasVnrType)] = "None"
house$FireplaceQu[is.na(house$FireplaceQu)] = "Not applicable"
house$PoolQC[is.na(house$PoolQC)] = "Not applicable"
house$Fence[is.na(house$Fence)] = "Not applicable"
house$MiscFeature[is.na(house$MiscFeature )] = "Not applicable"

#Basement
house$BsmtQual[is.na(house$BsmtQual)] = "Not applicable"
house$BsmtCond[is.na(house$BsmtCond)] = "Not applicable"
house$BsmtExposure[is.na(house$BsmtExposure)] = "Not applicable"
house$BsmtFinType1[is.na(house$BsmtFinType1)] = "Not applicable"
house$BsmtFinType2[is.na(house$BsmtFinType2)] = "Not applicable"

#Garage
house$GarageType[is.na(house$GarageType)] = "Not applicable"
house$GarageYrBlt [is.na(house$GarageYrBlt )] = "Not applicable"
house$GarageFinish[is.na(house$GarageFinish)] = "Not applicable"
house$GarageQual[is.na(house$GarageQual)] = "Not applicable"
house$GarageCond[is.na(house$GarageCond)] = "Not applicable"

#Numerical
#Replace LotFrontage NAs with mean
house$LotFrontage[is.na(house$LotFrontage)] <- mean(house$LotFrontage, na.rm = TRUE)
#Replace MasVnrArea NAs with 0
house$MasVnrArea[is.na(house$MasVnrArea)] <- "0"

#Electrical NA deleted
house <- na.omit(house)
#Dataset has no NA values left

#Part 3
#EDA

#1.Distribution of SalePrice in general and by Suburbs
ggplot(house, aes(log(SalePrice), fill = ..count..))+
  geom_histogram(bins = 30)+
  theme_light()+
  ggtitle("Houses sale price in Ames")+
  xlab("SalePrice (log)")

#2. The most popular house type and their prices
#House style
p2.1 <- ggplot(house, aes(x=HouseStyle, fill = ..count..))+
  geom_bar()+
  geom_text(aes(label = ..count..),stat= "count", vjust = -0.2)+
  ggtitle("House styles in Ames")+
  xlab("House Style")+
  ylab("Number of houses")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
p2.1
#Average prices for each house style
house_pop <- house %>% 
  select(HouseStyle, SalePrice) %>% 
  group_by(HouseStyle) %>% 
  summarise(avgSalePrice = mean(SalePrice))

#rounding decimal digits
house_pop$avgSalePrice <- round(house_pop$avgSalePrice, digits = 0)
house_pop$avgSalePrice <- format(house_pop$avgSalePrice, big.mark = ",")

p2.2 <- ggplot(house_pop, aes(x=HouseStyle, y = avgSalePrice))+
  geom_bar(stat = "identity", fill = "lightblue")+
  geom_text(aes(label = avgSalePrice), vjust = -0.5)+
  ggtitle("Average house price for each style in Ames")+
  xlab("House Style")+
  ylab("Average Prices")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
p2.2
#Merge 2 plot
ggarrange(p2.1,p2.2, nrow = 2)

#3.	The time pattern of sold properties during the given period
house_time <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  group_by(MoSold) %>% 
  summarise(totalSale = sum(SalePrice)/1000000)

house_time$MoSold <- format(house_time$MoSold, format = "%m")
house_time$totalSale <- round(house_time$totalSale, digits = 2)

a <- seq(1,12,1)
p3.1 <- ggplot(house_time, aes(x=MoSold, y = totalSale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("Net Sales in each month 2006 - 2010")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  geom_text(aes(label = totalSale), vjust = -0.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)
p3.1


#By year
house_2006 <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  filter(YrSold == "2006") %>% 
  group_by(MoSold) %>% 
  summarise(monthlySale = sum(SalePrice)/1000000)

house_2007 <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  filter(YrSold == "2007") %>% 
  group_by(MoSold) %>% 
  summarise(monthlySale = sum(SalePrice)/1000000)

house_2008 <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  filter(YrSold == "2008") %>% 
  group_by(MoSold) %>% 
  summarise(monthlySale = sum(SalePrice)/1000000)

house_2009 <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  filter(YrSold == "2009") %>% 
  group_by(MoSold) %>% 
  summarise(monthlySale = sum(SalePrice)/1000000)

house_2010 <- house %>% select(YrSold, MoSold, SalePrice) %>% 
  filter(YrSold == "2010") %>% 
  group_by(MoSold) %>% 
  summarise(monthlySale = sum(SalePrice)/1000000)

p3.2<-ggplot(house_2006, aes(x=MoSold, y = monthlySale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("2006")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)


p3.3<-ggplot(house_2007, aes(x=MoSold, y = monthlySale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("2007")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)

p3.4<-ggplot(house_2008, aes(x=MoSold, y = monthlySale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("2008")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)

p3.5<-ggplot(house_2009, aes(x=MoSold, y = monthlySale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("2009")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)

p3.6<-ggplot(house_2010, aes(x=MoSold, y = monthlySale))+
  geom_bar(stat = "identity", fill = "lightblue")+
  ggtitle("2010")+
  xlab("Month")+
  ylab("Total Sales ($mil)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(0,13), breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), minor_breaks = NULL)

ggarrange(p3.2, p3.3, p3.4, p3.5, p3.6, nrow = 2, ncol = 3)

#4House prices by suburbs
house_suburb <- house %>% 
  select(SalePrice, Neighborhood) %>% 
  group_by(Neighborhood) %>% 
  summarise(SalePrice)

ggplot(house_suburb, aes(x = Neighborhood, y = SalePrice, col = Neighborhood))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#5 Attributes that might affect house prices
house$OverallCond <- as.character(house$OverallCond)
house$OverallQual <- as.numeric(house$OverallQual)

ggplot(house, aes(x= OverallQual, y=log(SalePrice), col = OverallCond))+
  geom_jitter()+
  ggtitle("Scatter plots for house quality, condition and sale price")+
  ylab("Sale Price (log)")+
  xlab("Overall quality")+
  scale_x_continuous(limits = c(1,10), breaks = seq(1,10, by = 1), minor_breaks = NULL)+
  facet_wrap(~OverallCond,ncol = 3)

ggplot(house, aes(x = LotFrontage, y = log(SalePrice), col = Street))+
  geom_point()+
  ggtitle("House frontage and sale price")+
  ylab("Sale Price (log)")+
  xlab("Frontage (ft)")+
  geom_smooth(method = lm, color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Street)

house$BedroomAbvGr <- as.character(house$BedroomAbvGr)

ggplot(house, aes(x = GrLivArea, y = log(SalePrice), col = BedroomAbvGr))+
  geom_point()+
  ggtitle("Number of bedrooms")+
  ylab("Sale Price (log)")+
  xlab("Bedrooms")+
  geom_smooth(method = lm, color = "red")+
  theme(plot.title = element_text(hjust = 0.5))

#Further Processing
#Create new variable called Age
house <- select(house, -last_col())
house$YearRemodAdd <- as.numeric(house$YearRemodAdd)
house <- house %>% 
  mutate(Age = YrSold - YearRemodAdd)

#final dataset
house_final <- house %>% select(LotFrontage, LotArea, 
                                GrLivArea, GarageArea, Age, SalePrice)
cor(house_final)

#Plots for each varibale 
p4.1 <- ggplot(house_final, aes(x=LotFrontage, y=SalePrice))+
  geom_jitter()+
  geom_smooth(method = "lm")

p4.2 <-ggplot(house_final, aes(x=LotArea, y=SalePrice))+
  geom_jitter()+
  geom_smooth(method = "lm")

p4.3 <-ggplot(house_final, aes(x=GrLivArea, y=SalePrice))+
  geom_jitter()+
  geom_smooth(method = "lm")

p4.4 <-ggplot(house_final, aes(x=GarageArea, y=SalePrice))+
  geom_jitter()+
  geom_smooth(method = "lm")

p4.5 <-ggplot(house_final, aes(x=Age, y=SalePrice))+
  geom_jitter()+
  geom_smooth(method = "lm")
ggarrange(p4.1,p4.2,p4.3,p4.4,p4.5, ncol = 3, nrow = 2)
ggpairs(house_final)

#5. Modelling
z.house <- scale(house_final)
lof.house_final<- lofactor(z.house, 5)
print(which(lof.house_final > 1.5))
print(house_final[which(lof.house_final > 1.5), ])
str(house_final)
#create models
model1 <- lm(house_final[,6] ~ house_final[,1]+ house_final[,2]+ house_final[,3]+ house_final[,4]+ house_final[,5] )
model2 <- lm(house_final[,6] ~ house_final[,3]+ house_final[,4]+ house_final[,5] )
model3 <- lm(house_final[,6] ~ house_final[,3] )
model1
model2
model3
summary(model1)
summary(model2)
summary(model3)
#model 1 was chosen
plot(model1)
outlierTest(model1)
#remove outliers
house_final <- house_final[-c(1299,524,692,1183,899,441,1170,1047,804),]
#re-train the model
model1 <- lm(house_final[,6] ~ house_final[,1]+ house_final[,2]+ house_final[,3]+ house_final[,4]+ house_final[,5] )
model1
summary(model1)
#predict with test data
test <- test[-c(1299,524,692,1183,899,441,1170,1047,804),]

test <- test %>% 
  mutate(prediction = predict(model1, newdata = test)) %>% 
  mutate(diff = abs(prediction - SalePrice))

rmse <- sqrt(mean((test$SalePrice - test$prediction)^2))
  rmse
rmse(model1, test)
#plot residuals
badlmdata <- test %>% filter(diff>100000) %>% arrange(desc(diff))
residuals <- ggplot(test, aes(x=SalePrice, y=prediction),col=diff)+
  geom_point(aes(x=SalePrice, y = prediction, color = diff))+
  geom_point(data = badlmdata, colour="red" )+
  scale_color_gradient(name="|y - ybar|",limits = c(0,100000))+
  geom_abline(slope = 1, intercept = 0)+
  xlab("y")+
  ylab("ybar")+
  ggtitle("Linear model residual")

residuals
