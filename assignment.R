
#Getting data from csv file into car dataframe
#setwd("D:\\PGDDA\\Course 3\\M2- Assignment- Linear Regression")
car <- read.csv("CarPrice_Assignment.csv", header = T, stringsAsFactors = F)

#Installing and loading "MASS" and "car" pacakge.
#install.packages("MASS")
#install.packages("car")
library(MASS)
library(car)

#-----------------------DATA PREPARATION---------------------------------------------------------

#View the dataset
View(car)

#Check the structure of car dataset
str(car)


#Converting into Categorical variable
#symboling is categorical but it is imported as numeric
car$symboling <- as.factor(car$symboling)

car$fueltype <- as.factor(car$fueltype)

car$aspiration <- as.factor(car$aspiration)

car$doornumber <- as.factor(car$doornumber)

car$carbody <- as.factor(car$carbody)

car$drivewheel <- as.factor(car$drivewheel)

car$enginelocation <- as.factor(car$enginelocation)

car$enginetype <- as.factor(car$enginetype)

car$cylindernumber <- as.factor(car$cylindernumber)

car$fuelsystem <- as.factor(car$fuelsystem)


#Converting integer variable into numeric
car$symboling <- as.numeric(levels(car$symboling))[car$symboling]


#Converting cylinder number into numeric 
levels(car$cylindernumber)
levels(car$cylindernumber)[1] <- "8"
levels(car$cylindernumber)[2] <- "5"
levels(car$cylindernumber)[3] <- "4"
levels(car$cylindernumber)[4] <- "6"
levels(car$cylindernumber)[5] <- "3"
levels(car$cylindernumber)[6] <- "12"
levels(car$cylindernumber)[7] <- "2"
car$cylindernumber <- as.numeric(levels(car$cylindernumber))[car$cylindernumber]

#View the structure of dataset
str(car)

#View the dataset car
View(car)

#Derived Matric
#Seperating car company name from carname variable
car$CarName <- tolower(car$CarName)
summary(as.factor(car$CarName))
car$carcompany <- gsub("\\ .*", "", car$CarName)

str(car)

#Converting carCompany name as factor
car$carcompany <- as.factor(car$carcompany)
summary(car$carcompany)
levels(car$carcompany)

#"mazda" and "maxda" are same.
levels(car$carcompany)[10] <- "mazda"
levels(car$carcompany)

#"porsche" and "porcshce" are same.
levels(car$carcompany)[16] <- "porsche"
levels(car$carcompany)

#"toyota" and "toyouta" are same.
levels(car$carcompany)[21] <- "toyota"
levels(car$carcompany)

#"vokswagen", "volkswagen" and "vw" are same. So, keeping all as "volkswagen"
levels(car$carcompany)[21] <- "volkswagen"
levels(car$carcompany)

levels(car$carcompany)[23] <- "volkswagen"
levels(car$carcompany)


#Duplicate Value Check
unique(car)
#Total observation is equal to unique observation i.e. 205, so there is no dupplicate values.

#Missing Value Check
sum(is.na(car))
#there is no missing value available in car dataset.


#Outlier treatment
#If there is a gap between the consecutive low and high values then i am normalizing them
#Wheelbase
quantile(car$wheelbase, seq(0,1,0.01))
car$wheelbase[which(car$wheelbase > 115.544)] <- 115.544

#carlength
quantile(car$carlength, seq(0,1,0.01))
car$carlength[which(car$carlength > 202.480)] <- 202.480

#carwidth
quantile(car$carwidth, seq(0,1,0.01))
#car$carwidth[which(car$carwidth > 115.544)] <- 115.544

#carheight
quantile(car$carheight, seq(0,1,0.01))
#car$carheight[which(car$carheight > 115.544)] <- 115.544

#curbweight
quantile(car$curbweight, seq(0,1,0.01))
car$curbweight[which(car$curbweight < 1819.72)] <- 1819.72

#enginesize
quantile(car$enginesize, seq(0,1,0.01))
car$enginesize[which(car$enginesize > 209.00)] <- 209.00

#boreratio
quantile(car$boreratio, seq(0,1,0.01))
car$boreratio[which(car$boreratio < 2.91)] <- 2.91

#stroke
quantile(car$stroke, seq(0,1,0.01))
car$stroke[which(car$stroke > 3.64)] <- 3.64

#compression ratio
quantile(car$compressionratio, seq(0,1,0.01))
car$compressionratio[which(car$compressionratio > 10.9400)] <- 10.9400

#horsepower
quantile(car$horsepower, seq(0,1,0.01))
car$horsepower[which(car$horsepower > 184.00)] <- 184.00

#citympg
quantile(car$citympg, seq(0,1,0.01))
car$citympg[which(car$citympg > 38.00)] <- 38.00

#hoghwaympg
quantile(car$highwaympg, seq(0,1,0.01))
car$highwaympg[which(car$highwaympg > 46.92)] <- 46.92

str(car)

#Car_ID and CarName are of no use , so removing them.
car1 <- car[,-c(1,3)]
str(car1)


#Dummy variable creation

#Fuel Type : 2 levels
levels(car1$fueltype)
# "1" represent diesel and "0" represent gas
levels(car1$fueltype) <- c(1,0)
car1$fueltype <- as.numeric(levels(car1$fueltype))[car1$fueltype]

#Aspiration : 2 levels
levels(car1$aspiration)
# "1" represent std and "0" represent turbo
levels(car1$aspiration) <- c(1,0)
car1$aspiration <- as.numeric(levels(car1$aspiration))[car1$aspiration]

#Door number : 2 levels
levels(car1$doornumber)
# "1" represent four and "0" represent two doornumber
levels(car1$doornumber) <- c(1,0)
car1$doornumber <- as.numeric(levels(car1$doornumber))[car1$doornumber]

#Carbody. It has 5 levels
dummy1 <- data.frame(model.matrix( ~carbody, data = car1))
dummy1<-dummy1[,-1]

#Drivewheel : It has 3 levels
dummy2 <- data.frame(model.matrix( ~drivewheel, data = car1))
dummy2<-dummy2[,-1]

#Engine location : it has 2 levels
levels(car1$enginelocation)
# "1" represent front and "0" represent rear
levels(car1$enginelocation) <- c(1,0)
car1$enginelocation <- as.numeric(levels(car1$enginelocation))[car1$enginelocation]


#Engine Type : It has 7 levels
dummy3 <- data.frame(model.matrix( ~enginetype, data = car1))
dummy3<-dummy3[,-1]



#Fulesystem : It has 8 levels
dummy4 <- data.frame(model.matrix( ~fuelsystem, data = car1))
dummy4<-dummy4[,-1]

#carcompany : It has 22 levels
dummy5 <- data.frame(model.matrix( ~carcompany, data = car1))
dummy5<-dummy5[,-1]

#Creating final car2 dataframe by combining dummy and numerical variables
car2 <- cbind(car1[ , c(1:4,7:12,15,17:24)], dummy1,dummy2,dummy3,dummy4,dummy5)

View(car2)
str(car2)

#----------------------------------DATA CLEANING ENDS------------------------------------------


#----------------------------------MODELING STARTS---------------------------------------------

#Seperating train and test data in 70-30% ratio
set.seed(100)
indices <- sample(1:nrow(car2), 0.7*nrow(car2))

train <- car2[indices,]
test <- car2[-indices,]

#Creating the First model
model1 <-lm(price~.,data=train[,-1])
summary(model1)

#stepAIC function to get only significant variables
step <- stepAIC(model1, direction="both")
step

#Based on the output of stepAIC function, we are considering only those variables which are gicing in last 
#call of function and creating the model
model2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + stroke + horsepower + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyhonda, data = train[, -1])
summary(model2)
sort(vif(model2))

#Based on P value and vif, we are removing the variables
#removing carbodyhardtop. It has high vif and larger p value.
model3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyhonda, data = train[, -1])
summary(model3)
sort(vif(model3))

#removing wheelbase
model4 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen + 
               carcompanyhonda, data = train[, -1])
summary(model4)
sort(vif(model4))

#removing csrcompanyhonda
model5 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model5)
sort(vif(model5))

#removing enginetypeohc
model6 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypel + enginetypeohc + enginetypeohcf + 
               enginetypeohcv + enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model6)
sort(vif(model6))

#removing enginetypeohcv
model7 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model7)
sort(vif(model7))

#removing carbodysedan
model8 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke + horsepower +  
               carbodyhatchback  + carbodywagon + drivewheelrwd + 
               enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model8)
sort(vif(model8))


#removing horsepower
model9 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke +   
               carbodyhatchback  + carbodywagon + drivewheelrwd + 
               enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model9)
sort(vif(model9))

#removing drivewheelrwd 
model10 <- lm(formula = price ~ aspiration + enginelocation + 
               carwidth + curbweight + stroke +   
               carbodyhatchback  + carbodywagon + 
               enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + carcompanybmw + carcompanybuick + 
               carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
               carcompanymazda + carcompanymercury + carcompanymitsubishi + 
               carcompanynissan + carcompanyplymouth + carcompanyrenault + 
               carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model10)
sort(vif(model10))


#removing stroke
model11 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight +   
                carbodyhatchback  + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model11)
sort(vif(model11))

#removing aspiration
model12 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodyhatchback  + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda + carcompanymercury + carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model12)
sort(vif(model12))


#removing Carcompanymercury
model13 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodyhatchback  + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanysaab + carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model13)
sort(vif(model13))

#removing carcompanysaab
model14 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodyhatchback  + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                 carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model14)
sort(vif(model14))

#removing enginetyperoto
model15 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodyhatchback  + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model15)
sort(vif(model15))


#removing carbodyhatchback
model16 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                 carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet + carcompanydodge + carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model16)
sort(vif(model16))

#removing carcompanydodge
model17 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan + carcompanyplymouth + carcompanyrenault + 
                carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model17)
sort(vif(model17))


#removing carcompanyplymouth
model18 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanynissan +  carcompanyrenault + 
                carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model18)
sort(vif(model18))


#removing carcompanynissan
model19 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                 carcompanyrenault + 
                carcompanytoyota + carcompanyvolkswagen , data = train[, -1])
summary(model19)
sort(vif(model19))


#removing carcompanyvolkswagen
model20 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanyrenault + 
                carcompanytoyota, data = train[, -1])
summary(model20)
sort(vif(model20))

#removing carbodywagon
model21 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda +  carcompanymitsubishi + 
                carcompanyrenault + 
                carcompanytoyota, data = train[, -1])
summary(model21)
sort(vif(model21))


#removing carcompanymitsubishi
model22 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda + 
                carcompanyrenault + 
                carcompanytoyota, data = train[, -1])
summary(model22)
sort(vif(model22))

#removing carcompanyrenault
model22 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanymazda + 
                carcompanytoyota, data = train[, -1])
summary(model22)
sort(vif(model22))

#removing carcompanymazda
model23 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar + 
                carcompanytoyota, data = train[, -1])
summary(model23)
sort(vif(model23))

#removing carcompanytoyota
model24 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanychevrolet +  carcompanyjaguar , data = train[, -1])
summary(model24)
sort(vif(model24))

#removing carcompanychevrolet
model25 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypel + enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                 carcompanyjaguar, data = train[, -1])
summary(model25)
sort(vif(model25))

#Now all the variables are 3 star. It is difficul to remove based on p value and vif
#So, we are taking correlation with price
cor(train$price, train$enginelocation)
#Engine location is negatively correlated with price

cor(train$price, train$carwidth)
cor(train$price, train$curbweight)
cor(train$price, train$enginetypel)
cor(train$price, train$enginetypeohc)
cor(train$price, train$enginetypeohcf)
cor(train$price, train$carcompanyjaguar)
cor(train$price, train$carcompanybuick)
cor(train$price, train$carcompanybmw)

#enginetypel : It is least correlated with price
model26 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypeohc + enginetypeohcf + 
                carcompanybmw + carcompanybuick + 
                carcompanyjaguar, data = train[, -1])
summary(model26)
sort(vif(model26))

#enginetypeohcf : 2 star
model27 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                enginetypeohc +  
                carcompanybmw + carcompanybuick + 
                carcompanyjaguar, data = train[, -1])
summary(model27)
sort(vif(model27))

#enginetypeohc : 2 star
model28 <- lm(formula = price ~   enginelocation + 
                carwidth + curbweight +   
                carcompanybmw + carcompanybuick + 
                carcompanyjaguar, data = train[, -1])
summary(model28)
sort(vif(model28))

#carwidth : It is highly correlated with curbweight, so we are removing it.
model28 <- lm(formula = price ~   enginelocation + 
                curbweight +   
                carcompanybmw + carcompanybuick + 
                carcompanyjaguar, data = train[, -1])
summary(model28)
sort(vif(model28))

cor(train$carwidth, train$curbweight)
cor(train$price, train$carwidth)
cor(train$price, train$curbweight)




#-----------------------------------------Testing-------------------------------------------


Predict1 <- predict(model27,test[,-which(colnames(test) == 'price')])

#creating new column "test_price" in test dataset
test$test_price <- Predict1


#calculate the test R-square value using test dataset
cor(test$price,test$test_price)
cor(test$price,test$test_price)^2

#Train R-square is 0.8887 whereas test R-square is 0.8414963. So this model is not having much difference
# and it is accepted.

#Residual Plot creation
residualPlot(model28)
#There is no such specific pattern. so values are randomly distributed. This confirm our model best explains the 
# the requirements


#-------------------------------------ANALYSIS-----------------------------------------------

#By business understanding, we can say that the person having knowledge about technical specification about
#cars will go with "enginelocation" and "curbweight". These 2 parameters describes the stability of car.
#Also, brand name matters in the market. "BMW". "JAGUAR" and "BUICK" has its own name and they are well known
# for good and efficient cars. Top Brands never compromise with Quality.
#That's why, the price of car depends on these 5 parameters.
# Most 5 important parameters on which price depends are:
# 1.  enginelocation
# 2.  carcompanybmw
# 3.  carcompanyjaguar
# 4.  carcompanybuick
# 5.  curbweight


#-----------------------------------------END OF ANALYSIS-------------------------------------------



