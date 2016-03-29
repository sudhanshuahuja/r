##Perform regression analysis to predict mileage from vehicle data
##Data from UCI Machine Learning Repository

##Read data in to variable auto
auto <- na.omit(read.table("auto-mpg.data")) 

##Apply meaningful columnmanes from data source
colnames(auto) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year","origin","car_name")

##Change horsepower to factor data type
auto$horsepower <- as.numeric(levels(auto$horsepower))[auto$horsepower] 

##Remove N/As
auto <- na.omit(auto)

##Inspect scatterplots for relationships among variables
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + model_year+origin)

##Regression models for 11 different combinations of independent variables
##Mmdel 1
auto.fit <- lm(mpg~. -car_name,data=auto) 

##Study model characteristics
summary(auto.fit)

##mModel 2
auto.fit1 <- lm(mpg ~ displacement + horsepower + weight , data=auto) 
summary(auto.fit1)

##Model 3
auto.fit2 <- lm(mpg ~ acceleration + horsepower + weight, data = auto) 
summary(auto.fit2)

##Model 4
auto.fit3 <- lm(mpg ~ model_year + horsepower + weight, data = auto) 
summary(auto.fit3)

##Model 5
auto.fit4 <- lm(mpg ~ model_year + horsepower + weight + origin, data = auto) 
summary(auto.fit4)

##Model 6
auto.fit5 <- lm(mpg ~ model_year + acceleration + weight + origin, data = auto) 
summary(auto.fit5)

##Model 7
auto.fit6 <- lm(mpg ~ model_year + weight + origin, data = auto) 
summary(auto.fit6)

##Model 8
auto.fit7 <- lm(mpg ~ I(displacement^2) + model_year + weight + origin, data = auto) 
summary(auto.fit7)

##Model 9
auto.fit8 <- lm(mpg ~ I(horsepower^1) + I(horsepower^2) + I(horsepower^3) + model_year + weight + origin, data = auto) 
summary(auto.fit8)

##Model 10
auto.fit9 <- lm(mpg ~ horsepower + model_year + weight + origin, data = auto) 
summary(auto.fit9)

##Model 11
auto.fit10 <- lm(mpg ~ model_year + weight + origin + poly(horsepower,2) , data=auto) 
summary(auto.fit10)
