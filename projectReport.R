# Dependencies
library(psych)
library(tidyverse)
library(GGally)
library(leaps)

# EDA
airbnb_df <- read.csv("data/AB_NYC_2019.csv")
# first we drop all extra features 
airbnb_df <- subset(airbnb_df, select=c(reviews_per_month, room_type, 
                                        latitude, longitude, price, neighbourhood_group,
                                        minimum_nights, availability_365, 
                                        calculated_host_listings_count))

# MERGING min_nights and price to make min_price
airbnb_df$minimum_price = airbnb_df$price*airbnb_df$minimum_nights
airbnb_df <- subset(airbnb_df, select=-c(price, minimum_nights))

# to check NA values in all columns
sapply(airbnb_df, function(x) sum(is.na(x)))

airbnb_df <- na.omit(airbnb_df)

ggpairs(subset(airbnb_df, select=-c(room_type)))

# reviews per month dist
hist(airbnb_df$reviews_per_month, xlab = "Reviews Per Month", main = "Distribution of Reviews Per Month")
hist(airbnb_df$calculated_host_listings_count, xlab = "Calculated Host Listings", main = "Distribution of Calculated Host Listings")
hist(airbnb_df$minimum_price, xlab = "Minimium Price", main = "Distribution of Minimum Price")

#removing outliers from cal_host listings count
cal_host_list_mean <- mean(airbnb_df$calculated_host_listings_count)
cal_host_list_sd <- sd(airbnb_df$calculated_host_listings_count)
airbnb_df <- subset(airbnb_df, calculated_host_listings_count <= cal_host_list_mean + 1.96*cal_host_list_sd)

#transformed histograms
hist(log(airbnb_df$reviews_per_month),xlab = "Transformed Reviews Per Month (log transformation)", main = "Distribution of Transformed Reviews Per Month")
hist(log(airbnb_df$minimum_price)^(1/3),xlab = "Transformed Minimum Price (log transformation)", main = "Distribution of Transformed Minimum Price")
hist(log(airbnb_df$calculated_host_listings_count)^(1/2),xlab = "Transformed Calculated host listings (log transformation)", main = "Distribution of Transformed Calculated host listings")

#plotting everything against response with log transformation
plot(data=airbnb_df, I(log(reviews_per_month))~latitude, xlab = "Latitude", ylab = "Transformed Reviews Per Month (log)")
title("Latitude vs Reviews per Month")
plot(data=airbnb_df, I(log(reviews_per_month))~longitude, xlab = "Longitude", ylab = "Transformed Reviews Per Month (log)")
title("Longitude vs Reviews per Month")
plot(data=airbnb_df, I(log(reviews_per_month))~availability_365, xlab = "Availability (no. of days)", ylab = "Transformed Reviews Per Month (log)")
title("Availability vs Reviews per Month")
plot(data=airbnb_df, I(log(reviews_per_month))~log(airbnb_df$minimum_price)^(1/3), xlab = "Minimum Price (in $)", ylab = "Transformed Reviews Per Month (log)")
title("Minimum Price vs Reviews per Month")
plot(data=airbnb_df, I(log(reviews_per_month))~log(airbnb_df$calculated_host_listings_count)^(1/2), xlab = "No. of listings per host", ylab = "Transformed Reviews Per Month (log)")
title("Calculated Host Listings vs Reviews per Month")

#training full model no interactions
full_reg <- lm(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                 latitude+ longitude+ neighbourhood_group+
                 availability_365+ I(log(airbnb_df$minimum_price)^(1/3))+ 
                 I(log(calculated_host_listings_count)^(1/2)))
summary(full_reg)

s_reg <- regsubsets(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                      latitude+ longitude+ neighbourhood_group+
                      availability_365+ I(log(airbnb_df$minimum_price)^(1/3))+ 
                      I(log(calculated_host_listings_count)^(1/2)), nvmax=14)
ss_reg <- summary(s_reg)
ss_reg$which
ss_reg$cp
ss_reg$adjr2
plot(seq(2, length(ss_reg$cp)+1), ss_reg$cp)
title("Cp vs p for Full Model (no interaction)")
abline(0,1)

# model with interaction b/w long & lat
reg2 <- lm(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
             latitude*longitude+ neighbourhood_group+
             availability_365+ I(log(airbnb_df$minimum_price)^(1/3))+ 
             I(log(calculated_host_listings_count)^(1/2)))
summary(reg2)

s_reg2 <- regsubsets(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                       latitude*longitude+ neighbourhood_group+
                       availability_365+ I(log(airbnb_df$minimum_price)^(1/3))+ 
                       I(log(calculated_host_listings_count)^(1/2)), nvmax=14)
ss_reg2 <- summary(s_reg2)
ss_reg2$which
ss_reg2$cp
ss_reg2$adjr2

plot(seq(2, length(ss_reg2$cp)+1), ss_reg2$cp)
title("Cp vs p for Latitude*Longitude")
abline(0,1)

# model with interaction b/w neighbourhood_group and price
reg3 <- lm(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
             latitude+ longitude+ availability_365+
             neighbourhood_group*I(log(airbnb_df$minimum_price)^(1/3))+ 
             I(log(calculated_host_listings_count)^(1/2)))
summary(reg3)

s_reg3 <- regsubsets(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                       latitude+ longitude+ availability_365+
                       neighbourhood_group*I(log(airbnb_df$minimum_price)^(1/3))+ 
                       I(log(calculated_host_listings_count)^(1/2)), nvmax=18)
ss_reg3 <- summary(s_reg3)
ss_reg3$which
ss_reg3$cp
ss_reg3$adjr2

plot(seq(2, length(ss_reg3$cp)+1), ss_reg3$cp)
title("Cp vs p for Neighbourhood_group*Minimum_price")
abline(0,1)

# model with interaction b/w price and calc_host_listing
reg4 <- lm(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
             latitude+ longitude+ availability_365+
             neighbourhood_group+I(log(airbnb_df$minimum_price)^(1/3))*
             I(log(calculated_host_listings_count)^(1/2)))
summary(reg4)

s_reg4 <- regsubsets(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                       latitude+ longitude+ availability_365+
                       neighbourhood_group + I(log(airbnb_df$minimum_price)^(1/3))* 
                       I(log(calculated_host_listings_count)^(1/2)), nvmax=16)
ss_reg4 <- summary(s_reg4)
ss_reg4$which
ss_reg4$cp
ss_reg4$adjr2

plot(seq(2, length(ss_reg4$cp)+1), ss_reg4$cp)
title("Cp vs p for Minimum_price*Calculated_host_listings")
abline(0,1)

# final model
final_reg <- lm(data=airbnb_df, I(log(reviews_per_month))~room_type+ 
                  latitude+ longitude+ availability_365+
                  neighbourhood_group+I(log(airbnb_df$minimum_price)^(1/3))*
                  I(log(calculated_host_listings_count)^(1/2)))
summary(final_reg)

# cross-validation :
train <- 1:as.integer(dim(airbnb_df)[1]/2)

fold1 <- airbnb_df[train,,]
fold2 <- airbnb_df[-train, ]

train_final_reg1 <- lm(I(log(reviews_per_month)) ~ room_type +
                         latitude + longitude + availability_365 +
                         neighbourhood_group + I(log(minimum_price)^(1/3)) *
                         I(log(calculated_host_listings_count)^(1/2)),
                       data = fold1)
error1 <- sum((fold2$reviews_per_month - exp(predict(train_final_reg1, fold2)))^2, na.rm = T)
train_final_reg2 <- lm(I(log(reviews_per_month)) ~ room_type +
                         latitude + longitude + availability_365 +
                         neighbourhood_group + I(log(minimum_price)^(1/3)) *
                         I(log(calculated_host_listings_count)^(1/2)),
                       data = fold2)
error2 <- sum((fold1$reviews_per_month - exp(predict(train_final_reg2, fold1)))^2, na.rm = T)
error <-  (error1+error2)/dim(airbnb_df)[1,]
error

#residual plot
plot(fitted(final_reg), resid(final_reg), 
     xlab = "Fitted Values for reviews per month", 
     ylab = "Residual Values for reviews per month", 
     main = "Residual plot for the Final Model")
abline(0,0, col="red")
#===============================================================================