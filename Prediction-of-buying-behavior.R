getwd()
install.packages("dplyr")
library("dplyr")
data <- read.csv("Geo-Fence Analytics.csv")
data$imp_large<- ifelse(data$imp_size == "728x90",1,0)
head(data)
data$cat_entertainment<-ifelse(data$app_topcat %in% c("IAB1", "IAB1-6"),1,0)
data$cat_social<- ifelse(data$app_topcat  == "IAB14",1,0)
data$cat_tech<- ifelse(data$app_topcat  == "IAB19-6",1,0)                              
data$os_ios<-ifelse(data$device_os == "iOS",1,0)
install.packages("aspace")
library("aspace")
data$distance <- 6371*acos(cos(as_radians(data$device_lat)) * cos(as_radians(data$geofence_lat)) * 
                                  cos(as_radians(data$device_lon) - as_radians(data$geofence_lon)) + 
                                  sin(as_radians(data$device_lat)) * sin(as_radians(data$geofence_lat)))

data$distance_squared<-data$distance^2
data$ln_app_review_vol<-log(data$app_review_vol)

#Descriptive statistics
install.packages("psych")
library(psych)
attach(data)
variables<-cbind(didclick,distance, imp_large, cat_entertainment, cat_social, cat_tech, os_ios, ln_app_review_vol, app_review_val)
describe(variables)

cor(variables)

data$distance_group<-as.list(distance)
newdf <- data %>%
  mutate(distance_group = case_when(
    distance_group > 0 & distance_group<= 0.5 ~ "1",
    distance_group > 0.5 & distance_group <=1 ~ "2",
    distance_group > 1 & distance_group <=2   ~ "3",
    distance_group > 2 & distance_group <= 4  ~ "4",
    distance_group > 4 & distance_group <= 7  ~ "5",
    distance_group > 7 & distance_group <= 10 ~ "6",
    distance_group > 10  ~ "7",
    TRUE                                      ~ "NA"
    ))

newdf$impressions<-c(1)
plot(newdf %>% 
  group_by(distance_group) %>% 
  summarise(ctr = sum(didclick)/sum(impressions)))

plot(newdf %>% 
       group_by(app_review_val) %>% 
       summarise(ctr = sum(didclick)/sum(impressions)))
plot(newdf %>% 
       group_by(ln_app_review_vol) %>% 
       summarise(ctr = sum(didclick)/sum(impressions)))

newdf$norm_dist<-(distance-mean(distance))/sd(distance)
newdf$norm_dist_squared<-norm_dist^2

summary(glm(didclick ~ norm_dist + norm_dist_squared + imp_large + cat_entertainment + cat_social + cat_tech +
      os_ios + ln_app_review_vol + app_review_val, family="binomial", data=newdf))

