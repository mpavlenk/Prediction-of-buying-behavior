getwd()
library("dplyr")
data <- read.csv("HighNote Data.csv")
library(psych)
library(purrr)

#1. Summary statistics
keydata<-subset(data, select=-c(ID, avg_friend_age,avg_friend_male,friend_country_cnt))
keydata %>% split(.$adopter) %>% map(describe)

#Let's look at the differences in the mean values of the variables in the adopter vs non-adapter subsamples.
lapply(data[,c('age','male' , 'friend_cnt' , 'avg_friend_male' ,'avg_friend_age', 
               'friend_country_cnt'  , 'songsListened' , 'lovedTracks' , 
               'posts' , 'playlists' ,'shouts' , 'tenure' ,'good_country', 'subscriber_friend_cnt')], function(i) t.test(i ~ data$adopter))

# Descriptive statistics for adopter and nonadpoter have different outputs. The mean values of the key variables for adopter sample  are higher in comparison to the ones of nonadopter sample. The variance of values is generally higher in adopter sample, therefore the kurtosis is lower than in nonadopter sample, which means the distribution is less sharpenned. Adopter mean is greater than adopter median which indicates that the disctribution is skewed to the right. The same refers to nonadapter sample. The samples  have the same min values. Maximum values are higher in nonadopter sample which laso explains their higher range. 
# With all said, we can conclude some facts about the samples: 
#   Premium suscribers (adopters) are slighly older than free users (nonadopters) in its majority, with more male population than nonadopters, they have significantly more friends both free and premium users. They are much more engaged with the Highnote content. Premium subscribers also have longer experience with Highnote and its population is less US, UK and Germany users as it is observed among free users.
# 
# 2. Adopter vs non-adopters | Visualization
# 2.1. demographics
library("ggplot2")
# majority of non-adopters population are users between 18-30 years, and by count this population is much larger than population of adopters.
# Adopters are generally a little older than nonadopters by 2-3 years.
ggplot(data,aes(x=age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()

#Male population substencially prevails in both samples, by count we notice that adopter is a much smaller sample than non-adopter. 
ggplot(data,aes(x=male,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()

#Both samples have more users from the rest of the world and much fewer users are from US, UK, Germany. However, adopters are leading in world users.
ggplot(data,aes(x=good_country,group=adopter,fill=adopter))+
  geom_bar(position="dodge")+theme_minimal()


# 2.2 peer influence
# In general, average amount of friends for adopters is 40, whereas for non-adopters it is twice as less around 18 friends per user.
friend_cnt<-data %>%
  group_by(adopter)%>%
  summarise(friend_cnt=mean(friend_cnt))
ggplot(friend_cnt,aes(x = adopter,y=friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="orange")+theme_minimal()

# Both samples demonstrate similar social characteristics in dependance with age: the younger the user, 
# the more friends he has and vs the older the user, the less friends he has. Users with highest amount 
# of friends are aged between 15 and 35 and users aged 55-65 have smallest frinds list.
ggplot(data, aes(x = age, y = friend_cnt)) + 
  geom_point() +
  facet_wrap(~ adopter)+
  ylim(c(0, 2200))+
  geom_smooth(method = 'lm', color='red')

#Average friend age for majority adopters and non-adopters is around the same with slight difference 15-45 years old.
ggplot(data,aes(x=avg_friend_age,group=adopter,fill=adopter))+
  geom_histogram(position="identity",binwidth=0.5)+theme_minimal()

#Adopters have around three times more friends who are premium subscribers than non-adopters.
subscriber_friend_cnt<-data %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt))
ggplot(subscriber_friend_cnt,aes(x = adopter,y=subscriber_friend_cnt)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#Male-female distribution among 2 samples is very similar with slight difference in favor of males in adopters.
avg_friend_male<- data %>%
  group_by(adopter)%>%
  summarise(avg_friend_male=mean(avg_friend_male))
ggplot(avg_friend_male,aes(x = adopter,y=avg_friend_male)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

# 2.3 user engagement
# Adopters demonstrate a significantly higher engagement than non-adopters. On average, they listen to around 34k songs, whereas non-adopters listen to twice as less around 18k songs.
songsListened<- data %>%
  group_by(adopter)%>%
  summarise(songsListened=mean(songsListened))
ggplot(songsListened,aes(x = adopter,y=songsListened)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#LovedTracks follows the same tendency. Though the gap between the two samples is even higher: 260 vs 86.
lovedTracks<- data %>%
  group_by(adopter)%>%
  summarise(lovedTracks=mean(lovedTracks))
ggplot(lovedTracks,aes(x = adopter,y=lovedTracks)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#Adopters post much more than non-adopters. On average, premium users have 21 posts, whilst free users around 5.
posts<-data %>%
  group_by(adopter)%>%
  summarise(posts=mean(posts))
ggplot(posts,aes(x = adopter,y=posts)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#Adopters on average have around 0.9 playlists, while non-adopters only around 0.55.
playlists<-data %>%
  group_by(adopter)%>%
  summarise(playlists=mean(playlists))
ggplot(playlists,aes(x = adopter,y=playlists)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#Adopters received on average around 100 shouts from other users, while non-adopters received around 30 shouts.
shouts<-data %>%
  group_by(adopter)%>%
  summarise(shouts=mean(shouts))
ggplot(shouts,aes(x = adopter,y=shouts)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#On average, adopters have been on the site for around 46 months, while non-adopters around 43.
tenure<- data %>%
  group_by(adopter)%>%
  summarise(tenure=mean(tenure))
ggplot(tenure,aes(x = adopter,y=tenure)) +
  geom_bar(stat="identity",position=position_identity(), fill="green")+theme_minimal()

#Boxplot proves the conclusion about mean values of tenure variable.
boxplot(tenure~adopter,data=data, main="Adopter&Tenure Data", 
        xlab="Adopter", ylab="Tenure")

library(gapminder)
library(gganimate)
data$age_group<-as.list(data$age)
newdf <- data %>%
  mutate(age_group = case_when(
    age_group > 0 & age_group<= 17 ~ "1",
    age_group > 17 & age_group <=34 ~ "2",
    age_group > 34 & age_group <=49   ~ "3",
    age_group > 49 & age_group <= 64  ~ "4",
    age_group > 64  ~ "5",
    TRUE                                 ~ "NA"
  ))
# The following dynamic graph demonstrates user activity (songsListened) by age group throughout their time on site.
# For adopter sample, the groups 2 and 3 listened more songs, while the groups 1,2,3 are more active in non-adopter sample. Group 2 shows the highest level of engagement for both samples.
ggplot(newdf, aes(age_group, songsListened, colour = good_country)) +
  geom_point(alpha = 1, show.legend = FALSE) +
  scale_size(range = c(12)) +
  facet_wrap(~adopter) +
  labs(title = 'Tenure: {frame_time}', x = 'age_group', y = 'songsListened') +
  transition_time(tenure) +
  ease_aes('linear')+
  ylim(c(0,200000))

# This scatterplot matrix demonstrates relationships between different variables for free users (nonadopters).
# We can see linear relationships between friends and premium user friends for both samples which indicates the more friends a user has the
# more likely he will have premium users as his friends.
# The more time a user spent on site, the more songs a user listened.
adopter<-filter(data, adopter==1)
nonadopter<-filter(data, adopter==0)
pairs(~friend_cnt+tenure+subscriber_friend_cnt+songsListened, data=nonadopter,lower.panel = panel.smooth, upper.panel = panel.smooth,
      main="Scatterplot Matrix for NonAdopter")

#This matrix demonstrates relationships between different variables for premium users (adopters)
pairs(~friend_cnt+tenure+subscriber_friend_cnt+songsListened, data=adopter,lower.panel = panel.smooth, upper.panel = panel.smooth,
      main="Scatterplot Matrix for Adopter")

#3. Propensity Score Matching (PSM)

#grouping subscriber_friend_cnt into "treatment" group (1) and "control" group (0)
data$subscriber_friend_cnt <- ifelse(data$subscriber_friend_cnt >0,1,0)

with(data, t.test(subscriber_friend_cnt ~ adopter))
#Means of treatment and control groups are significantly different.

#Estimating means of covariates
data_cov <- c('age', 'male', 'good_country', 'friend_cnt', 'avg_friend_age', 'avg_friend_male', 'friend_country_cnt', 'songsListened', 'lovedTracks', 'posts', 'playlists', 'shouts', 'tenure' )
data %>%
  group_by(adopter) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

#Propensity score estimation
m_pscore <- glm(subscriber_friend_cnt ~ age + male + good_country + 
                  friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + 
                  songsListened + lovedTracks + posts + playlists + shouts + tenure,
                family = binomial(), data = data)
summary(m_pscore)

prscore_df <- data.frame(pr_score = predict(m_pscore, type = "response"),
                         subscriber_friend_cnt = m_pscore$model$subscriber_friend_cnt)
head(prscore_df)
head(m_pscore$model)


labs <- paste("Type of User:", c("Premium", "Non-Premium"))
pscore_df<-prscore_df %>%
  mutate(adopter = ifelse(subscriber_friend_cnt == 1, labs[1], labs[2]))
ggplot(prscore_df,aes(x = pr_score)) +
  geom_histogram(color="black", fill="green") +
  facet_wrap(~subscriber_friend_cnt) +
  xlab("Probability of Treatment (Having >1 Subscriber)") +
  theme_minimal() 

install.packages("MatchIt")
library(MatchIt)

#Looking for pairs of observations with similar propensity scores
data_nomiss <- data %>%  
  select(subscriber_friend_cnt, adopter, one_of(data_cov)) %>%
  na.omit()

match <- matchit(subscriber_friend_cnt ~ age + male + good_country + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure,
                 method = "nearest", data = data_nomiss)

#information if matching was successful
summary(match)

plot(match)



#dataframe with only matched observations
df_matched <- match.data(match)
head(df_matched)

#examining difference in covariate means of the matched sample
df_matched%>%
  group_by(subscriber_friend_cnt) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean))

lapply(data_cov, function(v) {
  t.test(df_matched[, v] ~ df_matched$subscriber_friend_cnt)
})


# Estimating treatment effects using Visual Inspection

fn_bal <- function(df_matched, variable) {
  df_matched$variable <- df_matched[, variable]
  df_matched$subscriber_friend_cnt <- as.factor(df_matched$subscriber_friend_cnt)
  support <- c(min(df_matched$variable), max(df_matched$variable))
  ggplot(df_matched, aes(x = distance, y = variable, color = subscriber_friend_cnt)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

install.packages("gridExtra")
library(gridExtra)

grid.arrange(
  fn_bal(df_matched, "age"),
  fn_bal(df_matched, "male") + theme(legend.position = "none"),
  fn_bal(df_matched, "good_country"),
  fn_bal(df_matched, "friend_cnt") + theme(legend.position = "none"),
  fn_bal(df_matched, "avg_friend_age"),
  fn_bal(df_matched, "avg_friend_male") + theme(legend.position = "none"),
  fn_bal(df_matched, "friend_country_cnt"),
  fn_bal(df_matched, "songsListened") + theme(legend.position = "none"),
  fn_bal(df_matched, "lovedTracks"),
  fn_bal(df_matched, "posts") + theme(legend.position = "none"),
  fn_bal(df_matched, "playlists"),
  fn_bal(df_matched, "shouts") + theme(legend.position = "none"),
  fn_bal(df_matched, "tenure"),
  nrow = 7, widths = c(1, 0.8)
)

# Estimating treatment effects using t-test
with(df_matched, t.test(adopter ~ subscriber_friend_cnt))

#T-test in adopter group 0.18 is higher than the one in non-adopter 0.09, which proves the hypothesis that having subscriber friends affects the likelihood of becoming an adopter 
#OLS with and without covariates
#without
treat_wt <- lm(adopter ~ subscriber_friend_cnt, data = df_matched)
summary(treat_wt)

#with
treat_with<- lm(adopter ~ subscriber_friend_cnt +  age + male + good_country + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt + songsListened + lovedTracks + posts + playlists + shouts + tenure, data = df_matched)
summary(treat_with)

#The difference in subscriber_friend_cnt coefficients between regressions with or without covariates is very small, 
#which indicates that PSM reduced the bias and we were able to estimate the effect of a treatment.

#Logistic Regression

result <- glm(adopter ~ male + age + subscriber_friend_cnt + friend_cnt + avg_friend_age + friend_country_cnt + songsListened + lovedTracks + good_country + playlists + tenure + shouts + posts + avg_friend_male,
              family = binomial(), data = data)
summary(result)

#regression with significant variables
adopter_res <- glm(adopter ~ age + male  + avg_friend_age+friend_country_cnt+subscriber_friend_cnt+
                     songsListened+lovedTracks+playlists+
                     tenure+good_country,
                   family = binomial(), data = data)
summary(adopter_res)

exp(adopter_res$coefficients)-1

# All the variables in this regression output have very low p-values, which indicate they are all significant in the model.
# Most of the variables (besides tenure and good_country) have linear relationships with the adopter variable. It indicates that 
# a one-unit increase in any of these variables, we expect an increase in the log-odds of the dependent variable adopter. Whereas tenure and 
# goo_country have inverse relationships with adopter variable, a one-unit increase in these variables, 
# we expect a decrease in the log-odds of the dependent variable.
# From this analysis we can conclude that the higher peer influence (many subscriber friends, high diversity of friends) is the more likely
# a free user can be converted to a premium subscriber. Another condition like user engagement positively affects a
# "free-to-fee" strategy. The higher engagement (songs listened, playlists, tracks, etc.) of a user is, the more chances to convert him to a premium user.
# Demographic picture of potential subscriber is a male in his late 20s or 30s from the "rest of the world".
# Recommendations to Highnote for new subscribers:
# 1) target groups with high social engagement (songs the user has listened to, playlists created, "shouts" sent to friends)
# 2) motivate premuim users to attract their friends to their paid services. peer subscribers can influence their friends to subscribe for premium account.
# 3) Highnote has more chances to obtain a new subscriber if they target a male in his late 20s or 30s who is not from US, UK or Germany.
# 
