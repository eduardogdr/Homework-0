# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

#Question 1
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N<-1500
p*N           #expected total number of voters in the sample choosing "Remain"

p             #the expected value of  X^ , the proportion of "Remain" voters
 

x <- sample(c(1,0), size = N, replace = TRUE, prob = c(p, 1-p))

X_hat<-mean(x) #total number of voters "Remain" from sample
X_hat
se_hat<-sqrt(X_hat*(1-X_hat)/N)  #the standard error of  X^ , the proportion of "Remain" voters
se_hat
se_hat*N          #the standard error of the total number of voters in the sample choosing "Remain"

se_spread<-2*sqrt(p*(1-p)/N) #the standard error of d, the spread between the proportion of "Remain" voters and "Leave" voters
se_spread

#Question 2
head(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

#Question 3
p <- 0.481 
Yougov<-brexit_polls[1,]$x_hat
Yougov
se_hat3<-sqrt(p*(1-p)/brexit_polls[1,]$samplesize)
se_hat3
alpha <- 1-.95
lower <-qnorm(alpha)
upper <-qnorm(1-alpha)
Yougov - 1.96*se_hat3
Yougov + 1.96*se_hat3
qnorm(1- 0.05/2)
qnorm(.975)



#Question 4
d<-(???0.038)

june_polls        
nrow(june_polls) # i)How many polls

#iii
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")%>%
                               mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
                               se_spread4=2*se_x_hat,
                               upper=spread + qnorm(0.975)*se_spread4,
                               lower=spread - qnorm(0.975)*se_spread4,
                               hit=upper>=0 & lower>= 0)%>% summarize(mean(hit))
#ii
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")%>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread4=2*se_x_hat,
         upper=spread + qnorm(0.975)*se_spread4,
         lower=spread - qnorm(0.975)*se_spread4,
         hit=upper>=0 & lower<= 0)%>% summarize(mean(hit))

#iV
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")%>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread4=2*se_x_hat,
         upper=spread + qnorm(0.975)*se_spread4,
         lower=spread - qnorm(0.975)*se_spread4,
         hit=upper>=d & lower<=d) #%>% summarize(mean(hit))
june_polls

#Q5


june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")%>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread4=2*se_x_hat,
         upper=spread + qnorm(0.975)*se_spread4,
         lower=spread - qnorm(0.975)*se_spread4,
         hit=upper>=d & lower<=d) %>% group_by(pollster) %>% summarise(N=n(),h=mean(hit)) %>% arrange(h)
june_polls
 

#Q6
june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")%>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread4=2*se_x_hat,
         upper=spread + qnorm(0.975)*se_spread4,
         lower=spread - qnorm(0.975)*se_spread4,
         hit=upper>=d & lower<=d) %>% ggplot(aes(poll_type,spread)) + geom_boxplot()
june_polls

#Q7 & Q8

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            lower7 = spread -qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),
            upper7= spread +qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N))
combined_by_type

#Q9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
        select(poll_type, hit)

two_x_two<-table(brexit_hit)
two_x_two
two_x_two<-table(brexit_hit)%>%chisq.test()

#Q10

online_true <- two_x_two[[1,2]]/two_x_two[[1,1]]
online_true

telephone_true<-two_x_two[[2,2]]/two_x_two[[2,1]]
telephone_true

oddsR <-online_true/telephone_true
oddsR

#Q11

brexit_polls %>% ggplot(aes(enddate,spread, col=poll_type))+
                  geom_point()+
                  geom_smooth(method = "loess",span = 0.4)+
                  geom_hline(yintercept = -0.038)

#Q12

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))%>% ggplot(aes(enddate,proportion,col=vote))+
                                 geom_point()+
                                 geom_smooth(method = "loess",span = 0.3)
brexit_long