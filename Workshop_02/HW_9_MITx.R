### Homework 9
### Did models and discontinuity exercises
library(readr)
library(tidyverse)

data<-read_csv("data/fastfood.csv")

# run a linear model in which you compare whether there are differences between fast-food restaurants located in NJ and Pennsylvania 
# prior to the change in the minimum wage in terms of the number of full-time employees and the starting wage.

lm(empft~state,data=data) %>% summary
lm(wage_st~state,data=data) %>% summary
4.629-0.021

# let's run the difference e in differences model to see whether the change created a difference in the employment. 
# According to what we saw in the lecture, the model to estimate should be the following: 
# empftit=??0+??1statei+??2postt+??3statei×postt  (equation 1)
# is this the same as the next formulat in a wide format?
# Alternatively, someone has suggested that instead we could run the following model:
# empfti2???empfti1=??0+??1statei+??i  (equation 2)

lm(empft2-empft~ state, data=data) %>% summary

long_data<-data %>% 
  select(state, empft, empft2)%>%  
  gather(time, emp, -state) %>% 
  mutate(post=ifelse(time=="empft",0,1))

lm(emp~state*post, data=long_data) %>% summary

#a: ??1 in eq 2 == ??3statei×postt in eq 1. 


# yearel: election year
# myoutcomenext: a dummy variable indicating whether the candidate of the incumbent party was elected
# difshare: a normalized running variable: proportion of votes of the party in the previous election -  0.5.  
# If   difshare>0  then the candidate runs for the same party as the incumbent. 
library(rdd)
data_pol<-read_csv("data/indiv_final.csv")

data_pol<-data_pol %>% 
  mutate(same_party=(difshare>0))

table(data_pol$same_party)/nrow(data_pol)

rdd::DCdensity(data_pol$difshare, cutpoint = 0,ext.out = TRUE)

new_data_pol<-data_pol %>% 
  # keep only the observations within 50 percentage points of the cutoff (the absolute value of difshare is less than or equal to 0.5).
  filter(abs(difshare)<= 0.5) %>% 
  mutate(same_party=(difshare>=0),
         difshare_2=difshare^2,
         difshare_3=difshare^3) 

m1<-lm(myoutcomenext~ same_party, data=new_data_pol)
summary(m1)
m2<-lm(myoutcomenext~ same_party+difshare, data=new_data_pol)  
summary(m2)
m3<-lm(myoutcomenext~ same_party*difshare, data=new_data_pol)  
summary(m3)
m4<-lm(myoutcomenext~ same_party+ difshare + difshare_2 , data=new_data_pol) 
summary(m4)
m5<-lm(myoutcomenext~ same_party*difshare +same_party*difshare_2 , data=new_data_pol)  
summary(m5)
m6<-lm(myoutcomenext~ same_party+difshare + difshare_2 +difshare_3 , data=new_data_pol)  
summary(m6)

m7<-lm(myoutcomenext~ same_party*difshare + same_party*difshare_2 + same_party*difshare_3 , data=new_data_pol) 
summary(m7)

?RDestimate
RDestimate(myoutcomenext~ difshare,cutpoint = 0,data=new_data_pol) %>% summary
