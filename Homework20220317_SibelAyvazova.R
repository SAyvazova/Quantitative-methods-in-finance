#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.

#####Problem 1#####

#Assuming that when you win you get back the amount you have bet.

####First Part####

outcome = c("Win", "Lose")
probabilities = c(0.486, 0.514)
 Budget <- 100
  Bet <- 1
  loop_test_budget<- NULL
  loop_test_bet<- NULL
for(k in 1:1000){
    Budget <- Budget - Bet
    if (sample(outcome, 1, prob = probabilities) == "Win"){
      Budget <- Budget + Bet
      Bet <-1
    }else {
      if (2*Bet > Budget){
        Bet <- Budget
      }else {
        Bet <- Bet*2
      }
    }
    loop_test_budget<-c(loop_test_budget, Budget)
    loop_test_bet<-c(loop_test_bet, Bet)
}


####Second Part####
  
outcome = c("Win", "Lose")
probabilities = c(0.486, 0.514)
LoopCount2 <-NULL

for(k in 1:1000){
  Budget <- 100
  Bet <- 1
  LoopCount<- NULL
  
  while(Budget > 0){
    LoopCount <- c(LoopCount, Budget)
    Budget <- Budget - Bet
   
    if (sample(outcome, 1, prob = probabilities) == "Win"){
      Budget <- Budget + Bet
      Bet <-1
    }else {
       if (2*Bet > Budget){
       Bet <- Budget
       }else {
        Bet <- Bet*2
      }
    }   
  }

LoopCount2 <- c(LoopCount2, length(LoopCount))

}

AvgRounds <- round(sum(LoopCount2)/length(LoopCount2), 0)


#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html
#Do all the exercises:

library(tidyverse)
library(dplyr)
library(nycflights13)

##### 5.2.4 Exercises#####
#1#
a <- dplyr::filter(flights, arr_delay >= 120)
b <- dplyr::filter(flights, dest %in% c('IAH', 'HOU'))
c <- dplyr::filter(flights, carrier %in% c('UA','AA', 'DL'))
d <- dplyr::filter(flights, month %in% c(7,8,9))
e <- dplyr::filter(flights, arr_delay > 120 & dep_delay <= 0)
f <- dplyr::filter(flights, dep_delay >= 60 & dep_delay-arr_delay > 30)
g <- dplyr::filter(flights, dep_time == 2400 | dep_time <= 600)

#2#
d2 <- dplyr::filter(flights, between(month,7,9))

#3#
missingDepTime <- dplyr::filter(flights, is.na(dep_time))
#we then see that dep_delay, arr_time, arr_delay and air_time have missing values
#which would mean that these missing values represent flights that did not occur.

#4#
#Because anything to the power of 0 equals 1. 
#Because it is an OR operator.
# Maybe it is the TRUE & FALSE = FALSE logic.

##### 5.3.1 Exercises#####
#1#
SortNA <- dplyr::arrange(flights, desc(is.na(dep_time)), dep_time)

#2#
dplyr::arrange(flights, desc(dep_delay))#most delayed
dplyr::arrange(flights, dep_delay)#left earliest

#3#
dplyr::arrange(flights, air_time, desc(distance))


#4#
view(dplyr::arrange(flights, desc(distance)))
view(dplyr::arrange(flights, distance))
                                         
##### 5.4.1 Exercises##### 
#1#
dplyr::select(flights, dep_time, dep_delay, arr_time, arr_delay)
dplyr::select(flights, starts_with("dep"), starts_with("arr"))
dplyr::select(flights, starts_with(c("dep","arr")))

#2#
dplyr::select(flights, year, year, year)
#the repeated column is selected just once.

#3#
vars <- c("year", "month", "day", "dep_delay", "arr_delay", "tail_num") #there is no column tail_num in flights
dplyr::select(flights, any_of(vars))
#any_of selects the columns in vars that actually exist in the flights table without giving an error. If we used all_of instead,
#it would have given an error, because as the name of the command suggests it has to select all of the variables, but if there is
#one that doesn't exist, then it can't do its job.

#4#
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = F))

##### 5.5.2 Exercises##### 
#1#
DepTime <- dplyr::select(flights, dep_time, sched_dep_time)
DepTime <- mutate(DepTime,
       dep_time_midnight = (dep_time %/% 100)*60+(dep_time %% 100),
       sched_dep_time_midnight = (sched_dep_time %/% 100)*60+(sched_dep_time %% 100)
)

#2#
air_time_test <- dplyr::select (flights, air_time, arr_time, dep_time)
  mutate(air_time_test,
       arr_time,
       dep_time,
       air_time_min = (air_time %/% 100)*60 + (air_time %% 100),
       dep_time_min = (dep_time %/% 100)*60+(dep_time %% 100),
       arr_time_min = (arr_time %/% 100)*60 + (arr_time %% 100),
       diff_arr_dep = arr_time_min - dep_time_min
)
  
 
#3#
#dep_delay should be equal to dep_time-sched_dep_time.

#4#
  dplyr::select(flights, desc(flights[1:10,6]))

#5#
1:3 + 1:10
#It gives an warning, because the length of the objects are not the same.
#This means that the shorter one (1:3) will be recycled.

#6#
sin(x)
cos(x)
tan(x)
asin(x)
acos(x)
atan(x)
atan2(y, x) = atan(y/x)
#for x multiple of a half
sinpi(x)
cospi(x)
tanpi(x)

                         
#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html
#####Problem 2#####

#Copy to the recording https://unisofiafaculty.sharepoint.com/:v:/r/sites/AccountingFinanceandDigitalapplicationsSeminargroupI/Shared%20Documents/General/Recordings/Quantitative%20methods%20in%20finance%202022-20220317_180538-Meeting%20Recording.mp4?csf=1&web=1&e=3m90AT