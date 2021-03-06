#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####

#Works for integers only and single numbers(not vectors as the original function does)
FactorialFunction <- function(InputNumber){
   InputNumber<- as.integer(InputNumber)
   for(k in 1:(InputNumber-1)){
    if(InputNumber > 0){
     InputNumber <- as.numeric(InputNumber*k) 
     Result <- InputNumber
    } else if (InputNumber == 0){
      Result <- 1
    } else{
      Result <- NaN
    }
  }
  return(Result)
}


#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####

#How is the Standard deviation formed?
#The Variance is determined as an average of the squared differences from the mean. To calculate the variance, follow these steps:
#First, calculate the mean, which is an average of the numbers.
#Second, for each number: subtract the Mean and square the result (the squared difference).
#In the last step, check out the average of those squared differences.


inputVector<-c(2,5,7,35,78,99)

SDFunction <- function(inputVector){
  Average <- mean(inputVector, na.rm = T)
  SquaredDifference<-(inputVector- Average)^2
  Average2 <- sum(SquaredDifference, na.rm = T)/(length(SquaredDifference)-1) #looked it up on the internet and saw that it is like that in, originally was mean(SquaredDifference)
  SD <- sqrt(Average2)
return(SD)
}

#####Problem 3#####
# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7

#Do all the exercises:

#####Problem 3#####

library(nycflights13)
library(tidyverse)

# 5.6.7 Exercises

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
####1.Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:####
#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
#A flight is always 10 minutes late.
#A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
#99% of the time a flight is on time. 1% of the time it’s 2 hours late.
#Which is more important: arrival delay or departure delay?####
  
#I saw the solution but I don't think I totally get it

####2.Come up with another approach that will give you####
#the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count())

#original
not_cancelled %>% 
  count(dest)
#alternative
not_cancelled%>%
  group_by(dest)%>%
  summarise(n())
#original
not_cancelled %>% 
  count(tailnum, wt = distance)
#alternative
not_cancelled%>%
  group_by(tailnum)%>%
  summarise(sum(distance))

####3.Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column?####

#If dep_delay is NA, it means that this flight is cancelled. Subsequently it will not have an arrival time as well.

####4.Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?####
cancelled <-flights%>%
  filter(is.na(dep_delay))%>%
  group_by(month, day)%>%
  summarise(CancelledPerDay = n())
  

ggplot(data = cancelled, mapping = aes(x = CancelledPerDay)) + 
  geom_freqpoly(binwidth = 10)

ggplot(data = cancelled, mapping = aes(x = CancelledPerDay, y = month)) + 
  geom_point(alpha = 1/10)



DelayAvg <- not_cancelled%>%
  group_by(month,day)%>%
  summarise(AverageDelay=mean(dep_delay))

ggplot(data = DelayAvg, mapping = aes(x = AverageDelay)) + 
  geom_freqpoly(binwidth = 10)
  
ggplot(data = DelayAvg, mapping = aes(x = AverageDelay, y = month)) + 
  geom_point(alpha = 1/10)

#5.Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not?####
#(Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
  

CarrierDest <- flights %>% 
  group_by(carrier, dest) %>%
  summarise(NrFlights = n(), AverageDelay = round(mean(dep_delay, na.rm = T), 2))%>%
  mutate(AvgDelayPerFlight = round((AverageDelay/NrFlights),2))

CarrierDest%>%
  filter(NrFlights > 200)%>%
ggplot(CarrierDest, mapping = aes(x = NrFlights, y = AverageDelay))+
  geom_point()+
  geom_smooth(se = FALSE)

#I think it is not easy to disentangle the effects.
 

#6.What does the sort argument to count() do. When might you use it?####
#If it is set to TRUE, it will essentially order the groups by the total number of observations, or, in other words, the rows in the new count 
#column will be arranged in descending order.

# 5.7.1 Exercises

#1.Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.
#Just every operation starts working on a group basis rather than on the whole data.

#2.Which plane (tailnum) has the worst on-time record?

by_tailnum <- flights %>%
  group_by(tailnum) %>%
  summarise(AverageDelay = mean(dep_delay, na.rm = T),
            .groups = "drop") %>%
  arrange(desc(AverageDelay)) %>%
  slice_head()

#3.What time of day should you fly if you want to avoid delays as much as possible?

when <- flights%>%
  group_by(dep_time)%>%
  summarise(AverageDelay = mean(dep_delay), .groups = "drop")%>%
  arrange(AverageDelay)%>%
  slice_head(n = 10)

#4.For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.

by_dest <- flights%>%
  group_by(dest,distance)%>%
  summarise(TotalDelay = sum(dep_delay, na.rm = T))%>%
  mutate(propDelayDistance = TotalDelay/distance)

#5.Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, 
#later flights are delayed to allow earlier flights to leave. Using lag(), explore how the delay of a flight is related to the delay of the 
#immediately preceding flight.


lag <- flights%>%
  group_by(origin)%>%
  arrange(month, day, dep_time)%>%
  summarise(DiffPrevDelay = dep_delay - lag(dep_delay, rm.na = T))

#6.Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). 
#Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?
  
#7.Find all destinations that are flown by at least two carriers.

dest_carrier <-flights%>%
  group_by(dest)%>%
  summarise(NrCarriers = n_distinct(carrier))%>%
  filter(NrCarriers >= 2)

# Use that information to rank the carriers.

dest_carrier2 <-flights%>%
  group_by(dest)%>%
  count(carrier, name = "NrFlights")%>%
  mutate(NrCarriers = n_distinct(carrier), rank = min_rank(desc(NrFlights)))%>%
  arrange(dest, rank)%>%
  filter(NrCarriers >= 2)

#8.For each plane, count the number of flights before the first delay of greater than 1 hour.

by_flight <- flights%>%
  group_by(tailnum)%>%
  filter(dep_delay <= 60)%>%
  summarise(NrFlights = n())

#####Problem 4#####

#Find the following:

#4.1 For each carrier what is the most common destination?
by_dest <- flights %>%
  select(carrier,dest) %>%
  group_by(carrier) %>%
  count(dest) %>%
  arrange(carrier,desc(n))%>%
  summarise(MaxDest= first(dest), NrFlights = max(n))

#4.2 For each carrier what is the biggest delay?
by_delay <- flights %>%
  select(carrier, dep_delay)%>%
  group_by(carrier)%>%
  summarise(MaxDepDelay = max(dep_delay, na.rm = T))

#4.3 Which are the three plane which have flown the most/least miles?

by_plane <- flights %>%
  select(tailnum, distance)%>%
  group_by(tailnum)%>%
  filter(!is.na(tailnum))%>%
  summarise(TotalDistance = sum(distance, na.rm = T))%>%
  arrange(desc(TotalDistance))%>%
  slice_head(n = 3)
  
by_plane2 <- flights %>%
  select(tailnum, distance)%>%
  group_by(tailnum)%>%
  filter(!is.na(tailnum))%>%
  summarise(TotalDistance = sum(distance, na.rm = T))%>%
  arrange(TotalDistance)%>%
  slice_head(n = 3)

#4.4 What are the first/last flights for each day in February 2013?
by_month <- flights%>%
  select(month, day, flight, carrier, distance, dep_time, dep_delay, arr_delay)

February <- by_month %>%
  select(month, day, flight,dep_time)%>%
  filter(month == 2 & !is.na(dep_time))%>%
  group_by(day)%>%
  summarise(FirstFlight = first(flight), LastFlight = last(flight))

#4.5 Which company flew the most miles in March 2013? 

March <- by_month%>%
  select(month, carrier, distance)%>%
  filter(month == 3)%>%
  group_by(carrier)%>%
  summarise(TotalMiles = sum(distance))%>%
  arrange(desc(TotalMiles))%>%
  slice_head()

#Which flew the least?

March2 <- by_month%>%
  select(month, carrier, distance)%>%
  filter(month == 3)%>%
  group_by(carrier)%>%
  summarise(TotalMiles = sum(distance))%>%
  arrange(TotalMiles)%>%
  slice_head()

#4.6 Which month had the most delays over 60 minutes?

DelaysMonth <- by_month%>%
  select(month, dep_delay)%>%
  filter(dep_delay > 60 & !is.na(dep_delay))%>%
  group_by(month)%>%
  summarise(NrDelays = n())%>%
  arrange(desc(NrDelays))%>%
  slice_head()

#4.7 What is the average time between two consecutive flights?

TimebwFlights <- flights%>%
  select(month,day,flight, dep_time)%>%
  filter(!is.na(dep_time))%>%
  mutate(DepTime= (((dep_time%/%100)*60) + dep_time%%100)%%1440, Difference = DepTime- lag(DepTime))%>%
  summarise(AverageTime = mean(Difference >0, na.rm = T))

#I get negative number when jumping to another day. I used the grouping and got NA instead, but when I group it the summarise works on each group
#and i could not get one result at the end. I tried the .group = "drop" argument,too. But i guess I don't get the hang of it or it does not work.

#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.

sd_by_month <- flights%>%
  select(month, dest, dep_delay)%>%
  group_by(month, dest)%>%
  summarise(SD = round(SDFunction(dep_delay),2))
  
#####Problem 4#####

#Also upload your homeworks on your own github repo.
