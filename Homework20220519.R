# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

#####Problem 1#####
# Download data for a stock of your choice and do the following:
# Calculate the 20 day SMA of the stock price and define upper and
# lower bounds around it which are equal to SMA +-2 standard deviation
# the past observations used to calculate the SMA.
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the price goes above the upper bound - sell.
# If the price goes below the lower bound - buy.
#####Problem 1#####

library(tidyverse)
library(tidyquant)
library(lubridate)

data <- tq_get("AAPL",
               from = "2021-01-01",
               to = "2022-05-01" )

dates <- data.frame(Date = seq.Date(from = ymd("2021-01-01"), to = ymd("2022-05-01"), by = "day"))

fin_data <- dates%>%
  left_join(data,by = c("Date"= "date"))%>%
  select(Date, symbol, adjusted)%>%
  fill(c(symbol, adjusted), .direction = "downup")

fin_data1<-fin_data%>%
  mutate(SMA20 = SMA(adjusted, n = 20),
         SD20 = RcppRoll::roll_sd(adjusted, n =20, align = "right", fill = NA),
         Upper = SMA20+2*SD20,
         Lower = SMA20-2*SD20,
         buy_sell = case_when(lag(adjusted)<lag(Upper) & adjusted > Upper ~ "sell",
                            lag(adjusted)> lag(Lower) & adjusted<Lower~"buy"))%>%
  mutate(ProfitCoeff = (adjusted/lag(adjusted)),
         ProfitCoeff = ifelse(is.na(ProfitCoeff), 1, ProfitCoeff),
         BenchmarkMoney = 100 * cumprod(ProfitCoeff),
         buy_sell_for_comparison = buy_sell)%>%
  fill(buy_sell_for_comparison, .direction = "down")%>%
  mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison), "buy", buy_sell_for_comparison),
         ProfitCoeff_strategy = case_when(buy_sell_for_comparison == "sell"~ 1,
                                          buy_sell_for_comparison == "buy"~ ProfitCoeff),
         StrategyMoney = 100 * cumprod(ProfitCoeff_strategy)) 

#In the last mutate i tried to assign the value for the profit coeff in the case_When(),
#but it did not accept the case for NA and kept on returning NA in the beggining:
#ProfitCoeff_strategy = case_when(buy_sell_for_comparison == NA ~ ProfitCoeff
#                                 buy_sell_for_comparison == "sell"~ 1,
#                                 buy_sell_for_comparison == "buy"~ ProfitCoeff
#Why could that be?



#####Problem 2#####
# Calculate the RSI using the instruction about the formula from here:
# https://www.investopedia.com/terms/r/rsi.asp
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the RSI above 65 - sell.
# If the price goes below 35 - buy.

#####Problem 2#####

fin_data2 <- fin_data %>%
  mutate(gain_loss = (adjusted/lag(adjusted))-1,
         gain_loss = if_else(is.na(gain_loss), 0, gain_loss),
         average_gain1 = case_when(gain_loss >= 0 ~gain_loss, 
                                   gain_loss <0 ~ 0),
         average_gain = SMA(average_gain1, 14),
         average_loss1 = case_when(gain_loss <= 0 ~abs(gain_loss), 
                                   gain_loss >0 ~ 0),
         average_loss = SMA(average_loss1, 14),
         RSI1 = 100-(100/(1+(average_gain/average_loss))),
         RSI = 100-(100/(1+((13*lag(average_gain)+ average_gain1)/(13*lag(average_loss)+average_loss1)))),
         buy_sell = case_when(RSI > 65 ~ "sell",
                              RSI < 35 ~ "buy"),
         ProfitCoeff = adjusted/lag(adjusted),
         ProfitCoeff = if_else(is.na(ProfitCoeff), 1, ProfitCoeff),
         BenchmarkMoney = 100 * cumprod(ProfitCoeff),
         buy_sell_for_comparison = buy_sell)%>%
  fill(buy_sell_for_comparison, .direction = "down")%>%
  mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison), "buy", buy_sell_for_comparison),
         ProfitCoeff_strategy = case_when(buy_sell_for_comparison == "sell"~ 1,
                                          buy_sell_for_comparison == "buy"~ ProfitCoeff),
         StrategyMoney = 100 * cumprod(ProfitCoeff_strategy))
  
#Upload your homeworks on your own github repo and send me an email, when you are done.
