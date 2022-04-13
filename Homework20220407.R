# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.
#####Problem 1#####

library(tidyverse)
library(tidyquant)
library(lubridate)

# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.

Data_frame1 <- tq_get(c("AMZN", "FB", "NFLX"), 
       get = "stock.prices",
       from = "2019-01-01", 
       to = "2021-04-01" )%>%
  select(symbol, date, adjusted)

# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.

Dates <- data.frame(Date = DATE_SEQUENCE("2019-01-01",
                                         "2021-04-01",
                                         by = "day" ))

Join <- Dates%>%
  dplyr::left_join(Data_frame1, by = c("Date" = "date"))%>%
  fill(symbol, adjusted, .direction = "down")

# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.

Data_frame2 <- Join%>%
  filter(symbol %in% c("AMZN", "FB"),
         Date %in% seq.Date(from = ymd("2019-01-01"),
                            to = ymd("2019-07-01"),
                            by = "day") | 
         Date %in% seq.Date(from = ymd("2020-04-01"),
                    to = ymd("2020-07-01"),
                    by = "day"))%>%
  arrange(symbol, desc(Date))

 
# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.

Select_first_last <- Data_frame2%>%
  group_by(symbol)%>%
  filter(row_number() == 1 | row_number() == n())

# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

Select_by_month <- Data_frame2%>%
  mutate(month = floor_date(Date, unit = "month"))%>%
  #mutate(year = substr(Date, 1:4,))%>%
  group_by(symbol, month)%>%
  slice_head()

#####Problem 1#####


#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 


# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.
#####Problem 2#####