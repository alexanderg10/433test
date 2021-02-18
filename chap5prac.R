library(tidyverse)
library(nycflights13)
flights
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
jan1
dec25 <- filter(flights, month == 12, day == 25)
#filter(flights, month = 1)
filter(flights, month == 1)
#computer uses approximation
sqrt(2) ^ 2 == 2
#> [1] FALSE
1 / 49 * 49 == 1
#> [1] FALSE
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE
#logical
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120) #same things


