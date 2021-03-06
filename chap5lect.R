#  This follows Chapter 5 in r4ds

library(dplyr)

# install.packages("nycflights13")
library(nycflights13)
library(ggplot2)

flights
?flights
flights[,5:10]
colnames(flights)
str(flights)

# Six key "verbs" in dplyr:

# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() 

# template:
# verb(dataFrame, action) produces a new data frame


#########################
####  filter ############
#########################


# Select all flights on January 1st!
flights

# lots of old ways to do this.  here is what I would have done after learning about which:
flights[which(flights[,2] == 1 & flights[,3]==1),]
# this is fast, but syntax is awkward to read and type.

# before learning about which, I would have written a for loop that goes through 
#   row by row and checks whether the row meets the conditions.  

# with dplyr
filter(flights, month == 1, day == 1)

# Filter collects a subset of the rows. Filter->rows.
# filter is SUPER USEFUL.

jan1 = filter(flights, month == 1, day == 1)

# do you know about :
#   <
#   <=
#   >=
#   !=
#   ==

sqrt(2)^2 == 2# WHAT??
sqrt(2)^2
sqrt(2)^2/2
sqrt(2)^2-2
near(sqrt(2)^2,2)

near(10^(-7), 0)
near(10^(-8), 0)

# Boolian operations!
#   |
#   &
#   !
#   xor

filter(flights, month == 11 | month == 12)
filter(flights, day ==29 , month ==2 | carrier == "HA")

# Whenever you start using complicated, 
# multipart expressions in filter(), 
# consider making them explicit variables instead. 
# That makes it much easier to check your work. 
# Youâ€™ll learn how to create new variables shortly.


filter(flights, between(day, 1,5), month ==2)

table(filter(flights, between(day, 1,5), month ==2)$day)
table(filter(flights, between(day, 1,5), month ==2)$month)


# to practice, do problems:
# r4ds 5.2.4: 1 (all subparts) and 3.

#########################
####  arrange ###########
#########################

## changes the order of rows.  
#  I find this function to be far less useful.
#     arrange --> rows.

arrange(flights, year, month, day)
arrange(flights, month, day, desc(arr_delay))
arrange(flights, desc(arr_delay))

arrange(flights, desc(is.na(arr_delay)))


#########################
####  select ############
#########################

## keep only a group of variables/columns.
#   I find this function to be moderately useful.


select(flights, arr_time, sched_arr_time, arr_delay)

####  Most useful when there are 100's or 1000's of variables!

# There are a number of helper functions you can use within select():
#   
# starts_with("abc"): matches names that begin with â€œabcâ€.
# 
# ends_with("xyz"): matches names that end with â€œxyzâ€.
# 
# contains("ijk"): matches names that contain â€œijkâ€.
# 
# matches("(.)\\1"): selects variables that match a regular expression. This one matches any variables that contain repeated characters. Youâ€™ll learn more about regular expressions in strings.
# 
# num_range("x", 1:3) matches x1, x2 and x3.

select(flights, contains("arr"))
select(flights, contains("arr_"))

rename(flights, tail_num = tailnum)

# change order of rows:
select(flights, time_hour, air_time, everything())

select(flights, contains("arr_") , contains("dep"))

select(flights, year:dep_time)

#########################
####  mutate ############
#########################

# create new variables!  mutate --> columns.

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours   #  <<<< you can reference variables that have alredy been made!
)

# how is transmute different? #keeps only new variables
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

# mutate allows for lots of different operations.  
#  I find it is best to just try.  
#  However, sometimes it won't act as desired... 
#  see here for more:  http://r4ds.had.co.nz/transform.html#mutate-funs


#########################
####   %>%   ############
#########################

# %>% is called the pipe operator.  
# Rstudio has a shortcut command+shift+m  to make it.

# recall: 
filter(flights, month == 1, day == 1)

# equivalent:
flights %>% filter(month == 1, day == 1)

# it puts the left hand side (LHS) into 
#  the *first* argument of the function on the right hand side (RHS)
# *should* work with any function.

rnorm(100) %>% sd
rnorm(100) %>% hist

# *magic* comes from putting several (dplyr) functions together 
# just a start:

flights %>% filter(between(day, 23,26), month ==12) %>% arrange(distance)

# we will talk about %>% much more.


#########################
####   group_by  ########
####   summarize ########
#########################

# summarize takes a tibble or data frame and simplifies it to one row. 
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# it is most interesting when used with group_by.
# group_by "changes the unit of analysis" from one row into one group.
#  So, functions will be performed on the groups.

flights %>% group_by(origin) %>% summarize(mean(arr_delay, na.rm = T))
flights %>% group_by(origin) %>% summarize(delay = mean(dep_delay, na.rm = T), 
                                           missing_delay = mean(is.na(dep_delay)))

flights %>% group_by(origin) %>% summarize(mean(is.na(arr_delay)))
flights %>% group_by(origin) %>% summarize(mean(is.na(dep_delay)))


flights %>% group_by(origin) %>% summarize(mean(arr_delay + dep_delay, na.rm = T))
flights %>% filter(between(day, 23,26), month ==12) %>%
  group_by(origin) %>% summarize(mean(arr_delay + dep_delay, na.rm = T))


# this line of code adds the departure date in a nice format
library(lubridate) # see Chapter 16 of r4ds!
# for now, don't worry about the syntax...
flightd = flights %>% 
  # select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute)) 

flightd %>% 
  select(time_hour, departure)

# what is this next line doing? 
flights %>% mutate(dayWeek = wday(time_hour)) %>% 
  group_by(dayWeek)%>% summarize(mean(arr_delay, na.rm = T)) %>% 
  plot

flights %>% filter(origin == "EWR") %>% mutate(dayWeek = wday(time_hour)) %>% 
  group_by(dayWeek)%>% summarize(mean(arr_delay, na.rm = T)) %>% 
  plot

flights %>% filter(origin == "EWR") %>% mutate(dayWeek = yday(time_hour)) %>% 
  group_by(dayWeek)%>% summarize(mean(arr_delay, na.rm = T)) %>% 
  plot(type = 'l')



##### Here are some examples without using %>% pipe:

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")
plot(delay[,3:4])

#Or, prettier with ggplot2:
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

### With the pipe:

flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL") %>%
  ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# what do you think "HNL" is in the filter?  fiddle to get a guess.  how else might you figure that out?


# Another example:

flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  ) %>% 
  ggplot(mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
#  what is going on with that tail distribution ?!!  planes with avg delay = 5 hours!
#  to understand more, make a scatter plot (instead of a histogram).
#  First....
# we have used this a lot:   
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
# probably should just make it a variable so we don't have to always recompute...


not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  ggplot(mapping = aes(y = n, x = delay)) + 
  geom_point()
# what does this show?  This highlights 2+ key concepts... 
# n vs var 
# summaries are dangerous


not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) %>% 
  filter(n>25) %>% 
  ggplot(mapping = aes(y = n, x = delay)) + 
  geom_point()


# logical subsetting still works!
not_cancelled %>% 
  group_by(origin) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% 
  group_by(origin) %>% 
  summarise(
    quantiles = quantile(arr_delay, probs = seq(.1,.9,by =.1))
  )  #  dplyr doesn't do everything that you want... what would be a way to do "this"?

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    doy = mean(yday(time_hour)),
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  ) %>% 
  ggplot(mapping = aes(x = doy, y = last_dep)) + geom_line()

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    doy = mean(yday(time_hour)),
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  ) %>% 
  # ungroup %>% 
  filter(last_dep == min(last_dep))  


not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)


not_cancelled %>% 
  count(tailnum, wt = distance) %>% 
  mutate(n = log(n)) %>% 
  arrange(desc(n))

# When you group by multiple variables, 
# each summary peels off one level of the grouping. 
# That makes it easy to progressively roll-up a dataset:
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))



# In class exercise:
# Look at the number of cancelled flights per day. 
# Is there a pattern? Is the proportion of cancelled flights related 
# to the average delay?

# If time:
# Which carrier has the worst delays? 
# Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
# Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))