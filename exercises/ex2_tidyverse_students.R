###############################################################
# Data Wrangling with Tidyverse (dplyr, tidyr)
###############################################################
# The Tidyverse suite of integrated packages are designed to work together to make common data science operations more user friendly. The packages have functions for data wrangling, tidying, reading/writing, parsing, and visualizing, among others.
# Tidyverse suite of packages create and use data structures, functions and operators to make working with data more intuitive. The two most basic changes are in the use of pipes and tibbles.
# We will explore the basic syntax for working with these packages, as well as, specific functions for data wrangling with the ‘dplyr’ package, data tidying with the ‘tidyr’ package, and data visualization with the ‘ggplot2’ package.

###############################################################
# load needed packages
# use install.packages() e.g. install.packages("nycflights13") if you do not have that specific library
library(tidyverse)
library(nycflights13)   # loads flights dataset. To get more info about it -- use ?flights
###############################################################

####### Pipes #######
# In base R, to string different commands together one has to nest them one inside another. E.g
round(sqrt(83), digit = 2)
# Stringing together commands in R can be quite daunting -- reading such lines becomes confusing (especially when more commands are used) because one needs to start reading it from the middle. Writing such commands also is quite uncomfortable.
# To make R code more human readable, the Tidyverse tools use the pipe, %>%, which was acquired from the ‘magrittr’ package and comes installed automatically with Tidyverse. The pipe allows the output of a previous command to be used as input to another command instead of using nested functions. E.g.
sqrt(83) %>% round(digit = 2)


####### ggplot2 #######
# we will use iris dataset for short intro about ggplot:
str(iris)

# basic ggplot form:
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# e.g.: 
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
# and then we can build on it further
# e.g. color by species
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()


###############################################################
####### dplyr functions overview #######
# dplyr package has few key functions that allow you to solve the vast majority of your data manipulation challenges:
# filter() picks rows based on their values.
# arrange() changes the ordering of the rows.
# select() extracts columns and returns a tibble.
# mutate() adds new variables that are functions of existing variables.
# summarise() reduces multiple values down to a single summary.
# group_by() performs functions on groups defined by variables
# rename() easily changes the name of a column(s)
# pull() extracts a single column as a vector.
# _join() group of functions that merge two data frames together, includes (inner_join(), left_join(), right_join(), and full_join()).
###############################################################

# Q1: what class/classes does flights data belong to?
# Tibbles are data frames, but slightly tweaked to work better in the tidyverse. As mentioned before -- different packages have their own classes for R data objects. 
# as you can see with str() function -- tibble and data.frame structure is essentially the same:
str(flights)
as.data.frame(flights) %>% str()
# and therefore what works with data.frame class is also applicable to this dataset e.g. subseting with numeric or logical vectors. However, class "tbl" allows the use of specific functions that ease data analysis. 
# NOTE: one of convenient tibble features is how it is printed in the console (especially for larger datasets). Compare:
flights
as.data.frame(flights)

# Q2: how many columns/rows are in flights dataset
# Q3: get summary statistics about each of the columns (use function summary()). How many columns contain NA values?
# it is always good to note if we have NA values in the data. NA is a special case and you can get unexpected results if you ignore NA and behave as of they are not present at all
# e.g. find mean air_time 
mean(flights$air_time)   
# find an argument to ignore NA cases in mean function


####### filter #######
# The first argument is the name of the data frame. The second and subsequent arguments are the expressions that filter the data frame. For example, we can select all flights on January 1st with:
filter(flights, month == 1 & day == 1)  # 842 flights
flights # 336776 flights
#When you run that line of code, dplyr executes the filtering operation and returns a new data frame. dplyr functions never modify their inputs, so if you want to save the result, you’ll need to use the assignment operator "<-"
dTest <- filter(flights, month == 1 & day == 1)
norw(dTest)   
# filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. If you want to preserve missing values, ask for them explicitly:
filter(flights, arr_time == 1)  %>% nrow()
filter(flights, is.na(arr_time)) %>% nrow()
filter(flights, is.na(arr_time) | arr_time == 1) %>% nrow()

# Q4: Find all flights that:
# Had an arrival delay of two or more hours
# Flew to Houston (IAH or HOU)
# Were operated by United, American, or Delta (for this you will need to look up carrier column value meanings)
# Departed in summer (July, August, and September)
# Arrived more than two hours late, but didn’t leave late
# Departed between midnight and 6am (inclusive) (NOTE: midnight is 2400, not 0)

####### filter #######
# arrange() works similarly to filter() except that instead of selecting rows, it changes their order. It takes a data frame and a set of column names (or more complicated expressions) to order by. If you provide more than one column name, each additional column will be used to break ties in the values of preceding columns:
# e.g. order by departure delay time
arrange(flights, dep_delay)
arrange(flights, desc(dep_delay))  # to see flights what were delayed the most

# Q5: find flights that had largest arrival delay. Write the same command in base R

####### select #######
# first argument for select is the name of the data.frame, followed by comma separated column names. They can be unquated or with quotes. 
select(flights, year, month, day)
select(flights, "year", "month", "day")
# select function also allows to select a range of columns using ":" and column names as if they were numbers
select(flights, dep_time:flight)
select(flights, -c(dep_time:flight))  # selection inversion with "-" sign (like for integer vectors) also works
# In addition to that there is a number of helper functions you can use within select():
# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# matches("(.)\\1"): selects variables that match a regular expression. This one matches any variables that contain repeated characters.
# num_range("x", 1:3): matches x1, x2 and x3.
# See ?select for more details. See if you understand all of them
# e.g. Maybe we are interested only in columns with word "TIME"
select(flights, contains("TIME"))   # NOTE: contains function is not sensitive to case. Look up ?contains to see what additional argument is needed to change that
select(flights, contains("TIME", ignore.case = FALSE))
# or starts with a "dep"
select(flights, starts_with("dep"))

# Q6: compare ways to select select year, origin and dest columns from from flight dataset using dplyr::select and base R. 
# write two possible ways how to select specified columns using base R (using integer and logic vectors)
# Q7: What happens if you include the name of a variable multiple times in a select() call?
# Q8: What does the one_of() function do? Why might it be helpful in conjunction with this vector?
# Q9: Select can be conveniently used to put specific columns to the beggining/end of the table.
select(flights, origin, everything())  # puts origin column as first and then prints out all the other variables
# write a line that puts origin column in between year and month columns
# Q10: Which flights travelled the shortest distance? modify output to print out distance as first column, flight as second and then everything else. Use %>% to combine two functions


####### mutate #######
# mutate() allows us to create new columns. mutate() always adds new columns at the end of your dataset so we’ll start by creating a narrower dataset so we can see the new variables.
dFlightsSmall <- select(flights, 
                        year:day, 
                        ends_with("delay"), 
                        distance, 
                        air_time
)
# will create several new statistics about each flight
mutate(dFlightsSmall,
       speed = distance / air_time * 60,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
# use transmute to retain only new columns
transmute(flights,
          speed = distance / air_time * 60,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
# There are many functions for creating new variables that you can use with mutate(). The key property is that the function must be vectorised: it must take a vector of values as input, return a vector with the same number of values as output. There’s no way to list every possible function that you might use, but here’s a selection of functions that are frequently useful:
# * Arithmetic operators: +, -, *, /, ^
# * Modular arithmetic: %/% (integer division) and %% (remainder), where x == y * (x %/% y) + (x %% y)
# * Logs: log(), log2(), log10()
# * Offsets: lead() and lag() allow you to refer to leading or lagging values. This allows you to compute running differences (e.g. x - lag(x)) or find when values change (x != lag(x))
# * Cumulative and rolling aggregates: R provides functions for running sums, products, mins and maxes: cumsum(), cumprod(), cummin(), cummax(); and dplyr provides cummean() for cumulative means.
# * Logical comparisons, <, <=, >, >=, !=, == and also %in%, & |
# * Ranking: there are a number of ranking functions, but you should start with min_rank()

# Q11: Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
# HINT: use modular aritmetics 
# HINT: midnight will be 24*60 = 1440 instead of 0 that we want. As it is the highest possible value we have if divide values by modulo operator %% with 1440 value, then only 1440 will be 0 and all other values will remain the same. We can use that to deal with midnight value 
flights <- mutate(flights, 
                  dep_time = (dep_time %/% 100 * 60  + dep_time %% 100) %% 1440,
                  sched_dep_time = (sched_dep_time %/% 100 * 60  + sched_dep_time %% 100) %% 1440)

####### summarise #######
# the last key verb is summarise(). It collapses a data frame to a single row:
summarise(flights, 
          dep_delay = mean(dep_delay, na.rm = TRUE), 
          arr_delay = mean(arr_delay, na.rm = TRUE))
# summarise() is not terribly useful unless we pair it with group_by(). This changes the unit of analysis from the complete dataset to individual groups. Then, when you use the dplyr verbs on a grouped data frame they’ll be automatically applied “by group”. For example, if we applied exactly the same code to a data frame grouped by date, we get the average delay per date:
dSummary <- flights %>%
  group_by(year, month, day) %>%  
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE), 
            arr_delay = mean(arr_delay, na.rm = TRUE))
# If you need to remove grouping, and return to operations on ungrouped data, use ungroup() 


ggplot(dSummary, aes(dep_delay, arr_delay)) +
  geom_point()












###############################################################
# tidyverse is a very usefull suite of packages and dplyr improves the flow of data transformation a lot. 
# However, one dplyr functions get slow with large dataset. A popular alternative for dplyr is data.table package. It works very fast with large datasets and has a very concise but not very hard to read syntax. 
# I do not want to mix you with all the different ways to write the code. However, I am used to it and am most likely to use it for any data manipulation tasks in R ... (habits are hard to beat)
# if you are intrested you can check out:
# data.table cheatsheat from https://www.rstudio.com/resources/cheatsheets/ (also present in github page)
# intro tutorial (quite exhoustive)
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# more about reshaping data from long to wide formats
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html
# advanced tips and tricks
# http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/   