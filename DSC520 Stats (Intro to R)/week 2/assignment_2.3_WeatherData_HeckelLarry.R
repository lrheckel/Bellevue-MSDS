#  Assignment: Assignment 2.3 Weather Data
#  Name: Heckel, Larry
#  23 March 2019
# 
# 1. Choose one of the Weather datasets [AKWeather or NEWeather] to use for this activity.   
#     a. What categories/variables of data do you have in your Weather dataset?
#
#         The data is a time series of weather data by day. Each observation contains
#           the weather data for that day. There are 3 variables, all quantitative.
#           They are Average Wind Speed, and Max and Min Temperature.
#
#     b.  Use the following R functions: View(); str(); nrow(); ncol() to investigate 
#         the dataset you are working with. Report the output from each function 
#         and provide a brief explanation of the output. 
#           1. View -- Provides a grid view of the data, similar to a spreadsheet.
#           2. str -- Provides the structure of the object called. In this case, it 
#               shows that we have a data.frame with 1,519 rows/observations and 6 columns.
#               It also gives the name, data type, and sample values for each data column.
#           3. nrow -- Provides the number of rows of data, and does not count the header
#               rows. There are actually 1,520 rows in the csv file, with the first row
#               being the header, and the remaining 1,519 rows of data.
#           4. ncol -- Provides the number of columns in the data set.
#         Remember the following resources, outside of R Studio can be used for additional help:
#         Google – Just add R to the end of your search criteria
#         R Bloggers - https://www.r-bloggers.com/
# 
# 2. For the Weather dataset you chose, what levels of measurement are included?
#         
#         There are 2 measurement levels in the data. 
#           First, the average wind speed isa continuous ratio, as the ratios 
#           of the wind speeds are relevant measures. For example, a wind speed 
#           of 10 mph is twice as fast as one of 5 mph.
#           
#           Second, the min/max temperatures are continuous intervals, in that the 
#           ratios of the two numbers don't yield useful values, but the intervals 
#           betweeen the numbers are equivalent (a 10 degree difference from 30 to 40
#           degrees is the same as that between 70 and 80 degrees). The date is also
#           a continuous interval, as the time from day to day is the same.
#           
# 3. Create a few vectors from your dataset and perform each of the following operations:
#      a. max(x)
#           The max function yields the highest (maximum) value for the vector evaluated.
#           For wind, it is 34 mph, meaning that during the 4+ years of wind measurements,
#             the wind never exceeded that speed.
#           For Max Temp, it is 102 degrees, meaning that the hottest day in the 4+
#             years reached that temperature.
#           For Min Temp, it is 78 degrees, meaning that the highest daily low temperature
#             was 78 degrees.       
#      b. min(x)
#           The min function yields the lowest (minimum) value for the vector evaluated.
#           For wind, it is 2.01 mph, meaning that during the 4+ years of wind measurements,
#             the wind blew every day at least to that speed.
#           For Max Temp, it is -1 degrees, meaning that the coldest high temperature
#             day in the 4+ years was that temperature.
#           For Min Temp, it is -17 degrees, meaning that the lowest temperature during
#             this time was -17 degrees.
#      c. sum(x)
#      d. mean(x)
#           The mean is simply the average value.
#           For wind, this is 11.12 mph.
#           For Max Temp, this is 62.86 degrees.
#           For Min Temp, this is 39.36 degrees.
#      e. median(x)
#           The mean is the value for which there are is an equal count of values both
#             higher and lower than this value. It is the "center value" of the data set,
#             when ordered from high to low or low to high.
#           For wind, this is 10.51 mph.
#           For Max Temp, this is 67 degrees.
#           For Min Temp, this is 39 degrees.
#           
#           Looking at the mean and median together yields a bit more information about
#             the data set than the values by themselves. If the two values are relatively
#             close to each other, than the data set can be said to be fairly normally
#             distibuted, but when they are significantly different, then the data
#             is skewed in some way, high or low. Wind speed and min temperature seem
#             to be fairly normally distibuted, although wind speed has a bit of a long
#             tail out to 34 mph.
#             
#           Max Temp, on the other hand, has a difference of just over 4 degrees between
#             the two values, with Median higher. What that tells us is that the data is
#             skewed a bit toward the higher temperatures. In practical terms, the region
#             has more warmer days, but the colder days, while fewer, are colder than
#             the warmer days are warm. The difference between the median and max temps
#             is only 35 degrees, while the difference is 68 to the lowest max temp.
#      f. range(x)
#      g. var(x)
#      h. sd(x)
#      Choose 4 of the operations above and provide an explanation 
#      of the result for each.
############################################

#read the csv file. Assume located in working/project directory
NEWeather <- read.csv("neweather.csv")

View(NEWeather)
str(NEWeather)
nrow(NEWeather)
ncol(NEWeather)

wind <- c(NEWeather$AWND)
maxTemp <- c(NEWeather$TMAX)
minTemp <- c(NEWeather$TMIN)

summary(wind)
max(wind)        
min(wind)
sum(wind)
mean(wind)
median(wind)
range(wind)
var(wind)
sd(wind)

summary(maxTemp)
max(maxTemp)        
min(maxTemp)
sum(maxTemp)
mean(maxTemp)
median(maxTemp)
range(maxTemp)
var(maxTemp)
sd(maxTemp)

summary(minTemp)
max(minTemp)        
min(minTemp)
sum(minTemp)
mean(minTemp)
median(minTemp)
range(minTemp)
var(minTemp)
sd(minTemp)

