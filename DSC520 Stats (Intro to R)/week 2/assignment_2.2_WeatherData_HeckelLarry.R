#  Assignment: Assignment 2.2 Weather Data
#  Name: Heckel, Larry
#  23 March 2019

# 1. Which city tended to have the highest temperatures in February?
#       Charleston tended to have the highest temperatures in February.
# 2. Which city tended to have the lowest temperatures? Describe how you can tell.
#       Pierre tended to have the lowest temperatures. One can see that most of the 
#         Pierre temperatures are below 55 degrees, which is where the heavy grouping
#         of Bakersfield temperatures begins,  and 65 degrees, where the Charleston
#         temperatures begin their heavy grouping.
# 3. Which city had the most day-to-day consistency in its high temperatures in February? Which city had the least? Explain how you can tell.
#         Charleston has the most day-to-day consistency in its high temperatures.
#           We can tell fromm the graph by seeing that the temperatures are heavily
#           clustered between 66 and 74 degrees, a spread of 8. For Bakersfield, the
#           same cluster goes from about 58 to 70, a spread of 12. Pierre's spread
#           is fairly evenly distributed between about 25 and 55, a difference of 30,
#           meaning also that Pierre had the least consistency.
#
#         I ran some summary statistics to check my observations, and they numerically
#           verify my visual observations from the graphs. Charleston has the highest
#           mean temperature and Pierre the lowest, and the standard deviations of the 
#           three locations show that Pierre has a greater temperature spread than
#           Bakersfield, which has a greater spread than Charleston.
#
#         One thing that I would like to note is that Bakersfield does not have a recorded
#           value for February 20, so it only has 27 values. We have not yet covered
#           options for dealing with missing values, so I just used the 27 in my analysis.
#
#         Finally, Charleston temperatures were provided from 3 separate locations in and 
#           around Charleston. I chose to keep all of them, concluding that their daily
#           average would provide a good view on temperatures in the greater Charleston
#           area. Another option would have been to only use one of the locations,
#           if temperatures in that specific Charleston area were required.
#
# You have decided to utilize a Stripchart to begin making some 
# recommendations for your friend. 
# 
# Note: An example script has been started for you to use but you will need to 
# modify it to complete the assignment. 
# Begin by opening the Assignment 2 Script.
# 
# Save each of your Plots so a comparison can be made 
# and referenced in your answers.
######################################

#read the csv files. Assume located in working/project directory
Pierre <- read.csv("sdweather.csv")
Charleston <- read.csv("scweather.csv")
Bakersfield <- read.csv("caweather.csv")

View(Pierre)
View(Charleston)
View(Bakersfield)

PierreTemps <- c(Pierre$Tmax)
CharlestonTemps <- c(Charleston$TMAX)
BakersfieldTemps <- c(Bakersfield$TMAX)

stripchart(PierreTemps, method="jitter", 
           main="Pierre, SD February High Temps",
           ylab="Count", xlab="Temperatures")

stripchart(CharlestonTemps, method="jitter", 
           main="Charlseton, SC February High Temps",
           ylab="Count", xlab="Temperatures")

stripchart(BakersfieldTemps, method="jitter", 
           main="Bakersfield, CA February High Temps",
           ylab="Count", xlab="Temperatures")

summary(PierreTemps)
summary(CharlestonTemps)
summary(BakersfieldTemps)

sd(PierreTemps)
sd(CharlestonTemps)
sd(BakersfieldTemps)
