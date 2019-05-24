# Assignment: ASSIGNMENT 3.1
# Name: HIGGINBOTHAM, RALPH (with Larry Heckel)
# Date: 30 MAR 2019

install.packages("ggplot2")
library(ggplot2)

install.packages("pastecs")
library(pastecs)

acs_data <- read.csv("acs-14-1yr-s0201.csv", stringsAsFactors = FALSE)
str(acs_data)
nrow(acs_data)
ncol(acs_data)
View(acs_data)

#Task 1
#The data is a csv consisting of of 136 rows of 8 columns, that is to say it imports as a 
#dataframe of 8 variables and 136 observations for each. The variables are:
#ID: a chr (string) variable
#ID2: an int (integer) variable
#Geography: a chr (string) variable describing county and state
#PopGroupID: an int (integer) variable.  The only value this takes on in the dataset is 1
#POPGROUP.display.label: a chr (string) variable that is set to "Total Population" for all
#   observations in the dataframe (this is probably the text description for the PopGoupID 
#   variable).
#RacesReported: an int (integer) variable
#HSDegree: a num (numeric) variable
#BachDegree: a num (numeric) variable

#Task 2
# > str(acs_data)
# 'data.frame':	136 obs. of  8 variables:
# $ Id                    : chr  "0500000US01073" "0500000US04013" "0500000US04019" "0500000US06001" ...
# $ Id2                   : int  1073 4013 4019 6001 6013 6019 6029 6037 6059 6065 ...
# $ Geography             : chr  "Jefferson County, Alabama" "Maricopa County, Arizona" "Pima County, Arizona" "Alameda County, California" ...
# $ PopGroupID            : int  1 1 1 1 1 1 1 1 1 1 ...
# $ POPGROUP.display.label: chr  "Total population" "Total population" "Total population" "Total population" ...
# $ RacesReported         : int  660793 4087191 1004516 1610921 1111339 965974 874589 10116705 3145515 2329271 ...
# $ HSDegree              : num  89.1 86.8 88 86.9 88.8 73.6 74.5 77.5 84.6 80.6 ...
# $ BachDegree            : num  30.5 30.2 30.8 42.8 39.7 19.7 15.4 30.3 38 20.7 ...
# > nrow(acs_data)
# [1] 136
# > ncol(acs_data)
# [1] 8
# > summary(acs_data$BachDegree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.40   29.65   34.10   35.46   42.08   60.30 
###########################################################################################
#
#HSDegree Variable:
#
# 3. Create a Histogram of the HSDegree variable using the ggplot2 package. 
#     Set a bin size for the Histogram.histSurveyHSD <- ggplot(data=acs_data,aes(x=HSDegree)) + 
histSurveyHSD <- ggplot(data=acs_data,aes(x=HSDegree)) + 
  geom_histogram(binwidth= 2.0, fill = "green")
ggsave("histSurveyHSD.png", width=12, height=8, unit="cm", dpi=300)

# 4. Include a Title and appropriate X/Y axis labels on your Histogram Plot.
histSurveyHSD <- ggplot(data=acs_data,aes(x=HSDegree)) + 
  geom_histogram(binwidth= 2.0, fill = "green") +
  labs(title = "High School Degrees Histogram", 
       x = "% Population with High School Degrees", 
       y = "County Count")
histSurveyHSD
ggsave("histSurveyHSD_labels.png", width=12, height=8, unit="cm", dpi=300)

# 
# 
# 
# 5. Answer the following questions based on the Histogram produced:
#   a. Based on what you see in this histogram, is the data distribution unimodal?
#       Based on the bin width of 2.0, the data distribution appears to be unimodal. However, since
#       HSDegree appears to be a continuous variable, mode is not really an appropriate measure
#       of central tendency.  A manual review of the data reveals that no single value occurs
#       more that a few times in the data set. So the existence of a mode is really just an
#       artifact of the binning.
#   b. Is it approximately symmetrical?
#       It is not symmetrical, in that the lower end of the data has a much
#       longer tail than the upper end.
#   c. Is it approximately bell-shaped?
#       It is approximately bell-shaped, with the addition of the long lower
#       tail on the data.
#   d. Is it approximately normal?
#       It does not appear to be not normal.
#   e. If not normal, is the distribution skewed? If so, in which direction? 
#         Provide specifics in your responses.
#           Yes, the distribution appears to be negatively skewed, in that there are more
#           values spread to the left (lower values) of the mean and median.
# 
# 
#   f. Include a normal curve to the Histogram that you plotted.
histSurveyHSDNorm <- ggplot(data=acs_data,aes(x=HSDegree)) + 
  geom_histogram(aes(y=..density..), binwidth=2.0, fill = "green") +
  stat_function(fun = dnorm, args=with(data=acs_data, c(mean = mean(HSDegree), sd = sd(HSDegree)))) +
  scale_x_continuous("HSDegree") +
  labs(title = "% Population with High School Degrees vs Normal Dist. Curve", 
       x = "% Population with High School Degrees", 
       y = "County Density")
histSurveyHSDNorm
ggsave("histSurveyHSDNorm.png", width=12, height=8, unit="cm", dpi=300) 

# 
# 
# 
#   g. Explain whether a normal distribution can accurately be used as a model 
#       for this data.
#         The normal curve cannot be used to accurately model this data.
#         We can see from the histogram that the data is negatively skewed and
#         and appears to be leptokurtic.
# 
# 6. Create a Probability Plot of the HSDegree variable.
histSurveyHSDProb <- ggplot(data=acs_data,aes(sample=HSDegree)) + 
  stat_qq() + stat_qq_line(col = "Red") + 
  labs(title = "Probability Plot: HSDegree vs Normal Dist.")
histSurveyHSDProb
ggsave("histSurveyHSDProb.png", width=12, height=8, unit="cm", dpi=300)

# 
# 
# 7. Answer the following questions based on the Probability Plot:
#   a. Based on what you see in this probability plot, is the distribution 
#       approximately normal? Explain how you know.
#         The distribution is not approximately normal, because the 
#           data points deviate away from the diagonal at both ends, 
#           and they cross over and back on the diagonal in the center.
#   b. If not normal, is the distribution skewed? If so, in which direction? 
#       Explain how you know.
#           The distribution is negatively skewed, as the graph shows the high
#             number of points curving away from the diagonal line.
# 
# 8. Now that you have looked at this data visually for normality, 
#     you will now quantify normality with numbers using the stat.desc() function.
#     Include a screen capture of the results produced.
library(pastecs)
options(scipen=100)
options(digits=2)
hsDegStats <- stat.desc(acs_data$HSDegree, norm = TRUE)
hsDegStats
View(hsDegStats)

#> hsDegStats
# nbr.val         nbr.null           nbr.na              min              max 
# 136.0000000000     0.0000000000     0.0000000000    62.2000000000    95.5000000000 
# range              sum           median             mean          SE.mean 
# 33.3000000000 11918.0000000000    88.7000000000    87.6323529412     0.4388597852 
# CI.mean.0.95              var          std.dev         coef.var         skewness 
# 0.8679296080    26.1933159041     5.1179405921     0.0584024098    -1.6747666105 
# skew.2SE         kurtosis         kurt.2SE       normtest.W       normtest.p 
# -4.0302539978     4.3528564623     5.2738853364     0.8773635436     0.0000000032 

#
# 
# 9. In several sentences provide an explanation of the result produced for skew, 
#     kurtosis, and z-scores.
#       The value for skewness is -1.67, indicating that the graph is significantly
#         negatively skewed.
#         
#       The kurtosis value of 4.35 indicates that there are a significant number
#         of data points in the tails of the distribution. For a normal curve,
#         the kurtosis value is 3, so the 4.35 value shows that this distribution
#         has a higher number of values in the tails (particularly the left tail)
#         than a normal curve.
#           
#      
#      Since the skew.2SE and kurt.2SE values are the respective skew and kurtosis values divided 
#      by two standard errors, the skew.2SE and kurt.2SE scores are just half the values of 
#      the normalized z-score skew and kurtosis values.  Thus the z-skew is about -8.06 and the
#      kurtosis is about 10.55.  Since the absolute values of these are both greater than 1.96,
#      they are indicative of a non-normal distribution.
        
#     In addition, explain how a change in the sample size may change your 
#     explanation?
#       As sample sizes increase, significant values come up from small deviations (in the data)
#         from normality. As a result, while using 1.96 to compute z-scores is good for smaller
#         sample sizes, we should increase this to 2.58 as sample sizes go above 200, and for
#         even larger sample sizes, we should simply look at the distribution's shape and the
#         skew and kurtosis values directly, rather than calculating their significance with
#         z-scores.
#         
#       So the explanation would need to be more focused on viewing the histogram and comparing
#         it to the normal curve, rather than the z-scores themselves.
#
#
###########################################################################################
#
#BachDegree Variable:
#

#Task 3
B_plot <- ggplot(acs_data, aes(BachDegree)) 
B_plot + geom_histogram(binwidth = 2.5, fill = "blue")
ggsave("histSurveyBach.png", width=12, height=8, unit="cm", dpi=300)

#Task 4 
B_plot + geom_histogram(binwidth = 2.5, fill = "blue") + 
    labs(title = "Bachelor Degree Histogram", x = "% Population with Bachelor Degrees",
         y = "County Count")
ggsave("histSurveyBach_labels.png", width=12, height=8, unit="cm", dpi=300)

#Task 5a: Based on what you see in this histogram, is the data distribution unimodal?
#Yes & No: The histogram seems to show a single mode (the tallest bar).  However, since
#   BachDegree appears to be a continuous variable, mode is not really an appropriate measure
#   of central tendency.  A manual review of the data reveals that no single value occurs
#   more that a few times in the data set. So the existence of a mode is really just an
#   artifact of the binning.

#Task 5b: Is it approximately symmetrical?
#No:  The histogram seems to have higher values on the low side.  Thus it look somewhat
# off center

#Task 5c: Is it approximately bell-shaped?
#Yes, it seems to conform roughly to a bell shape with the exception of the high "mode" bar

#Task 5d: Is it approximately normal?
# No, The distribution seems to have greater number of values on the low side.

#Task 5e: If not normal, is the distribution skewed? If so, in which direction? Provide specifics in your responses.
#Yes,   The histogram seems to be somewhat positively skewed (the mean > median supports this
#       as well)

#Task 5f: Include a normal curve to the Histogram that you plotted.
B_plot +
  geom_histogram(aes(y = ..density..), binwidth = 2.5, fill = "blue") +
  stat_function(fun = dnorm, col ="Red", args = list(mean = mean(acs_data$BachDegree),
                           sd = sd(acs_data$BachDegree))) +
  labs(title = "% Population with Bachelor Degrees vs Normal Dist. Curve", 
       x = "% Population with Bachelor Degrees", 
       y = "County Density")
ggsave("histSurveyBachNorm.png", width=12, height=8, unit="cm", dpi=300)

#Task 5g: Explain whether a normal distribution can accurately be used as a model for this data.
# the histogram does not appear to conform well to a normal distibution curve. Although a large
# number of bars on the histogram more or less conform to the normal curve,  the mode is a
# notable exception.  Thus a normal distribution model may not accurately reflect the data.

#Task 6: Create a Probability Plot of the HSDegree variable.
pp_plot <- ggplot(acs_data, aes(sample = BachDegree))
pp_plot + stat_qq() + stat_qq_line(col = "Red") +
    labs(title = "Probability Plot: BachDegree vs Normal Dist.")
ggsave("histSurveyBachProb.png", width=12, height=8, unit="cm", dpi=300)

#Task 7a: Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.
# Yes, although there is some deviation from the y=x line, the sample data largely conforms
# to a normal distribution with most of the sample vs. theoretical points falling very
# close to the line.

#Task 7b: If not normal, is the distribution skewed? If so, in which direction? Explain how you know.
#If the distribution were skewed, the dots would form a curve that diverges from the 
#  y=x line on one end or another.

#Task 8: Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
stat.desc(acs_data$BachDegree, basic = FALSE, norm = TRUE)
# > stat.desc(acs_data$BachDegree, basic = FALSE, norm = TRUE)
# median         mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
# 34.10000000  35.46102941   0.81545273   1.61271456  90.43498856   9.50973126   0.26817415 
# skewness     skew.2SE     kurtosis     kurt.2SE   normtest.W   normtest.p 
# 0.32843046   0.79035382  -0.27742492  -0.33612576   0.98316075   0.09206162


#Task 9: In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. In addition, explain how a change in the sample size may change your explanation?
# The skew and kurtosis value have low absolute values which suggest a normal distribution.
# Since the skew.2SE and kurt.2SE values are the respective skew and kurtosis values divided 
# by two standard errors, the skew.2SE and kurt.2SE scores are just half the values of 
# the normalized z-score skew and kurtosis values.  Thus the z-skew is about 1.58 and the
# z-kurtosis is about -0.67.  Since the absolute values of these are less that 1.96, they
# do not vary signinficantly from a normal distribution.
# However, as the size of the dataset increases, these tests are more sensitive to variation.
# Thus, for larger datasets the bar for significance would need to be raised to 2.58 or 
# 3.29.  For very large datasets, other means would need to be used to evaluate normality.
