#  Assignment: Assignment 3.1 2014 American Community Survey
#  Name: Heckel, Larry
#  30 March 2019
# 
#################################################################### 
# Each student should first produce their own version of the following 
# deliverables then share with their team members. 
# Team members will then collaborate on their approach and insights, 
# refining their work before submitting for a grade.
# 
# The following guidelines describe minimum deliverables needed for 
# the assignment submission.
########################################## 

library(ggplot2)

#read the csv file. Assume the file is located in working/project directory
survey <- read.csv("acs-14-1yr-s0201.csv")
View(survey)

 
# 1. Document the elements in your data including the categories and data types.
str(survey)

#     The following are the categories and data types:
#       1.  Categorical and Nominal:  Id (Factor with 136 levels), Geography (Factor with 136 levels)
#       2.  Continuous and Ratio:  RacesReported (int), HSDegree (num), BachDegree (num)
#       3.  Continuous and Interval:  Id2 (int)
#       4.  Categorical:  PopGroupID (int), POPGROUP.display.label (Ractor with 1 level) 
#         (While these categories are nominal, 
#         the data set only includes a single value for these variables.)
# 
# 
# 2. Report the output from at least the following functions: str(); nrow(); ncol()
nrow(survey)
ncol(survey)
 
#  There are 136 rows (observations) and 8 columns.
# 
# 
# 3. Create a Histogram of the HSDegree variable using the ggplot2 package. 
#     Set a bin size for the Histogram.
# 4. Include a Title and appropriate X/Y axis labels on your Histogram Plot.
histSurveyHSD <- ggplot(data=survey,aes(x=HSDegree)) + geom_histogram(aes(fill=..count..),binwidth=0.5) +
  ggtitle("% of Population with HS Degree") +
  xlab("% of Population") + ylab("Number of States") +
  scale_fill_gradient("Number of States", low="blue", high="green")
histSurveyHSD
ggsave("histSurveyHSD.png", width=12, height=8, unit="cm", dpi=300)
 
# 
# 
# 
# 5. Answer the following questions based on the Histogram produced:
#   a. Based on what you see in this histogram, is the data distribution unimodal?
#       Based on the bin width of 0.5, the data distribution is not unimodal, in
#       there are a number of peaks.
#   b. Is it approximately symmetrical?
#       It is not symmetrical, in that the lower end of the data has a much
#       longer tail than the upper end.
#   c. Is it approximately bell-shaped?
#       It is approximately bell-shaped, with the addition of the long lower
#       tail on the data.
#   d. Is it approximately normal?
#       It is not normal.
#   e. If not normal, is the distribution skewed? If so, in which direction? 
#         Provide specifics in your responses.
#           Yes, the distribution is skewed to the left, in that there are more
#           values spread to the left (lower values) of the mean and median.
# 
# 
#   f. Include a normal curve to the Histogram that you plotted.
histSurveyHSDNorm <- ggplot(data=survey,aes(x=HSDegree)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=0.5) +
  stat_function(fun = dnorm, args=with(data=survey, c(mean = mean(HSDegree), sd = sd(HSDegree)))) +
  scale_x_continuous("HSDegree") +
  ggtitle("Normal Curve with Histogram") +
  xlab("% of Population") + ylab("Number of States") +
  scale_fill_gradient("Number of States", low="blue", high="green")
histSurveyHSDNorm
ggsave("histSurveyHSDNorm.png", width=12, height=8, unit="cm", dpi=300) 
 
# 
# 
# 
#   g. Explain whether a normal distribution can accurately be used as a model 
#       for this data.
#         The normal curve cannot be used to accurately model this data.
#         We can see from the histogram that the data is skewed to the left,
#           and the normal curve is much longer on the left side than the
#           right side, so it is not symmetrical.
# 
# 6. Create a Probability Plot of the HSDegree variable.
histSurveyHSDProb <- ggplot(data=survey,aes(sample=HSDegree)) + 
    stat_qq() + stat_qq_line(col = "Red")
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
#           The distribution is skewed to the left, as the graph shows the high
#             number of points with negative values away from the diagonal line.
# 
# 8. Now that you have looked at this data visually for normality, 
#     you will now quantify normality with numbers using the stat.desc() function.
#     Include a screen capture of the results produced.
library(pastecs)
options(scipen=100)
options(digits=2)
hsDegStats <- stat.desc(survey$HSDegree, norm = TRUE)
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
#         skewed to the left.
#         
#       The kurtosis value of 4.35 indicates that there are a significant number
#         of data points in the tails of the distribution. For a normal curve,
#         the kurtosis value is 3, so the 4.35 value shows that this distribution
#         has a higher number of values in the tails (particularly the left tail)
#         than a normal curve.
#           
#       Skew.2SE and kurt.2SE are the skew and kurtosis values divided by two standard errors.
#         When the absolute of these values is greater than 1, then there is significant skew/kurtosis
#         in the data (at p < .05). Absolute values above 1.65 indicate significance at p < .001. 
#         As the absolute values of these are both far greater than 1.65, they indicate that the 
#         data has significant skew and kurtosis. The skew.2SE value is -4.03, and the kurt.2SE is
#         5.27.   
#     In addition, explain how a change in the sample size may change your 
#     explanation?
#       As sample sizes increase, significant values come up from small deviations (in the data)
#         from normality. As a result, while using 1.96 to compute z-scores is good for smaller
#         sample sizes, we should increase this to 2.58 as sample sizes go above 200, and for
#         even larger sample sizes, we should simply look at the distribution's shape and the
#         skew and kurtosis values directly, rather than calculating their significance with
#         z-scores.
#         
#       So my explanation would need to be more focused on viewing the histogram and comparing
#         it to the normal curve, rather than the z-scores themselves.
#
#
#########################################################
#
# 10.Repeat steps 3 through 9 for the BachDegree variable.
# 
# 3. Create a Histogram of the BachDegree variable using the ggplot2 package. 
#     Set a bin size for the Histogram.
# 4. Include a Title and appropriate X/Y axis labels on your Histogram Plot.
histSurveyBach <- ggplot(data=survey,aes(x=BachDegree)) + geom_histogram(aes(fill=..count..),binwidth=0.5) +
  ggtitle("% of Population with Bachelor's Degree") +
  xlab("% of Population") + ylab("Number of States") +
  scale_fill_gradient("Number of States", low="red", high="brown")
histSurveyBach
ggsave("histSurveyBach.png", width=12, height=8, unit="cm", dpi=300)

# 
# 
# 
# 5. Answer the following questions based on the Histogram produced:
#   a. Based on what you see in this histogram, is the data distribution unimodal?
#       Yes, the graph is unimodal. At a bin width of 0.5, there are some small peaks in the data
#         going to the tails, but there is only one overall peak.
#   b. Is it approximately symmetrical?
#       Yes, the data is approximately symmetrical, although it appears that the right tail
#         is a bit longer than the left tail.
#   c. Is it approximately bell-shaped?
#       Yes, it is approximately bell-shaped, with the same caveat as the previous answer.
#   d. Is it approximately normal?
#       Yes, it appears to be approximately normal.
#   e. If not normal, is the distribution skewed? If so, in which direction? 
#         Provide specifics in your responses.
#           The data appears to have a small right skew, in that there appear to be more
#             values to the right of the histogram's center, than values to its left.
# 
# 
#   f. Include a normal curve to the Histogram that you plotted.
histSurveyBachNorm <- ggplot(data=survey,aes(x=BachDegree)) + 
  geom_histogram(aes(y=..density.., fill=..count..), binwidth=0.5) +
  stat_function(fun = dnorm, args=with(data=survey, c(mean = mean(BachDegree), sd = sd(BachDegree)))) +
  scale_x_continuous("BachDegree") +
  ggtitle("Normal Curve with Histogram") +
  xlab("Density of Bachelor's Degree") + ylab("Number of States") +
  scale_fill_gradient("Number of States", low="red", high="brown")
histSurveyBachNorm
ggsave("histSurveyBachNorm.png", width=12, height=8, unit="cm", dpi=300) 

# 
#   g. Explain whether a normal distribution can accurately be used as a model 
#       for this data.
#         A normal distribution cannot be accurately use as a model for this data. While
#           the data appears to be close to normally distributed, the data's peak is to
#           the left of the median, and the right tail is longer than the left tail.
#           Visually, the graph is close to being normal, but not close enough to accurately
#           be described by a normal distribution.
# 
# 6. Create a Probability Plot of the HSDegree variable.
histSurveyBachProb <- ggplot(data=survey,aes(sample=BachDegree)) + 
    stat_qq() + stat_qq_line(col = "Red")
histSurveyBachProb
ggsave("histSurveyBachProb.png", width=12, height=8, unit="cm", dpi=300)

# 
# 
# 7. Answer the following questions based on the Probability Plot:
#   a. Based on what you see in this probability plot, is the distribution 
#         approximately normal? Explain how you know.
#           The probability plot shows that the graph is approximately normal, because
#             the data points stay very close to the diagonal, except at the extremes.
#             This closeness indicates that the distribution is close to normality.
#   b. If not normal, is the distribution skewed? If so, in which direction? 
#       Explain how you know.
#           The distribution is only slightly skewed, in that only a few of the end points
#             deviate from the diagonal on either end. Were the distribution to be skewed, as 
#             we saw with the HSDegree variable, we would see a large number of data points
#             deviating from the diagonal line, at one or both ends.
# 
# 
# 8. Now that you have looked at this data visually for normality, 
#     you will now quantify normality with numbers using the stat.desc() function.
#     Include a screen capture of the results produced.
options(scipen=100)
options(digits=2)
BachDegStats <- stat.desc(survey$BachDegree, norm = TRUE)
BachDegStats
View(BachDegStats)

#> BachDegStats
# nbr.val     nbr.null       nbr.na          min          max        range 
# 136.000        0.000        0.000       15.400       60.300       44.900 
# sum       median         mean      SE.mean CI.mean.0.95          var 
# 4822.700       34.100       35.461        0.815        1.613       90.435 
# std.dev     coef.var     skewness     skew.2SE     kurtosis     kurt.2SE 
# 9.510        0.268        0.328        0.790       -0.277       -0.336 
# normtest.W   normtest.p 
# 0.983        0.092 
#
# 
# 9. In several sentences provide an explanation of the result produced for skew, 
#     kurtosis, and z-scores.
#     In addition, explain how a change in the sample size may change your 
#     explanation?
#       The value for skewness is 0.328, indicating that the graph is only slightly
#         skewed, with values closer to 0 indicating lack of skew.
#         
#       The kurtosis value of -0.277 indicates that there the distribution's tails
#         don't contain a significantly larger number of values in the tails than
#         are present in a normal distribution. For a normal distribution,
#         the kurtosis value is 3, so the -0.277 value shows that this distribution
#         has a lower number of values in the tails than a normal curve.
#           
#       Skew.2SE and kurt.2SE are the skew and kurtosis values divided by two standard errors.
#         When the absolute of these values is greater than 1, then there is significant skew/kurtosis
#         in the data (at p < .05). Absolute values above 1.65 indicate significance at p < .001. 
#         As the absolute values of these are both far less than 1.65, they indicate that the 
#         data does not have significant skew or kurtosis. The skew.2SE value is 0.79, and the 
#         kurt.2SE is -0.336.
#         
#     In addition, explain how a change in the sample size may change your 
#     explanation? (This is the same answer as above.)
#       As sample sizes increase, significant values come up from small deviations (in the data)
#         from normality. As a result, while using 1.96 to compute z-scores is good for smaller
#         sample sizes, we should increase this to 2.58 as sample sizes go above 200, and for
#         even larger sample sizes, we should simply look at the distribution's shape and the
#         skew and kurtosis values directly, rather than calculating their significance with
#         z-scores.
#         
#       So my explanation would need to be more focused on viewing the histogram and comparing
#         it to the normal curve, rather than the z-scores themselves.
#
#
# 
############################################















