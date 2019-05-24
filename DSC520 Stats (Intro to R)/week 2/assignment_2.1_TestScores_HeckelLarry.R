#  Assignment: Assignment 2.1 Test Scores
#  Name: Heckel, Larry
#  23 March 2019

################################################
# Complete Assignment Here
#
# 1. What are the observational units in this study?
#
#       The observational units are students in the classes.
#   
# 2. Identify the variables mentioned in the narrative paragraph 
#      and determine which are categorical and quantitative?
#
#       The variables are the scores, which are Quantitative, and 
#         the type of section, either Sports or Regular, defining
#         the types of applications from which the data examples 
#         were taken. The section type is a Categorical variable.
#   
# 3. Create one variable to hold a subset of your data set that contains 
#    only the Regular Section and one variable for the Sports Section.
#         
#         See code below for SportScores and RegScores.
# 
# 4. Use the Plot function to plot each Sections scores and 
#    the number of students achieving that score. Use additional 
#    Plot Arguments to label the graph and give each axis an appropriate label.
#         
#         See the code below to plot the histograms. I have added a line of
#           code to plot the histograms aligned vertically, in order to 
#           better make comparisons between the graphs.
# 
#   Once you have produced your Plots answer the following questions:
#   
#    a. Comparing and contrasting the point distributions between the 
#       two section, looking at both tendency and consistency: 
#       Can you say that one section tended to score more points 
#       than the other? Justify and explain your answer.
#
#         On average, the Regular class scored higher than the Sports class,
#           as the histograms show. Additionally, the Sports section has a much wider spread
#           of data, with both a lower min and higher max, than the Regular
#           section. So we can conclude that the Regular section was more
#           consistent in its scoring than the Sports section.
# 
#    b. Did every student in one section score more points 
#       than every student in the other section? If not, explain 
#       what a statistical tendency means in this context.
#
#         The sections are not divided by Scores, in that the distributions of
#           scores in the two sections overlap. The mean of the Regular section is
#           greater than that of the Sports section, in that more students in Regular
#           have higher scores than those in Sports. 
#  
#    c. What could be one additional variable that was not mentioned 
#       in the narrative that could be influencing the point 
#       distributions between the two sections?
#
#         One key factor in the score distribution is that the student populations
#           are not random, in that they self-selected which section they would 
#           participate in. An additional variable that might influence the point
#           distributions is the Grade Point Average (GPA) of the students in each
#           class. A better correlation might be made between the GPA's of the 
#           students in each section and their test scores. The question at issue,
#           then, would be if teaching Sports-related applications would yield a
#           higher or lower score than the GPA for each student, after normalizing
#           the test scores to GPA's.
##############################################

#read the csv file. Assume located in working/project directory
Scores <- read.csv("scores.csv")
View(Scores)

# 3. Create one variable to hold a subset of your data set that contains 
#    only the Regular Section and one variable for the Sports Section.
SportScores <- subset(Scores, Section=="Sports", 
                      select = c("Count", "Score"))
RegScores <- subset(Scores, Section=="Regular", 
                      select = c("Count", "Score"))
View(SportScores)
View(RegScores)

plot(SportScores$Score, SportScores$Count, type="h", 
     main="Student Counts and Scores for Sports Section",
     xlab="Score", ylab="Number Students",asp=1)

plot(RegScores$Score, RegScores$Count, type="h", 
     main="Student Counts and Scores for Regular Section",
     xlab="Score", ylab="Number Students",asp=1)



