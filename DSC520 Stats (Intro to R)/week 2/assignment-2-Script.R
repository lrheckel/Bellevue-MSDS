# Example script provided for Assignment 2
# Make sure you set your working directory before execurting the following script

Weather <- read.csv("scweather.csv")
View(Weather)
counts <- table(Weather$Tmax)
print(Weather$TMAX)
stripchart(Weather$TMAX, method="jitter", main="Pierre, SD February High Temps",ylab="Count", xlab="Temperatures")
           
# Looking at the scatterplot provide your answers along with your explanation.  You will need to load the two 
# other data sets for each city and create a stripchart for those temperatures. 
# Save each Plot using the Export option in the Plots area.
# Save As Image option so a comparison can be made and referenced in your solution responses.