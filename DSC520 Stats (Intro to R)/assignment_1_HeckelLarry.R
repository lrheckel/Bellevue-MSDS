#  Assignment: Assignment 1

#  Name: Heckel, Larry

#  11 March 2019

# TASK a. Create two vectors assigning each a unique name.  
#         Each vector needs to contain at least 15 elements. 
#         Use the Range operator or the Sequence operator 
#         to create these two vectors.
vect1 <- c(131:145)
vect2 <- seq(116, 220, by = 7)
vect1
vect2
length(vect1)
length(vect2)
# TASK b. Leaving the two vectors unchanged, create a third vector, 
#         assigning it a unique name. Merge the two vectors from 
#         part a into a single list that is assigned to the third vector.
vect3 <-c()
vect3
vect3 <- c(vect1, vect2)
vect3
length(vect3)
# TASK c. Sort the third vector (merged at this point) in ascending, 
#         then descending order.
vect3 <- sort(vect3, decreasing = FALSE)
vect3
vect3 <- sort(vect3, decreasing = TRUE)
vect3
# TASK d. Use the length function to determine the number of elements 
#         contained in your third vector.
length(vect3)

# TASK e. Use the appropriate function to determine only 
#         the last 4 elements in your third vector.
tail(vect3, 4)

# TASK f. Use one of the logical operators to only select values greater 
#         than 5 from the vector.
which(vect3 > 5) #elements greater than 5
vect3[vect3 > 5] #values greater than 5

# TASK g. Calculate the mean and median for each vector.
mean(vect1)
median(vect1)

mean(vect2)
median(vect2)

mean(vect3)
median(vect3)

# TASK h. Save your third vector using the SaveRDS function. 
#         You do not need to submit the saved vector with your assignment 
#         but make sure you verify you have saved and can reload the RDS file.
saveRDS(vect3, file = "assignment1_RDS.rds")

vectNEW <- readRDS(file = "assignment1_RDS.rds")
vectNEW