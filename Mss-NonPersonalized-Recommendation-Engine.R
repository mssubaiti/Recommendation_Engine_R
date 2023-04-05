# IE - Master in Business Analytics and Big Data
Recommendation Systems

Building a Non Personalized Recommendation Engine

Author: Madi Subaiti

========================================================
  
    # Initialization
  
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggrepel")

#========================================================

    #Import libraries

library(data.table);
library(dplyr)
library(ggplot2)
library(ggrepel)

#========================================================

    #Load the critics dataset.

folder_path <- "/Users/madisubaiti/Downloads/IE-Lab1_assignment/Data"
critics <- fread(file.path(folder_path, "critics.csv"));

#========================================================


    # 1) Top 5 Movies, by rating mean
#Calculate mean rating for each movie, ordered with the highest rating listed first, and plot the top 5.

# Calculate mean rating for each movie
mean_ratings <- colMeans(critics[, -1], na.rm = TRUE)  # Exclude User column

# Create a dataframe to store the results
movie_meandf <- data.frame(Movie = names(mean_ratings), Mean_Rating = mean_ratings)

# Order the results by Mean_Rating in decreasing order
movie_meandf <- movie_meandf[order(movie_meandf$Mean_Rating, decreasing = TRUE), ]

# Print the top 5 movies with the highest mean rating
top_movies <- head(movie_meandf, 5)

# Print the top 5 movies with the highest mean rating
print(top_movies)


######
#Another way: Remove User column, calculate the mean for each movie within the movie_ratings dataframe excluding any NAs. 
movie_ratings <- critics
movie_ratings$User <- NULL
mean_ratings <- sort(colMeans(movie_ratings, na.rm = T), decreasing = T)

#show top 5
mean_ratings[1:5]



#========================================================
        **Expected result**:
        3.600000, 'The Shawshank Redemption'
        3.266667, 'Star Wars IV - A New Hope'
        3.222222, 'Blade Runner'
        3.166667, 'Groundhog Day'
        3.062500, 'The Silence of the Lambs'
#========================================================



    # 2) Top 5 Movies, by rating distribution
#Calculate the percentage of ratings for each movie that are 4 stars or higher. Order with the highest percentage first, and plot the top 5 movies with more high ratings distribution.


```{r fig.width=7, fig.height=6}
# Define function to calculate percentage
func <- function(x){
  sum(x>=4, na.rm=T)/sum(x>=0, na.rm=T)
}

# Apply function to each column (movie) in movie_ratings dataframe
top_perc <- sort(apply(movie_ratings, 2, func), decreasing=T)

# Create a dataframe with movie names and their corresponding percentages
top_percentdf <- data.frame(Movie = names(top_perc), Percent = top_perc)

# Show top 5 movies with highest percentage
top_5 <- head(top_percentdf, 5)

# Print the result
print(top_5)


#========================================================
        **Expected result**:
        0.7000000, 'The Shawshank Redemption'
        0.5333333, 'Star Wars IV - A New Hope'
        0.5000000, 'Gladiator'
        0.4444444, 'Blade Runner'
        0.4375000, 'The Silence of the Lambs'
#========================================================


    # 3) Top 5 Movies, by quantity of ratings
#Count the number of ratings for each movie, order the one with most number of ratings first, submit the top 5.
#number_of_ratings <- data.frame()
#counting <- count(number_of_ratings, vars = NULL, wt_var = NULL)


# Calculate number of ratings for each movie
number_of_ratings <- colSums(!is.na(critics[, -1]))  # Exclude User column

# Create a dataframe to store the results
top_5_moviesdf <- data.frame(Movie = names(number_of_ratings), Number_of_Ratings = number_of_ratings)

# Order the results by Number_of_Ratings in decreasing order
top_5_moviesdf <- top_5_moviesdf[order(top_5_moviesdf$Number_of_Ratings, decreasing = TRUE), ]

# Get the top 5 movies with the most number of ratings
top_movies <- head(top_5_moviesdf, 5)

# Print the top 5 movies
print(top_movies)


#####
#Another way. Need to null the user column and get a count of how many ratings each movie has without nulls
number_rating2<-sort(apply(critics[,-"User"],2,function(x){
  sum(x>=0,na.rm=T)}),decreasing=T)

print(head(number_rating2,5))
```


#========================================================
        **Expected result**:
        17, 'Toy Story'
        16, 'The Silence of the Lambs'
        15, 'Star Wars IV - A New Hope'
        14, 'Star Wars VI - Return of the Jedi'
        13, 'Independence Day'

#========================================================


    # 4) People who watched Star Wars IV also watched ...
#Calculate movies that most often occur with other movie. 
#For each movie, calculate the percentage of the other movie raters who also rated that movie. 
#Order with the highest percentage first, and submit the top 5. Select "Star.Wars.IV...A.New.Hope" movie by defaut. 
#Use the (x+y)/x method.

# Filter the data for users who have rated "Star Wars IV - A New Hope"
crtcs <- as.vector(!is.na(critics$"Star Wars IV - A New Hope">= 1))
seen_SWIV <- critics[,-c("User","Star Wars IV - A New Hope"  )][crtcs,]

# Count the number of ratings for "Star Wars IV - A New Hope"
count_movie <- sum(!is.na(critics$`Star Wars IV - A New Hope`))

# Define a function to calculate the percentage of ratings for each movie
func <- function(x){
  sum(!is.na(x))/count_movie
}

# Apply the function to calculate percentage for each movie
top <- sort(apply(seen_SWIV,2,func),decreasing=T)

# Create a dataframe to store the results
result <- data.frame(Movie = names(top), Percentage = top)

# Print the top 5 movies with the highest percentage of ratings
print(head(result, 5))

#========================================================
        Top Occurences:
        0.9333333, 'Toy Story'
        0.8666667, 'Star Wars: Episode VI - Return of the Jedi'
        0.8000000, 'The Silence of the Lambs'
        0.7333333, 'Independence Day'
        0.6666667, 'Total Recall'
#========================================================



    # 5) People who liked Babe also liked ...
#Calculate the movies better rated of people who liked a movie. Select the people who liked the movie "Babe" (4 or 5 stars) and provide the top 5 movies they liked most.

# Select people who liked "Babe" (rated it 4 or 5 stars)
liked_Babe <- critics[critics$Babe >= 4, ]

# Calculate mean ratings for each movie among people who liked "Babe"
mean_ratings <- colMeans(liked_Babe[, -1], na.rm = TRUE)

# Order mean ratings in decreasing order
sorted_ratings <- sort(mean_ratings, decreasing = TRUE)

# Select the top 5 movies with the highest mean ratings
top_movies <- head(sorted_ratings, 5)

# Get the movie names
top_movie_names <- names(top_movies)

# Combine movie names and mean ratings into a data frame
babe2 <- data.frame(Mean_Rating = top_movies, Movie_Name = top_movie_names)

# Print the result
print(babe2)

#========================================================
        **Expected result**:
        5.000, 'Pulp.Fiction'
        5.000, 'Groundhog.Day'
        4.500, 'The.Shawshank.Redemption'
        4.333, 'Toy.Story'
        4.000, 'Blade.Runner'
#========================================================
