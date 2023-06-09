# IE - Master in Business Analytics and Big Data
Recommendation Systems

Building a Collaborative Filtering Engine

Author: Madi Subaiti

========================================================
  
  # Initialization
  
  #Import libraries

library(data.table)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyr)

#========================================================


    #Load the critics dataset.

folder_path <- "/Users/madisubaiti/Downloads/IE-Lab1_assignment/Data"
critics <- fread(file.path(folder_path, "critics.csv"));
critics;
View(critics)

#========================================================



    # 1) Pearson correlation coefficient 
#Calculate the Pearson correlation coefficient between Sophia and Nuria

#Making DF and Transposing the dataset  
user_col <- as.data.frame(critics[, -1])
user_col_transpose <- t(user_col)
# Assigning names to the transposed column (users) since it is has been deleted
colnames(user_col_transpose) <- t(critics[,1])
rownames(user_col) <- t(critics[,1])

# Find only "Nuria" and "Sophia", and then correlate them using cor function.
PCC_ns <- as.data.frame(na.omit(user_col_transpose[,c("Nuria", "Sophia")]))
print(cor(PCC_ns[,1],PCC_ns[,2]))


#It appears to be a weak positive correlation between Nuria and Sophia's ratings


#========================================================
        #**Expected correlation between Sophia and Nuria**: 
        0.3651484

#========================================================

    # 2) Compare two movie critics
#Using the calculated Pearson correlation in the previous section. Compare and show the movie ratings for Sophia and Nuria on a scatter plot. How is this compared with the same ratings of Maria and Nerea on a scatter plot? What about Chris and Jim? What it means?


# Find only "Maria" and "Nerea", and then correlate them using cor function.
PCC_maria_nerea <- as.data.frame(na.omit(user_col_transpose[,c("Maria", "Nerea")]))
print(cor(PCC_ns[,1],PCC_ns[,2]))

# Find only "Chris" and "Jim", and then correlate them using cor function.
PCC_chris_jim <- as.data.frame(na.omit(user_col_transpose[,c("Chris", "Jim")]))
print(cor(PCC_ns[,1],PCC_ns[,2]))

#Scatter plot for Sophia and Nuria
plot_ns = PCC_ns %>%
  ggplot(aes(x=Nuria, y=Sophia)) +
  geom_point(col="blue") +
  labs(x="Nuria Ratings", y="Sophia Ratings", title="Correlation of Sophia vs. Nuria")+
  geom_text_repel(aes(label=rownames(PCC_ns))) + 
  coord_fixed(1, xlim = c(1, 5), ylim = c(1,5))
print(plot_ns);

# Scatter plot of Maria and Nerea
plot_maria_nerea = PCC_maria_nerea %>%
  ggplot(aes(x=Maria, y=Nerea)) +
  geom_point(col="red") +
  labs(x="Maria Ratings", y="Nerea Ratings", title="Correlation of Maria vs. Nerea")+
  geom_text_repel(aes(label=rownames(PCC_maria_nerea))) + 
  coord_fixed(1, xlim = c(1, 5), ylim = c(1,5))
print(plot_maria_nerea);

# Scatter plot of Chris and Jim

plot_chris_jim = PCC_chris_jim %>%
  ggplot(aes(x=Chris, y=Jim)) +
  geom_point(col="purple") +
  labs(x="Chirs Ratings", y="Jim Ratings", title="Correlation of Chris vs. Jim")+
  geom_text_repel(aes(label=rownames(PCC_chris_jim))) + 
  coord_fixed(1, xlim = c(1, 5), ylim = c(1,5))
print(plot_chris_jim);


#========================================================


    # 3) Top Recommendations 
#Return the top-5 recommended movies for Sophia. Use the weighted average of every other critic's rankings.

# creating distict columns for movies, ratings and weighted averages from each user rating 
critics1 <- critics %>%
gather("movie", "rating", -User) %>%
group_by(User) %>%
mutate(mean = mean(rating, na.rm = TRUE)) %>%
ungroup()

# Listing movies Sophia hasn´t seen or rated by filtering out her ratings
sophia_movies_not_seen <- critics1 %>%
filter(User == "Sophia" & is.na(rating)) %>%
extract2("movie")

# Finding Sophia´s movie ratings 
sophia_ratings <- critics1 %>%
filter(User == "Sophia") %>%
extract2("rating")

# Correlate between Sophia´s ratings and the rest by creating new variables eliminating the nulls
sophia_corr <- critics1 %>%
group_by(User) %>%
mutate(
pearson = cor(rating, sophia_ratings, use = "complete.obs"),
pearson_abs = abs(pearson)
) %>%
ungroup()

# The movies recommended to Sophia filtering to movies not rated by her
sophia_recom <- sophia_corr %>%
filter(movie %in% sophia_movies_not_seen) %>%
group_by(movie) %>%
summarise(
score = sum((rating - mean) * pearson, na.rm = TRUE) / (sum(pearson_abs) - 1)) %>%
mutate(score = score + mean(sophia_ratings, na.rm = TRUE)) %>%
arrange(desc(score)) 

head(sophia_recom,5)

#========================================================
        #**Expected result**:
        3.7917013044215, 'The Matrix'
        3.50776533175371, 'Forrest Gump'     
        3.33118834864677, 'The Sixth Sense' 
        3.11491825315719, 'Shakespeare in Love'
        2.9124513228665, 'Blade Runner'


#========================================================


    # 4) Top similar critics
#Return the top 5 critics that are most similar to Sophia. 

# variable for critic
critic <-"Sophia"

#getting the position of sophia
position <- grep(critic, rownames(user_col))
no_rate <- c(is.na(user_col[position,]))

#  an empty vector to iterate the correlations of sophia with other users

correlations<- c()
for (i in 1:ncol(user_col_transpose))
{
  PCC_ns <-na.omit(as.data.frame(cbind(user_col_transpose[,i],user_col_transpose[,critic])))
  correlations[i] <- cor(PCC_ns[,1],PCC_ns[,2])
  ;
}

corr_df <- data.frame(correlation = correlations[-position],
row.names = rownames(user_col[-position,]))


head(corr_df[order(-corr_df$correlation), , drop = FALSE],5)


#========================================================        
        #**Expected result**:
        0.9449112, 'Rachel'
        0.5976143, 'Ana' 
        0.5789794, 'Oriol'
        0.4925922, 'Maria'
        0.4273247, 'Carles'
#========================================================
