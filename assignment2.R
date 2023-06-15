library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)

#question a
data(iris)
boxplot(Sepal.Length ~ Species, data = iris, main = "Sepal Length by Species",
        xlab = "Species", ylab = "Sepal Length")

boxplot(Petal.Length ~ Species, data = iris, main = "Petal Length by Species",
        xlab = "Species", ylab = "Petal Length")
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species, 
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Scatterplot of Sepal Length vs. Petal Length")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 1)


#question c
library(MASS)
data("ships")
ship_data= cbind(ships[1],ships[5])
plot(ship_data)
#from the plot we can see type B has the most incedents.


#question d
title <- c()
titles <- c()
view <- c()
views <- c()
answer <- c()
answers <- c()
vote <- c()
votes <- c()
urls_codes <- 1:13854
for (i in 1:13854)
{
  urls_codes[i] <- paste("https://stats.stackexchange.com/questions?tab=newest&page=", urls_codes[i], sep = '')
}
for (i in 1:13854)
{
  html <- read_html(urls_codes[i])
  titles <- html %>% html_elements("#questions .s-link") %>% html_text()
  views <- html %>% html_elements(".s-post-summary--stats-item__emphasized~ .s-post-summary--stats-item+ .s-post-summary--stats-item .s-post-summary--stats-item-number") %>% html_text()
  answers <- html %>% html_elements(".s-post-summary--stats-item__emphasized+ .s-post-summary--stats-item .s-post-summary--stats-item-number") %>% html_text()
  votes <- html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text()
  title = append(title, titles)
  view = append(view, views)
  answer = append(answer, answers)
  vote = append(vote, votes)
}
Data_frame <- data.frame("title"=title,"views"=view,"answers"=answer,"votes"=vote)



#question b
flip <- function(img)
{
img<- load.image(img)
dimensions <- dim(img)
col.mat <- as.array(img[, ,1, ])
col1.mat <- as.array(img[, ,1,])
for (i in 1:dimensions[1])
{
  cropped.mat <- col.mat[i,, ]
  col1.mat[dimensions[1]-i,,] <- cropped.mat
}
crop.img <- as.cimg(col1.mat)
plot(crop.img)
}



#question e
pull_a_tablet <- function() {
  if (runif(1) < 0.5) {
    
    return(1)
  } else {
    
    return(0)
  }
}

calculateAvg_days <- function(iterations) 
{
  totalno._days <- 0
  
  for (i in 1:iterations) 
  {
    number_days <- 0
    bottle <- rep(1, 100) 
    
    while (pull_a_tablet() == 0) 
    {
      number_days <- number_days + 1
      bottle <- c(bottle, 0.5)
    }
    
    totalno._days <- totalno._days + number_days
  }
  
  Avg_days <- totalno._days / num_iterations
  return(Avg_days)
}

num_of_iterations <- 2000

average_days <- calculateAvg_days(num_of_iterations)
print(paste("Average number of days:", round(average_days, 2)))

