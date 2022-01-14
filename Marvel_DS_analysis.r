library(readr)
library(tidyverse)

dir1 <- "/Users/shuchitamishra/Desktop/DS5110 IDMP/Miniposter"
path <- file.path(dir1,"mcu dataset.csv")
data <- read.csv(path)

library(tibble)
as_tibble(data)

library(tidyverse)

#Imputing the NA values
findMode <- function(data) { 
  for(i in 1:ncol(data)) {
    if(is.character(data[ ,i])) {
      uniqv <- unique(data[ ,i][!is.na(data[ ,i])])
      modeResult <- uniqv[which.max(tabulate(match(data[ ,i], uniqv)))]
    } }
  return(modeResult)
}
for(i in 1:ncol(data)) {
  if(is.numeric(data[ ,i])) { 
    data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
  }
  if(is.character(data[ ,i])) {
    data[ , i][is.na(data[ , i])] <- findMode(data)
  }
}

#Cleaning the column values by removing $, and white spaces in prices columns
data2 <- data %>% select('Budget', 'Domestic.Gross', 'Total.Gross', 'Opening.Gross')
#data %>% 
#  select('Budget', 'Domestic.Gross', 'Total.Gross', 'Opening.Gross') %>%   arrange(desc(Budget))
#as_tibble(data2)

for(i in 1:nrow(data2)) {
  for(j in 1:ncol(data2)) {
    data2[i,j] <- str_remove_all(data2[i,j],"[$, ]")
    data2[i,j] <- as.numeric(data2[i,j])
    #print(data2[i,j])
  }
}

#as_tibble(data2)

#Replacing the columns
data$Budget <- data2$Budget
data$Domestic.Gross <- data2$Domestic.Gross
data$Total.Gross <- data2$Total.Gross
data$Opening.Gross <- data2$Opening.Gross
#as_tibble(data)

#Finally, I converted the datatype of all the new modified columns to 'numeric'.

#Converting char to numeric type
data$Budget <- as.numeric(data$Budget)
data$Opening.Gross <- as.numeric(data$Opening.Gross)
data$Domestic.Gross <- as.numeric(data$Domestic.Gross)
data$Total.Gross <- as.numeric(data$Total.Gross)

#The rest of the dataset did not require any additional tidying per say as the data followed these rules consistently :
#  * One variable is definite in single column only.
#* One observation is stated in one row.
#Hence, no further tidying was necessary to be performed. 

#Printing first several lines of the tidy data

#Clean data
as_tibble(data)

#Describe the dataset and its variables. Comment on whether you had to tidy the dataset, and how you tidied the data (if you did).**
  
#Describing how the data was tidied

#As mentioned above, I had to tidy the data of columns 

#* Budget, 
#* Domestic.Gross, 
#* Total.Gross and 
#* Opening.Gross 

#as they were character datatype including characters such as '$', ',' and white spaces which made visualization difficult.

#Hence, I followed the below steps to tidy these columns :
  
#  A. Impute the missing values

#I did this using the logic I had coded for the previous assignment in order to impute any and all missing values (NAs) from the dataset. 

#B. Formatting the data in price columns

#I first created a copy of these 4 columns in another dataset so that to ensure my modifications do not disturb the stability of the rest of the dataset. I had to remove the special characters in the values of these columns - '$' , ',' and white spaces. 
#Then I replaced these columns in the original dataset with the modified columns to preserve stability of the rest of the dataset. 
#Finally, I converted the datatype of all the new modified columns to 'numeric'.

#The rest of the dataset did not require any additional tidying.

#Hence, no further tidying was necessary to be performed. 

## Problem 2


#Use `ggplot2` to create visualizations to identify interesting or unexpected relationships in the dataset.


### **Solution**
#Which movie was made with the highest Budget over all three phases?
#We plot the "Movie title" against "Budget" and group by "IMDB rating" to analyze the MCU movie which was made in the highest budget, and what rating it fetched. 
#As the darker shades of blue have lower budget as compared to the lighter shades, this indicates there is a trend of increasing IMDB ratings as Budget increases.

library(ggplot2)

#Highest Budgeted movie of the MCU universe categorized by IMDB rating
ggplot( 
  data = data) +
  geom_histogram(mapping = aes(x = Name, y = Budget, fill = IMDB.rating), stat='identity')+
  coord_flip()+
  labs(title = "Avengers : Endgame - The Highest Budgeted movie of the MCU universe",
       x = "Movie Title",
       y = "Budget (in Million US$)")+
  theme(text = element_text(size = 7))

#The bar is the highest for "Avengers : Endgame" hence that was the movie which was made in the highest budget.**
  
  
#Which was the highest grossing movie across all three phases? 
  
#This graph is plotting the "Total Gross" with the grouping done on "Phase" to highlight which MCU movie was the highest grossing of all time, and which phase it belonged to. 
#This further helps to analyze the success of the MCU Phase. 
#Again, we see the lighter shades of blue having more Total Gross as compared to the darker shades thus indicating the Total Gross increases over the Phases of MCU, with the highest being in Phase 3.


#Highest Grossing movie of the MCU universe
ggplot(
  data = data,
  mapping = aes(group = Phase, x = Name, y = Total.Gross, fill = Phase)) +
  geom_bar(stat = 'identity')+
  #geom_smooth(method = 'lm', formula = y~x) +
  labs(title = "Avengers : Endgame - Highest Grossing movie of the MCU Universe",
       x = "Movie",
       y = "Total Gross (in Million US$)")+
  theme(text = element_text(size = 7), axis.text.x = element_text(angle = 45,hjust=1))
#The bar is the highest for "Avengers : Endgame hence that is the highest grossed MCU movie of all time.**

#Is the IMDB.rating and metascore rating similar? 

#Before we can analyze the trend for Budget against rating, we need to decide which rating we must choose. Since we have 2 ratings - Imdb and metascore, we need to check if both are inter-changeable or not. 
#Hence, we plot the IMDB.rating against metascore to observe the relation between the two. 
#We observe that the general trend line follows a **strong positive linear correlation **and hence we conclude that although there are some outliers, IMDB.rating and metascore ratings can be used inter-changeably as they indicate the same pattern of ratings. 


#IMDB rating vs metascore
ggplot( data = data,
        mapping = aes(x = IMDB.rating, y = metascore)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x, color = "red")+
  labs(title = "Both ratings agree : Metascore v/s IMDB",
       x = "IMDB ratings", 
       y = "Metascore")
#Which is the most successful MCU Phase? 

#To analyze the answer to this question, we plot Budget and IMDB ratings faceted by Phase, as this would highlight the most expensive movie as well as the highest rated movie in specific Phases.
#Phase 2 was relatively less positive, and remained constant, except for few outliers. 
#The plot of Phase 3 is the steepest, thus emphasizing the greater success of Phase 3 as compared to the other two phases.
#**Hence, MCU Phase 3 is the most successful phase (yet!)**

#Compare the IMDB ratings vs Budget faceted by Phase
ggplot( 
  data = data,
  mapping = aes(x = Budget, y = IMDB.rating, group = Phase)) +
  facet_grid(~Phase)+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = 'lm', formula = y~x) +
  labs(title = "MCU Phase 3 - The Most successful MCU Phase",
       x = "Budget (in Million US$)",
       y = "IMDB ratings ")+
  theme(text = element_text(size = 7))

