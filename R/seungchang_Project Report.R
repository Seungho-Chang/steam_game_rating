rm(list=ls())

#Daniel Chang
#Final Project
#12/09/22

setwd("/Users/Daniel Chang/Desktop")
getwd()

###Data Cleaning & Reduction (Steam games complete dataset)###

#Load steam_games.csv file
steam_csv <- read.csv("steam_games.csv", na.strings = c("", NaN))

#Subset unnecessary columns
steam_csv <- subset(steam_csv, select = -c(url, desc_snippet, developer, publisher, popular_tags, recent_reviews, game_description, mature_content, minimum_requirements, recommended_requirements, discount_price, languages))

#Change columns names
colnames(steam_csv) <- c("type", "name", "rating", "release_date", "game_details", "achievements", "genre", "price")

#Remove rows that have NA
steam_csv <- na.omit(steam_csv)

#Drop duplicates
steam_csv <- unique(steam_csv)

#Subset only app(games) and delete column
steam_csv <- subset(steam_csv, steam_csv$type == "app")
steam_csv <- subset(steam_csv, select = -type)

#Delete $ sign in price feature
steam_csv$price <- as.numeric(gsub("\\$", "", steam_csv$price))

#Drop rows that have NA in price feature
steam_csv <- na.omit(steam_csv)

#Drop early excess game
steam_csv <- steam_csv[!grepl("Early Access", steam_csv$genre),]

#Drop rows with text-crashed(name)
steam_csv <- steam_csv[!grepl("Ã", steam_csv$name),]
steam_csv <- steam_csv[!grepl("Â", steam_csv$name),]
steam_csv <- steam_csv[!grepl("¢", steam_csv$name),]
steam_csv <- steam_csv[!grepl("æ", steam_csv$name),]
steam_csv <- steam_csv[!grepl("å", steam_csv$name),]
steam_csv <- steam_csv[!grepl("ã", steam_csv$name),]
steam_csv <- steam_csv[!grepl("Ä", steam_csv$name),]
steam_csv <- steam_csv[!grepl("Å", steam_csv$name),]
steam_csv <- steam_csv[!grepl("ä", steam_csv$name),]
steam_csv <- steam_csv[!grepl("#", steam_csv$name),]
steam_csv <- steam_csv[!grepl("€", steam_csv$name),]
steam_csv <- steam_csv[!grepl(">", steam_csv$name),]
steam_csv <- steam_csv[!grepl("°", steam_csv$name),]
steam_csv <- steam_csv[!grepl("¾", steam_csv$name),]
steam_csv <- steam_csv[!grepl("½", steam_csv$name),]
steam_csv <- steam_csv[!grepl("œ", steam_csv$name),]
steam_csv <- steam_csv[!grepl("¬", steam_csv$name),]
steam_csv <- steam_csv[!grepl("©", steam_csv$name),]
steam_csv <- steam_csv[!grepl("±", steam_csv$name),]
steam_csv <- steam_csv[!grepl("¦", steam_csv$name),]
steam_csv <- steam_csv[!grepl("µ", steam_csv$name),]
steam_csv <- steam_csv[!grepl("ð", steam_csv$name),]
steam_csv <- steam_csv[!grepl("š", steam_csv$name),]

#remain very first genre
steam_csv_genre <- gsub("\\,.*", "", steam_csv$genre)
steam_csv$genre <- steam_csv_genre

#remain very first game_detail
steam_csv_detail <- gsub("\\,.*", "", steam_csv$game_details)
steam_csv$game_details <- steam_csv_detail

#remain only text rating
steam_csv_rating <- gsub("\\,.*", "", steam_csv$rating)
steam_csv$rating <- steam_csv_rating

#Drop rating with only few reviews
steam_csv <- steam_csv[!grepl("reviews", steam_csv$rating),]

#Replace rating to point
steam_csv["rating"][steam_csv["rating"] == "Overwhelmingly Positive"] <- 90
steam_csv["rating"][steam_csv["rating"] == "Very Positive"] <- 80
steam_csv["rating"][steam_csv["rating"] == "Positive"] <- 70
steam_csv["rating"][steam_csv["rating"] == "Mostly Positive"] <- 60
steam_csv["rating"][steam_csv["rating"] == "Mixed"] <- 50
steam_csv["rating"][steam_csv["rating"] == "Mostly Negative"] <- 40
steam_csv["rating"][steam_csv["rating"] == "Negative"] <- 30
steam_csv["rating"][steam_csv["rating"] == "Very Negative"] <- 20
steam_csv["rating"][steam_csv["rating"] == "Overwhelmingly Negative"] <- 10

#Change rating to numerical value
steam_csv$rating <- as.numeric(steam_csv$rating)

#Change achievements to numerical value
steam_csv$achievements <- as.numeric(steam_csv$achievements)

#Change "multi-player" to "Online Multi-Player" in game_type column to make it same
steam_csv["game_details"][steam_csv["game_details"] == "Multi-player"] <- "Online Multi-Player"

#Drop "Steam Achievements" in game_type because it is not a game
steam_csv <- steam_csv[!grepl("Steam Achievements", steam_csv$game_details),]

#leave only year for release date
steam_csv_date <- gsub(".*,", "", steam_csv$release_date)
steam_csv$release_date <- steam_csv_date

#Drop very 19XX games
steam_csv <- steam_csv[!grepl("83", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("84", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("91", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("94", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("95", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("96", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("97", steam_csv$release_date),]
steam_csv <- steam_csv[!grepl("98", steam_csv$release_date),]

#Change column name from release date to year
colnames(steam_csv) <- c("name", "rating", "year", "game_details", "achievements", "genre", "price")
View(steam_csv)
nrow(steam_csv)

###Web Scrapping from SteamDB website###

#I wasn't able to do web scrapping "SteamDB" website with R because website because Steam blocks the html for web scrapping
#So I web scrapped "SteamDB" website with "power automate" which Professor Colbert told me during project check-in

#Load streamdb.csv file###
steamdb_csv <- read.csv("steamdb.csv", na.strings = c("", NaN))
View(steamdb_csv)
str(steamdb_csv)

##################################################################################################################################

###Horizontally integrate two data###

#data that has only same name will be left
steam_merged <- merge(steam_csv, steamdb_csv, by = "name")

#Re-correct the column names
colnames(steam_merged) <- c("name", "rating", "year", "game_type", "achievements", "genre", "price", "24h_peak")
str(steam_merged)
steam_merged$`24h_peak` <- as.numeric(steam_merged$`24h_peak`)
View(steam_merged)

##################################################################################################################################

###Analysis###

###3.2 Game rating by game type###
library(dplyr)
summary_gametype <- steam_merged %>% group_by(game_type) %>% summarise(count = n(), mean_rating = mean(rating))
summary_top_gametype <- head(summary_gametype[order(summary_gametype$mean_rating, decreasing = TRUE),], n = 50)

#change column names of summary_top_gametype
colnames(summary_top_gametype) <- c("game_type", "number", "average_rating")
summary_top_gametype

#bar plot (game rating by game type)
gametype <- c("Local Multi-Player", "Single-Player", "Online Multi-Player")
gametype
str(summary_top_gametype)
gametype_bar<- barplot(t(as.matrix(summary_top_gametype$average_rating)), 
        beside = TRUE, 
        main = "Game Rating by Game Type",
        xlab = "Game Type",
        ylab = "Average Rating",
        col = rainbow(3),
        legend.text = rownames(gametype),
        ylim=c(0,100))
text(gametype_bar, 80, gametype, cex = 1, pos = 3)

###3.3 Game rating by release year###
library(dplyr)
summary_release_year <- steam_merged %>% group_by(year) %>% summarise(count = n(), mean_rating = mean(rating))
summary_top_release_year <- head(summary_release_year[order(summary_release_year$mean_rating, decreasing = TRUE),], n = 50)

#change column names of summary_top_release_year
colnames(summary_top_release_year) <- c("release_year", "number", "average_rating")
summary_top_release_year

#line graph (game rating by release year)
library(ggplot2)
ggplot(data = summary_top_release_year, aes(x = release_year, y = average_rating, group=1)) +
  geom_line(color = "red")+
  geom_point()

###3.4(a) Scatter plot (Game Rating by achievement)###
library(ggplot2)
ggplot(steam_merged, aes(x = achievements, y = rating)) +
  geom_point(size=2, shape=23)

###3.4(b) Game rating by Genre###
library(dplyr)
summary_genre <- steam_merged %>% group_by(genre) %>% summarise(count = n(), mean_rating = mean(rating))
summary_top_genre <- head(summary_genre[order(summary_genre$mean_rating, decreasing = TRUE),], n = 50)

#change column names of summary_top_genre
colnames(summary_top_genre) <- c("genre", "number", "average_rating")
summary_top_genre

#Drop "Free to Play" genre (it is not free according to the price column)
#Also Drop "Massive Multi-player" and "Utilities" (only one data with high rating)
summary_top_genre <- summary_top_genre[-c(1, 2, 8),]
summary_top_genre

###3.4(c) Game rating by price###

#summary by price
summary(steam_merged$price)

#linear relationship between the price and rating
price_rating <- cor(steam_merged$price, steam_merged$rating)
price_rating

#line graph (game rating by price)
library(dplyr)
summary_price <- steam_merged %>% group_by(price) %>% summarise(count = n(), mean_rating = mean(rating))
summary_top_price <- head(summary_price[order(summary_price$mean_rating, decreasing = TRUE),], n = 100)

#change column names of summary_price
colnames(summary_top_price) <- c("price", "number", "average_rating")
summary_top_price

#line graph (game rating by price)
library(ggplot2)
ggplot(data = summary_top_price, aes(x = price, y = average_rating, group=1)) +
  geom_line(color = "red")+
  geom_point()

#price histogram
hist(steam_merged$price,
     main = "Price Histogram",
     xlab = "Pirce",
     ylab = "Number",
     xlim = c(0,100),
     ylim = c(0,1500))

###3.5 Game rating by 24h peak###

#summary by 24h peak
summary(steam_merged$`24h_peak`)

#Top 10 24h peak
top_24_10 <- head(steam_merged[order(steam_merged$`24h_peak`, decreasing = TRUE),], n = 10)
top_24_10

#Linear relationship between 24h peak and rating
rating_24 <- cor(steam_merged$`24h_peak`, steam_merged$rating)
rating_24
