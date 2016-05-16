#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#
# R code final
#   R code for documenting the final project for the R/Python course
#   By: Andy Hammes
#   Due: May 16, 2016
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#==============================================================================#
# loading libraries
#==============================================================================#
library(twitteR)
library(plyr)
library(rworldmap)
library(rworldxtra)
library(tm)
library(wordcloud)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(data.table)
library(pander)

options(stringsAsFactors = FALSE)

#==============================================================================#
# setting up Twitter Authentication
#   This code is commented out as it does not include the keys and secrets 
#   it would need to run
#==============================================================================#
setup_twitter_oauth(consumer_key = '-----',
                    consumer_secret = '-----',
                    access_token = '-----',
                    access_secret = '-----')

#==============================================================================#
# reading in the data from Twitter
#   This code is also commented out as it will not run without authentication
#==============================================================================#
zika_archive <- searchTwitter('zika', n = 100000000)
zika_archive <- ldply(zika_archive, function(x) x$toDataFrame() )
write.csv(zika_archive, 'zika_archive.csv')

#==============================================================================#
# reading in data already loaded from Twitter
#==============================================================================#
zika_raw_1 <- read.csv('------/zika_archive.csv')
zika_raw_2 <- read.csv('------/zika_archive.csv')
zika_raw_3 <- read.csv('------/zika_archive.csv')

#==============================================================================#
# making a single dataset (combining the pulls)
#   Combining is done by making a key by person, time, and text
#   to ensure no duplicates if time frames overlap
#==============================================================================#
#------------------------------------------------------------------------------#
# making the key
#------------------------------------------------------------------------------#
zika_raw_1$key <- paste(zika_raw_1$screenName, zika_raw_1$created, zika_raw_1$text)
zika_raw_2$key <- paste(zika_raw_2$screenName, zika_raw_2$created, zika_raw_2$text)
zika_raw_3$key <- paste(zika_raw_3$screenName, zika_raw_3$created, zika_raw_3$text)

#------------------------------------------------------------------------------#
# binding the datasets together
#------------------------------------------------------------------------------#
zika_full <- rbind(zika_raw_1, zika_raw_2, zika_raw_3)

#------------------------------------------------------------------------------#
# removing duplicates
#------------------------------------------------------------------------------#
zika_full <- zika_full[!duplicated(zika_full$key), ]

#------------------------------------------------------------------------------#
# getting rid of the key variable
#------------------------------------------------------------------------------#
zika_full <- zika_full[-length(zika_full)]
zika_full <- zika_full[!is.na(zika_full$X), ]

#==============================================================================#
# getting locations of all possible
#==============================================================================#
#------------------------------------------------------------------------------#
# finding the locations of everyone
#   commented out here as this will not be logged into Twitter
#   note outer loop is to repeat for arbitrarily long to get 
#   around twitter API limiting
#------------------------------------------------------------------------------#
zika_full$new_locations <- NA
people <- unique(zika_full$screenName)
for(q in 1:100){  # loop to fill in missing as API resets
  for(i in 1:length(people)){  # looping through tweets
    holder <- zika_full[zika_full$screenName == people[i], ]
    holder <- holder[1, ]
    if(is.na(holder$new_locations)){
      person <- holder$screenName
      holder <- person
      person <- tryCatch(getUser(person),  # if possible get the user information 
                         # from the screen name, otherwise NA
                         error = function(err){
                           bad <- NA
                           return(bad)
                         }
      )
      if(!is.null(person)){
        if(!is.na(person)){# if possible get location of the person, otherwise NA
          location <- tryCatch(twitteR::location(person), 
                               error = function(err){
                                 bad <- NA
                                 return(bad)
                               }
          )
        }
      }
      if(!is.na(location)){  # if we can get the location of the person, getting
        # the lat/lon from google
        location <- tryCatch(geocode(location, output = 'latlon', source = 'dsk'),
                             error = function(err){
                               bad <- NA
                               return(bad)
                             }
        )
      }
      if(!is.na(location)){
        latitude_new <- location$lat
        longitude_new <- location$lon
      } else {  # if we were unable to get the location from Twitter NAa
        latitude_new <- NA
        longitude_new <- NA
      }
      to_return <- paste(latitude_new, ', ', longitude_new, sep = '')  # returning
      # lat/lon
      zika_full$new_locations[zika_full$screenName == people[i]] <- to_return
    }
    zika_full$new_locations[zika_full$new_locations == 'NA, NA'] <- NA
    print(paste('person:', i, 'of:', length(people)))
    if(i %% 50000 == 0){
      write.csv(zika_full, 'zika_locations.csv')
    }
  }
  write.csv(zika_full, 'zika_locations.csv')
}


#------------------------------------------------------------------------------#
# reading in the saved zika locations file
#------------------------------------------------------------------------------#
zika_full <- read.csv('failsafes/zika_locations 4.csv')

#==============================================================================#
# 
# mapping
#
#==============================================================================#
#------------------------------------------------------------------------------#
# separating the latitude and longitude in the new_locations column
#------------------------------------------------------------------------------#
for(i in 1:length(zika_full$text)){
  if(is.na(zika_full$latitude[i])){
    holder <- strsplit(zika_full$new_locations[i], ', ')
    zika_full$latitude[i] <- holder[[1]][1]
    zika_full$longitude[i] <- holder[[1]][2]
  }
  print(i)
}

#------------------------------------------------------------------------------#
# writing this to avoid doing the loop again
#------------------------------------------------------------------------------#
write.csv(zika_full, 'zika_for_locations.csv')

#------------------------------------------------------------------------------#
# reading in dataset
#------------------------------------------------------------------------------#
zika_full <- read.csv('zika_for_locations.csv')

#------------------------------------------------------------------------------#
# plotting on a map outright
#------------------------------------------------------------------------------#
newmap <- getMap(resolution = 'medium')
plot(newmap)
points(zika_full$longitude, zika_full$latitude, col = 'blue', cex = 0.05)

#------------------------------------------------------------------------------#
# plotting on a map by time
#------------------------------------------------------------------------------#
# getting a color palatte
colors <- rainbow(n = 24, start = 0, end = 0.75)

# making an hour variable
zika_full$hour <- substr(zika_full$created, 12, 13)
zika_full$hour <- as.numeric(zika_full$hour)

# plotting by time
plot(newmap)
for(i in 1:24){
  holder <- zika_full[zika_full$hour == i, ]
  points(holder$longitude, holder$latitude, col = colors[i], cex = 0.45)
}

#==============================================================================#
# 
# text analysis
#
#==============================================================================#
#==============================================================================#
# word analysis
#   note this will make a VERY LARGE dataset, only do with much RAM 
#==============================================================================#
zika_long <- unnest_tokens(zika_full, word, text)

#------------------------------------------------------------------------------#
# sentiment analysis
#------------------------------------------------------------------------------#
bing <- sentiments %>%
  filter(lexicon == 'bing') %>%
  select(-score)

zika_sentiment <- zika_long %>%
  inner_join(bing) %>%
  count(screenName, sentiment) %>%
  spread(sentiment, n, fill  = 0) %>%
  mutate(sentiment = positive - negative)

#------------------------------------------------------------------------------#
# looking at the extreme outlier
#------------------------------------------------------------------------------#
min_name <- zika_sentiment$screenName[zika_sentiment$sentiment == min(zika_sentiment$sentiment)]
min_tweet <- zika_full[zika_full$screenName == min_name, ]
max_name <- zika_sentiment$screenName[zika_sentiment$sentiment == max(zika_sentiment$sentiment)]
max_tweet <- zika_full[zika_full$screenName == max_name, ]

#------------------------------------------------------------------------------#
# looking at distributions with a lower (and upper?) bound
#------------------------------------------------------------------------------#
zika_sentiment$sentiment[zika_sentiment$sentiment <= -10] <- -10
zika_sentiment$sentiment[zika_sentiment$sentiment >= 10] <- 10

hist(zika_sentiment$sentiment, breaks = 20)

#------------------------------------------------------------------------------#
# plotting by sentiment
#------------------------------------------------------------------------------#
# getting the sentiment and full dataset merged by person
zika_sent_full <- merge(zika_full, zika_sentiment, all.x = TRUE,
                        by.x = 'screenName', by.y = 'screenName')
zika_sent_plot <- zika_sent_full[!is.na(zika_sent_full$longitude), ]
zika_sent_plot <- zika_sent_plot[!is.na(zika_sent_plot$sentiment), ]

# plotting
plot(newmap)
colors <- rainbow(length(unique(zika_sent_plot$sentiment)), start = 0, end = 0.75)
for(i in min(zika_sent_plot$sentiment):max(zika_sent_plot$sentiment)){
  holder <- zika_sent_plot[zika_sent_plot$sentiment == i, ]
  points(holder$longitude, holder$latitude, col = colors[i], cex = 0.5)
}

#==============================================================================#
# making a table of the most used words within the bing lexicon
#==============================================================================#
zika_count <- zika_long %>% count(word, sort = TRUE)
zika_count <- zika_count[zika_count$word %in% c('zika', bing$word), ]

#------------------------------------------------------------------------------#
# making the table itself
#------------------------------------------------------------------------------#
pander(zika_count[1:50, ], caption = 'Table 1: Most common words in all the tweets.')

#==============================================================================#
# making a word-pairing table
#==============================================================================#
#------------------------------------------------------------------------------#
# making word pairing datasets of words paired with zika
#------------------------------------------------------------------------------#
zika_pairs <- zika_long %>% 
  pair_count(screenName, word, sort = TRUE)
zika_pairs <- zika_pairs[((zika_pairs$value1 %in% bing$word) & (zika_pairs$value2 %in% bing$word)) | 
                           (zika_pairs$value1 == 'zika' & (zika_pairs$value2 %in% bing$word)) |
                           ((zika_pairs$value1 %in% bing$word) & zika_pairs$value2 == 'zika'), ]

#------------------------------------------------------------------------------#
# making the table itself
#------------------------------------------------------------------------------#
pander(zika_pairs[1:50, ], caption = 'Table 2: Most paired sets of words in all the tweets.')

#==============================================================================#
# table of heavy tweeters
#==============================================================================#
zika_datatable <- data.table(zika_sent_full)
zika_coll <- zika_datatable[ ,
                             list(
                               tweets = length(text),
                               sentiment = mean(sentiment),
                               longitude = mean(longitude),
                               latitude = mean(latitude)
                             ),
                             by = screenName]

# sorting zika_coll
zika_coll <- zika_coll[order(zika_coll$tweets, decreasing = TRUE)]

#------------------------------------------------------------------------------#
# making the table itself
#------------------------------------------------------------------------------#
pander(zika_coll[1:50, ], caption = 'Table 3: Most prolific tweeters within the tweets acquired.')

#==============================================================================#
#
# time based analysis
#
#==============================================================================#
zika_time <- zika_sent_full

#==============================================================================#
# getting day/time subsets for use next
#==============================================================================#
#------------------------------------------------------------------------------#
# getting seprate days, minutes, seconds, hours, months...
#------------------------------------------------------------------------------#
zika_time$seconds <- as.numeric(substr(zika_time$created, 18, 19))
zika_time$minutes <- as.numeric(substr(zika_time$created, 15, 16))
zika_time$days <- as.numeric(substr(zika_time$created, 9, 10))
zika_time$months <- as.numeric(substr(zika_time$created, 6, 7))
zika_time$date <- as.numeric(substr(zika_time$created, 1, 10))
zika_time$date <- as.Date(zika_time$date, format = '%Y-%m-%d')

#------------------------------------------------------------------------------#
# getting weekdays
#------------------------------------------------------------------------------#
zika_time$weekday <- weekdays(zika_time$date, abbreviate = TRUE)
zika_time$weekdayn <- 0
zika_time$weekdayn[zika_time$weekday == 'Mon'] <- 1
zika_time$weekdayn[zika_time$weekday == 'Tue'] <- 2
zika_time$weekdayn[zika_time$weekday == 'Wed'] <- 3
zika_time$weekdayn[zika_time$weekday == 'Thu'] <- 4
zika_time$weekdayn[zika_time$weekday == 'Fri'] <- 5
zika_time$weekdayn[zika_time$weekday == 'Sat'] <- 6
zika_time$weekdayn[zika_time$weekday == 'Sun'] <- 7

#------------------------------------------------------------------------------#
# combining time measurements to get larger time scales
#------------------------------------------------------------------------------#
zika_time$min_sec <- zika_time$minutes + (zika_time$seconds / 60)
zika_time$hour_min <- zika_time$hour + (zika_time$min_sec / 60)
zika_time$day_hm <- zika_time$days + (zika_time$hour_min / 24)
zika_time$month_dhm <- zika_time$months + (zika_time$day_hm / 30)

zika_time$weektime <- zika_time$weekdayn + (zika_time$hour_min/24)

#==============================================================================#
# plotting things
#==============================================================================#
hist(zika_time$weektime, breaks = 100, ylab = 'day of the week', xlab = 'tweets',
     main = 'Zika Tweets by Day of the Week')
hist(zika_time$month_dhm, breaks = 100, ylab = 'month', xlab = 'tweets',
     main = 'Zika Tweets Within Overall Timeframe')
hist(zika_time$hour_min, breaks = 100, ylab = 'hour', xlab = 'tweets',
     main = 'Zika Tweets by Hour of the Day')