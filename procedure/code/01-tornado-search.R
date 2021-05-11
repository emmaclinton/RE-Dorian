---
title: "tweetMIoubreak"
author: "Emma Clinton"
date: "4/29/2021"
output: html_document
---
  #search geographic twitter data for GA tornadoes, by Emma Clinton, 2021
  #to search, you first need a twitter API token: https://rtweet.info/articles/auth.html 
  

#install package for twitter and initialize the library
packages = c("rtweet","here")
install.packages("tidyverse")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())), quietly=TRUE)

library(rtweet)
library(here)
library(tidyverse)

## store api keys (these are fake example values; replace with your own keys)
api_key <- ""
api_secret_key <- ""
access_token <- ""
access_token_secret <- ""


############# SEARCH TWITTER API ############# 

#reference for search_tweets function: https://rtweet.info/reference/search_tweets.html 
#don't add any spaces in between variable name and value. i.e. n=1000 is better than n = 1000
#the first parameter in quotes is the search string, searching tweet contents and hashtags
#n=10000 asks for 10,000 tweets
#if you want more than 18,000 tweets, change retryonratelimit to TRUE and wait 15 minutes for every batch of 18,000
#include_rts=FALSE excludes retweets.
#token refers to the twitter token you defined above for access to your twitter developer account
#geocode is equal to a string with three parts: longitude, latitude, and distance with the units mi for miles or km for kilometers

#set up twitter API information
#this should launch a web browser and ask you to log in to twitter
#replace app, consumer_key, and consumer_secret data with your own developer acct info
twitter_token <- create_token(
  app = "",  					#replace yourapp with your app name
  consumer_key = "",  		#replace yourkey with your consumer key
  consumer_secret = "",  #replace yoursecret with your consumer secret
  access_token = NULL,
  access_secret = NULL
)

#get tweets for CA fire (past 7 days)
#CAfire <- search_tweets("fire OR wildfire OR evacuate OR burned", n=200000, include_rts=FALSE, token=twitter_token, geocode="33,-116,1000mi", retryonratelimit=TRUE)

#get tweets for GA tornado (past 7 days)
GAtornado <- search_tweets("tornado OR warning OR shelter OR storm", n=200000, include_rts=FALSE, token=twitter_token, geocode="33,-84,3000mi", retryonratelimit=TRUE)

#get tweets for GA tornado (past 7 days)
GAbaseline <- search_tweets("-tornado OR -warning OR -shelter OR -storm", n=200000, include_rts=FALSE, token=twitter_token, geocode="33,-84,3000mi", retryonratelimit=TRUE)

############# FILTER TWEETS FOR CREATING PRECISE GEOMETRIES ############# 

# reference for lat_lng function: https://rtweet.info/reference/lat_lng.html
# adds a lat and long field to the data frame, picked out of the fields
# that you indicate in the c() list
# sample function: lat_lng(x, coords = c("coords_coords", "bbox_coords"))

# list and count unique place types
# NA results included based on profile locations, not geotagging / geocoding.
# If you have these, it indicates that you exhausted the more precise tweets 
# in your search parameters and are including locations based on user profiles
count(GAtornado, place_type)

# convert GPS coordinates into lat and lng columns
# do not use geo_coords! Lat/Lng will be inverted
GAtornado = lat_lng(GAtornado, coords=c("coords_coords"))


# select any tweets with lat and lng columns (from GPS) or 
# designated place types of your choosing
GAtornado = subset(GAtornado, 
                   place_type == 'city'| place_type == 'neighborhood'| 
                     place_type == 'poi' | !is.na(lat))


# convert bounding boxes into centroids for lat and lng columns
GAtornado = lat_lng(GAtornado,coords=c("bbox_coords"))


# re-check counts of place types
count(GAtornado, place_type)

############# FILTER TWEETS FOR CREATING PRECISE GEOMETRIES (Baseline) ############# 

# reference for lat_lng function: https://rtweet.info/reference/lat_lng.html
# adds a lat and long field to the data frame, picked out of the fields
# that you indicate in the c() list
# sample function: lat_lng(x, coords = c("coords_coords", "bbox_coords"))

# list and count unique place types
# NA results included based on profile locations, not geotagging / geocoding.
# If you have these, it indicates that you exhausted the more precise tweets 
# in your search parameters and are including locations based on user profiles
count(GAbaseline, place_type)

# convert GPS coordinates into lat and lng columns
# do not use geo_coords! Lat/Lng will be inverted
GAbaseline = lat_lng(GAbaseline, coords=c("coords_coords"))


# select any tweets with lat and lng columns (from GPS) or 
# designated place types of your choosing
GAbaseline = subset(GAbaseline, 
                    place_type == 'city'| place_type == 'neighborhood'| 
                      place_type == 'poi' | !is.na(lat))


# convert bounding boxes into centroids for lat and lng columns
GAbaseline = lat_lng(GAbaseline,coords=c("bbox_coords"))


# re-check counts of place types
count(GAbaseline, place_type)

############# SAVE FILTERED TWEET IDS TO DATA/DERIVED/PUBLIC ############# 

write.table(GAbaseline$status_id,
            here("data","derived","public","GAbaselineids.txt"), 
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

write.table(GAtornado$status_id,
            here("data","derived","public","GAtweetids.txt"), 
            append=FALSE, quote=FALSE, row.names = FALSE, col.names = FALSE)

############# SAVE TWEETs TO DATA/DERIVED/PRIVATE ############# 

saveRDS(GAtornado, here("data","derived","private","GAtornado.RDS"))
saveRDS(GAbaseline, here("data","derived","private","GAbaseline.RDS"))
```

#install packages for twitter, census, data management, and mapping
packages = c("rtweet","tidycensus","tidytext","maps","RPostgres","igraph","tm", "ggplot2","RColorBrewer","rccmisc","ggraph","here")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())), quietly=TRUE)

#initialize the libraries. this must be done each time you load the project
library(rtweet)
library(igraph)
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggraph)
library(tidycensus)
library(ggplot2)
library(RPostgres)
library(RColorBrewer)
library(DBI)
library(rccmisc)
library(here)

############# TEMPORAL ANALYSIS ############# 

#this is here as an example. change to the dorian3 data you processed in the previous script to try...

#create temporal data frame & graph it

twisterByHour <- ts_data(GAtornado, by="hours")
ts_plot(GAtornado, by="hours")


############# NETWORK ANALYSIS ############# 

# Create network data frame. 
# Other options for 'edges' in the network include mention, retweet, and reply
GAtornadoNetwork <- network_graph(GAtornado, c("quote"))

plot.igraph(GAtornadoNetwork)
# This graph needs serious work... e.g. subset to a single state maybe?


############# TEXT / CONTEXTUAL ANALYSIS ############# 

# remove urls, fancy formatting, etc. in other words, clean the text content
tornadoText = GAtornado %>% select(text) %>% plain_tweets()

# parse out words from tweet text
tornadoWords = tornadoText %>% unnest_tokens(word, text)

# how many words do you have including the stop words?
count(tornadoWords)

# create list of stop words (useless words not worth analyzing) 
data("stop_words")

# add "t.co" twitter links to the list of stop words
# also add the twitter search terms to the list
stop_words = stop_words %>% 
  add_row(word="t.co",lexicon = "SMART") %>% 
  add_row(word="tornado",lexicon = "Search") %>% 
  add_row(word="storm",lexicon = "Search") %>% 
  add_row(word="shelter",lexicon = "SMART") %>% 
  add_row(word="warning",lexicon = "Search")

#delete stop words from dorianWords with an anti_join
tornadoWords =  tornadoWords %>% anti_join(stop_words) 

# how many words after removing the stop words?
count(tornadoWords)

# graph frequencies of words
tornadoWords %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# separate words and count frequency of word pair occurrence in tweets
tornadoWordPairs = tornadoText %>% 
  mutate(text = removeWords(tolower(text), stop_words$word)) %>%
  unnest_tokens(paired_words, text, token = "ngrams", n = 2) %>%
  separate(paired_words, c("word1", "word2"),sep=" ") %>%
  count(word1, word2, sort=TRUE)

# graph a word cloud with space indicating association.
# you may change the filter to filter more or less than pairs with 30 instances
tornadoWordPairs %>%
  filter(n >= 25 & !is.na(word1) & !is.na(word2)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network of Tweets during Recent GA Tornado",
       x = "", y = "") +
  theme_void()

############# SPATIAL ANALYSIS ############# 

#first, sign up for a Census API here: https://api.census.gov/data/key_signup.html
#replace the key text 'yourkey' with your own key!
counties <- get_estimates("county",product="population",output="wide",geometry=TRUE,keep_geo_vars=TRUE, key="")

#select only the states you want, with FIPS state codes in quotes in the c() list
#look up fips codes here: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code 
counties = filter(counties,STATEFP %in% c('54', '51', '50', '47', '45', '44', '42', '39', '37','36', '34', '33', '29', '28', '25', '24', '23', '22', '21', '18', '17','13', '12', '11', '10', '09', '05', '01') )

counties = filter(counties,STATEFP %in% c('01', '13', '29', '22', '28', '12') )

#map results with GGPlot
#note: cut_interval is an equal interval classification function, while cut_numer is a quantile / equal count function
#you can change the colors, titles, and transparency of points
ggplot() +
  geom_sf(data=counties, aes(fill=cut_number(DENSITY,5)), color="grey")+
  scale_fill_brewer(palette="GnBu")+
  guides(fill=guide_legend(title="Population Density"))+
  geom_point(data = GAtornado, aes(x=lng,y=lat),
             colour = 'purple', alpha = .5) +
  labs(title = "Tweet Locations During Recent GA Hurricane")+
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
