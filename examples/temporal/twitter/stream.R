library(streamR)
library(jsonlite)
library(RJSONIO)
library(dplyr)

load("my_oauth.Rdata")

filterStream(file.name = "data/loc.json", # Save tweets in a json file
             # track = c("obama"), # Collect tweets mentioning typhoon
             #language = "en",
             location = c(127.040, 37.497, 127.105, 37.527), # latitude/longitude pairs providing southwest and northeast corners of the bounding box.
             timeout = 60, # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

df <- parseTweets("data/loc.json")
