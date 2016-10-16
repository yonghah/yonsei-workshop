# Run this in vanila R console

library(ROAuth)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "ShwmmHPl3QFqnaXnglNlYXeGJ" # https://apps.twitter.com/
consumerSecret <- "	Eh8NwdKl6EbK1Ybe9m4qdD97QkI3oO5SXtO8KZd2OH5RISoNQz" # keys and access tokens

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

# enter pin and then save

save(my_oauth, file = "~/Repo/twitR/my_oauth.Rdata")