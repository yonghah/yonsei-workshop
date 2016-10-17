inst.pckg <- rownames(installed.packages())
required.pckg = c('devtools', 
                  'twitteR', 
                  'RCurl', 
                  'RJSONIO', 
                  'stringr', 
                  'tm', 
                  'wordcloud', 
                  'KoNLP', 
                  'Unicode')

for (name.pckg in required.pckg) {
  if (!(name.pckg %in% inst.pckg)) install.packages(name.pckg)
  library(name.pckg, character.only=TRUE)
}

library("twitteR")
library("base64enc")
library("httpuv")

consumerKey <- "ShwmmHPl3QFqnaXnglNlYXeGJ" # https://apps.twitter.com/
consumerSecret <- "Eh8NwdKl6EbK1Ybe9m4qdD97QkI3oO5SXtO8KZd2OH5RISoNQz" # keys and access tokens
access_token <- "2554061088-xPynklWnra2QRrhlGBEeyqjeJ3Akd61HsGH6m6U"
access_secret <- "ZYNYXX85KzPeOD943USU9zozZjvmAlszjIUocvNwnRgWi"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    access_token,
                    access_secret)


r_stats <- searchTwitter("대교", n = 1000, lang="ko", since="2013-08-20")
length(r_stats)
result.df <- twListToDF(r_stats)
result.text <- result.df$text  # get text info only from the data frame
result.text <- gsub('\n', '', result.text)
result.text <- gsub('\r', '', result.text)
result.text <- gsub('@[a-zA-Z0-9_]+', '', result.text)  # remove twitter user id
result.text <- gsub('[0-9]+', '', result.text)  # remove numbers
result.text <- gsub('RT', '', result.text)  # remove 'RT'
result.text <- gsub('http[[:alpha:]]*://[[:graph:]]*', '', result.text)  # remove URLs
result.text <- gsub('http*', '', result.text)  # remove exceptional URLs

result_nouns <- Map(extractNoun, result.text)  # extract nouns from the above result

result_words <- unlist(result_nouns, use.name=F)

result_words <- gsub('[[:punct:]]', "", result_words)
result_words <- Filter(function(x){nchar(x)>=2}, result_words)

pal <- brewer.pal(8,'Accent')
result_wordcount <- table(result_words)
wordcloud(names(result_wordcount), freq=result_wordcount, scale=c(5,0.5), 
          min.freq=5, random.order=F, rot.per=.1, colors=pal)