library(WikipediR)

wp_content <- revision_content("en","wikipedia", revisions = 552373187)

# Lovelyz updates

#https://en.wikipedia.org/w/api.php?action=query&prop=revisions&titles=Lovelyz&format=json&rvlimit=max