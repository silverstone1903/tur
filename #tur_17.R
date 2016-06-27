install.packages(c("devtools", "rjson", "bit64", "httr", "ROAuth", "tm", "wordcloud", "RColorBrewer", "ggplot2"))
devtools::install_github("geoffjentry/twitteR")
library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)


# authorization

api_key <- "your api key"

api_secret <- "your api secret"

access_token <- "your access token"

access_token_secret <- "your access token secret"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


# get tweets
tur_17 <- searchTwitter('#TUR+euro2016', since='2016-06-17', n = 8000, lang = "tr") #I tried it with until argument but it didn't worked. 
length(tur_17)
head(tur_17)




# create a corpus
tur_17_text <- sapply(tur_17, function(x) x$getText())
tur_17_corpus <- Corpus(VectorSource(tur_17_text))
tur_17_corpus <- tm_map(tur_17_corpus, content_transformer(tolower))
tur_17_corpus <- tm_map(tur_17_corpus, removePunctuation)
tur_17_corpus <- tm_map(tur_17_corpus, function(x)removeWords(x,stopwords()))

# wordcloud
pal2 <- brewer.pal(8,"Dark2")
wordcloud(tur_17_corpus,min.freq=10,max.words=15, random.order=T, colors=pal2)

#############################################################################
#                                                                           #
#                                                                           #
#                               Part 2                                      #
#                                                                           #
#                                                                           #
#############################################################################


# dtm
dtm <- DocumentTermMatrix(tur_17_corpus)
dtm

# tdm
tdm <- TermDocumentMatrix(tur_17_corpus)
tdm

# freq
freq <- colSums(as.matrix(dtm))   
length(freq) 
# order
ord <- order(freq)   

# save the file as csv
# m <- as.matrix(dtm)   
# dim(m)   
# write.csv(m, file="dtm.csv") 


freq[head(ord)]
# sparse terms
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 20) 

freq <- colSums(as.matrix(dtms))   
freq 

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)

findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.

wf <- data.frame(word=names(freq), freq=freq) 
head(wf)


# visualization
library(ggplot2)   
p <- ggplot(subset(wf, freq>100), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

# find the associations 
findAssocs(dtm, c("milli" , "veda"), corlimit=0.80) # specifying a correlation limit of 0.80  


findAssocs(dtms, "çocuğu", corlimit=0.80) # specifying a correlation limit of 0.80  


wordcloud(names(freq), freq, min.freq=50, scale=c(5, .8), colors=brewer.pal(6, "Dark2")) 

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=30, rot.per=0.2, colors=dark2)



####### clustering #######

dtmss <- removeSparseTerms(dtm, 0.90) # This makes a matrix that is only 90% empty space, maximum.   
inspect(dtmss) 


library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit  
plot(fit, hang=-1)


###### k-means ######

library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
