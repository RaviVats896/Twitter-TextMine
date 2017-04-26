library(tm)
library(wordcloud)
library(cluster)
library(twitteR)
library(SnowballC)

#enter api_key,apisecret, access token and access secret in order.
#get the above mentioned keys & tokens from your twitter app. Link:  https://apps.twitter.com/
setup_twitter_oauth(<your_api_key>,<your_api_secret>,<your_access_token>,<your_access_secret>)

tweets<-searchTwitter("modi",n=5000,lang="en")
mytweets<-sapply(tweets,function(x) x$getText())
mytweets<- sapply(mytweets,function(x) iconv(x, "latin1", "ASCII", sub=""))
mycorpus<-Corpus(VectorSource(mytweets)) 


RedundantWords<-c("modi","narendra","pm","rt","amp","via","https","retweeted","tweets","http")

#Cleaning the corpus.
clean<-tm_map(mycorpus,removeNumbers)
clean<-tm_map(clean,removeWords,stopwords(kind="en"))
clean<-tm_map(clean,content_transformer(tolower))
clean<-tm_map(clean,removeWords,RedundantWords)
clean<-tm_map(clean,removePunctuation)
clean<-tm_map(clean,stripWhitespace) 
clean <- tm_map(clean,stemDocument)
clean <- tm_map(clean,PlainTextDocument)

#making a document term matrix
TermMatrix<-DocumentTermMatrix(clean)
rownames(TermMatrix)<-c(1:5000)

#finding most frequent Terms.
mostfreqterms <- findFreqTerms(TermMatrix,lowfreq=50)

#making associations
assoc<-c()
for(i in mostfreqterms)
  {
     
  myassoc<-findAssocs(TermMatrix,i,corlimit = 0.50) 
    assoc<-c(assoc,myassoc)
} 

#saving it as a file
sink("D:/assoc.txt")
assoc
sink()

#Removing less frequent terms.
NewTermMatrix <- removeSparseTerms(TermMatrix,0.95)

#finding distance matrix, on which kmeans would be applied.
result <- kmeans(dist(t(NewTermMatrix)), 4)

Words<-data.frame(colnames(result$centers))
colnames(Words)<-c("Names")
Words$freq<-colSums(as.matrix(NewTermMatrix))
Words$class<-result$cluster
wordcloud(Words$Names,freq=Words$freq,colors = result$cluster,ordered.colors = TRUE,random.order = FALSE,rot.per=0)
#We want to color the words based on their clusters.
#Such that all members of 1 cluster get the same color.
