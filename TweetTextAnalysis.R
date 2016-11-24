library(tm)
library(wordcloud)
library(cluster)
library(twitteR)
library(SnowballC)

#enter api_key,apisecret, acces token and access secret in order.
setup_twitter_oauth("fUn007J6pP6z23eTI1To5cg7S","5CfT128TEAnCS9e7Y6e0JKp8Pbl1iDtdqzqfslTjHrq44SxQLs","3172067551-W4DOzJAEsjpyYsQbI0M1n9W2vaZYbh9M2577fS7","HKBGmb8vvirivCvrT83XRTGfZTpxabe2l0Wo4ysoEXl3W")

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