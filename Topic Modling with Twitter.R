library(twitteR) #Used to get data from Twitter
library(dplyr) #Used for data manipulation
library(ggplot2) #Used to make plots and graphs
library(scales) #A helper function for formatting scales and numbers
library(LDAvis) #Creates a visual which is used to explore topics
library(lda) #contains fucntion for LDA
library(tm) #a topic modeling package with lots of helpful functions
library(servr) #for rendering data to the vis
library(RJSONIO) #to read and write data from the JSON
library(reshape2) #Used for data manipulation
library(ggthemes) #a library of themes for ggplot

#Set the working directory
setwd("C:/Users/Greg/Documents/Topic Modeling 7-12-2016")


#Set up the authenition needed to pull back tweets.  See the vignette for hte twitteR package. 
#It walks throught the process pretty painlessly.  Took me about 10 min to set up.
setup_twitter_oauth(consumer_key = "x",
                    consumer_secret = "x",
                    access_token = "x",
                    access_secret = "x")


#Bring back tweets for the given search term. I also stripped retweets to make the data more clean.
Search_Term = "#Pokemongo"
Twitter_Text_Data = searchTwitter(Search_Term, lang = "en", n = 1000)%>%
  strip_retweets(strip_mt = TRUE, strip_manual = TRUE)
                      


Twitter_Text =  sapply(Twitter_Text_Data, function(x){x$text})



#Create a helper function to clean up text.
Pre_Process_Text = function(x){
  x <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", replacement = "", x) #remove urls
  x <- gsub(Search_Term, "", x, fixed = TRUE ) #Take out mentions of the search time
  x <- gsub("#\\S+", "", x)
  x <- gsub("[^[:alnum:] ]","", x)
  x <- gsub("'", "", x)  # remove apostrophes
  x <- gsub("[[:punct:]]", " ", x)  # replace punctuation with space
  x <- gsub("[[:cntrl:]]", " ", x)  # replace control characters with space
  x <- gsub("^[[:space:]]+", "", x) # remove whitespace at beginning of documents
  x <- gsub("[[:space:]]+$", "", x) # remove whitespace at end of documents
  x <- tolower(x)  # force to lowercase
  
 
}

Twitter_Text = Pre_Process_Text(Twitter_Text)



#Create a list and make each item in the list a character vector with the words in each tweet.
doc.list = strsplit(Twitter_Text, "[[:space:]]+")

#Create a list of all the terms in the tweets and sort them
term.table = table(unlist(doc.list))
term.table = sort(term.table, decreasing = TRUE)

#Create a list of stop words
stop_words = c(stopwords("SMART"), "amp", "brexit")


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 3
term.table <- term.table[!del]
vocab <- names(term.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)


length(documents)# Number of Documents
length(vocab) #total number of words
sapply(documents, function(x) sum(x[2, ])) #Number of words per tweet after pre processing


t1 = Sys.time()

#This fucntion actually does the topic modeling
fit = lda.collapsed.gibbs.sampler(documents = documents, K = 7, vocab = vocab, 
                                         num.iterations = 2000, alpha = .02, 
                                         eta = .02, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
t2 = Sys.time()
  
t2 - t1

#What is in the model output we've created?
summary(fit)
head(fit$assignments) #Shows the assignment of words to topic for each tweet
fit$topics[1:7,1:3] #Shows the counts of how words are assigned to topics
fit$topic_sums
fit$document_sums[1:7,1:3] #Shows the number of times a topic is assigned in for each tweet

#Create variables of topic distributions for visualization
#Theta is the distribution of topics per doc
theta <- t(apply(fit$document_sums + .02, 2, function(x) x/sum(x)))

#Phi is the distribution of words per topic
phi <- t(apply(t(fit$topics) + .02, 2, function(x) x/sum(x)))

Tweet_Topics <- list(phi = phi,
                     theta = theta,
                     doc.length = sapply(documents, function(x) sum(x[2, ])),
                     vocab = vocab,
                     term.frequency = as.integer(term.table))


# create the JSON object to feed the visualization:
json <- createJSON(phi = Tweet_Topics$phi, 
                   theta = Tweet_Topics$theta, 
                   doc.length = Tweet_Topics$doc.length, 
                   vocab = Tweet_Topics$vocab, 
                   term.frequency = Tweet_Topics$term.frequency)

#This saves the creates a new directory in your working directory and creates the visualization
serVis(json, out.dir = 'vis7', open.browser = TRUE)


#Find the original text from a few tweets most related to a particular topic
#This is really confusing because the vis topic numbers aren't the same
#as the model top numbers because reasons
Topic_from_Vis = 4
Top_Tweets = order(fit$document_sums[fromJSON(json)$topic.order[Topic_from_Vis],]
                   ,decreasing = TRUE)[1:10]
sapply(Twitter_Text_Data, function(x) x$text)[Top_Tweets]


#Display a barchart with topic distributions
Number_Of_Docs_to_Plot = 10
topic_proportions = t(fit$document_sums)/colSums(fit$document_sums)
Sample_Topic_Proportions = sample_n(as.data.frame(topic_proportions[which(colSums(fit$document_sums) != 0),]),
                                    Number_Of_Docs_to_Plot )

#Find the top words per topic and transcribe them to match the interactive Vis  
top_words = top.topic.words(fit$topics, 5)[,fromJSON(json)$topic.order]

#Change the Column names to the top words in the topic
colnames(Sample_Topic_Proportions) = apply(top_words, 2, paste, collapse = ", ")

ggplot(melt(data = cbind(Sample_Topic_Proportions, document = factor(1:Number_Of_Docs_to_Plot)),
            variable.name = "topic", id.vars = "document"))+
  geom_bar(aes(x = topic, y = value, fill = document), stat = "identity")+
  scale_y_continuous(labels = percent)+
  facet_wrap(~document)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")





