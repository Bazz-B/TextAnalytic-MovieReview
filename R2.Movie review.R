# 1. LOAD PACKAGE: tm, SnowballC, textstem, wordcloud, ggplot2

# 2. LOAD DATA

#create corpus from CSV
review.data <- read.csv("C:/Users/syaki/Documents/YAYASAN PENERAJU/WEEK 6 - DS05 TEXT ANALYTICS 2/DATA/movie_review_thecreator_imdb_10.csv", header = TRUE)

review.docs <- Corpus(DataframeSource(review.data))

#incase want to delete a corpus
#remove (review.data)
#remove(review.docs)


# 3. view corpus information
review.docs
summary(review.docs)

#inspect a particular document (e.g. the 3rd doc)
writeLines(as.character(review.docs[[3]]))


# 4. TEXT PREPROSESSING

#checkout tm package transformation functions
getTransformations()

#create toSpace content transformer
toSpace <- content_transformer(function(x,pattern) {return(gsub(pattern," ",x))})

#convert hypen to space using toSpace content transformer
docs.clean <- tm_map(review.docs, toSpace, "_")

writeLines(as.character(review.docs[[3]]))

writeLines(as.character(docs.clean[[3]]))

#clean "(2023)" from text [3]
docs.clean <- tm_map(docs.clean, toSpace, "(2023)")

writeLines(as.character(review.docs[[3]]))

#apply removePunctuation
docs.clean <-tm_map(docs.clean, removePunctuation)

#convert corpus to lower case
review.docs <-tm_map(docs.clean, content_transformer(tolower))

#remove digits in corpus
docs.clean <-tm_map(docs.clean, removeNumbers)

writeLines(as.character(docs.clean[[3]]))

#remove stopwords using the standard list in tm
docs.clean <- tm_map(docs.clean, removeWords, stopwords("english"))

writeLines(as.character(docs.clean[[3]]))

#remove additional stop words
custom.stopwords <- c('I', 'My', 'The', 'A', 'While', 'find')
docs.clean <- tm_map(docs.clean, removeWords, custom.stopwords)

writeLines(as.character(docs.clean[[3]]))


# 5. TEXT NORMALIZATION (stemming & lemmatization)
  
# STEMMING (load SnowbollC)
#duplicate object for testing
docs.stem <- docs.clean
docs.lemma <- docs.clean

#stem the corpus
docs.stem<-tm_map(docs.clean, stemDocument)

#display the content after steamming 
writeLines(as.character(docs.clean[[3]]))

# Lemmatization (load textstempackage)
#duplicate object for testing

#lemmatize the corpus
docs.lemma<-tm_map(docs.clean, lemmatize_strings)

#display the content after lemmatization
writeLines(as.character(docs.clean[[3]]))

writeLines(as.character(docs.stem[[3]]))

writeLines(as.character(docs.lemma[[3]]))


# 6. DTM

#create document term matrix to find the most used word.
movie.dtm <- DocumentTermMatrix(docs.lemma)

movie.dtm

sum(movie.dtm)

inspect(movie.dtm)

#inspect document term matrix by specifying rows and columns
# view row 1 to row 5, term 11 to term 20.
inspect(movie.dtm[1:5,11:20])

#after cleaning total word remaining
sum(movie.dtm)


# 7. TEXT MINING CORPUS TO TOKENAZIATION

# 7.1 Term Frequency. Get frequency of each word
movie.freq<-colSums(as.matrix(movie.dtm))

#check total number of terms
length(movie.freq)

#create sort order (descending)
movie.ord<-order(movie.freq, decreasing=TRUE)

#inspect most frequently occurring terms (default is 6)
movie.freq[head(movie.ord)]

#inspect most frequently occurring terms (e.g. 10)
movie.freq[head(movie.ord, 10)]

#inspect less frequently occurring terms (default is 6)
movie.freq[tail(movie.ord)]

#inspect less frequently occurring terms (e.g. 10)
movie.freq[tail(movie.ord, 10)]

# 7.2 Term Reduction
dtm.tr<-DocumentTermMatrix(docs.lemma, control = list(wordLengths=c(4,20), bounds=list(global=c(2,8))))

# Get frequency of each word
freq.tr<-colSums(as.matrix(dtm.tr))

#Check total number of terms after reduction
length(freq.tr)

#create sort order (descending)
movie.ord<-order(freq.tr, decreasing=TRUE)

#inspect most frequently occurring terms (default is 6)
freq.tr[head(movie.ord)]

#inspect most frequently occurring terms (e.g. 10)
freq.tr[head(movie.ord, 10)]

#inspect less frequently occurring terms (default is 6)
freq.tr[tail(movie.ord)]

#inspect less frequently occurring terms (e.g. 10)
freq.tr[tail(movie.ord, 10)]

# 7.3 Term Correlation
#find term correlation. Find term with 0.6 above in film
findAssocs(dtm.tr, "film", 0.5)

findAssocs(dtm.tr, "movie", 0.5)

findAssocs(dtm.tr, "story", 0.5)

# 8. PLOT SIMPLE GRAPHIC

# 8.1 Plot simple frequency histogram

#create a data frame (consists of name of the column, e.g. term and occurencesand the data)
wordfreq=data.frame(term=names(movie.freq), occurences=movie.freq)

wordfreq

# load ggplot2 package

#invoke ggplot(plot only terms more than 3 times, label x and y-axis using aes)
phisto<-ggplot(subset(wordfreq, movie.freq>3), aes(term, occurences))

#set the height of the bar using stat="bin" or "identity" ("identify" means the height is based on the data value mapped to y-axis)
phisto<-phisto+ geom_bar(stat="identity", fill="lightblue")

#specify that the x-axis text is at 45 degree angle and horizontally justified
phisto<-phisto+ theme(axis.text.x=element_text(angle=45, hjust=1))

#specify the freq of terms on the plot
phisto<-phisto+ geom_text(aes(label=occurences), color="black")

#display histogram
phisto


# 8.2 PLOT WORD CLOUD (load Wordcloud package)

#check object
movie.freq

#setting the seed before each plot to ensure consistent look for clouds (optional)
set.seed(35)

#limit words by specifying min frequency
wordcloud(names(movie.freq), movie.freq, min.freq=4, scale=c(4.0,0.5))

#limit words by specifying min frequency (with colour)
wordcloud(names(movie.freq), movie.freq, min.freq=5, scale=c(4.0,0.5), colors=brewer.pal(6,"Dark2"))

wordcloud(names(movie.freq), movie.freq, min.freq=5, scale=c(4.0,0.5), colors=brewer.pal(6,"Dark2"))

#load wordcloud2 package

wordcloud2(data=wordfreq, size=5, color='random-dark')

wordcloud2(data=wordfreq, size=1, color='random-dark', shape='star')


# 8.3 PLOT Dendrogram

dtm.tr.cluster <- removeSparseTerms(dtm.tr, sparse = 0.7)

m.cluster <- as.matrix(dtm.tr.cluster)

tdm.tr.cluster <- t(dtm.tr.cluster)

m2.cluster <- as.matrix(tdm.tr.cluster)

#Distance matrix computation
m2.cluster.distMatrix <- dist(scale(m2.cluster), method = "euclidean")

m2.cluster.fit <- hclust(m2.cluster.distMatrix, method = "ward.D")

#Plot
plot(m2.cluster.fit, main="Term Clusters for Reviews", cex = 0.6)

# k indicate the number of clusters.k=4 cluster  
rect.hclust(m2.cluster.fit, k = 4)

rect.hclust(m2.cluster.fit, k = 3)


# 8.4 PLOT WORDS ASSOCIATED WITH "film"

associations <- findAssocs(dtm.tr, "film", 0.6)

associations <- as.data.frame(associations)

associations$terms <- row.names(associations)

associations$terms <- factor(associations$terms, levels = associations$terms)

assocgraph <- ggplot(associations, aes(y=terms)) + geom_point(aes(x=film), data = associations, size=3) + theme_gdocs() + geom_text(aes(x=film, label=film), color="darkred", hjust=-.25, size=3) + theme(text = element_text(size = 10), axis.title.y = element_blank())

assocgraph
