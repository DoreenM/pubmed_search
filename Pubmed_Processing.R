# install packages if required

if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm" , repos = "http://cran.us.r-project.org")
if(!require(SnowballC)) install.packages("SnowballC" , repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud" , repos = "http://cran.us.r-project.org")

# load libraries of installed packages

library("tm")
library("SnowballC")
library(ggplot2)
library("wordcloud")
library("RColorBrewer")
library(tidytext)
library(dplyr)


#Load pubmed_data.csv
pubmed_data <- read.csv("data/pubmed_data.csv")
View(pubmed_data)

# number of documents on RSV per year

count <- as.data.frame(table(pubmed_data$Year))
names(count)[names(count)=="Var1"] <- "Year"
names(count)[names(count)=="Freq"] <- "number"

jpeg(filename = "figures/documentperYear.jpeg")
ggplot(count, aes(x = Year, y= number)) +
  geom_bar(stat = 'identity')
dev.off()
# Generate corpus to use for text mining

pubmed_records <- Corpus(VectorSource(pubmed_data$Abstract))

# Convert the text to lower case

pubmed_records<-tm_map(pubmed_records, content_transformer(tolower))

# Remove digits/numbers

pubmed_records<-tm_map(pubmed_records, removeNumbers)

#stem words 

pubmed_records <- tm_map(pubmed_records, stemDocument)

# Remove english common stopwords

pubmed_records <- tm_map(pubmed_records, removeWords, stopwords("english"))

# Remove punctuations and white spaces

pubmed_records <- tm_map(pubmed_records, removePunctuation)
pubmed_records <- tm_map(pubmed_records, stripWhitespace)


# Remove additional stopwords
pubmed_records<- tm_map(pubmed_records, removeWords, c("clintonemailcom", "stategov", "hrod"))

# Corpus ready so generate TermDocumentMatrix
dtm <- TermDocumentMatrix(pubmed_records)
inspect(dtm)

#Convert TermDocumentMatrix to type matrix 
#generate Dataframe of word verses frequency of word in corpus

dtMatrix <- as.matrix(dtm)
v <- sort(rowSums(dtMatrix),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
freqr <- colSums(as.matrix(dtm))

# WordVector with words that exist in corpus at a frequency of more than 500
# Will use this to filter the terms

wordVector <- subset(d,freqr<2500)

#tidy the text using tidytext and map id to document
a<-tidy(dtm)
a<- a[,c("document","term", "count")]
pubmed_data$id <- seq(1,length.out = nrow(pubmed_data), by=1)

#document id conversion from character to numeric to be able to merge the dataframes
a$document <- as.numeric(a$document)

colnames(a)[1] <- "id"
mergedc <- merge(pubmed_data,a , by= "id")

write.csv(mergedc , file = "data/Merged_document.csv")

TempMerge <- mergedc %>% 
  select(-Abstract,-Title,-id) %>%
  filter(count != 1, !term %in% c("≤","•","≥","··","°c","µm","μm","€","∼", term == wordVector)) %>% 
  group_by(Year, term) %>%
  summarise(n=n()) 

plot1 <- ggplot(TempMerge,aes(factor(Year),n)) + 
  geom_boxplot() +
  #geom_jitter() +
  scale_y_log10() +
  labs(y="log10 words", x="Year",title="") +
  theme_minimal()

jpeg(filename = "figures/WordsPerYear.jpeg")
plot1
dev.off()


wf=data.frame(term=names(freqr),occurrences=freqr)

p <- ggplot(subset(wf, freqr>500), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_blank(),
               axis.title.x = element_blank(),
               axis.ticks.x = element_blank())

jpeg(filename = "figures/word_frequencies.jpeg")
p
dev.off()


# Generate the WordCloud
 par(bg="white")
png(file="figures/WordCloud.png",width=1000,height=700, bg="white")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "all terms in a wordCloud", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

 png("figures/AnotherWordCloud.png",width = 800,height = 500)
 wordcloud(d$word,d$freq, min.freq=100)
 dev.off()
 
 