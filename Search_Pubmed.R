# install packages if required

if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
if(!require(RISmed)) install.packages("RISmed",repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")

# load libraries of installed packages

library(ggplot2)
library(RISmed)
library(tidyverse)

# List of search terms related to my topic of study

search_topic <- ' Respiratory Syncytial Virus '
search_topic2 <- 'rsv'
search_topic3 <- 'Transmission Bottleneck'
search_topic4 <- 'hrsv'

#Search queries to execute

search_query <- EUtilsSummary(search_topic,retmax =5000, mindate =1960,maxdate=2017)
summary(search_query)
search_query2 <- EUtilsSummary(search_topic2,retmax =5000, mindate =1960,maxdate=2017)
summary(search_query2)
search_query3 <- EUtilsSummary(search_topic3,retmax =5000, mindate =1960,maxdate=2017)
summary(search_query3)
search_query4 <- EUtilsSummary(search_topic4,retmax =5000, mindate =1960,maxdate=2017)
summary(search_query4)


#Get pubmed abstracts and store in data frame

records1 <- EUtilsGet(search_query)
records2 <- EUtilsGet(search_query2)
records3 <- EUtilsGet(search_query3)
records4 <- EUtilsGet(search_query4)


#Construct dataframe with columns PMID, Title,Abstract,Year

pubmed_data1 <- data.frame('Pid' = PMID(records1),
                          'Title' = ArticleTitle(records1),
                          'Abstract' = AbstractText(records1),
                          'Year'= YearPubmed(records1))

pubmed_data2 <- data.frame('Pid' = PMID(records2),
                          'Title' = ArticleTitle(records2),
                          'Abstract' = AbstractText(records2),
                          'Year'= YearPubmed(records2))

pubmed_data3 <- data.frame('Pid' = PMID(records3),
                          'Title' = ArticleTitle(records3),
                          'Abstract' = AbstractText(records3),
                          'Year'= YearPubmed(records3))

pubmed_data4 <- data.frame('Pid' = PMID(records4),
                          'Title' = ArticleTitle(records4),
                          'Abstract' = AbstractText(records4),
                          'Year'= YearPubmed(records4))

# Merge dataframes and get unique documents incase abstracts overlap


pubmed_data <- rbind(pubmed_data1,pubmed_data2,pubmed_data3,pubmed_data4)
pubmed_data <- unique(pubmed_data)

write.csv(pubmed_data,file = "data/pubmed_data.csv")

 



