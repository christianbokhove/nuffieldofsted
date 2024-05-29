## This script starts with the ofstedcorp corpus

## Descriptives

library("ggplot2")
library("lda")
library(dplyr)
library(reshape2)
library(quanteda)
library(pals)

## Whole dataset

table(ofstedcorp$year)

ggplot(ofstedcorp) +
geom_bar(aes(x = year, fill = as.factor(year)), 
       position = "dodge", stat = "count")

require(stringr)
meanwords <-  ofstedcorp$text

library(data.table)

# word counts
ofstedcorp$wordcounts <- sapply(strsplit(ofstedcorp$text, " "), length)
# Table with values
words <- aggregate(ofstedcorp$wordcounts, list(ofstedcorp$year), FUN=mean)

# Graph with average inspection report word count by year
ggplot(data=ofstedcorp, mapping=aes(x=year, y=wordcounts)) + 
  stat_summary(fun.data=mean_sdl, geom="bar") + geom_bar(aes(x = year, fill = as.factor(year)), 
                                                         position = "dodge", stat = "identity")

# Uses quanteda
summary(ofstedcorp)

## Make subsets of documents ##

## By 'hinge points'
# Myths document
# The clarification (myths) was published October 2014
# Up to 2013
ofstedcorp1 <- subset(ofstedcorp, year<2014)
# Only four years before
ofstedcorp1small <- subset(ofstedcorp, year<2014 & year > 2009)

ofsted_myths_before <- subset(ofstedcorp, year==2013)
ofsted_myths_after <- subset(ofstedcorp, year==2015)

# Check if filter worked with a bar chart
ggplot(ofsted_myths_after) +
  geom_bar(aes(x = year, fill = as.factor(year)), 
           position = "dodge", stat = "count")

# From 2015
ofstedcorp2 <- subset(ofstedcorp, year>2014)

# New inspection framework
# Up to 2018
ofstedcorp3 <- subset(ofstedcorp, year<2019)
# Only four years before
ofstedcorp3small <- subset(ofstedcorp, year<2019 & year > 2014)

ofsted_eif_before <- subset(ofstedcorp, year==2018)
ofsted_eif_after <- subset(ofstedcorp, year>2019 & year<2022 )

# Check if filter worked with a bar chart
ggplot(ofsted_eif_after) +
  geom_bar(aes(x = year, fill = as.factor(year)), 
           position = "dodge", stat = "count")

# From 2020
ofstedcorp4 <- subset(ofstedcorp, year>2019)

# Only last report by URN (should correspond with judgement)
# Uses dplyr top_n
ofstedcorplatest <- ofstedcorp %>% group_by(urn) %>% top_n(1, year)

## End subsets ##

# Latent Dirichlet Allocation - see https://ladal.edu.au/topicmodels.html
# Working with ofstedcorpX
library(lda)
library(tm)
library(topicmodels)
install.packages("ldatuning")
library(ldatuning)

###################################################
# New LDA January 2024 smaller subsets.

# compute document term matrix with terms >= minimumFrequency
minimumFrequency1 <- 500

# pre-processing - myths before
c1_myths_before <- gsub("'", "", ofsted_myths_before$text)  # remove apostrophes
c1_myths_before <- gsub("[[:punct:]]", " ", c1_myths_before)  # replace punctuation with space
c1_myths_before <- gsub("[[:cntrl:]]", " ", c1_myths_before)  # replace control characters with space
c1_myths_before <- gsub("^[[:space:]]+", "", c1_myths_before) # remove whitespace at beginning of documents
c1_myths_before <- gsub("[[:space:]]+$", "", c1_myths_before) # remove whitespace at end of documents
c1_myths_before <- tolower(c1_myths_before)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
c1_myths_before = stringr::str_replace_all(c1_myths_before, stopwords_regex, '')
# pre-processing - myths after
c1_myths_after <- gsub("'", "", ofsted_myths_after$text)  # remove apostrophes
c1_myths_after <- gsub("[[:punct:]]", " ", c1_myths_after)  # replace punctuation with space
c1_myths_after <- gsub("[[:cntrl:]]", " ", c1_myths_after)  # replace control characters with space
c1_myths_after <- gsub("^[[:space:]]+", "", c1_myths_after) # remove whitespace at beginning of documents
c1_myths_after <- gsub("[[:space:]]+$", "", c1_myths_after) # remove whitespace at end of documents
c1_myths_after <- tolower(c1_myths_after)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
c1_myths_after = stringr::str_replace_all(c1_myths_after, stopwords_regex, '')

# Create dtm - myths before
DTM_myths_before <- DocumentTermMatrix(c1_myths_before, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM_myths_before)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx1 <- slam::row_sums(DTM_myths_before) > 0
DTM_myths_before <- DTM_myths_before[sel_idx1, ]
# Create dtm - myths after
DTM_myths_after <- DocumentTermMatrix(c1_myths_after, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM_myths_after)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx1 <- slam::row_sums(DTM_myths_after) > 0
DTM_myths_after <- DTM_myths_after[sel_idx1, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel_myths_before <- LDA(DTM_myths_before, K, method="Gibbs", control=list(iter = 500, verbose = 25))

    # have a look a some of the results (posterior distributions)
    tmResult_myths_before <- posterior(topicModel_myths_before)
    # format of the resulting object
    attributes(tmResult_myths_before)
    nTerms(DTM_myths_before)  
    # topics are probability distributions over the entire vocabulary
    beta_myths_before <- tmResult_myths_before$terms   # get beta from results
    dim(beta_myths_before)                # K distributions over nTerms(DTM) terms
    rowSums(beta_myths_before)            # rows in beta sum to 1
    nDocs(DTM_myths_before)               # size of collection
    # for every document we have a probability distribution of its contained topics
    theta_myths_before <- tmResult_myths_before$topics 
    dim(theta_myths_before)               # nDocs(DTM) distributions over K topics
    rowSums(theta_myths_before)[1:10]     # rows in theta sum to 1
    # 10 most likely terms
    terms(topicModel_myths_before, 20)
    exampleTermData_myths_before <- terms(topicModel_myths_before, 10)
    exampleTermData_myths_before[, 1:8]
    # Give the topics names
    top5termsPerTopic_myths_before <- terms(topicModel_myths_before, 5)
    Next5_myths_before <- terms(topicModel_myths_before, 20)[6:10,]
    topicNames_myths_before <- apply(Next5_myths_before, 2, paste, collapse=" ")
    topicNames_myths_before
    
    # Viz tested with the smallest corpus first
    # get mean topic proportions per year
    topic_proportion_myths_before <- aggregate(theta_myths_before, by = list(year = ofsted_myths_before$year), mean)
    # set topic names to aggregated columns
    colnames(topic_proportion_myths_before)[2:(K+1)] <- topicNames_myths_before
    # reshape data frame
    vizDataFrame_myths_before<- melt(topic_proportion_myths_before, id.vars = "year")
    # plot topic proportions per decade as bar plot
    ggplot(vizDataFrame_myths_before, aes(x=year, y=value, fill=variable)) + 
      geom_bar(stat = "identity") + ylab("proportion") + 
      # Comment next for rainbow plot
      scale_fill_manual(values = unname(glasbey()), name = "year") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # number of topics
  K <- 18
  # set random number generator seed
  set.seed(9161)
  # compute the LDA model, inference via 1000 iterations of Gibbs sampling
  topicModel_myths_after <- LDA(DTM_myths_after, K, method="Gibbs", control=list(iter = 500, verbose = 25))
  
    # have a look a some of the results (posterior distributions)
    tmResult_myths_after <- posterior(topicModel_myths_after)
    # format of the resulting object
    attributes(tmResult_myths_after)
    nTerms(DTM_myths_after)  
    # topics are probability distributions over the entire vocabulary
    beta_myths_after <- tmResult_myths_after$terms   # get beta from results
    dim(beta_myths_after)                # K distributions over nTerms(DTM) terms
    rowSums(beta_myths_after)            # rows in beta sum to 1
    nDocs(DTM_myths_after)               # size of collection
    # for every document we have a probability distribution of its contained topics
    theta_myths_after <- tmResult_myths_after$topics 
    dim(theta_myths_after)               # nDocs(DTM) distributions over K topics
    rowSums(theta_myths_after)[1:10]     # rows in theta sum to 1
    # 10 most likely terms
    terms(topicModel_myths_after, 20)
    exampleTermData_myths_after <- terms(topicModel_myths_after, 10)
    exampleTermData_myths_after[, 1:8]
    # Give the topics names
    top5termsPerTopic_myths_after <- terms(topicModel_myths_after, 5)
    Next5_myths_after <- terms(topicModel_myths_after, 20)[6:10,]
    topicNames_myths_after <- apply(Next5_myths_after, 2, paste, collapse=" ")
    topicNames_myths_after
    
    # Viz tested with the smallest corpus first
    # get mean topic proportions per year
    topic_proportion_myths_after <- aggregate(theta_myths_after, by = list(year = ofsted_myths_after$year), mean)
    # set topic names to aggregated columns
    colnames(topic_proportion_myths_after)[2:(K+1)] <- topicNames_myths_after
    # reshape data frame
    vizDataFrame_myths_after<- melt(topic_proportion_myths_after, id.vars = "year")
    # plot topic proportions per decade as bar plot
    ggplot(vizDataFrame_myths_after, aes(x=year, y=value, fill=variable)) + 
      geom_bar(stat = "identity") + ylab("proportion") + 
      # Comment next for rainbow plot
      scale_fill_manual(values = unname(glasbey()), name = "year") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
# Semantic similarity
    # Compare before and after
    # Create new DFMs
    # Improve filtering here
    library("quanteda")
    mbefore <- corpus(ofsted_myths_before)
    mafter <- corpus(ofsted_myths_after)
    mbeforedfm <- dfm(mbefore, tolower = T, stem = T,
                      remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
    mafterdfm <- dfm(mafter, tolower = T, stem = T,
                      remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
    
    # Read both comparison texts (already for later)
    library(readtext) # easily read in text in directories
    # Convert PDFs to TXT files
    sourcetexts <- "C://Rprojects//NUFFIELD_OFSTED//compare"
    # make a vector of PDF file names
    myfiles2compare <- list.files(path = sourcetexts, pattern = "pdf",  full.names = TRUE)
    # convert each PDF file that is named in the vector into a text file 
    # text file is created in a director TXT directory as the PDFs (easiest to create)
    lapply(myfiles2compare, function(i) system(paste('"C://Rprojects//NUFFIELD_OFSTED//tools//xpdf-tools-win-4.04//bin64//pdftotext.exe"', paste0('"', i, '"'),
                                                     paste0('"C://Rprojects//NUFFIELD_OFSTED//compare//TXT//', basename(tools::file_path_sans_ext(i)), '.txt"')), wait = FALSE))
    # recursively get filepaths for documents
    comparedocs <- list.files(path = c('compare//TXT'), recursive = T, full.names = T)
    clarifydoc <- readtext(comparedocs[1], docvarsfrom = "filenames")
    eifdoc <- readtext(comparedocs[2], docvarsfrom = "filenames")
    
    clarifycorpus <- corpus(clarifydoc)
    eifcorpus <- corpus(eifdoc)
    
    # Create DFMS
    claritydfm <- dfm(clarifycorpus, tolower = T, stem = T,
                      remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
    eifdfm <- dfm(eifcorpus, tolower = T, stem = T,
                  remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
    
    # Compare similarity
    # mbeforedfm and mafterdfm with claritydfm
    library(quanteda)
    install.packages("quanteda.textstats")
    library(quanteda.textstats)
    similarity_myths_before= textstat_simil(mbeforedfm,  claritydfm, margin = "documents", method = "cosine")
    similarity_myths_after= textstat_simil(mafterdfm,  claritydfm, margin = "documents", method = "cosine")
    mb <- as.data.frame(similarity_myths_before)
    mb['document2'] <- "2013"
    ma <- as.data.frame(similarity_myths_after)
    ma['document2'] <- "2015"
    mythschange <- bind_rows(mb, ma)
    mythschange %>% ggplot(aes(x = document2, y = cosine, colour=document2)) + geom_jitter() + labs(color='Myths document') 
    
   

##################################################
#    2013 to 2015 separate

ofsted_myths <- subset(ofstedcorp, year>2012 & year<2016)
    
    # pre-processing - myths before
    corpus_myths <- gsub("'", "", ofsted_myths$text)  # remove apostrophes
    corpus_myths <- gsub("[[:punct:]]", " ", corpus_myths)  # replace punctuation with space
    corpus_myths <- gsub("[[:cntrl:]]", " ", corpus_myths)  # replace control characters with space
    corpus_myths <- gsub("^[[:space:]]+", "", corpus_myths) # remove whitespace at beginning of documents
    corpus_myths <- gsub("[[:space:]]+$", "", corpus_myths) # remove whitespace at end of documents
    corpus_myths <- tolower(corpus_myths)  # force to lowercase
    stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
    corpus_myths = stringr::str_replace_all(corpus_myths, stopwords_regex, '')
    
# Create dtm - myths before
    DTM_myths <- DocumentTermMatrix(corpus_myths, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
    # have a look at the number of documents and terms in the matrix
    dim(DTM_myths)
    # due to vocabulary pruning, we have empty rows in our DTM
    # LDA does not like this. So we remove those docs from the
    # DTM and the metadata
    sel_idx1 <- slam::row_sums(DTM_myths) > 0
    DTM_myths <- DTM_myths[sel_idx1, ]
    
#NOT DONE - ONLY WHEN TIME LEFT
    # THIS TAKES AGES - here resulted in K=
    result_n_myths <- ldatuning::FindTopicsNumber(
      DTM_myths,
      topics = seq(from = 2, to = 20, by = 1),
      metrics = c("CaoJuan2009",  "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 77),
      verbose = TRUE
    )
    FindTopicsNumber_plot(result_n_myths)

# Fit LDA   
    # number of topics
    K <- 18
    # set random number generator seed
    set.seed(9161)
    # compute the LDA model, inference via 1000 iterations of Gibbs sampling
    topicModel_myths <- LDA(DTM_myths, K, method="Gibbs", control=list(iter = 500, verbose = 25))
    
    # have a look a some of the results (posterior distributions)
    tmResult_myths <- posterior(topicModel_myths)
    # format of the resulting object
    attributes(tmResult_myths)
    nTerms(DTM_myths)  
    # topics are probability distributions over the entire vocabulary
    beta_myths <- tmResult_myths$terms   # get beta from results
    dim(beta_myths)                # K distributions over nTerms(DTM) terms
    rowSums(beta_myths)            # rows in beta sum to 1
    nDocs(DTM_myths)               # size of collection
    # for every document we have a probability distribution of its contained topics
    theta_myths <- tmResult_myths$topics 
    dim(theta_myths)               # nDocs(DTM) distributions over K topics
    rowSums(theta_myths)[1:10]     # rows in theta sum to 1
    # 10 most likely terms
    terms(topicModel_myths, 20)
    exampleTermData_myths <- terms(topicModel_myths, 10)
    exampleTermData_myths[, 1:8]
    # Give the topics names
    top5termsPerTopic_myths <- terms(topicModel_myths, 5)
    Next5_myths <- terms(topicModel_myths, 20)[6:10,]
    topicNames_myths <- apply(Next5_myths, 2, paste, collapse=" ")
    topicNames_myths
    
    # Viz tested with the smallest corpus first
    # get mean topic proportions per year
    topic_proportion_myths <- aggregate(theta_myths, by = list(year = ofsted_myths$year), mean)
    # set topic names to aggregated columns
    colnames(topic_proportion_myths)[2:(K+1)] <- topicNames_myths
    # reshape data frame
    vizDataFrame_myths<- melt(topic_proportion_myths, id.vars = "year")
    # plot topic proportions per decade as bar plot
    ggplot(vizDataFrame_myths, aes(x=year, y=value, fill=variable)) + 
      geom_bar(stat = "identity") + ylab("proportion") + 
      # Comment next for rainbow plot
      scale_fill_manual(values = unname(glasbey()), name = "year") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### SENTIMENT
    
    sentiment.myths <- unnest_tokens(ofsted_myths, word, text)
    rating.sentiment.myths.afinn <- sentiment.myths %>% inner_join(get_sentiments("afinn"))
    rating.sentiment.myths.afinn[1:10, c("word", "value")]
    rating.sentiment.myths.afinn.score <- rating.sentiment.myths.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
    myths_sent <- merge(ofsted_myths,rating.sentiment.myths.afinn.score, by = "urn")
    
    library(dplyr)
    
    # Figure with sentiments
    myths_sent %>% ggplot(aes(x = year, y = sentiment)) + geom_jitter() +
    scale_x_continuous(breaks = 0:2100)
    
# Only with judgement
    ofsted_myths_judged <- ofsted_myths[ofsted_myths$outcome %in% c("Outstanding", "Good", "Requires Improvement", "Inadequate"),]
  
    sentiment.myths <- unnest_tokens(ofsted_myths_judged, word, text)
    rating.sentiment.myths.afinn <- sentiment.myths %>% inner_join(get_sentiments("afinn"))
    rating.sentiment.myths.afinn[1:10, c("word", "value")]
    rating.sentiment.myths.afinn.score <- rating.sentiment.myths.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
    myths_sent <- merge(ofsted_myths,rating.sentiment.myths.afinn.score, by = "urn")
    
    library(dplyr)
    
    # Figure with sentiments
    myths_sent %>% ggplot(aes(x = year, y = sentiment, colour=outcome)) + geom_jitter() +
      scale_x_continuous(breaks = 0:2100) + scale_color_manual(values=c("yellow", "red", "green", "orange")) + scale_fill_discrete(limits=c('Outstanding', 'Good', 'Requires Improvement', 'Inadequate'))
    
    
# For each year, and seperately for foreign or not, calculate mean x.
data.means <- myths_sent %>% 
  group_by(year, outcome) %>%
  summarize(meansentiment = mean(sentiment))

# plot. You don't need group = foreign
ggplot(data.means, aes(x = year, y = meansentiment, linetype = factor(outcome), colour=outcome)) + 
  geom_line(linewidth=2)

###################################################
# EIF year before and years after - not run because changed mind 
    ###################################################
    # New LDA January 2024 smaller subsets.
    
    # compute document term matrix with terms >= minimumFrequency
    minimumFrequency1 <- 500
    
    # pre-processing - myths before
    c1_eif_before <- gsub("'", "", ofsted_eif_before$text)  # remove apostrophes
    c1_eif_before <- gsub("[[:punct:]]", " ", c1_eif_before)  # replace punctuation with space
    c1_eif_before <- gsub("[[:cntrl:]]", " ", c1_eif_before)  # replace control characters with space
    c1_eif_before <- gsub("^[[:space:]]+", "", c1_eif_before) # remove whitespace at beginning of documents
    c1_eif_before <- gsub("[[:space:]]+$", "", c1_eif_before) # remove whitespace at end of documents
    c1_eif_before <- tolower(c1_eif_before)  # force to lowercase
    stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
    c1_eif_before = stringr::str_replace_all(c1_eif_before, stopwords_regex, '')
    # pre-processing - eif after
    c1_eif_after <- gsub("'", "", ofsted_eif_after$text)  # remove apostrophes
    c1_eif_after <- gsub("[[:punct:]]", " ", c1_eif_after)  # replace punctuation with space
    c1_eif_after <- gsub("[[:cntrl:]]", " ", c1_eif_after)  # replace control characters with space
    c1_eif_after <- gsub("^[[:space:]]+", "", c1_eif_after) # remove whitespace at beginning of documents
    c1_eif_after <- gsub("[[:space:]]+$", "", c1_eif_after) # remove whitespace at end of documents
    c1_eif_after <- tolower(c1_eif_after)  # force to lowercase
    stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
    stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
    c1_eif_after = stringr::str_replace_all(c1_eif_after, stopwords_regex, '')
    
    # Create dtm - eif before
    DTM_eif_before <- DocumentTermMatrix(c1_eif_before, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
    # have a look at the number of documents and terms in the matrix
    dim(DTM_eif_before)
    # due to vocabulary pruning, we have empty rows in our DTM
    # LDA does not like this. So we remove those docs from the
    # DTM and the metadata
    sel_idx1 <- slam::row_sums(DTM_eif_before) > 0
    DTM_eif_before <- DTM_eif_before[sel_idx1, ]
    # Create dtm - eif after
    DTM_eif_after <- DocumentTermMatrix(c1_eif_after, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
    # have a look at the number of documents and terms in the matrix
    dim(DTM_eif_after)
    # due to vocabulary pruning, we have empty rows in our DTM
    # LDA does not like this. So we remove those docs from the
    # DTM and the metadata
    sel_idx1 <- slam::row_sums(DTM_eif_after) > 0
    DTM_eif_after <- DTM_eif_after[sel_idx1, ]
    
    # number of topics
    K <- 18
    # set random number generator seed
    set.seed(9161)
    # compute the LDA model, inference via 1000 iterations of Gibbs sampling
    topicModel_eif_before <- LDA(DTM_eif_before, K, method="Gibbs", control=list(iter = 500, verbose = 25))
    
    # have a look a some of the results (posterior distributions)
    tmResult_eif_before <- posterior(topicModel_eif_before)
    # format of the resulting object
    attributes(tmResult_eif_before)
    nTerms(DTM_eif_before)  
    # topics are probability distributions over the entire vocabulary
    beta_eif_before <- tmResult_eif_before$terms   # get beta from results
    dim(beta_eif_before)                # K distributions over nTerms(DTM) terms
    rowSums(beta_eif_before)            # rows in beta sum to 1
    nDocs(DTM_eif_before)               # size of collection
    # for every document we have a probability distribution of its contained topics
    theta_eif_before <- tmResult_eif_before$topics 
    dim(theta_eif_before)               # nDocs(DTM) distributions over K topics
    rowSums(theta_eif_before)[1:10]     # rows in theta sum to 1
    # 10 most likely terms
    terms(topicModel_eif_before, 20)
    exampleTermData_eif_before <- terms(topicModel_eif_before, 10)
    exampleTermData_eif_before[, 1:8]
    # Give the topics names
    top5termsPerTopic_eif_before <- terms(topicModel_eif_before, 5)
    Next5_eif_before <- terms(topicModel_eif_before, 20)[6:10,]
    topicNames_eif_before <- apply(Next5_eif_before, 2, paste, collapse=" ")
    topicNames_eif_before
    
    # Viz tested with the smallest corpus first
    # get mean topic proportions per year
    topic_proportion_eif_before <- aggregate(theta_eif_before, by = list(year = ofsted_eif_before$year), mean)
    # set topic names to aggregated columns
    colnames(topic_proportion_eif_before)[2:(K+1)] <- topicNames_eif_before
    # reshape data frame
    vizDataFrame_eif_before<- melt(topic_proportion_eif_before, id.vars = "year")
    # plot topic proportions per decade as bar plot
    ggplot(vizDataFrame_eif_before, aes(x=year, y=value, fill=variable)) + 
      geom_bar(stat = "identity") + ylab("proportion") + 
      # Comment next for rainbow plot
      scale_fill_manual(values = unname(glasbey()), name = "year") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    # number of topics
    K <- 18
    # set random number generator seed
    set.seed(9161)
    # compute the LDA model, inference via 1000 iterations of Gibbs sampling
    topicModel_eif_after <- LDA(DTM_eif_after, K, method="Gibbs", control=list(iter = 500, verbose = 25))
    
    # have a look a some of the results (posterior distributions)
    tmResult_eif_after <- posterior(topicModel_eif_after)
    # format of the resulting object
    attributes(tmResult_eif_after)
    nTerms(DTM_eif_after)  
    # topics are probability distributions over the entire vocabulary
    beta_eif_after <- tmResult_eif_after$terms   # get beta from results
    dim(beta_eif_after)                # K distributions over nTerms(DTM) terms
    rowSums(beta_eif_after)            # rows in beta sum to 1
    nDocs(DTM_eif_after)               # size of collection
    # for every document we have a probability distribution of its contained topics
    theta_eif_after <- tmResult_eif_after$topics 
    dim(theta_eif_after)               # nDocs(DTM) distributions over K topics
    rowSums(theta_eif_after)[1:10]     # rows in theta sum to 1
    # 10 most likely terms
    terms(topicModel_eif_after, 20)
    exampleTermData_eif_after <- terms(topicModel_eif_after, 10)
    exampleTermData_eif_after[, 1:8]
    # Give the topics names
    top5termsPerTopic_eif_after <- terms(topicModel_eif_after, 5)
    Next5_eif_after <- terms(topicModel_eif_after, 20)[6:10,]
    topicNames_eif_after <- apply(Next5_eif_after, 2, paste, collapse=" ")
    topicNames_eif_after
    
    # Viz tested with the smallest corpus first
    # get mean topic proportions per year
    topic_proportion_eif_after <- aggregate(theta_eif_after, by = list(year = ofsted_eif_after$year), mean)
    # set topic names to aggregated columns
    colnames(topic_proportion_eif_after)[2:(K+1)] <- topicNames_eif_after
    # reshape data frame
    vizDataFrame_eif_after<- melt(topic_proportion_eif_after, id.vars = "year")
    # plot topic proportions per decade as bar plot
    ggplot(vizDataFrame_eif_after, aes(x=year, y=value, fill=variable)) + 
      geom_bar(stat = "identity") + ylab("proportion") + 
      # Comment next for rainbow plot
      scale_fill_manual(values = unname(glasbey()), name = "year") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
# Semantic similarity
# Compare before and after
# Create new DFMs
# Improve filtering here
library("quanteda")
ebefore <- corpus(ofsted_eif_before)
eafter <- corpus(ofsted_eif_after)
ebeforedfm <- dfm(ebefore, tolower = T, stem = T,
                  remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
eafterdfm <- dfm(eafter, tolower = T, stem = T,
                 remove = stopwords('english'), remove_punct = T,  remove_numbers = T)

# Read both comparison texts (already for later)
library(readtext) # easily read in text in directories
# Convert PDFs to TXT files
sourcetexts <- "C://Rprojects//NUFFIELD_OFSTED//compare"
# make a vector of PDF file names
myfiles2compare <- list.files(path = sourcetexts, pattern = "pdf",  full.names = TRUE)
# convert each PDF file that is named in the vector into a text file 
# text file is created in a director TXT directory as the PDFs (easiest to create)
lapply(myfiles2compare, function(i) system(paste('"C://Rprojects//NUFFIELD_OFSTED//tools//xpdf-tools-win-4.04//bin64//pdftotext.exe"', paste0('"', i, '"'),
                                                 paste0('"C://Rprojects//NUFFIELD_OFSTED//compare//TXT//', basename(tools::file_path_sans_ext(i)), '.txt"')), wait = FALSE))
# recursively get filepaths for documents
comparedocs <- list.files(path = c('compare//TXT'), recursive = T, full.names = T)
clarifydoc <- readtext(comparedocs[1], docvarsfrom = "filenames")
eifdoc <- readtext(comparedocs[2], docvarsfrom = "filenames")

clarifycorpus <- corpus(clarifydoc)
eifcorpus <- corpus(eifdoc)

# Create DFMS
claritydfm <- dfm(clarifycorpus, tolower = T, stem = T,
                  remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
eifdfm <- dfm(eifcorpus, tolower = T, stem = T,
              remove = stopwords('english'), remove_punct = T,  remove_numbers = T)

# Compare similarity
# mbeforedfm and mafterdfm with claritydfm
library(quanteda)
install.packages("quanteda.textstats")
library(quanteda.textstats)
similarity_eif_before= textstat_simil(ebeforedfm,  eifdfm, margin = "documents", method = "cosine")
similarity_eif_after= textstat_simil(eafterdfm,  eifdfm, margin = "documents", method = "cosine")
eb <- as.data.frame(similarity_eif_before)
eb['document2'] <- "2018"
ea <- as.data.frame(similarity_eif_after)
ea['document2'] <- "2021/2022"
eifchange <- bind_rows(eb, ea)
eifchange %>% ggplot(aes(x = document2, y = cosine, colour=document2)) + geom_jitter() + labs(color='eif document') 


##################################################
#    2018 to 2021 separate - EIF

ofsted_eif <- subset(ofstedcorp, year>2017 & year<2022)

# pre-processing - eif before
corpus_eif <- gsub("'", "", ofsted_eif$text)  # remove apostrophes
corpus_eif <- gsub("[[:punct:]]", " ", corpus_eif)  # replace punctuation with space
corpus_eif <- gsub("[[:cntrl:]]", " ", corpus_eif)  # replace control characters with space
corpus_eif <- gsub("^[[:space:]]+", "", corpus_eif) # remove whitespace at beginning of documents
corpus_eif <- gsub("[[:space:]]+$", "", corpus_eif) # remove whitespace at end of documents
corpus_eif <- tolower(corpus_eif)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
corpus_eif = stringr::str_replace_all(corpus_eif, stopwords_regex, '')

# Create dtm - eif before
DTM_eif <- DocumentTermMatrix(corpus_eif, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM_eif)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx1 <- slam::row_sums(DTM_eif) > 0
DTM_eif <- DTM_eif[sel_idx1, ]

#NOT DONE - ONLY WHEN TIME LEFT
# THIS TAKES AGES - here resulted in K=
result_n_eif <- ldatuning::FindTopicsNumber(
  DTM_eif,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result_n_eif)

# Fit LDA   
# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel_eif <- LDA(DTM_eif, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult_eif <- posterior(topicModel_eif)
# format of the resulting object
attributes(tmResult_eif)
nTerms(DTM_eif)  
# topics are probability distributions over the entire vocabulary
beta_eif <- tmResult_eif$terms   # get beta from results
dim(beta_eif)                # K distributions over nTerms(DTM) terms
rowSums(beta_eif)            # rows in beta sum to 1
nDocs(DTM_eif)               # size of collection
# for every document we have a probability distribution of its contained topics
theta_eif <- tmResult_eif$topics 
dim(theta_eif)               # nDocs(DTM) distributions over K topics
rowSums(theta_eif)[1:10]     # rows in theta sum to 1
# 10 most likely terms
terms(topicModel_eif, 20)
exampleTermData_eif <- terms(topicModel_eif, 10)
exampleTermData_eif[, 1:8]
# Give the topics names
top5termsPerTopic_eif <- terms(topicModel_eif, 5)
Next5_eif <- terms(topicModel_eif, 20)[6:10,]
topicNames_eif <- apply(Next5_eif, 2, paste, collapse=" ")
topicNames_eif

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_eif <- aggregate(theta_eif, by = list(year = ofsted_eif$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_eif)[2:(K+1)] <- topicNames_eif
# reshape data frame
vizDataFrame_eif<- melt(topic_proportion_eif, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame_eif, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### SENTIMENT EIF

sentiment.eif <- unnest_tokens(ofsted_eif, word, text)
rating.sentiment.eif.afinn <- sentiment.eif %>% inner_join(get_sentiments("afinn"))
rating.sentiment.eif.afinn[1:10, c("word", "value")]
rating.sentiment.eif.afinn.score <- rating.sentiment.eif.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
eif_sent <- merge(ofsted_eif,rating.sentiment.eif.afinn.score, by = "urn")

library(dplyr)

# Figure with sentiments
eif_sent %>% ggplot(aes(x = year, y = sentiment)) + geom_jitter() +
  scale_x_continuous(breaks = 0:2100)

# Only with judgement
ofsted_eif_judged <- ofsted_eif[ofsted_eif$outcome %in% c("Outstanding", "Good", "Requires Improvement", "Inadequate"),]

sentiment.eif <- unnest_tokens(ofsted_eif_judged, word, text)
rating.sentiment.eif.afinn <- sentiment.eif %>% inner_join(get_sentiments("afinn"))
rating.sentiment.eif.afinn[1:10, c("word", "value")]
rating.sentiment.eif.afinn.score <- rating.sentiment.eif.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
eif_sent <- merge(ofsted_eif,rating.sentiment.eif.afinn.score, by = "urn")

library(dplyr)

# Figure with sentiments
eif_sent %>% ggplot(aes(x = year, y = sentiment, colour=outcome)) + geom_jitter() +
  scale_x_continuous(breaks = 0:2100) + scale_color_manual(values=c("yellow", "red", "green", "orange")) + scale_fill_discrete(limits=c('Outstanding', 'Good', 'Requires Improvement', 'Inadequate'))


###################################################

# Corpus 1 - 2010-2013 - THESE take VERY LONG to fit!
# compute document term matrix with terms >= minimumFrequency
minimumFrequency1 <- 500

# pre-processing:
pcorp1 <- gsub("'", "", ofstedcorp1small$text)  # remove apostrophes
pcorp1 <- gsub("[[:punct:]]", " ", pcorp1)  # replace punctuation with space
pcorp1 <- gsub("[[:cntrl:]]", " ", pcorp1)  # replace control characters with space
pcorp1 <- gsub("^[[:space:]]+", "", pcorp1) # remove whitespace at beginning of documents
pcorp1 <- gsub("[[:space:]]+$", "", pcorp1) # remove whitespace at end of documents
pcorp1 <- tolower(pcorp1)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
pcorp1 = stringr::str_replace_all(pcorp1, stopwords_regex, '')

# Create dtm
DTM1 <- DocumentTermMatrix(pcorp1, control = list(bounds = list(global = c(minimumFrequency1, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM1)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx1 <- slam::row_sums(DTM1) > 0
DTM1 <- DTM1[sel_idx1, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel1 <- LDA(DTM1, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult1 <- posterior(topicModel1)
# format of the resulting object
attributes(tmResult1)
nTerms(DTM1)  
# topics are probability distributions over the entire vocabulary
beta1 <- tmResult1$terms   # get beta from results
dim(beta1)                # K distributions over nTerms(DTM) terms
rowSums(beta1)            # rows in beta sum to 1
nDocs(DTM1)               # size of collection
# for every document we have a probability distribution of its contained topics
theta1 <- tmResult1$topics 
dim(theta1)               # nDocs(DTM) distributions over K topics
rowSums(theta1)[1:10]     # rows in theta sum to 1
# 10 most likely terms
terms(topicModel1, 20)
exampleTermData1 <- terms(topicModel1, 10)
exampleTermData1[, 1:8]
# Give the topics names
top5termsPerTopic1 <- terms(topicModel1, 5)
Next5 <- terms(topicModel1, 20)[6:10,]
topicNames1 <- apply(Next5, 2, paste, collapse=" ")
topicNames1

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year1 <- aggregate(theta1, by = list(year = ofstedcorp1small$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year1)[2:(K+1)] <- topicNames1
# reshape data frame
vizDataFrame1<- melt(topic_proportion_per_year1, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame1, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Corpus 2 -  2015-2018 to do
# compute document term matrix with terms >= minimumFrequency
minimumFrequency2 <- 500

# pre-processing:
pcorp2 <- gsub("'", "", ofstedcorp3small$text)  # remove apostrophes
pcorp2 <- gsub("[[:punct:]]", " ", pcorp2)  # replace punctuation with space
pcorp2 <- gsub("[[:cntrl:]]", " ", pcorp2)  # replace control characters with space
pcorp2 <- gsub("^[[:space:]]+", "", pcorp2) # remove whitespace at beginning of documents
pcorp2 <- gsub("[[:space:]]+$", "", pcorp2) # remove whitespace at end of documents
pcorp2 <- tolower(pcorp2)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
pcorp2 = stringr::str_replace_all(pcorp2, stopwords_regex, '')

# Create dtm
DTM2 <- DocumentTermMatrix(pcorp2, control = list(bounds = list(global = c(minimumFrequency2, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM2)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx2 <- slam::row_sums(DTM2) > 0
DTM2 <- DTM2[sel_idx2, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel2 <- LDA(DTM2, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult2 <- posterior(topicModel2)
# format of the resulting object
attributes(tmResult2)
nTerms(DTM2)  
# topics are probability distributions over the entire vocabulary
beta2 <- tmResult2$terms   # get beta from results
dim(beta2)                # K distributions over nTerms(DTM) terms
rowSums(beta2)            # rows in beta sum to 1
nDocs(DTM2)               # size of collection
# for every document we have a probability distribution of its contained topics
theta2 <- tmResult2$topics 
dim(theta2)               # nDocs(DTM) distributions over K topics
rowSums(theta2)[1:10]     # rows in theta sum to 1

# 10 most likely terms
terms(topicModel2, 20)
exampleTermData2 <- terms(topicModel2, 10)
exampleTermData2[, 1:8]

# Give the topics names
top5termsPerTopic2 <- terms(topicModel2, 5)
Next5b <- terms(topicModel2, 20)[6:10,]
topicNames2 <- apply(Next5b, 2, paste, collapse=" ")
topicNames2

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year2 <- aggregate(theta2, by = list(year = ofstedcorp3small$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year2)[2:(K+1)] <- topicNames2
# reshape data frame
vizDataFrame2 <- melt(topic_proportion_per_year2, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame2, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  #  scale_fill_manual(values = paste0(alphabet(18), "FF"), name = "year") + 
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Corpus 4 - 2020 onward
# compute document term matrix with terms >= minimumFrequency
minimumFrequency4 <- 500

# pre-processing:
pcorp4 <- gsub("'", "", ofstedcorp4$text)  # remove apostrophes
pcorp4 <- gsub("[[:punct:]]", " ", pcorp4)  # replace punctuation with space
pcorp4 <- gsub("[[:cntrl:]]", " ", pcorp4)  # replace control characters with space
pcorp4 <- gsub("^[[:space:]]+", "", pcorp4) # remove whitespace at beginning of documents
pcorp4 <- gsub("[[:space:]]+$", "", pcorp4) # remove whitespace at end of documents
pcorp4 <- tolower(pcorp4)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
pcorp4 = stringr::str_replace_all(pcorp4, stopwords_regex, '')

# Create dtm
DTM4 <- DocumentTermMatrix(pcorp4, control = list(bounds = list(global = c(minimumFrequency4, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM4)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx4 <- slam::row_sums(DTM4) > 0
DTM4 <- DTM4[sel_idx4, ]

# THIS TAKES AGES - ONLY DONE WITH corpus1 and corpus3 -> k=18
# create models with different number of topics
result4 <- ldatuning::FindTopicsNumber(
  DTM4,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result4)

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel4 <- LDA(DTM4, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult4 <- posterior(topicModel4)
# format of the resulting object
attributes(tmResult4)
nTerms(DTM4)  
# topics are probability distributions over the entire vocabulary
beta1 <- tmResult4$terms   # get beta from results
dim(beta4)                # K distributions over nTerms(DTM) terms
rowSums(beta4)            # rows in beta sum to 1
nDocs(DTM4)               # size of collection
# for every document we have a probability distribution of its contained topics
theta4 <- tmResult4$topics 
dim(theta4)               # nDocs(DTM) distributions over K topics
rowSums(theta4)[1:10]     # rows in theta sum to 1
# 10 most likely terms
terms(topicModel4, 20)
exampleTermData4 <- terms(topicModel4, 10)
exampleTermData4[, 1:8]
# Give the topics names
top5termsPerTopic4 <- terms(topicModel4, 5)
Next5c <- terms(topicModel4, 20)[6:10,]
topicNames4 <- apply(Next5c, 2, paste, collapse=" ")

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year4 <- aggregate(theta4, by = list(year = ofstedcorp4$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year4)[2:(K+1)] <- topicNames4
# reshape data frame
vizDataFrame4 <- melt(topic_proportion_per_year4, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame4, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
#  scale_fill_manual(values = paste0(alphabet(18), "FF"), name = "year") + 
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Full Ofstedcorpus to get diagram over time
# Compute document term matrix with terms >= minimumFrequency
minimumFrequencyfull <- 500

# pre-processing:
pofstedcorp <- gsub("'", "", ofstedcorp$text)  # remove apostrophes
pofstedcorp <- gsub("[[:punct:]]", " ", pofstedcorp)  # replace punctuation with space
pofstedcorp <- gsub("[[:cntrl:]]", " ", pofstedcorp)  # replace control characters with space
pofstedcorp <- gsub("^[[:space:]]+", "", pofstedcorp) # remove whitespace at beginning of documents
pofstedcorp <- gsub("[[:space:]]+$", "", pofstedcorp) # remove whitespace at end of documents
pofstedcorp <- tolower(pofstedcorp)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
pofstedcorp = stringr::str_replace_all(pofstedcorp, stopwords_regex, '')

# Create dtm
DTMfull <- DocumentTermMatrix(pofstedcorp, control = list(bounds = list(global = c(minimumFrequencyfull, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTMfull)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idxf <- slam::row_sums(DTMfull) > 0
DTMfull <- DTMfull[sel_idxf, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModelFull <- LDA(DTMfull, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResultFull <- posterior(topicModelFull)
# format of the resulting object
attributes(tmResultFull)
nTerms(DTMfull)  
# topics are probability distributions over the entire vocabulary
betafull <- tmResultFull$terms   # get beta from results
dim(betafull)                # K distributions over nTerms(DTM) terms
rowSums(betafull)            # rows in beta sum to 1
nDocs(DTMfull)               # size of collection
# for every document we have a probability distribution of its contained topics
thetafull <- tmResultFull$topics 
dim(thetafull)               # nDocs(DTM) distributions over K topics
rowSums(thetafull)[1:10]     # rows in theta sum to 1
# 10 most likely terms - can filter or vary e.g. terms(topicModelFull, 10)[6:10,]
terms(topicModelFull, 20)
exampleTermDataFull <- terms(topicModelFull, 10)
exampleTermDataFull[, 1:8]
# Give the topics names
top5termsPerTopicFull <- terms(topicModelFull, 5)
Next5d <- terms(topicModelFull, 20)[6:10,]
topicNamesFull <- apply(Next5d, 2, paste, collapse=" ")

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_yearFull <- aggregate(thetafull, by = list(year = ofstedcorp$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_yearFull)[2:(K+1)] <- topicNamesFull
# reshape data frame
vizDataFrameFull <- melt(topic_proportion_per_yearFull, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrameFull, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # scale_fill_manual(values = paste0(letter(18), "FF"), name = "year") + 
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Structural Topic Modelling - e.g. see https://bookdown.org/valerie_hase/TextasData_HS2021/tutorial-13-topic-modeling.html

## Structural Topic Modelling
# This is now only applied to one corpus - LDA to the 'phases'.

## Whole dataset

ofstedcorplatest2 <- ofstedcorplatest[ofstedcorplatest$outcome %in% c("Outstanding", "Good", "Requires Improvement", "Inadequate"),]
# Check if filtered
unique(ofstedcorplatest2$outcome)
# Filter out 2009 and 2010
ofstedcorplatest3 <- ofstedcorplatest2[!ofstedcorplatest2$year %in% c(2009, 2010),]

table(ofstedcorplatest$year)
table(ofstedcorplatest3$year)

ggplot(ofstedcorplatest) +
  geom_bar(aes(x = year, fill = as.factor(year)), 
           position = "dodge", stat = "count")

ggplot(ofstedcorplatest3) +
  geom_bar(aes(x = year, fill = outcome), 
           position = "dodge", stat = "count")

# create document feature matrix from corpus
corplatest <- corpus(ofstedcorplatest)
corplatest3 <- corpus(ofstedcorplatest3)

# Improve filtering here
ofsted_latestdfm <- dfm(corplatest, tolower = T, stem = T,
                   remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
# view top 16 tokens
latesttop <- topfeatures(ofsted_latestdfm, 16)
data.frame(latesttop)

# Improve filtering here
ofsted_latestdfm3 <- dfm(corplatest3, tolower = T, stem = T,
                        remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
# view top 16 tokens
latesttop3 <- topfeatures(ofsted_latestdfm3, 16)
data.frame(latesttop3)

# This is the original uncleaned streak
library(stm)
ofsted_lateststm <- convert(ofsted_latestdfm, to = 'stm')
plotRemoved(ofsted_lateststm$documents, lower.thresh = seq(1, 25, by = 1))
ofsted_lateststm <- prepDocuments(ofsted_lateststm$documents,ofsted_lateststm$vocab,
                                  ofsted_lateststm$meta, lower.thresh = 1000)

# Fitting models
fit_stm <- stm(documents = ofsted_lateststm$documents, vocab = ofsted_lateststm$vocab,
               K = 18,  seed = 9161,
               data = ofsted_lateststm$meta, sigma.prior = .1)

plot(fit_stm)
# Top 10 per topic
labels <- labelTopics(fit_stm, n = 10)
#only keep FREX weighting
topwords <- data.frame("features" = t(labels$frex))
#assign topic number as column name
colnames(topwords) <- paste("Topic", c(1:18))
#Return the result
topwords[1:18]

# Used to randomly check documents manually
thetastm <- make.dt(fit_stm)
thetastm[1,1:16]

# Present tableaus of topics and terms
par(mfrow = c(1,2))
plot(fit_stm, type = 'labels', topics = c(8), labeltype = 'score', main = 'score')
plot(fit_stm, type = 'labels', topics = c(2, 4:6), labeltype = 'score', main = 'score')

# This is the streak with ofsted_latestdfm3
ofsted_lateststm3 <- convert(ofsted_latestdfm3, to = 'stm')
plotRemoved(ofsted_lateststm3$documents, lower.thresh = seq(1, 25, by = 1))
# This also added an upper thresh of 100000
ofsted_lateststm3 <- prepDocuments(ofsted_lateststm3$documents,ofsted_lateststm3$vocab,
                                  ofsted_lateststm3$meta, lower.thresh = 1000, upper.thresh=100000)

# Fitting models
fit_stm3 <- stm(documents = ofsted_lateststm3$documents, vocab = ofsted_lateststm3$vocab,
               K = 18,  seed = 9161,
               data = ofsted_lateststm3$meta, sigma.prior = .1)

plot(fit_stm3)
# Top 10 per topic
labels3 <- labelTopics(fit_stm3, n = 10)
#only keep FREX weighting
topwords3 <- data.frame("features" = t(labels3$frex))
#assign topic number as column name
colnames(topwords3) <- paste("Topic", c(1:18))
#Return the result
topwords3[1:18]

# Used to randomly check documents manually
thetastm3 <- make.dt(fit_stm3)
thetastm3[1,1:16]

# Present tableaus of topics and terms
par(mfrow = c(1,2))
plot(fit_stm3, type = 'labels', topics = c(4), labeltype = 'score', main = 'score')
plot(fit_stm3, type = 'labels', topics = c(2, 4:6), labeltype = 'score', main = 'score')

# From here the section that did not work with the original - had to filter because 'outcome' not clean
par(mfrow = c(1,1))
for (i in 1:length(unique(ofstedcorplatest3$outcome))) plot(fit_stm3, type = 'perspectives',
                                                        topics = c(1,18), labeltype = 'frex',
                                                        covarlevels = unique(ofstedcorplatest3$outcome)[i],
                                                        text.cex = .75, main = unique(ofstedcorplatest3$outcome)[i])

thoughts15 <- findThoughts(fit_stm3, texts = ofstedcorplatest3$text,
                           topics = 4, n = 7)
plotQuote(thoughts15, width = 95, text.cex = .65, maxwidth = 275)
cloud(fit_stm3, topic = 4)

# Fitting models - same one as original but now with outcome included
fit_stm3v2 <- stm(documents = ofsted_lateststm3$documents, vocab = ofsted_lateststm3$vocab,
               K = 18,  seed = 9161, prevalence = ~outcome,
               data = ofsted_lateststm3$meta, sigma.prior = .1)

cloud(fit_stm3v2, topic = 6)
cloud(fit_stm3v2, topic = 14)
cloud(fit_stm3v2, topic = 16)
cloud(fit_stm3v2, topic = 3)
cloud(fit_stm3v2, topic = 7)

est_stm3 <- estimateEffect(formula =~ outcome, stmobj=fit_stm3v2, metadata = ofsted_lateststm3$meta)

# Maybe filter some topics?
plot(est_stm3, "outcome", method = "pointestimate", topics = c(3, 6, 7, 14, 16), model = fit_stm3v2)

# For K=6 (to compare with Damian's work)
# Fitting models - same one as original but now with outcome included
fit_stm3v3 <- stm(documents = ofsted_lateststm3$documents, vocab = ofsted_lateststm3$vocab,
                  K = 6,  seed = 9161, prevalence = ~outcome,
                  data = ofsted_lateststm3$meta, sigma.prior = .1)

plot(fit_stm3v3)
# Top 10 per topic
labels3 <- labelTopics(fit_stm3v3, n = 10)
#only keep FREX weighting
topwords3 <- data.frame("features" = t(labels3$frex))
#assign topic number as column name
colnames(topwords3) <- paste("Topic", c(1:6))
#Return the result
topwords3[1:6]

cloud(fit_stm3v3, topic = 3)
cloud(fit_stm3v3, topic = 6)

est_stm3v3 <- estimateEffect(formula =~ outcome, stmobj=fit_stm3v3, metadata = ofsted_lateststm3$meta)

# Maybe filter some topics?
plot(est_stm3, "outcome", method = "pointestimate", topics = c(3,6), model = fit_stm3v3)


# Use filtered outcome corpus
# Create separate ones per outcome (judgement)
# compute document term matrix with terms >= minimumFrequency
mf <- 500
# Max as well this time
xf <- 200000

# pre-processing:
latest <- gsub("'", "", ofstedcorplatest3$text)  # remove apostrophes
latest <- gsub("[[:punct:]]", " ", latest)  # replace punctuation with space
latest <- gsub("[[:cntrl:]]", " ", latest)  # replace control characters with space
latest <- gsub("^[[:space:]]+", "", latest) # remove whitespace at beginning of documents
latest <- gsub("[[:space:]]+$", "", latest) # remove whitespace at end of documents
latest <- tolower(latest)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
latest = stringr::str_replace_all(latest, stopwords_regex, '')

# Create dtm
latestDTM <- DocumentTermMatrix(latest, control = list(bounds = list(global = c(mf, xf))))
# have a look at the number of documents and terms in the matrix
dim(latestDTM)
# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idxl <- slam::row_sums(latestDTM) > 0
latestDTM <- latestDTM[sel_idxl, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel5 <- LDA(latestDTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult5 <- posterior(topicModel5)
# format of the resulting object
attributes(tmResult5)
nTerms(latestDTM)  
# topics are probability distributions over the entire vocabulary
beta5 <- tmResult5$terms   # get beta from results
dim(beta5)                # K distributions over nTerms(DTM) terms
rowSums(beta5)            # rows in beta sum to 1
nDocs(latestDTM)               # size of collection
# for every document we have a probability distribution of its contained topics
theta5 <- tmResult5$topics 
dim(theta5)               # nDocs(DTM) distributions over K topics
rowSums(theta5)[1:10]     # rows in theta sum to 1
# 10 most likely terms
terms(topicModel5, 10)
exampleTermData5 <- terms(topicModel5, 10)
exampleTermData5[, 1:8]
# Give the topics names
top5termsPerTopic5 <- terms(topicModel5, 5)
topicNames5 <- apply(top5termsPerTopic5, 2, paste, collapse=" ")
topicNames5

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year5 <- aggregate(theta5, by = list(year = ofstedcorplatest3$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year5)[2:(K+1)] <- topicNames5
# reshape data frame
vizDataFrame5<- melt(topic_proportion_per_year5, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrame5, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Now the same by outcome
# First make subsets

outstanding <- subset(ofstedcorplatest3, ofstedcorplatest3$outcome=="Outstanding")
good <- subset(ofstedcorplatest3, ofstedcorplatest3$outcome=="Good")
reqimp <- subset(ofstedcorplatest3, ofstedcorplatest3$outcome=="Requires Improvement")
inadequate <- subset(ofstedcorplatest3, ofstedcorplatest3$outcome=="Inadequate")

# Lower values because smaller corpora (divided by 5)
# Create separate ones per outcome (judgement) - OUTSTANDING
mf2 <- 100
# Max as well this time
xf2 <- 40000
# pre-processing:
oustanding2 <- gsub("'", "", outstanding$text)  # remove apostrophes
oustanding2 <- gsub("[[:punct:]]", " ", oustanding2)  # replace punctuation with space
oustanding2 <- gsub("[[:cntrl:]]", " ", oustanding2)  # replace control characters with space
oustanding2 <- gsub("^[[:space:]]+", "", oustanding2) # remove whitespace at beginning of documents
oustanding2 <- gsub("[[:space:]]+$", "", oustanding2) # remove whitespace at end of documents
oustanding2 <- tolower(oustanding2)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
oustanding2 = stringr::str_replace_all(oustanding2, stopwords_regex, '')
# Create dtm
latestDTMoutstanding <- DocumentTermMatrix(oustanding2, control = list(bounds = list(global = c(mf2, xf2))))
sel_idx666 <- slam::row_sums(latestDTMoutstanding) > 0
latestDTMoutstanding <- latestDTMoutstanding[sel_idx666, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModelOutstanding <- LDA(latestDTMoutstanding, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResultOutstanding <- posterior(topicModelOutstanding)
# topics are probability distributions over the entire vocabulary
betaOut <- tmResultOutstanding$terms   # get beta from results
dim(betaOut)                # K distributions over nTerms(DTM) terms
rowSums(betaOut)            # rows in beta sum to 1
nDocs(latestDTMoutstanding)               # size of collection
# for every document we have a probability distribution of its contained topics
thetaOutstanding <- tmResultOutstanding$topics 
# 10 most likely terms
terms(topicModelOutstanding, 10)
# Give the topics names
top5termsPerTopicOutstanding <- terms(topicModelOutstanding, 5)
topicNamesOutstanding <- apply(top5termsPerTopicOutstanding, 2, paste, collapse=" ")
topicNamesOutstanding

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year_outstanding <- aggregate(thetaOutstanding, by = list(year = outstanding$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year_outstanding)[2:(K+1)] <- topicNamesOutstanding
# reshape data frame
vizDataFrameOut<- melt(topic_proportion_per_year_outstanding, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrameOut, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Lower values because smaller corpora (divided by 5)
# Create separate ones per outcome (judgement) - GOOD
mf2 <- 100
# Max as well this time
xf2 <- 40000
# pre-processing:
good2 <- gsub("'", "", good$text)  # remove apostrophes
good2 <- gsub("[[:punct:]]", " ", good2)  # replace punctuation with space
good2 <- gsub("[[:cntrl:]]", " ", good2)  # replace control characters with space
good2 <- gsub("^[[:space:]]+", "", good2) # remove whitespace at beginning of documents
good2 <- gsub("[[:space:]]+$", "", good2) # remove whitespace at end of documents
good2 <- tolower(good2)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
good2 = stringr::str_replace_all(good2, stopwords_regex, '')
# Create dtm
latestDTMgood <- DocumentTermMatrix(good2, control = list(bounds = list(global = c(mf2, xf2))))
sel_idx667 <- slam::row_sums(latestDTMgood) > 0
latestDTMgood <- latestDTMgood[sel_idx667, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModelGood <- LDA(latestDTMgood, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResultGood <- posterior(topicModelGood)
# topics are probability distributions over the entire vocabulary
betaGood <- tmResultGood$terms   # get beta from results
dim(betaGood)                # K distributions over nTerms(DTM) terms
rowSums(betaGood)            # rows in beta sum to 1
nDocs(latestDTMgood)               # size of collection
# for every document we have a probability distribution of its contained topics
thetaGood <- tmResultGood$topics 
# 10 most likely terms
terms(topicModelGood, 10)
# Give the topics names
top5termsPerTopicGood <- terms(topicModelGood, 5)
topicNamesGood <- apply(top5termsPerTopicGood, 2, paste, collapse=" ")
topicNamesGood

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year_good <- aggregate(thetaGood, by = list(year = good$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year_good)[2:(K+1)] <- topicNamesGood
# reshape data frame
vizDataFrameGood<- melt(topic_proportion_per_year_good, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrameGood, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Lower values because smaller corpora (divided by 5)
# Create separate ones per outcome (judgement) - REQUIRING IMPROVEMENT
mf2 <- 100
# Max as well this time
xf2 <- 40000
# pre-processing:
oustanding2 <- gsub("'", "", reqimp$text)  # remove apostrophes
oustanding2 <- gsub("[[:punct:]]", " ", oustanding2)  # replace punctuation with space
oustanding2 <- gsub("[[:cntrl:]]", " ", oustanding2)  # replace control characters with space
oustanding2 <- gsub("^[[:space:]]+", "", oustanding2) # remove whitespace at beginning of documents
oustanding2 <- gsub("[[:space:]]+$", "", oustanding2) # remove whitespace at end of documents
latest <- tolower(oustanding2)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
oustanding2 = stringr::str_replace_all(oustanding2, stopwords_regex, '')
# Create dtm
latestDTMoutstanding <- DocumentTermMatrix(oustanding2, control = list(bounds = list(global = c(mf2, xf2))))
sel_idx666 <- slam::row_sums(latestDTMoutstanding) > 0
latestDTMoutstanding <- latestDTMoutstanding[sel_idx666, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModelOutstanding <- LDA(latestDTMoutstanding, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResultOutstanding <- posterior(topicModelOutstanding)
# topics are probability distributions over the entire vocabulary
betaOut <- tmResultOutstanding$terms   # get beta from results
dim(betaOut)                # K distributions over nTerms(DTM) terms
rowSums(betaOut)            # rows in beta sum to 1
nDocs(latestDTMoutstanding)               # size of collection
# for every document we have a probability distribution of its contained topics
thetaOutstanding <- tmResultOutstanding$topics 
# 10 most likely terms
terms(topicModelOutstanding, 10)
# Give the topics names
top5termsPerTopicOutstanding <- terms(topicModelOutstanding, 5)
topicNamesOutstanding <- apply(top5termsPerTopicOutstanding, 2, paste, collapse=" ")
topicNamesOutstanding

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year_outstanding <- aggregate(thetaOutstanding, by = list(year = outstandi$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year_outstanding)[2:(K+1)] <- topicNamesOutstanding
# reshape data frame
vizDataFrameOut<- melt(topic_proportion_per_year_outstanding, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrameOut, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Lower values because smaller corpora (divided by 5)
# THIS code used for all four subsequently - CLEAN UP

# Create separate ones per outcome (judgement) - OUTSTANDING
mf2 <- 100
# Max as well this time
xf2 <- 40000
# pre-processing:
oustanding2 <- gsub("'", "", good$text)  # remove apostrophes
oustanding2 <- gsub("[[:punct:]]", " ", oustanding2)  # replace punctuation with space
oustanding2 <- gsub("[[:cntrl:]]", " ", oustanding2)  # replace control characters with space
oustanding2 <- gsub("^[[:space:]]+", "", oustanding2) # remove whitespace at beginning of documents
oustanding2 <- gsub("[[:space:]]+$", "", oustanding2) # remove whitespace at end of documents
latest <- tolower(oustanding2)  # force to lowercase
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
oustanding2 = stringr::str_replace_all(oustanding2, stopwords_regex, '')


# Create dtm
latestDTMoutstanding <- DocumentTermMatrix(oustanding2, control = list(bounds = list(global = c(mf2, xf2))))
sel_idx666 <- slam::row_sums(latestDTMoutstanding) > 0
latestDTMoutstanding <- latestDTMoutstanding[sel_idx666, ]

# number of topics
K <- 18
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 500 iterations of Gibbs sampling
topicModelOutstanding <- LDA(latestDTMoutstanding, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResultOutstanding <- posterior(topicModelOutstanding)
# topics are probability distributions over the entire vocabulary
betaOut <- tmResultOutstanding$terms   # get beta from results
dim(betaOut)                # K distributions over nTerms(DTM) terms
rowSums(betaOut)            # rows in beta sum to 1
nDocs(latestDTMoutstanding)               # size of collection
# for every document we have a probability distribution of its contained topics
thetaOutstanding <- tmResultOutstanding$topics 
# 10 most likely terms
terms(topicModelOutstanding, 20)
# Give the topics names
top5termsPerTopicOutstanding <- terms(topicModelOutstanding, 5)
NextOut <- terms(topicModelOutstanding, 20)[6:10,]
topicNamesOutstanding <- apply(NextOut, 2, paste, collapse=" ")
topicNamesOutstanding

# Viz tested with the smallest corpus first
# get mean topic proportions per year
topic_proportion_per_year_outstanding <- aggregate(thetaOutstanding, by = list(year = good$year), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_year_outstanding)[2:(K+1)] <- topicNamesOutstanding
# reshape data frame
vizDataFrameOut<- melt(topic_proportion_per_year_outstanding, id.vars = "year")
# plot topic proportions per decade as bar plot
ggplot(vizDataFrameOut, aes(x=year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  # Comment next for rainbow plot
  scale_fill_manual(values = unname(glasbey()), name = "year") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Still do three other judgements...

# Calculate AFFINN for all the terms
library("tidytext")
install.packages("textdata")
library("textdata")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Full ofstedcorp - only works with more RAM (cannot allocate vector of size 4.0 Gb)
sentiment.full <- unnest_tokens(ofstedcorp, word, text)
rating.sentiment.full.afinn <- sentiment.full %>% inner_join(get_sentiments("afinn"))
rating.sentiment.full.afinn[1:10, c("word", "value")]
rating.sentiment.full.afinn.score <- rating.sentiment.full.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.full.afinn.score$sentiment)

# Full only with outcome ofstedcorplatest3

sentiment.full2 <- unnest_tokens(ofstedcorplatest3, word, text)
rating.sentiment.full2.afinn <- sentiment.full2 %>% inner_join(get_sentiments("afinn"))
rating.sentiment.full2.afinn[1:10, c("word", "value")]
rating.sentiment.full2.afinn.score <- rating.sentiment.full2.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
addyear <- merge(ofstedcorplatest3,rating.sentiment.full2.afinn.score, by = "urn")

library(dplyr)

# Figure with sentiments
addyear %>% ggplot(aes(x = year, y = sentiment, colour=outcome)) + geom_jitter() +
  scale_x_continuous(breaks = 0:2100) + scale_color_manual(values=c("yellow", "red", "green", "orange")) + scale_fill_discrete(limits=c('Outstanding', 'Good', 'Requires Improvement', 'Inadequate'))

# For each year, and seperately for foreign or not, calculate mean x.
data.means <- addyear %>% 
  group_by(year, outcome) %>%
  summarize(meansentiment = mean(sentiment))

# plot. You don't need group = foreign
ggplot(data.means, aes(x = year, y = meansentiment, linetype = factor(outcome), colour=outcome)) + 
  geom_line(linewidth=2)


subset <- subset(ofstedcorplatest3, year>2019)
sentiment.full2 <- unnest_tokens(subset, word, text)
rating.sentiment.full2.afinn <- sentiment.full2 %>% inner_join(get_sentiments("afinn"))
rating.sentiment.full2.afinn[1:10, c("word", "value")]
rating.sentiment.full2.afinn.score <- rating.sentiment.full2.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
addyear <- merge(subset,rating.sentiment.full2.afinn.score, by = "urn")

library(dplyr)
# For each year, and seperately for foreign or not, calculate mean x.
data.means <- addyear %>% 
  group_by(year, outcome) %>%
  summarize(meansentiment = mean(sentiment))

# plot. You don't need group = foreign
ggplot(data.means, aes(x = year, y = meansentiment, linetype = factor(outcome), colour=outcome)) + 
  geom_line(linewidth=2)


# Do inadequate first - fewest texts
sentiment.inadequate <- unnest_tokens(inadequate, word, text)
rating.sentiment.inadequate.afinn <- sentiment.inadequate %>% inner_join(get_sentiments("afinn"))
rating.sentiment.inadequate.afinn[1:10, c("word", "value")]
rating.sentiment.inadequate.afinn.score <- rating.sentiment.inadequate.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.inadequate.afinn.score$sentiment)

# Requires Improvement
sentiment.reqimp <- unnest_tokens(reqimp, word, text)
rating.sentiment.reqimp.afinn <- sentiment.reqimp %>% inner_join(get_sentiments("afinn"))
rating.sentiment.reqimp.afinn[1:10, c("word", "value")]
rating.sentiment.reqimp.afinn.score <- rating.sentiment.reqimp.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.reqimp.afinn.score$sentiment)

# Good
sentiment.good <- unnest_tokens(good, word, text)
rating.sentiment.good.afinn <- sentiment.good %>% inner_join(get_sentiments("afinn"))
rating.sentiment.good.afinn[1:10, c("word", "value")]
rating.sentiment.good.afinn.score <- rating.sentiment.good.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.good.afinn.score$sentiment)


good2 <- subset(good, year>2017)
sentiment.good <- unnest_tokens(good2, word, text)
rating.sentiment.good.afinn <- sentiment.good %>% inner_join(get_sentiments("afinn"))
rating.sentiment.good.afinn[1:10, c("word", "value")]
rating.sentiment.good.afinn.score <- rating.sentiment.good.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.good.afinn.score$sentiment)

# Do inadequate first - fewest texts
sentiment.outstanding <- unnest_tokens(outstanding, word, text)
rating.sentiment.outstanding.afinn <- sentiment.outstanding %>% inner_join(get_sentiments("afinn"))
rating.sentiment.outstanding.afinn[1:10, c("word", "value")]
rating.sentiment.outstanding.afinn.score <- rating.sentiment.outstanding.afinn %>% group_by(urn) %>% summarise(sentiment = sum(value))
mean(rating.sentiment.outstanding.afinn.score$sentiment)


# Semantic similarity
# Two comparisons
# Before and after 2014 compared to myths document
# Load myths document

# Create new DFMs
# Improve filtering here
library("quanteda")
ofstedcorp1s <- corpus(ofstedcorp1small)
ofstedcorp3s <- corpus(ofstedcorp3small)
ofstedcorp4s <- corpus(ofstedcorp4)
corpus1dfm <- dfm(ofstedcorp1s, tolower = T, stem = T,
                         remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
corpus2dfm <- dfm(ofstedcorp3s, tolower = T, stem = T,
                  remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
corpus3dfm <- dfm(ofstedcorp4s, tolower = T, stem = T,
                  remove = stopwords('english'), remove_punct = T,  remove_numbers = T)

# Read texts
library(readtext) # easily read in text in directories
# Convert PDFs to TXT files
sourcetexts <- "C://Rprojects//NUFFIELD_OFSTED//compare"
# make a vector of PDF file names
myfiles2compare <- list.files(path = sourcetexts, pattern = "pdf",  full.names = TRUE)
# convert each PDF file that is named in the vector into a text file 
# text file is created in a director TXT directory as the PDFs (easiest to create)
lapply(myfiles2compare, function(i) system(paste('"C://Rprojects//NUFFIELD_OFSTED//tools//xpdf-tools-win-4.04//bin64//pdftotext.exe"', paste0('"', i, '"'),
                                         paste0('"C://Rprojects//NUFFIELD_OFSTED//compare//TXT//', basename(tools::file_path_sans_ext(i)), '.txt"')), wait = FALSE))
# recursively get filepaths for documents
comparedocs <- list.files(path = c('compare//TXT'), recursive = T, full.names = T)
clarifydoc <- readtext(comparedocs[1], docvarsfrom = "filenames")
eifdoc <- readtext(comparedocs[2], docvarsfrom = "filenames")

clarifycorpus <- corpus(clarifydoc)
eifcorpus <- corpus(eifdoc)

# Create DFMS
claritydfm <- dfm(clarifycorpus, tolower = T, stem = T,
                               remove = stopwords('english'), remove_punct = T,  remove_numbers = T)
eifdfm <- dfm(eifcorpus, tolower = T, stem = T,
                 remove = stopwords('english'), remove_punct = T,  remove_numbers = T)

# Compare similarity
# First try corpus3dfm with eifdfm
library(quanteda)
install.packages("quanteda.textstats")
library(quanteda.textstats)
eif_after= textstat_simil(corpus3dfm,  eifdfm, margin = "documents", method = "cosine")
eif_before= textstat_simil(corpus2dfm,  eifdfm, margin = "documents", method = "cosine")
eifbefore <- as.data.frame(eif_before)
eifbefore['document2'] <- "<2019"
eifafter <- as.data.frame(eif_after)
eifafter['document2'] <- ">2019"
eif <- bind_rows(eifbefore, eifafter)
eif %>% ggplot(aes(x = document2, y = cosine, colour=document2)) + geom_jitter() + labs(color='Introduction EIF') 

myth_after= textstat_simil(corpus2dfm,  claritydfm, margin = "documents", method = "cosine")
myth_before= textstat_simil(corpus1dfm,  claritydfm, margin = "documents", method = "cosine")
mythbefore <- as.data.frame(myth_before)
mythbefore['document2'] <- "<2014"
mythafter <- as.data.frame(myth_after)
mythafter['document2'] <- ">2014"
myth <- bind_rows(mythbefore, mythafter)
myth %>% ggplot(aes(x = document2, y = cosine, colour=document2)) + geom_jitter() + labs(color='Clarification myths') 


# Before and after framework
# Load 2019 framework
