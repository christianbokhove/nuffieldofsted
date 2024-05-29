#########################################################
##
##  This script generates a clean corpus.
##
#########################################################

library(readtext) # easily read in text in directories
library(tidytext)

# Convert PDFs to TXT files
source <- "C://Rprojects//NUFFIELD_OFSTED//run_030622//all"

# make a vector of PDF file names
myfiles <- list.files(path = source, pattern = "pdf",  full.names = TRUE)

# convert each PDF file that is named in the vector into a text file 
# text file is created in a director TXT directory as the PDFs (easiest to create)
lapply(myfiles, function(i) system(paste('"C://Rprojects//NUFFIELD_OFSTED//tools//xpdf-tools-win-4.04//bin64//pdftotext.exe"', paste0('"', i, '"'),
                                         paste0('"C://Rprojects//NUFFIELD_OFSTED//run_030622//all//TXT//', basename(tools::file_path_sans_ext(i)), '.txt"')), wait = FALSE))

# recursively get filepaths for documents
ofsted_paths <- list.files(path = c('run_030622/all/TXT/'), recursive = T, full.names = T)

# Here iterate through all files with code per 'type of report'.

# read in documents (after that need cleaning)
ofsted_docs_full <- readtext(ofsted_paths, docvarsfrom = "filenames")

library(dplyr)
ofsted_docs_full <- rename(ofsted_docs_full, fileid=docvar1)

# copy dataframe newtotal5
dataset <- newtotal5
dataset = select(dataset, -LreportURN, -test)

ofsted_merged <- merge(ofsted_docs_full, dataset, on='fileid')
# Note when data are missing outcome is a number - so needs to be filtered out

# pre-processing
library(quanteda)

# Convert to corpus for 
ofsted_merged$year = as.numeric(ofsted_merged$inspectionyear)
ofsted_merged$since1997 = ofsted_merged$year-1997
ofsted_merged_corpus <- corpus(ofsted_merged$text, docvars = data.frame(outcome = ofsted_merged$outcome, year = ofsted_merged$year, since1997 = ofsted_merged$since1997, fileid = ofsted_merged$fileid, urn = ofsted_merged$Urn))

# Clean up
library(tm)
stop_words <- stopwords("SMART")

# pre-processing:
ofsted_merged_corpus_clean <- gsub("'", "", ofsted_merged_corpus)  # remove apostrophes
ofsted_merged_corpus_clean <- gsub("[[:punct:]]", " ", ofsted_merged_corpus_clean)  # replace punctuation with space
ofsted_merged_corpus_clean <- gsub("[[:cntrl:]]", " ", ofsted_merged_corpus_clean)  # replace control characters with space
ofsted_merged_corpus_clean <- gsub("^[[:space:]]+", "", ofsted_merged_corpus_clean) # remove whitespace at beginning of documents
ofsted_merged_corpus_clean <- gsub("[[:space:]]+$", "", ofsted_merged_corpus_clean) # remove whitespace at end of documents
ofsted_merged_corpus_clean <- gsub("[0-9.-]", "", ofsted_merged_corpus_clean) # remove numbers
ofsted_merged_corpus_clean <- tolower(ofsted_merged_corpus_clean)  # force to lowercase
ofsted_merged_corpus_clean <- gsub("\\s+", " ", ofsted_merged_corpus_clean) # obsolete whitespace

# Stop words in tidytext
# data("stop_words")
# ofsted_merged_corpus_clean <- ofsted_merged_corpus_clean %>% anti_join(stop_words)

## FURTHER PRE-PROCESSING

ofstedcorp <- data.frame(text = sapply(ofsted_merged_corpus_clean, as.character), fileid=ofsted_merged_corpus_clean$fileid, urn=ofsted_merged_corpus_clean$urn, outcome=ofsted_merged_corpus_clean$outcome, year=ofsted_merged_corpus_clean$year, stringsAsFactors = FALSE)
saveRDS(ofstedcorp, file = "ofstedcorp.rds")

# Preparations for using LDA
# tokenize on space and output as a list:
doc.list <- strsplit(ofsted_merged_corpus_clean, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

