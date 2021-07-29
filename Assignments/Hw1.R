### ----------------------------------------------------------------------------------------------- ###
### S5205 Frameworks 2 ###
### Assn. 1 || chb2132 ###
### ----------------------------------------------------------------------------------------------- ###

RNGversion(vstr = 3.6);

### ----------------------------------------------------------------------------------------------- ###

setwd("~/R/Assignment 1");

### ----------------------------------------------------------------------------------------------- ###

library(dplyr);
library(tidytext);
library(ggthemes);
library(ggplot2);
library(qdap);
library(tm);

### ----------------------------------------------------------------------------------------------- ###

get_sentiments(lexicon = "bing");
get_sentiments(lexicon = "afinn"); 1;

### ----------------------------------------------------------------------------------------------- ###

bb_revs <- read.csv("baby_reviews.csv", stringsAsFactors = F);

nrc_lex <- read.csv("nrc.csv", stringsAsFactors = F);
afinn_lex <- read.csv("afinn.csv", stringsAsFactors = F);
bing_lex <- read.csv("bing.csv", stringsAsFactors = F);

### ----------------------------------------------------------------------------------------------- ###

str(bb_revs);
mean(bb_revs$review_rating);

bb_revs$review <- lapply(bb_revs$review, as.character);
mean(nchar(bb_revs$review));

cor(nchar(bb_revs$review), bb_revs$review_rating);

median(str_count(string = bb_revs$review, pattern = '\\S+'));

max(str_count(string = bb_revs$review, pattern = '\\S+'));

### ----------------------------------------------------------------------------------------------- ###  
### tm_map() ###

### rev_terms <- bb_revs$review %>% ###
###    unnest_tokens(word, text) %>% ###
###    count(book, word, sort = TRUE); ###

### rev_terms ###
### ----------------------------------------------------------------------------------------------- ###

corpus = Corpus(VectorSource(bb_revs$review));
corpus  = tm_map(corpus, FUN = content_transformer(tolower));

### ----------------------------------------------------------------------------------------------- ###
### Quick check just to see if we need to account for existence of URLS within review strings ###
### 24 reviews contain "http", 4954 without. ###
### ----------------------------------------------------------------------------------------------- ###

url_check <- data.frame(bb_revs);
url_check$url_found <- ifelse(grepl("http", bb_revs$review, ignore.case = T), TRUE, FALSE);

### ----------------------------------------------------------------------------------------------- ###
### url_check V2.0 Still returns 24 positive flags for likely URL entries within reviews, 4954 without. ###
### ----------------------------------------------------------------------------------------------- ###

url_check$url_found_2 <- 
    ifelse(grepl("http://", bb_revs$review, ignore.case = T), TRUE, 
           ifelse(grepl("https://", bb_revs$review, ignore.case = T), TRUE,
                  ifelse(grepl(".com/", bb_revs$review, ignore.case = T), TRUE, FALSE)));

### ----------------------------------------------------------------------------------------------- ###
### Stemming corpus/document (Stop Words Present) ###
### Chopped-off stems from words reintegrated back into library so data is not lost/fidelity preserved ###
### ----------------------------------------------------------------------------------------------- ###

corpus = tm_map(corpus, FUN = 
                    content_transformer(FUN = function(x){
                        gsub(pattern = 'http[[:alnum:][:punct:]]*',
                        replacement = ' ', x = x)}
                    ));

### ----------------------------------------------------------------------------------------------- ###

corpus = tm_map(corpus, FUN = removePunctuation);

### transform corpus to remove stop words, using tidytext lib: df stop_words (def. lang is 'en') ###

corpus <- tidy_documents %>%
    filter(!word %in% stop_words$word)

### Note: Token Frequency Wghts. for Stemming Case Appears Unaffected by stripWhitespace transform ###

corpus = tm_map(corpus, FUN = stripWhitespace);

### ----------------------------------------------------------------------------------------------- ###

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(bb_revs$review))), lowfreq = 0);
dict_corpus = Corpus(VectorSource(dict));

### ----------------------------------------------------------------------------------------------- ###

corpus = tm_map(corpus, FUN = stemDocument);

### ----------------------------------------------------------------------------------------------- ###

### Creating dtm by tokenizing word/terms so each is quantified/assigned a numeric value (Stop Words Present) ###

dtm1 = DocumentTermMatrix(corpus);

### ----------------------------------------------------------------------------------------------- ###

### Removing Sparse Terms, Adding Created Dictionary to Complete Stems, Recover Fidelity/Information (Stop Words Present) ###

xdtm = removeSparseTerms(dtm1, sparse = 0.95);

xdtm = as.data.frame(as.matrix(xdtm));
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type = 'prevalent');
colnames(xdtm) = make.names(colnames(xdtm));

### ----------------------------------------------------------------------------------------------- ###

### Sorting Tokens By Frequency And/Or Value, Studying Highest Frequency Values/Terms (Stop Words Present) ###

sort(colSums(xdtm), decreasing = T)

### ----------------------------------------------------------------------------------------------- ###

freq_terms(text.var = bb_revs$review, top = 10, stopwords = tm::stopwords(kind = "en"));

### Commenting out line to suppress plot production for subsequent section run-throughs ###
### plot(freq_terms(text.var=bb_revs$review, top = 10, stopwords = tm::stopwords(kind="en"))); ###

### ----------------------------------------------------------------------------------------------- ###
### ASSN. 1 ###
### EOF ###
### ----------------------------------------------------------------------------------------------- ###
