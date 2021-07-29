### ----------------------------------------------------------------------------------------------- ###
### S5205 Frameworks 2 ###
### Assn. 2 || chb2132 ###
### ----------------------------------------------------------------------------------------------- ###

RNGversion(vstr = 3.6);

### ----------------------------------------------------------------------------------------------- ###

setwd("~/R/Assignment 2");

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

### Tokenize to calc. word count of each review. Step 1: calculating word count for all revs ###
bb_revs %>%

bb_revs %>%
    select(id, review) %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    ungroup() %>%
    group_by(id) %>%
    summarize(count = n())

### ----------------------------------------------------------------------------------------------- ###

### Tokenize after for the total number of words in all reviews by combine of all local review word counts ### 
bb_revs %>%
    group_by(id) %>%
    select(id, review) %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    ungroup() %>%
    count()
group_by(id) %>%
summarize(count = n())

### ----------------------------------------------------------------------------------------------- ###

### Using Bing Lexicon to Classify Sentiment for Review Words/Terms ###
as.data.frame(get_sentiments(lexicon = c("bing")));

get_sentiments(lexicon = c("bing")) %>%
    group_by(sentiment) %>%
    count();
    
### ----------------------------------------------------------------------------------------------- ###
    
### Determining proportion of positive words in reviews for each review rating (1-5)

bb_revs %>%
    select(id, review, review_rating) %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    ungroup() %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(review_rating, sentiment) %>%
    summarize(n = n()) %>%
    mutate(proportion = n/sum(n)) %>%
    ggplot(aes(x = review_rating, y = proportion, fill = sentiment))+geom_col()+theme_economist()+coord_flip()

### ----------------------------------------------------------------------------------------------- ###

###  Determining proportion of positive words for each of the tiers (1-5) ###

### bb_revs %>%
### select(id, review, review_rating) %>%
### group_by(id) %>%
### unnest_tokens(output = word, input = review) %>%
### ungroup() %>%
###  inner_join(get_sentiments("bing")) %>%
###   group_by(review_rating, sentiment) %>%
###   summarize(n = n()) %>%
### mutate(proportion = n/sum(n)) %>%
### ggplot(aes(x = review_rating, y = proportion, fill = sentiment)) + geom_col() + theme_gdocs() + coord_flip()

### ----------------------------------------------------------------------------------------------- ###

sum(nrc$sentiment == "surprise")
sum(nrc_lex$sentiment == "anticipation")
min(afinn_lex$value)
    
### ----------------------------------------------------------------------------------------------- ###

bb_revs %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(nrc) %>%
    group_by(sentiment) %>%
    count()

### ----------------------------------------------------------------------------------------------- ###

bb_revs %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(nrc_lex) %>%
    group_by(id, sentiment, review_rating) %>%
    count()

### ----------------------------------------------------------------------------------------------- ###

bb_revs_n <- bb_revs %>%
                group_by(id) %>%
                unnest_tokens(output = word, input = review) %>%
                inner_join(nrc_lex) %>%
                group_by(id, sentiment, review_rating) %>%
                count()

### ----------------------------------------------------------------------------------------------- ###

### bb_revs_n1 <- bb_revs_n$n %>% ### 
###                filter(n %in% 1); ###

### ----------------------------------------------------------------------------------------------- ###

bb_revs_ns <- 
    bb_revs %>%
        group_by(id) %>%
        unnest_tokens(output = word, input = review) %>%
        inner_join(nrc_lex) %>%
        group_by(id, sentiment, review_rating) %>%
        count() %>%
        group_by(sentiment, review_rating) %>%
        summarize(n = mean(n)) %>%
        data.frame()

which(min(bb_revs_ns$n));
### which(bb_revs_ns$n == "1");

afinn = get_sentiments('afinn') ;

afinn %>%
    group_by(value) %>%
    count()

### ----------------------------------------------------------------------------------------------- ###

### 91 ###
bb_revs %>%
    select(id, review) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    filter(id == 91) %>%
    summarize(reviewSentiment = mean(value))

### 146 ###
bb_revs %>%
    select(id, review) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    filter(id == 146) %>%
    summarize(reviewSentiment = mean(value))

### 238 ###
bb_revs %>%
    select(id, review) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    filter(id == 238) %>%
    summarize(reviewSentiment = mean(value))

### 1432 ###
bb_revs %>%
    select(id, review) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    filter(id == 1432) %>%
    summarize(reviewSentiment = mean(value))


### 2598 ###
bb_revs %>%
    select(id, review) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    filter(id == 2598) %>%
    summarize(reviewSentiment = mean(value))

### ----------------------------------------------------------------------------------------------- ###

bb_revs %>%
    select(id, review) %>%
    group_by(id) %>%
    unnest_tokens(output = word, input = review) %>%
    inner_join(afinn) %>%
    summarize(reviewSentiment = mean(value)) %>%
    ungroup() %>%
    summarize(min = min(reviewSentiment), max = max(reviewSentiment), 
              median = median(reviewSentiment), mean = mean(reviewSentiment))

### ----------------------------------------------------------------------------------------------- ###



### ----------------------------------------------------------------------------------------------- ###
### ASSN. 2 | FRAMEWORKS ### 
### END OF FILE ###
### ----------------------------------------------------------------------------------------------- ###
