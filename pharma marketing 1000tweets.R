#library(twitter)
library(twitteR)
#authentication
library(ROAuth)
setup_twitter_oauth('3BCyiLxVEuZMtijDyAnCFumye','ScVmMLaQbL8RoU3xBTy2Lw6yEw3gIbNHQ01FJtIxfIWTEwQqqG','1316496649863258112-fJ6JCffuj4Q5lasLinVoYb3g2UtfyS','BS0ltvJ2DmuenDyFbX0pVyv9lFO3yAwzFfRcUotXVCAbV')
#search tweets
pharma_marketing<-searchTwitter('pharma marketing',n=1000)
pharma_marketingdf<-twListToDF(pharma_marketing)
#pharma_marketing.xlsx
library(xlsx)
write.xlsx(pharma_marketingdf,"e=pharma_marketing.xlsx")
library(dplyr)
pharma_marketingf<- tibble(line = 1:1000, text = pharma_marketing)
library(tidytext)
#use unnest_tokens
pharma_words<-e_pharma_marketing%>%unnest_tokens('word','text')
#use stop_words
pharma_stopwords<- pharma_words %>% anti_join(stop_words)
# use count(word)
pharmacount<-pharma_stopwords %>%count(word, sort = TRUE)
#ggplot to show plot for words
library(ggplot2)
pharma_stopwords%>% count(word, sort = TRUE) %>% filter(n >100) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(n, word)) + geom_col() + labs(y = NULL)
#get sentiment ('afinn'),('BING'),('nrc')
library(textdata)
get_sentiments(afinn)
pharma_afinn<- pharmacount %>%inner_join(get_sentiments("afinn"))
get_sentiments('bing')
pharma_bing<- pharmacount %>%inner_join(get_sentiments("bing"))
pharma_bingcount<-get_sentiments("bing") %>% count(sentiment)
get_sentiments('nrc')
pharma_nrc<-pharmacount %>%inner_join(get_sentiments("nrc"))
#wordcloud to figure words
library(wordcloud)
pharma_words%>%anti_join(stop_words) %>%count(word) %>%with(wordcloud(word, n, max.words =100))
#bigram, shows 2 words repeated together
pharmabigrams <- pharma_marketingdf %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
pharmabigrams_count<-pharmabigrams %>%count(bigram, sort = TRUE) 
library(ggplot2)
pharmabigrams%>% count(bigram, sort = TRUE) %>% filter(n >50) %>% mutate(bigram = reorder(bigram, n)) %>% ggplot(aes(n, bigram)) + geom_col() + labs(y = NULL)


