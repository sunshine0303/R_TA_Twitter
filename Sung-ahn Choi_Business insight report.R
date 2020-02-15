#loading libraries
library(dplyr)
library(tidytext)
library(tidyverse)
library(twitteR)
library(tm)

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'Wdt1cGLM16o9KNSmmYGgGpICk'
consumer_secret <- 'T7a0DVbWtyv6tamh6SBgjoUrKjUkAdblmfHKZs1WHWwUyiPxVz'
access_token <- '2722967934-wN4hiCQrKRDRlylROVLWso7rqFOTAoH1l2chNlf'
access_secret <- '9z2HbQkQhSLA6RaHMpCyKfvcht6OzWRSJ5R0DeqJ5dRb7'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

ai <- twitteR::searchTwitter('#artificial intelligence', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
a = twitteR::twListToDF(ai)

blockchain <- twitteR::searchTwitter('#blockchain', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
b = twitteR::twListToDF(blockchain)

iot <- twitteR::searchTwitter('#iot', n = 1000, lang = 'en', since = '2015-06-01', retryOnRateLimit = 1e3)
c = twitteR::twListToDF(iot)


# Cleaning the datasets
# Remove http and https elements manually
a$text <- gsub("http[^[:space:]]*","",  d$text) # For http
a$text <- gsub("http[^[:space:]]*","", d$text) # For https

b$text <- gsub("http[^[:space:]]*","",  e$text) # For http
b$text <- gsub("http[^[:space:]]*","", e$text) # For https

c$text <- gsub("http[^[:space:]]*","",  a$text) # For http
c$text <- gsub("http[^[:space:]]*","", a$text) # For https


#tokenizing all 3 datasets from twitter
tidy_ai <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_blockchain <- b %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_iot <- c %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#################################################
### Combining all 3 tidy data frames and creating correlograms
#################################################

library(tidyr)
frequency <- bind_rows(mutate(tidy_ai, author="AI"),
                       mutate(tidy_blockchain, author= "Blockchain"),
                       mutate(tidy_iot, author="IoT")) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Blockchain`, `IoT`)


#let's plot the correlograms:
library(scales)

ggplot(frequency, aes(x=proportion, y=`AI`, 
                      color = abs(`AI`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "AI", x=NULL)

#Taking a look at correlation coefficients
cor.test(data=frequency[frequency$author == "Blockchain",],
         ~proportion + `AI`)

cor.test(data=frequency[frequency$author == "IoT",],
         ~proportion + `AI`)

############################################
## Sentiment analysis 
#############################################
library(tidytext)
get_sentiments('afinn') # Show example of the table

# pulling in sentiment for these 3 tokenized datasets
tidy_ai %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_blockchain %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

tidy_iot %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(id) %>% #if you remove the group_by it will calculate sentiment for all the data
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

#let's take a look at the most positive and most negative tokens in the AI dataset

tidy_ai_sentiment <- tidy_ai %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T)

print(tidy_ai_sentiment)

tidy_ai_sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

############################################
## TF-IDF analysis
#############################################
combined_tech <- bind_rows(mutate(d, make="AI"),
                           mutate(e, make= "Blockchain"),
                           mutate(a, make="IoT")
)

ai_modif <- combined_tech %>%
  unnest_tokens(word, text) %>%
  count(make, word, sort=TRUE) %>%
  ungroup()

ai_modif2 <- ai_modif %>%
  group_by(make) %>%
  summarize(total=sum(n))

ai_leftjoined <- left_join(ai_modif, ai_modif2)

tidy_ai_tfidf <- ai_leftjoined %>%
  bind_tf_idf(word, make, n)

tidy_ai_tfidf # we get all the zeors because we are looking at stop words ... too common

tidy_ai_tfidf %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
tidy_ai_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(make) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=make))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~make, ncol=2, scales="free")+
  coord_flip()

