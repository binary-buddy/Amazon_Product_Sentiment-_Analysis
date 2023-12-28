#sentiment Analysis#
## including library ##
library(syuzhet)

#reading file#
text_df1 <- read.csv("Areview.csv", stringsAsFactors = FALSE, fileEncoding="latin1")

## Read File ##
# Assuming "content" is the first column
review1<-as.character(text_df1$Content)

## obtaining sentiments scores ##
get_nrc_sentiment('happy')
get_nrc_sentiment('abuse')

## store this data set into new variables ###
s1<-get_nrc_sentiment(review1)

### combining text and sebtiment columns ###
review_sentiment1<-cbind(text_df1$Content,s1)

## barplot ##
barplot(colSums(s1), col = rainbow(9), ylab = 'Count', main = 'Amazon Feedback')

##  calculating scores of each emotion ##
get_sent_values("awesome")
get_sent_values("bully")

## Word Polarity ##

install.packages("tidytext")
install.packages("dplyr")

library(tidytext)
library(dplyr)

bing_lexicon <- get_sentiments("bing")

review_sentiment1 <- data.frame(text_df1 = review1)

tokenized_data <- review_sentiment1 %>%
  unnest_tokens(word, text_df1)

sentiment_data <- tokenized_data %>%
  inner_join(bing_lexicon, by = "word")

sentiment_scores <- sentiment_data %>%
  group_by(text_df1) %>%
  summarize(sentiment_score = sum(sentiment))




# Assuming you have a data frame 'review_sentiment1' with a column 'text_df1' containing your reviews
# Make sure 'review1' is a character vector of reviews

# Install and load required packages
install.packages("tidytext")
install.packages("dplyr")
install.packages("wordcloud")
library(tidytext)
library(dplyr)
library(wordcloud)

# Get the Bing sentiment lexicon
bing_lexicon <- get_sentiments("bing")

# Tokenize the data
tokenized_data <- review_sentiment1 %>%
  unnest_tokens(word, text_df1)

# Join with the Bing lexicon to get sentiment labels for each word
word_sentiments <- tokenized_data %>%
  inner_join(bing_lexicon, by = "word")


# Create a data frame with word frequencies
word_freq <- word_sentiments %>%
  count(word, sentiment)

# Convert sentiment to a factor to ensure proper coloring
word_freq$sentiment <- factor(word_freq$sentiment, levels = c("positive", "negative", "neutral"))

# Create a word cloud with sentiment colors
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          scale = c(2, 0.5),
          colors = c("darkgreen", "darkred", "orange"))

# Assuming you have a data frame 'review_sentiment1' with a column 'text_df1' containing your reviews
# Make sure 'review1' is a character vector of reviews

library(tidytext)
library(dplyr)

tokenized_data <- review_sentiment1 %>%
  unnest_tokens(word, text_df1)

dtm <- tokenized_data %>%
  count(text_df1, word) %>%
  cast_dtm(document = text_df1, term = word, value = n)

