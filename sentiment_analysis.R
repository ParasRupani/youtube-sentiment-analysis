##################################
###    Name: Paras Rupani      ###
###    ID  : 8961758           ###
##################################

# Clear all plots
if(!is.null(dev.list())) dev.off()
# Clear entire console
cat("\014") 
# Clean and clear the workspace
rm(list=ls())


#########################################
###        Required Packages          ###
#########################################

if (!requireNamespace("tuber", quietly = TRUE)) {
  install.packages("tuber")
};library(tuber)

if (!requireNamespace("syuzhet", quietly = TRUE)) {
  install.packages("syuzhet")
};library(syuzhet)

if (!requireNamespace("tidytext", quietly = TRUE)) {
  install.packages("tidytext")
};library(tidytext)

if (!requireNamespace("textdata", quietly = TRUE)) {
  install.packages("textdata")
};library(textdata)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
};library(dplyr)

if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
};library(tidyr)

if (!requireNamespace("wordcloud", quietly = TRUE)) {
  install.packages("wordcloud")
};library(wordcloud)

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
};library(stringr)

if (!requireNamespace("SnowballC", quietly = TRUE)) {
  install.packages("SnowballC")
};library(SnowballC)

if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
};library(tm)

if (!requireNamespace("kableExtra", quietly = TRUE)) {
  install.packages("kableExtra")
};library(kableExtra)

if (!requireNamespace("textTinyR", quietly = TRUE)) {
  install.packages("textTinyR")
};library(textTinyR)

if (!requireNamespace("textstem", quietly = TRUE)) {
  install.packages("textstem")
};library(textstem)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
};library(ggplot2)

#########################################
###        Reading the Dataset        ###
#########################################

# Setting the working directory:
setwd("D:/Desktop_Files/Conestoga/David/bonus")

# Reading the Dataset:
data <- read.csv('transcript_clean.csv', header=FALSE)

# Converting into a dataframe
df <- data.frame(data)

#########################################
###       DataFrame Formatting        ###
#########################################

# Assign row numbers as comment IDs
df$comment_id <- seq_len(nrow(df))

# Rename columns and reorder them
colnames(df) <- c("time", "comment_text", "comment_id")
df <- df[, c("comment_id", "time", "comment_text")]

# Visualising the dataset:
kable(head(df), format = "html", caption = "Summary") %>%
  kable_styling(full_width = FALSE)


#######################################
###       Data Preprocessing        ###
#######################################

# Clean the comment text by removing URLs, special characters, and irrelevant information
clean_text <- function(text) {
  # Remove URLs
  text <- gsub("http\\S+|www\\S+|https\\S+", "", text)
  # Remove special characters and punctuation
  text <- gsub("[[:punct:]]", " ", text)
  # Remove extra white spaces
  text <- str_trim(text)
  return(text)
}

# Apply the cleaning function to the comment text column
df$comment_text <- sapply(df$comment_text, clean_text)

# Perform text normalization techniques
normalize_text <- function(text) {
  # Convert text to lowercase
  text <- tolower(text)
  # Remove stopwords
  text <- removeWords(text, stopwords("english"))
  # Lemmatizing
  # text <- wordStem(text)
  text <- lemmatize_strings(text)
  return(text)
}

# Apply text normalization function to the cleaned_text column
df$comment_text <- sapply(df$comment_text, normalize_text)

# View the pre-processed data
tidy_comments<-df

# Visulaising a few instances of the processed dataset:
kable(head(df), format = "html", caption = "Summary") %>%
  kable_styling(full_width = FALSE)


#######################################
###       Sentiment Analysis        ###
#######################################

data("sentiments")

comment_sentiment <- df %>%
  unnest_tokens(word, comment_text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(comment_id) %>%
  summarize(sentiment_score = sum(value)) %>%
  mutate(sentiment = ifelse(sentiment_score > 0, "Positive",
                            ifelse(sentiment_score < 0, "Negative", "Neutral")))
# Calculate percentage of each sentiment type
comment_sentiment <- comment_sentiment %>%
  group_by(sentiment) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Assign numerical values to sentiment categories
sentiment_values <- c("Positive" = 1, "Neutral" = 0, "Negative" = -1)

# Calculate overall sentiment score
overall_sentiment_score <- sum(comment_sentiment$percentage / 100 * sentiment_values[comment_sentiment$sentiment])

# Determine overall sentiment
overall_sentiment <- ifelse(overall_sentiment_score > 0, "Positive",
                            ifelse(overall_sentiment_score < 0, "Negative", "Neutral"))

# Print the overall sentiment
cat("Overall sentiment:", overall_sentiment, "with score:", overall_sentiment_score)


#############################################
####         Data Visualisations         ####
#############################################
###   Bar Plot Sentiment Classification   ###
#############################################

# Plot the sentiment scores
barplot(comment_sentiment$percentage,
        names.arg = comment_sentiment$sentiment,
        col = rainbow(nrow(comment_sentiment)),
        ylab = "Percentage",
        main = "Sentiment scores of YouTube comments (using AFINN lexicon)")


######################################
###     Detailed Classification    ###
######################################

# Compute sentiment scores using the NRC sentiment lexicon
nrc_sentiments <- get_nrc_sentiment(df$comment_text)

# Calculate percentages
percentages <- 100 * colSums(nrc_sentiments) / sum(nrc_sentiments)

# Plotting bar plot of detailed sentiment scores
barplot(percentages, las = 2, ylab = "Percentage", main = "Detailed Sentiment Scores", col = "skyblue", ylim = c(0, 100))



#########################################
##     Sentiment Analysis by word      ##
#########################################

df_sentiment <- df %>%
  mutate(comment_text = as.character(comment_text)) %>%
  unnest_tokens(word, comment_text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word, sentiment = ifelse(value > 0, "Positive", ifelse(value < 0, "Negative", "Neutral"))) %>%
  summarize(sentiment_score = sum(value))

# Separate positive and negative sentiments
positive_words <- df_sentiment %>%
  filter(sentiment == "Positive") %>%
  select(word) %>%
  ungroup()

negative_words <- df_sentiment %>%
  filter(sentiment == "Negative") %>%
  select(word) %>%
  ungroup()

# Create word frequency tables for positive and negative sentiments
positive_freq <- positive_words %>%
  count(word) %>%
  arrange(desc(n))

negative_freq <- negative_words %>%
  count(word) %>%
  arrange(desc(n))


####################################
###   Visulising Word Cloud      ###
####################################

# Plot word clouds for positive and negative sentiments
{wordcloud(positive_freq$word, positive_freq$n, scale=c(0.8,1),
           main="Word Cloud for Positive Sentiments")
  title(main = "Word Cloud for Positive Sentiments", cex.main = 1.5)}

{wordcloud(negative_freq$word, negative_freq$n, scale=c(0.8,1),
           main="Word Cloud for Negative Sentiments")
  title(main = "Word Cloud for Negative Sentiments", cex.main = 1.5)}


# Convert comment_text to character if it's not already
# df$comment_text <- as.character(df$comment_text)

###########################################
###   Visualising the Sentiment across   ###
###   Entire time frame of the video.   ###
###########################################

# Convert time column to a time object in df
df$time <- as.POSIXct(strptime(df$time, format = "%M:%S"))

# Tokenize the comment_text and calculate sentiment scores for each comment
df_sentiment <- df %>%
  mutate(comment_text = as.character(comment_text)) %>%
  unnest_tokens(word, comment_text) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(comment_id, time) %>%
  summarize(sentiment_score = sum(value))

# Plotting sentiment scores across time
ggplot(df_sentiment, aes(x = time, y = sentiment_score)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Sentiments Across Time", x = "Time", y = "Sentiment Score") +
  scale_x_time(labels = function(x) strftime(x, format = "%M:%S")) + # Format x-axis labels
  theme_minimal()


##################################################################################################
##################################################################################################
##################################################################################################
