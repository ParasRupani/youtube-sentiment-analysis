# Sentiment Analysis of YouTube Video Comments
                          
This project involves performing sentiment analysis on the comments of a YouTube video using R.

The goal is to gain insights into the overall sentiment expressed by viewers towards the video content.

## Video Source
https://www.youtube.com/watch?v=QKplGhhF1E4  

## Data Collection:
Get access and save the video transcript.

## Data Pre-Processing:
1. Clean the comment text by removing URLs, special characters, and irrelevant information.
2. Perform text normalization techniques, such as converting text to lowercase, removing stopwords, and stemming/lemmatizing words.

## Sentiment Analysis:
1. Use the tidytext package to perform sentiment analysis on the cleaned comments.
2. Utilize pre-trained sentiment lexicons like the Bing lexicon or the NRC Word-Emotion Association Lexicon to assign sentiment scores to each comment.
3. Calculate the overall sentiment score for the video by aggregating the individual comment sentiment scores.

## Data Visualization:
1. Create a bar plot or histogram to visualize the distribution of sentiment scores across comments.
2. Generate a word cloud to display the most frequently used positive and negative words in the comments.
3. Plot the sentiment scores over time to identify trends or patterns in viewer sentiment as the video ages.