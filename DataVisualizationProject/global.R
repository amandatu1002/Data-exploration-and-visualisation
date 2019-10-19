# global.R

library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(readr)
library(lubridate)
library(scales)
library(tm)
library(tmap)
library(RColorBrewer)
library(wordcloud)
library(memoise)
library(text2vec)
options(scipen=999)
#install.packages("text2vec")



#### Data Wrangling ####
## create dataframe from listing.csv ## 
listingdf <- read.csv('listings2.csv')
listdf <- listingdf %>%
  select(id, name, host_id, host_name, neighbourhood_cleansed, zipcode, latitude, longitude, property_type, room_type, price, review_scores_rating, number_of_reviews)

# eliminate all rows with at least one NA in any column
listcleandf <- listdf[complete.cases(listdf), ] 

listcleandf$price <- as.numeric(gsub(",", "", substring(listcleandf$price, 2)))

# variables
area <- as.character(unique(listcleandf$neighbourhood_cleansed))
room <- c("Entire home/apt", "Private room", "Shared room")
groupColors <- colorFactor(c("#E03A3C", "#009DDC","#62BB47"),
                           domain = c("Entire home/apt", "Private room","Shared room"))

## create dataframe from calendar.csv ##
calendar2017 <- read_csv("calendar2017.csv")
calendar2018 <- read_csv("calendar2018.csv")
calendar2019 <- read_csv("calendar2019.csv")
calendar2019$adjusted_price <- NULL
calendar2019$minimum_nights <- NULL
calendar2019$maximum_nights <- NULL

combinedCalendar <- rbind(calendar2017, calendar2018, calendar2019)
combinedCalendar$price <- as.numeric(gsub(",", "", substring(combinedCalendar$price, 2)))

groupedCalendarAll <- combinedCalendar %>% 
  group_by(date = date) %>% 
  summarise(avg_priceday = mean(price, na.rm = TRUE)) %>%
  mutate(year = year(date), month=month(date))

groupedCalendarAll$year <- as.factor(as.character(groupedCalendarAll$year))

groupedCalendarYear <- groupedCalendarAll %>%
  group_by(year) %>%
  summarise(avg_pricemon = mean(avg_priceday, na.rm = TRUE))

groupedCalendarMon <- groupedCalendarAll %>% group_by(month = month) %>% 
  summarise(month_avg = mean(avg_priceday, na.rm = TRUE))



 ## create wordcloudvector from reviews.csv ##
 #http://text2vec.org/glove.html
#reviews <- read_csv("reviews.csv")
 # GloVe Word Embeddings

#library(text2vec)
 # Create iterator over tokens
#tokens <- space_tokenizer(as.character(reviews$comments))
 # Create vocabulary. Terms will be unigrams (simple words)
#it = itoken(tokens, progressbar = FALSE)
#vocab <- create_vocabulary(it)
 # construct term-co-occurence matrix (TCM)
 # Use our filtered vocabulary
#vectorizer <- vocab_vectorizer(vocab)
 # use window of 5 for context words
#tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
 # Now we have a TCM matrix and can factorize it via the GloVe algorithm
#glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
#word_vectors = glove$fit_transform(tcm, n_iter = 20)

#save(word_vectors, file = "word_vectors.RData")
load("word_vectors.RData")

getTermMatrix <- memoise(function(book) {
  
  p1 = word_vectors[book, , drop = FALSE] 
  cos_sim = sim2(x = word_vectors, y = p1, method = "cosine", norm = "l2")
  p1 = sort(cos_sim[,1], decreasing = TRUE)
  
  df = data.frame(item = as.character(names(p1)),freq = as.numeric(p1))
  df$item = gsub(",","",df$item)
  df = df[!duplicated(df$item), ]
  
})



 ## create wordcloud from reviews.csv ##
#reviews <- read_csv("reviews.csv")

#splitreviewscoloumn <- unlist(strsplit(as.character(reviews$comments), split=" "))
#reviewsDF <- data.frame("textword" = splitreviewscoloumn)
#wordDF <- reviewsDF %>% count(textword, sort = TRUE) %>% 
#  ungroup()

#corpus <- Corpus(VectorSource(splitreviewscoloumn), readerControl = list(language="en")) 
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, content_transformer(tolower))
#corpus = tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeWords, c(stopwords("SMART"),"stay", "place", "us"))

#newcorpusdf <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)
#newcorpusdffiltered <- newcorpusdf %>% filter(text != "")
#wordDF <- newcorpusdffiltered %>% count(text, sort = TRUE) %>% 
#  ungroup()

#save(wordDF, file = "wordDF.RData")
load("wordDF.RData")
