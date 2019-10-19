## title: Data exploration and visualization of Airbnb in Melbourne
## date: 4/26/2019
## resource: http://insideairbnb.com/get-the-data.html

library(readr)
library(dplyr)
library(ggplot2)
options(scipen=999)
reviews <- read_csv("reviews.csv")


###Spatial Data Analysis ###
library(leaflet)
listingdf <- read.csv('listings.csv')

#Airbnb Listings in Melbourne
leaflet(listingdf) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude,labelOptions = labelOptions(noHide = F),clusterOptions = markerClusterOptions()
             ,popup = paste0("<b> Name: </b>", listingdf$name , "<br/><b> Host Name: </b>"
                             , listingdf$host_name, "<br> <b> Price: </b>"
                             , listingdf$price, "<br/><b> Room Type: </b>"
                             , listingdf$room_type, "<br/><b> Property Type: </b>"
                             , listingdf$property_type
  )) %>% 
  setView(144.96, -37.81, zoom = 10) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap)


### Demand and Price Analysis ###
library(lubridate)
library(scales)

### The Explosive Growth of Airbnb in Melbourne

reviewsNum <- reviews %>% group_by(date = reviews$date) %>% summarise(number = n())

ggplot(reviewsNum, aes(date, number)) +
  geom_point(na.rm=TRUE, color = "grey35", alpha=0.5) +geom_smooth(color = "darkred") + 
  ggtitle("The Explosive Growth of Airbnb in Melbourne",
          subtitle = "Number of reviews across years") +
  labs(x = "Year", y = "Reviews/ Listing") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme_grey()


# .	How demand changes over the year by month?

demand20172018 <- ggplot(reviewsNum[year(reviewsNum$date) >= 2017 & year(reviewsNum$date) <2019,], aes(date, number)) +
  geom_point(na.rm=TRUE, color = "steelblue") +geom_smooth(color = "darkred")+
  ggtitle("Demand by Season",
          subtitle = "Number of reviews across Months in 2017-2018") +
  labs(x = "Month", y = "Reviews/ Listing") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme_grey()


demand20172018


# .	How price changes over the year?

calendar2017 <- read_csv("calendar2017.csv")
calendar2018 <- read_csv("calendar2018.csv")
calendar2019 <- read_csv("calendar2019.csv")
calendar2019$adjusted_price <- NULL
calendar2019$minimum_nights <- NULL
calendar2019$maximum_nights <- NULL

combinedCalendar <- rbind(calendar2017, calendar2018, calendar2019)
combinedCalendar$price <- as.numeric(gsub(",", "", substring(combinedCalendar$price, 2)))

groupedCalendarAll <- combinedCalendar %>% group_by(date = date) %>% 
  summarise(averagePrice = mean(price, na.rm = TRUE)) %>% 
  mutate(year = year(date), commonYear = paste("2017",substring(date, 6),sep="-"))

groupedCalendarAll$year <- as.factor(as.character(groupedCalendarAll$year))
groupedCalendarAll$commonYear <- ymd(groupedCalendarAll$commonYear)

ggplot(groupedCalendarAll[year(groupedCalendarAll$date) >= 2017 & year(groupedCalendarAll$date) < 2019,], 
       aes(commonYear, averagePrice)) +
       geom_point(na.rm=TRUE, alpha=0.5, color = "steelblue") + geom_smooth(color = "darkred")+ facet_grid(~year)+
       ggtitle("Price by Season", subtitle = "Average listing price across Months") +
       labs(x = "Month", y = "Average price/listing") +
       theme(plot.title = element_text(face = "bold")) +
       theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
       theme_grey() + scale_x_date(labels = date_format("%b")) 


# .	Does the price rise on weekends?

# a box plot of average prices by day of the week to understand this phenomenon
groupedCalendarAll <- groupedCalendarAll %>% mutate(day = strftime(date,'%A'))
groupedCalendarAll$day <- factor(groupedCalendarAll$day
                                 , levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
                                 , labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(groupedCalendarAll, aes(x = factor(day), y = averagePrice)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.5, width = 0.1, color = "steelblue") +
  ggtitle("Price by day of the Week", subtitle = "Boxplots of Price by Day of the Week") +
  labs(x = "Day of the week", y = "Average Price") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme_grey()


### Review Analysis Using Word Cloud ###

#.	What do Airbnb users care about?

splitreviewscoloumn <- unlist(strsplit(as.character(reviews$comments), split=" "))
reviewsDF <- data.frame("textword" = splitreviewscoloumn)
wordDF <- reviewsDF %>% count(textword, sort = TRUE) %>% 
  ungroup()


library(tm)
corpus <- Corpus(VectorSource(splitreviewscoloumn), readerControl = list(language="en")) 
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords("SMART"),"stay", "place", "us"))

newcorpusdf <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)
newcorpusdffiltered <- newcorpusdf %>% filter(text != "")
wordDF <- newcorpusdffiltered %>% count(text, sort = TRUE) %>% 
  ungroup()

library(RColorBrewer)
#install.packages("wordcloud")
library(wordcloud)
set.seed(789)
wordcloud(words = wordDF$text, 
          freq = wordDF$n,
          min.freq = 5000,
          max.words=300, random.order=FALSE,
          colors = c("#e06f69", "#59c6f3","#2a7d82", "#1f5560") )



###Comment Analysis Using Word Cloud###
#http://text2vec.org/glove.html

# GloVe Word Embeddings
#install.packages("text2vec")
library(text2vec)
# Create iterator over tokens
tokens <- space_tokenizer(as.character(reviews$comments))
# Create vocabulary. Terms will be unigrams (simple words)
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
# construct term-co-occurence matrix (TCM)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
# Now we have a TCM matrix and can factorize it via the GloVe algorithm
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
word_vectors = glove$fit_transform(tcm, n_iter = 20)
word_vectors1 <- glove$components



####Building Word Vectors from Reviews
#"uncomfortable"
library(tidyverse)
library(tm)
library(wordcloud)
#install.packages("tmap",dependencies=TRUE)
library(tmap)
p1 = word_vectors["uncomfortable", , drop = FALSE] 
cos_sim = sim2(x = word_vectors, y = p1, method = "cosine", norm = "l2")
p1 = sort(cos_sim[,1], decreasing = TRUE)

df = data.frame(item = as.character(names(p1)),freq = as.numeric(p1))
df$item = gsub(",","",df$item)
df = df[!duplicated(df$item), ]

set.seed(1234)
wordcloud(words = df$item, freq = df$freq, scale = c(2,0.2),
          max.words=80, random.order=FALSE, rot.per=0.2,
          colors = c("#37db1a", "#e07014", "#159b1e","#700e2a"))



#"comfortable"
p2 = word_vectors["comfortable", , drop = FALSE] 
cos_sim2 = sim2(x = word_vectors, y = p2, method = "cosine", norm = "l2")
p2 = sort(cos_sim2[,1], decreasing = TRUE)

df2 = data.frame(item = as.character(names(p2)),freq = as.numeric(p2))
df2$item = gsub(",","",df2$item)
df2 = df2[!duplicated(df2$item), ]

set.seed(1234)
wordcloud(words = df2$item, freq = df2$freq, scale = c(2,0.2),
          max.words=80, random.order=FALSE, rot.per=0.2, 
          colors = c("#37db1a","#3783b2","#3783b2","#9e5007"))

