library(shiny)
library(readr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(ggpubr)
library(sp)
library(xts)
library(leaflet)
library(ggthemes)
library(ggridges)
library(tidyverse)
library(stringi)
library(ggpubr)
library(tm)
library(quanteda)
library(png)
library(utils)

# business <- stream_in(file("business.json"))
# business <- jsonlite::flatten(business)
# business_tbl <- as_data_frame(business)
# saveRDS(business_tbl, "business_tbl.RDS")
business_tbl <- readRDS("business_tbl.RDS")

restaurants.types <- business_tbl %>% 
  filter(city == "Las Vegas") %>%
  select(-starts_with("hours")) %>%
  select(business_id, name, neighborhood, address, latitude, longitude, stars, review_count,     attributes.RestaurantsPriceRange2, categories) %>%
  filter(str_detect(categories, "Restaurant")) %>%
  mutate(categories2 = as.character(categories)) %>%
  unnest(categories) %>%
  group_by(categories) %>%
  mutate(n = n()) %>%
  arrange(-n) %>%
  filter(n > 20)



restaurants.types$categories[restaurants.types$categories == "American(New)"|
                               restaurants.types$categories == "American(Traditional)"|
                               restaurants.types$categories == "Hot Dogs"| 
                               restaurants.types$categories == "Diners" | 
                               restaurants.types$categories == "Steakhouses"|
                               restaurants.types$categories == "Cajun/Creole"|
                               restaurants.types$categories == "Barbeque" |
                               restaurants.types$categories == "Burgers" |
                               restaurants.types$categories == "Southern" |
                               restaurants.types$categories == "Cheesesteaks"|
                               restaurants.types$categories == "Chicken Wings"|
                               restaurants.types$categories == "Hawaiian" |
                               restaurants.types$categories == "Soul Food"|
                               restaurants.types$categories == "Delis"] <- "American"

restaurants.types$categories[restaurants.types$categories == "Bars"|
                               restaurants.types$categories == "Beer Bar" | 
                               restaurants.types$categories == "Breweries"  |
                               restaurants.types$categories == "Dive Bars" |
                               restaurants.types$categories == "Cocktail Bars" |
                               restaurants.types$categories == "Music Venues" |
                               restaurants.types$categories == "Nightlife" |
                               restaurants.types$categories == "Sports Bars" |
                               restaurants.types$categories == "Wine Bars" |
                               restaurants.types$categories == "Pubs" |
                               restaurants.types$categories == "Gastropubs" |
                               restaurants.types$categories == "Lounges" |
                               restaurants.types$categories == "Karaoke" |
                               restaurants.types$categories ==   "Dance Clubs"] <- "Bars/Nightlife"

restaurants.types$categories[restaurants.types$categories == "Thai"| 
                               restaurants.types$categories == "Japanese" | 
                               restaurants.types$categories == "Korean"  |
                               restaurants.types$categories == "Indian" |
                               restaurants.types$categories == "Filipino" |
                               restaurants.types$categories == "Dim Sum" |
                               restaurants.types$categories == "Chinese" |
                               restaurants.types$categories == "Cantonese" |
                               restaurants.types$categories == "Sushi Bars" |
                               restaurants.types$categories == "Taiwanese" |
                               restaurants.types$categories == "Vietnamese" |
                               restaurants.types$categories == "Noodles" |
                               restaurants.types$categories == "Poke" |
                               restaurants.types$categories == "Ramen" |
                               restaurants.types$categories == "Szechuan" |
                               restaurants.types$categories ==   "Asian Fusion"    
                             ] <- "Asian" 

restaurants.types$categories[restaurants.types$categories == "Mexican"| 
                               restaurants.types$categories == "Latin American" | 
                               restaurants.types$categories == "Tex-Mex"  |
                               restaurants.types$categories == "Tapas Bars" |
                               restaurants.types$categories == "Tapas/Small Plate" |
                               restaurants.types$categories ==  "Tacos" |
                               restaurants.types$categories ==  "Caribbean" 
                             ] <- "Latino"    

restaurants.types$categories[restaurants.types$categories == "Persian/Iranian"| 
                               restaurants.types$categories == "Halal" | 
                               restaurants.types$categories == "Middle Eastern" ] <- "Middle Eastern"    

restaurants.types$categories[restaurants.types$categories ==  "Fast Food"| 
                               restaurants.types$categories == "Food Trucks"  | 
                               restaurants.types$categories == "Food Court" |
                               restaurants.types$categories == "Food Stands"|
                               restaurants.types$categories == "Street Vendors"
                             ] <- "Fast Food"

restaurants.types$categories[restaurants.types$categories == "Cafes"| 
                               restaurants.types$categories == "Bubble Tea" | 
                               restaurants.types$categories == "Bakeries" | 
                               restaurants.types$categories == "Breakfast & Brunch" |
                               restaurants.types$categories ==  "Bagels" | 
                               restaurants.types$categories ==  "Juice Bars & Smoothies" |
                               restaurants.types$categories ==  "Coffee & Tea"  |
                               restaurants.types$categories ==  "Ice Cream & Frozen Yogurt"
                             ] <- "Café/Coffee"

restaurants.types$categories[restaurants.types$categories ==  "Pizza"| 
                               restaurants.types$categories ==  "Italian" 
                             ] <- "Italian"

restaurants.types$categories[restaurants.types$categories ==  "French"| 
                               restaurants.types$categories ==   "Mediterranean" | 
                               restaurants.types$categories == "Spanish" |
                               restaurants.types$categories ==   "Greek" 
                             ] <- "European"

restaurants.types$categories[restaurants.types$categories == "Gluten-Free"| 
                               restaurants.types$categories == "Vegetarian" | 
                               restaurants.types$categories == "Vegan" ] <- "Vegan/Vegetarian"



# We kept only the observations that fall into our selected categories. And dropped the dupliucated cases (created by renaming the categories.)
restaurants <- restaurants.types %>% 
  filter(categories %in% c("American",  "Asian","Bars/Nightlife","Café/Coffee", "Casinos","European","Fast Food", "Italian",  "Latino","Vegan/Vegetarian")) 

restaurants <-  restaurants[!duplicated(restaurants[c(1, 11)]), ] 



restaurants$attributes.RestaurantsPriceRange2[which(restaurants$attributes.RestaurantsPriceRange2 == "1")] <- "$"
restaurants$attributes.RestaurantsPriceRange2[which(restaurants$attributes.RestaurantsPriceRange2 == "2")] <- "$$"
restaurants$attributes.RestaurantsPriceRange2[which(restaurants$attributes.RestaurantsPriceRange2 == "3")] <- "$$$"
restaurants$attributes.RestaurantsPriceRange2[which(restaurants$attributes.RestaurantsPriceRange2 == "4")] <- "$$$$"
restaurants$stars1 <- restaurants$stars
restaurants$stars1[which(restaurants$stars == 1.5)] <- 1
restaurants$stars1[which(restaurants$stars == 2.5)] <- 2
restaurants$stars1[which(restaurants$stars == 3.5)] <- 3
restaurants$stars1[which(restaurants$stars == 4.5)] <- 4
as.character(restaurants$stars1)
#clean
restaurants$address = NULL
restaurants$review_count = NULL
restaurants$n = NULL
restaurants$categories2 =NULL

restaurants1 <- restaurants %>% filter(categories!= "Casinos")

#casino
casino <- restaurants %>%
  filter(categories == "Casinos")


cat <- c("All","American", "Asian","Bars/Nightlife","Café/Coffee","European","Fast Food", "Italian",  "Latino","Vegan/Vegetarian")
price <- c("All","$","$$","$$$","$$$$")
star <- c("All",1, 2, 3, 4, 5)

#Jen's part, but fixed by Haowen
yelpReviews <- read_csv("yelpReviews.csv")

clean_corpus <- function(corpus){     #use the function introduced in Lecture08
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"),"food"))  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

ReviewSample <- yelpReviews %>% 
  select(business_id, text, stars) %>% 
  arrange(desc(stars)) %>% 
  sample_n(1000)

sample.corpus <- Corpus(VectorSource(ReviewSample$text))
sample.cleaned.corpus <- clean_corpus(sample.corpus)

set.seed(44)
wordcloud(sample.cleaned.corpus, max.words = 50, scale=c(3,.5), random.order=F, 
          colors= c('#9ecae1','#6baed6','#3182bd','#08519c')) 
text(x=0.5, y=0.95, "Most Frequently Used Words in Yelp Reviews")


####Haowen's part

business2 <- readRDS("restaurant.RDS")
business3 <- readRDS("res1.RDS") #this is the dataset with price range as numbers (int)

#For all categories
categories <- group_by(business2, categories) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(order = row_number()) %>%
  filter(categories != "Casinos")


g1 <- ggplot(categories, aes(-order, n)) +
  geom_bar(stat = "identity", show.legend = F, fill = '#bdbdbd') +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = -categories$order, labels =categories$categories, expand = c(0,0)) +
  xlab("") + 
  ylab("Number of restaurants") +
  ggtitle("Number of restaurants by categories in Las Vegas") +
  theme_tufte() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = rel(0.8)),
        axis.text.y = element_text(size=rel(1.0), hjust=0.6))

#latino vs. Italian

business4 <- business3 %>% filter(!is.na(attributes.RestaurantsPriceRange2)) %>% 
  filter(categories == "Latino" | categories == "Italian") 

business4$categories3 <- factor(business4$categories, levels = c("Latino", "Italian"))  

g2 <-  ggplot(business4, aes(attributes.RestaurantsPriceRange2, categories3)) +
  geom_density_ridges(alpha = .7) +
  scale_x_continuous(labels = c("$", "$$", "$$$", "$$$$"), breaks = seq(1, 4, by = 1),
                     expand = c(0.01, 0)) +
  scale_y_discrete(limit = c("Italian", "Latino"),expand = c(0.01, 0)) +
  labs(x = "Price Range",
       y = "",
       title = "Distribution of Latino vs. Italian Food on the Price Range",
       caption = "Customer Reviews | Source: Yelp") +
  theme_ridges(grid = FALSE)


#use graph 3
business5 <-  business3 %>% filter(!is.na(attributes.RestaurantsPriceRange2)) %>% filter(categories != "Casinos")
business5$categories3 <- factor(business5$categories)  

# g3 <-  ggplot(business5, aes(attributes.RestaurantsPriceRange2, categories3, fill=categories3)) +
  # geom_density_ridges(alpha = .7) +
  # scale_x_continuous(labels = c("$", "$$", "$$$", "$$$$"), breaks = seq(1, 4, by = 1),
                     # expand = c(0.01, 0)) +
  # scale_y_discrete(expand = c(0.01, 0)) +
  # scale_fill_manual(values = c("#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","#bdbdbd","orange", "blue","#bdbdbd")) +
  # labs(x = "Price Range",
       # y = "",
       # title = "Distribution of Different Kinds of Food on Price Range",
       # caption = "Customer Reviews | Source: Yelp") +
  # theme_ridges(grid = FALSE) +
  # guides(fill=FALSE)

g3 <- business5 %>% filter(categories == "Latino" | categories == "Italian") %>%
  ggplot(aes(attributes.RestaurantsPriceRange2, fill = categories)) +
  geom_bar(alpha = .7) +
  facet_grid(. ~ categories) +
  scale_x_continuous(labels = c("$", "$$", "$$$", "$$$$"), breaks = seq(1, 4, by = 1),
                     expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = "Price Range",
       y = "",
       title = "Distribution of Latino and Italian Food on Price Range") +
  scale_fill_manual(values = c("orange", "blue")) +
  theme_ridges(grid = FALSE) +
  theme(text = element_text(size=15)) + 
  guides(fill=FALSE)

#reviews. compare Italian vs. Latino. 
review <- read.csv("yelpReviews.csv")

review1 <- filter(review, business_id %in% business4$business_id)
review.merged <- left_join(review1, business4, by = "business_id")

pyr_m <- read.table("pyr_m.txt",header=TRUE,row.names=1)


set.seed(44)
commonality.cloud(pyr_m, max.words = 60, random.order=FALSE, colors = c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"))
text(x=0.1, y=1.0, "Common Words in Latino and Italian Food Reviews")
#make a comparison wordcloud for latino and italian food.
colnames(pyr_m) = c("Italian", "Latino")

set.seed(44)
comparison.cloud(pyr_m, colors = c("orange", "blue"), 
                 scale=c(0.2,1.5), title.size= 1, 
                 max.words = 50)

#Make a comparison graph for sentiment analysis. 
review.merged2 <- left_join(review1, business5, by = "business_id")
review.merged2$text <- as.character(review.merged2$text)

latino.sa <- review.merged2 %>% filter(categories == "Latino") 
Italian.sa <- review.merged2 %>% filter(categories == "Italian") 

sentimentScore1 <- latino.sa %>% #used Jen's code for sentiment analysis
  unnest_tokens(word, text) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            sentimentScores = sum(score))

sentimentScore2 <- Italian.sa %>%
  unnest_tokens(word, text) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            sentimentScores = sum(score))

g.s1 <-   sentimentScore1 %>%
  top_n(20, abs(sentimentScores)) %>%
  mutate(word = reorder(word, sentimentScores)) %>%
  head(20) %>%
  ggplot(aes(word, sentimentScores, fill = sentimentScores > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y='Sentiment Score for Latino food', x='') +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#b2182b", "#2166ac")) +
  coord_flip() + theme_bw()

g.s2 <-   sentimentScore2 %>%
  top_n(20, abs(sentimentScores)) %>%
  mutate(word = reorder(word, sentimentScores)) %>%
  head(20) %>%
  ggplot(aes(word, sentimentScores, fill = sentimentScores > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y='Sentiment Score for Italian food', x='') +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#b2182b", "#2166ac")) +
  coord_flip() + theme_bw()

#Here is to get a sense what kinds of sentimental words are used in the reviews. 
g.sentiment <- ggarrange(g.s1, g.s2, 
                         ncol = 2, nrow = 1)

#Let's see if the tones differ between the two kinds of food! I'm using the Hu & Liu dictionary

dict_dir <- "dictionaries" #These codes were taken from Lab09
positiveHuLiu <- readLines(paste(dict_dir, "positive-words.txt", sep="/"))
positiveHuLiu <- stri_enc_toutf8(positiveHuLiu, is_unknown_8bit = T) 

negativeHuLiu <- readLines(paste(dict_dir, "negative-words.txt", sep="/"))
negativeHuLiu <- stri_enc_toutf8(negativeHuLiu, is_unknown_8bit = T) 

dictHuLiu <- dictionary(list(positive= positiveHuLiu, negative=negativeHuLiu)) #Define the dictionary 

latino.italian <- review.merged2 %>% filter(categories == "Latino" | categories == "Italian") 
corpus <- Corpus(VectorSource(latino.italian$text))
corpus <- corpus(corpus)
sentiment_hl <- dfm(corpus, dictionary = dictHuLiu, tolower = T) 
sentiment.df <- data.frame(sentiment_hl)

sentiment.df$tone <- (sentiment.df$positive - sentiment.df$negative)/(sentiment.df$positive + sentiment.df$negative)
sentiment.df$row <- seq.int(nrow(sentiment.df))
latino.italian$row <- seq.int(nrow(latino.italian))

sa.merge <- left_join(sentiment.df, latino.italian, by = "row") %>%
  filter(!is.na(tone)) #get rid of the missing values

tone.analysis <- 
  ggplot(sa.merge, aes(tone, categories, col = categories)) +
  geom_jitter(alpha = 0.5) + 
  geom_smooth() +
  theme_bw() +
  scale_color_manual(values =c("orange", "blue")) +
  ggtitle("Any differences in tone between Latino and Italian food?", subtitle = "I do not see any!")


#badgraph <- readPNG("badgraph.png")
#bg <- base64enc::dataURI(file="myfile.png", mime="image/png")
