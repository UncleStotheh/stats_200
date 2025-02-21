setwd("C:/Users/Sam/Downloads/Stats_200")
install.packages("devtools")
devtools::install_github('charlie86/spotifyr')
library(devtools)
library(tidyverse)
library(stringr)
library(httr)
library(lubridate)
library(assertthat)
library(rvest)
library(spotifyr)
library(radarchart)
library(tidytext)
library(kableExtra)

#Credit: this script is adapted from https://medium.com/@simranvatsa5/taylor-f656e2a09cc3 and https://github.com/davidklaing/kendrick/blob/master/src/scrape_kendrick.R, who adapted it from http://rcharlie.com/2017-02-16-fitteR-happieR/

spotify_client_id <- '169a6a2cc5bb45cdb6d82f091e51c948'
spotify_client_secret <- '5e99928f3bd4416f909e443afdfeaea6'
token <- '0TAz58C60jZ1sGo7KSaADeTiLqhGOav41J5IEw4eY0REMQcij28QPxEgzvT6vfeR'

#set access token as environmental varibables
Sys.setenv(SPOTIFY_CLIENT_ID = '169a6a2cc5bb45cdb6d82f091e51c948')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '5e99928f3bd4416f909e443afdfeaea6')

eminem <- get_artist_audio_features('eminem')

#filter eminem dataset with music info for only the kamikaze album
eminem <- eminem %>% filter(album_name == "Kamikaze")

#make function to get artist lyrics from genius website
genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('eminem')


# Getting track urls
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

# Filtering to get lyric urls only for tracks in which Eminem is the primary artist
filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Eminem") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
    
    index <- append(index, i)
  }
}

eminem_lyrics <- data.frame(filtered_track_lyric_urls, filtered_track_lyric_titles)
eminem_lyrics <- eminem_lyrics[filtered_track_lyric_titles %in% eminem$track_name, ]

eminem_lyrics$filtered_track_lyric_urls <- as.character(eminem_lyrics$filtered_track_lyric_urls)
eminem_lyrics$filtered_track_lyric_titles <- as.character(eminem_lyrics$filtered_track_lyric_titles)

#the above code for getting titles and urls failed (only got 6 out of 13 songs in album)
#must add the remaining songs manually since code only downloaded a part of the album
eminem_lyrics <- read.csv("SH_STATS_Lyrics.csv", header = T)
#done

#pulling lyrics from urls
lyric_text <- rep(NA, 13)
for (i in 1:13) {
  lyric_text[i] <- html_session(as.character(eminem_lyrics$filtered_track_lyric_urls[i])) %>% 
    html_nodes(".lyrics p") %>% 
    html_text()
}

# Cleaning lyrics to get rid of symbols or wrong punctation
for (i in 1:13) {
  lyric_text[i] <- gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text[i])
  lyric_text[i] <- gsub("\n", " ", lyric_text[i])
  lyric_text[i] <- gsub("\\[.*?\\]", " ", lyric_text[i])
  lyric_text[i] <- tolower(lyric_text[i])
  lyric_text[i] <- gsub("[ [:punct:] ]", " ", lyric_text[i])
  lyric_text[i] <- gsub(" {2,}", " ", lyric_text[i])
}
#making a dataframe with lyrics in it
genius_data <- data.frame(track_name = eminem_lyrics$filtered_track_lyric_titles, lyrics = lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)

# joining Spotify and Genius data
spotify_genius <- full_join(genius_data, eminem, by = "track_name")

#add character count of each set of lyrics
spotify_genius$song_char_count <- nchar(spotify_genius$lyrics)
#add word count
word_count <- c()
for (i in 1:length(spotify_genius$lyrics)) {
  word_count <- append(word_count, length(strsplit(spotify_genius$lyrics[i],' ')[[1]]))
}

spotify_genius$song_word_count <- word_count


filtered_track_pageviews <- c()
index <- c()
for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Eminem") {
    if (!is.null(track_lyric_urls[[i]]$stats$pageviews)) {
      filtered_track_pageviews <- append(filtered_track_pageviews, track_lyric_urls[[i]]$stats$pageviews)
    } else {
      filtered_track_pageviews <- append(filtered_track_pageviews, NA)
    }
    index <- append(index, i)
  }
}
track_pageviews <- filtered_track_pageviews[filtered_track_lyric_urls %in% eminem_lyrics$filtered_track_lyric_urls]
#determine which lyric is missing
intersect(filtered_track_lyric_urls, eminem_lyrics$filtered_track_lyric_urls)
#add NA for missing pageview data in paul skit
track_pageviews <- append(track_pageviews, NA, after = 4)
#add page views to data
spotify_genius$track_pageviews <- track_pageviews

# Save it all to csv.
#write.csv(spotify_genius, "./scraped_eminem_data_SH.csv")


#get summary stats and build regression 
#fitting model to regress popularity against track page view and tempo
#all continuous variables
fit_eminem <- lm(spotify_genius$track_popularity ~ log(spotify_genius$track_pageviews) + spotify_genius$tempo)
summary(fit_eminem)


equation1=function(x){coef(fit_eminem)[2]*x+coef(fit_eminem)[1]}
equation2=function(x){coef(fit_eminem)[2]*x+coef(fit_eminem)[1]+coef(fit_eminem)[3]}

equation1=function(x){coef(fit_eminem)[2]*x+coef(fit_eminem)[1]}
equation2=function(x){coef(fit_eminem)[2]*x+coef(fit_eminem)[1]+coef(fit_eminem)[3]}

ggplot(spotify_genius,aes(y= track_popularity,x= track_pageviews, colour = tempo))+geom_point() + scale_color_distiller(palette = "RdPu", name = "Tempo (BPM)") + xlab("Song Page View Count") + ylab("Song Popularity") + geom_text(aes(label=spotify_genius$track_name), hjust = 0.35, vjust = 0)
