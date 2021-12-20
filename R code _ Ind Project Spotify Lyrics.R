#install.packages('tm')
#install.packages('textclean')
#install.packages('ggthemes')
#install.packages('ggpubr')
require(textclean)
require(tm)
require(dplyr)
require(stringr)
require(ggplot2)
library(ggthemes)
library(ggpubr)

df=read.csv('16k_songs_and_lyrics.csv')

nrow(df)


## 1. NLP PREPROCESSING LYRICS
#simple pipeline with traditional NLP preprocessing. 
#each LoC does an operation that is vital in the process of making the lyrics quantifiable

df = df %>% 
  rowwise( ) %>% 
  mutate(lyrics = gsub('\n', ' ', lyrics)) %>% 
  mutate(lyrics = tolower(lyrics)) %>% 
  mutate(lyrics = replace_contraction(lyrics, ignore.case = TRUE)    )%>% 
  mutate(lyrics = removePunctuation(lyrics))%>% 
  mutate(lyrics =   removeWords(x = lyrics, stopwords('en') )    )%>% 
  mutate(lyrics =   gsub('urlcopyembedcopy', '', lyrics) )%>% 
  mutate(lyrics =   gsub('embedshare', '', lyrics) )%>% 
  mutate(lyrics =   gsub('\\s+',' ', lyrics) ) 

df = df %>% 
  mutate(lyrics_list = str_split(lyrics, ' ') )

saveRDS(df, '16k_songs_and_lyrics_proc.rds')

df <- readRDS('16k_songs_and_lyrics_proc.rds') # if we want to skip the preprocessing and read the formatted RDS

names(df)

## Artists and Genre Preprocessing

df_artists = read.csv('artists.csv')

df_artists = df_artists %>% 
  mutate(artist_genres = gsub( pattern = '\\[','',  artist_genres )) %>% 
  mutate(artist_genres = gsub( pattern = '\\]','',  artist_genres )) %>% 
  mutate(artist_genres = gsub( pattern = '\'','',  artist_genres )) %>% 
  mutate(artist_genres = gsub( pattern = '\\s+','',  artist_genres )) %>%
  mutate(artist_genres_list = str_split(artist_genres, ',')) %>% 
  mutate(artist_genres_list = lapply(artist_genres_list, trimws ))

df_artists$artist_genres_list[16]

genres = c()

for (i in df_artists$artist_genres_list ){
  genres = append(genres, i)
}
genres_freq = as.data.frame(table(genres[!genres %in% '']))

genres_freq %>% 
  arrange(desc(Freq)) %>% 
  top_n(10)

df_artists$artist_genres_list[10]


genre_standard = function(v)
{
  ifelse(v %in% c('rock', 'classicrock', 'albumrock', 'modenrock'),
         'rock',
         ifelse(v %in% c('pop', 'dancepop', 'poprock',
                         'poprap', 'classicukpop'),
                'pop',
                ifelse( v %in% c('rap', 'trap', 'hiphop', 'r&b'),
                        'rap',
                        ifelse(v  %in% c('alernativemetal', 'hardrock', 'metal', 'numetal'),
                               'metal',
                               ifelse(v %in% c('newwavepop', 'folkrock', 'alternativerock',
                                               'indiepop', 'indierock'),
                                      'indie',
                                      ifelse(v %in% c('disco', 'edm'),
                                             'disco/edm',
                                             'others'))))  ))
}


df_artists$artist_genres_list[10]


df_artists = df_artists %>% 
  mutate(genre_standarized = lapply(artist_genres_list, genre_standard) )

df_artists$genre_standarized[14]
df_artists$genre_standarized[11]


df_artists = df_artists %>% 
  rowwise() %>% 
  mutate(main_genre =
           ifelse('disco/edm' %in% genre_standarized, 'disco/edm',
                  ifelse('indie' %in% genre_standarized, 'indie', 
                         ifelse('metal' %in% genre_standarized, 'metal', 
                                ifelse('rap' %in% genre_standarized, 'rap',
                                       ifelse('pop' %in% genre_standarized, 'pop',
                                              ifelse('rock' %in% genre_standarized, 'rock', 'others'
                                              )))))))


#df_artists = df_artists %>% transmute(main_genre = main_genre[[1]])
df_artists = df_artists[!duplicated(df_artists),]

table( df_artists$main_genre )

dim(df)
#we paste the artists and genres to the main dataframe to finish with the preprocessing.
df_merged = merge(x = df, y = df_artists, by = "artist_id",
                  all.x = TRUE)

names(df)

names(df_merged)

df_merged$main_genre

dim(df_merged)

saveRDS(df_merged, '16k_sngs_lyr_art_gnr.rds')


# |v|v|v| start from here to skip  |v|v|v|
# |v|v|v| 20 mins of preprocessing |v|v|v|
df <- readRDS('16k_sngs_lyr_art_gnr.rds')

df$main_genre
names(df)

dim(df)
df$main_genre

## 2. FEATURE ENGINEERING

df = df %>% 
  rowwise() %>% 
  mutate(n_words = length(lyrics_list)) %>% 
  mutate(n_unique = length(unique(lyrics_list)))

df = df[df$n_words<2000,] # only songs below 2000

df$duration_minutes  = df$duration_ms/60000
df$unique_per_minute = df$n_unique/df$duration_minutes
df$words_per_minute = df$n_words/df$duration_minutes
df$words_per_unique = df$n_words/df$n_unique

df = df %>% mutate(year = substr(track_release_date,1,4)) 
df$year = as.factor(df$year)
df$year_num = as.numeric(as.character(df$year))

df = df %>% mutate(decade =  paste(substr(year,3,3), '0', sep='')) 
df = df[!df$decade == 'NA0',]
df$decade <- factor(df$decade, levels = c('50','60','70','80','90','00','10','20'))
df = df[df$decade != '50', ] # we filter 50s because its too few songs

table(decade)

attach(df)



## 3. VISUALIZATIONS

df



df=  df[df$duration_minutes < 15, ]
df=  df[df$words_per_minute < 250, ]
df$popular = ifelse(df$track_popularity>0, 1, 0 )
df_popular = df[df$popular == 1 , ]

attach(df_popular)

dim(df_popular)


# 3.1. Univariate Descriptions
ggplot(df) + 
  geom_density( aes(x = log(n_words), color = 'no. of words (log)'), adjust=5, size=0.8) + 
  geom_density( aes(x = log(n_unique), color = 'no. of unique (log)'), adjust=5 , size=0.8) +
  scale_colour_manual("Density", values = c("blue4", "chocolate3") ) +
  labs(title = 'Distribution of number of words and number of unique words (smoothed)') + 
  xlab('Variables') + 
  ylab('Density')  + theme_fivethirtyeight()


ggplot(df) + 
  geom_density( aes(x = log(words_per_minute) , color = 'words-per-minute (log)'), adjust=5, size = 0.8) +
  geom_density( aes(x = log(unique_per_minute), color = 'unique-per-minute (log)'), adjust=5, size=0.8) + 
  scale_colour_manual("Density", values = c("blue4", "chocolate3") ) +
  labs(title = 'Distribution of unique-per-minute and words-per-minute (smoothed)') + 
  xlab('Variables') + 
  ylab('Density')  + theme_fivethirtyeight()
  

ggplot(df) + 
  geom_density( aes(x = log(duration_minutes) , color = 'duration in minutes (log)'), adjust=5, size = 0.8) +
  geom_density( aes(x = log(words_per_unique), color = 'words-per-unique (log)'), adjust=5, size=0.8) + 
  scale_colour_manual("Density", values = c("blue4", "chocolate3") ) +
  labs(title = 'Distribution of duration (minutes) and words-per-minute (smoothed)') + 
  xlab('Variables') + 
  ylab('Density')  + theme_fivethirtyeight()



df %>% 
  ggplot(aes(x = decade)) +
  geom_histogram(stat='count', fill='blue4') +
  labs(title='Histogram of songs in dataset by decade') + 
  xlab('Decades') + 
  ylab('Frequency')  +theme_fivethirtyeight()


df %>% 
  ggplot(aes(x = main_genre)) +
  geom_histogram(stat='count', fill='blue4') +
  labs(title='Histogram of songs in dataset by genre') + 
  xlab('Decades') + 
  ylab('Frequency')  +theme_fivethirtyeight()


# 3.2. Multivariate Vis

df=  df[df$duration_minutes < 15, ]
df=  df[df$words_per_minute < 250, ]
df$popular = ifelse(df$track_popularity>0, 1, 0 )
df_popular = df[df$popular == 1 , ]
attach(df_popular)


df_popular %>% 
  ggplot(aes(x = duration_minutes, y = track_popularity)) +
  geom_point(alpha=0.01, color='blue4') + 
  geom_smooth( se = FALSE)+
  xlab('Minutes') + ylab('Popularity Score') +
  labs('Correlation between minutes of a song and its popularity')

df_popular %>% 
  ggplot(aes(y = track_popularity )) +
  geom_point( aes(x = words_per_minute, color = 'words-per-minute'), alpha=0.01) + 
  geom_point( aes(x = unique_per_minute, color='unique-words-per-minute'), alpha = 0.01) + 
  geom_smooth( aes(x = words_per_minute, color = 'words-per-minute'), se=FALSE) +
  geom_smooth( aes(x = unique_per_minute, color='unique-words-per-minute'), se=FALSE) +
  scale_colour_manual("Density", values = c("blue4", "chocolate3") ) +
  labs(title='Relationship between words-per-minute, uniques-per-minute and song popularity ') + 
  xlab('Amount of words') + 
  ylab('Song Popularity') 

# if an artist fills a minute with repeating lyrics, after a certain moent people start to dislike it
# whereas if a song has lots of words in a minute, but different words (think of a really good rap verse)
  # people wont dislike it, probably because is not something that gets boring fast.
# the blue line hits a plateau and stays, while the orange line starts to descend for the second half of the curve


plot1 = df_popular %>% 
  ggplot(aes(y = words_per_minute, x = decade)) +
  geom_bar(stat = 'summary')

plot2 = df_popular %>% 
  ggplot(aes(y = n_words, x = decade)) +
  geom_bar(stat = 'summary')

plot3 = df_popular %>% 
  ggplot(aes(y = track_popularity, x = decade)) +
  geom_bar(stat = 'summary') # tracks are popular among all decades so we can analyze all of them

plot <- ggarrange(plot1, 
                  plot2, plot3,
                  ncol=3, nrow=1,
                  legend="bottom")


annotate_figure(plot, top = text_grob("Words-per-minute, no. of words \n and Track Popularity through the decades",
                                      color = 'blue4',
                                      size = 13))




# words_per_unique


ggplot(data = df_popular, aes(x=decade,y=log(words_per_unique) )) + 
  geom_jitter(alpha=0.05, color='blue4') +
  geom_boxplot(fill="lightblue1",color="black",alpha=0.4) + 
  labs(title='Words-per-unique-word (log) through the decades') + 
  xlab(label='Decades') + 
  ylab(label ='Lyric Diversity')

df_popular %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity)) +
  xlab('words-per-unique-word') + 
  ylab('Track Popularity Score') +
  labs( title = 'Correlation of WPU with Track Popularity') + 
  geom_point(alpha = 0.01, color='blue4') + 
  geom_smooth( se=FALSE, color = 'chocolate3', size = 1.7)





df_popular[df_popular$words_per_unique<15,] %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity, color=decade)) +
  geom_point( alpha = 0.01) +
  geom_smooth( se = F, size=1) +
  xlab('WPU') + 
  ylab('Track Popularity Score')+
  labs(title = 'WPU~Popularity Correlation each decade') +
  scale_colour_brewer( palette = "Set1")
# in the 70s, people penalized repetitive songs (high words per unique tended to reduce your popularity)
# this behaviours tended to decrease iver time until the 2000s, the birth of 
# pop bands as nsync, etc, people started actually "rewarding" songs that had 
# lots of words per unique


df_popular[df_popular$decade %in% c('70' , '80', '20'), ]  %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity, color=decade)) +
  geom_point( alpha = 0.05, size=0.5) +
  geom_smooth( se = F, size=1)+ xlab('WPU') + 
  ylab('Track Popularity Score')+
  labs(title = 'WPU~Popularity Correlation in the 70S, 80s and 2020s') +
  scale_colour_brewer( palette = "Set1")

      
require(gtools)
df_popular$popularity_group = quantcut(df_popular$track_popularity, q = 3,
                                       labels = c('not popular', 'average','very popular' ))


df_popular %>% 
  group_by(decade, popularity_group) %>% 
  summarise(Mean_entropy = mean(words_per_unique)) %>% 
  ggplot(aes(x = decade, y = Mean_entropy, color = popularity_group, group=popularity_group)) +
  geom_line(size=2) + 
  ylim(1,3) +
  ylab('WPU') +
  xlab('Decade') + 
  labs( title = 'Average WPU of popularity segments per decade')


#in almost every decade, the top songs, thos who belong to the 15% most popular in their decade,
#had a higher entropy (more repetitive) than the rest. meaning that indeed being
# the repetitiveness of lyrics tands to be more noticeable on hits that not-hits


# genre + decade + entropy analysis


df_popular = df_popular[!is.na(df_popular$words_per_unique),]

attach(df_popular)

df_popular

names(df_popular)

table(main_genre)


df_popular$main_genre

df_popular[df_popular$main_genre %in% c('indie', 'metal', 'pop', 'rap' ),]  %>% 
  ggplot(aes(x = unique_per_minute, y = track_popularity, color=main_genre)) +
  geom_point( alpha = 0.05, size=0.5) +
  geom_smooth( se = F, size=2) +
  scale_colour_brewer( palette = "Set1") +
  ylab('Track Popularity') + xlab('Unique-words-per-minute') +
  labs(title = 'Impact of words-per-minute opened by genre')
# rap is genre where listeners tend to reward songs that say plenty of new words per minute.
# compared to pop, where there is a clear negative slope when an artists doesnt follow 
# easy, sing-alongable tunes



attach(df_popular)

df_popular

plot70s <- df_popular[(df_popular$main_genre %in% c('indie','rap', 'pop', 'rock') )
           &  (df_popular$decade == '70' )   ,]  %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity, color=main_genre)) +
  geom_point( alpha = 0.05, size=0.5) +
  geom_smooth( method ='lm', se = F, size=1) +
  scale_colour_brewer( palette = "Set1")+ xlim(0,10)+ ylim(20, 90) +
  xlab(label = 'Words per unique word') + 
  ylab('Track popularity') +
  labs(title = 'Songs released in the 70s')
  
plot80s <- df_popular[(df_popular$main_genre %in% c('indie','rap', 'pop',  'rock') )
                      &  (df_popular$decade == '80' )          ,]  %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity, color=main_genre)) +
  geom_point( alpha = 0.05, size=0.5) +
  geom_smooth( method ='lm', se = F, size=1) +
  scale_colour_brewer( palette = "Set1")+ xlim(0,10)+ ylim(20, 90) + 
  xlab(label = 'Words per unique word') + 
  labs(title = 'Songs released in the 80s')


plot2000s <- df_popular[(df_popular$main_genre %in% c('indie','rap', 'pop',  'rock') )
                      &  (df_popular$decade == '00' )          ,]  %>% 
  ggplot(aes(x = words_per_unique, y = track_popularity, color=main_genre)) +
  geom_point( alpha = 0.05, size=0.5) +
  geom_smooth( method ='lm', se = F, size=1) +
  scale_colour_brewer( palette = "Set1") + xlim(0,10) + ylim(20, 90) +
  xlab(label = 'Words per unique word') + 
  labs(title = 'Songs released in the 2000s')


plot <- ggarrange(plot80s + rremove("ylab"),
          plot2000s + rremove("ylab"),
          ncol=2, nrow=1, common.legend = TRUE, legend="bottom")


annotate_figure(plot, top = text_grob("Lyric Diversity by Genre and its effect in track popularity",
                                      color = 'blue4',
                                      size = 11))


# 4. MODELING

#4.1. PREDICTOR TO PLAY WITH RELEASE DATE


df_popular$flg_explicit = as.factor(flg_explicit)

df_popular$main_genre  = as.factor(main_genre )


attach(df_popular)

library(gbm)

boosted=gbm(track_popularity~duration_ms+
              flg_explicit+
              track_danceability+
              track_energy+
              track_loudness +
              track_mode + 
              track_speechiness + 
              track_acousticeess + 
              track_instrumentalness + 
              track_liveness + 
              track_valence + 
              track_tempo + 
              decade + 
              main_genre +
              n_words + 
              n_unique  +
              unique_per_minute +
              words_per_minute + 
              words_per_unique,
              data=df_popular,
              distribution= "gaussian", 
            n.trees=10000,
            interaction.depth=4)

summary(boosted)
# words_per_unique, out entropy metric, is the third most important 
# feature in a tree based classifier





names(df_popular)

write.table(df_popular[c(1,6,5500,2000,7000),
                       names(df_popular)[!names(df_popular) 
                                         %in% c('lyrics_list',
                                                'genre_standarized',
                                                'artist_genres_list') ]],
            "clipboard",
            sep="\t")


predicted_score=predict(boosted, newdata=df_popular, n.trees=10000)
mean(abs(predicted_score -track_popularity))


# what does the model predict for 
      # wonderful tonight
      df_song = df_popular[df_popular$track_name == 'Wonderful Tonight' , ]
      predicted_score_original=predict(boosted, newdata=df_song, n.trees=10000)
      print(predicted_score_original)
      df_song$track_popularity
      
      View(df_song)
      df_song$decade = 20
      
      predicted_score_altered=predict(boosted, newdata=df_song, n.trees=10000)
      print(predicted_score_altered)
      
      #karma chameleon
      df_song = df_popular[df_popular$track_name == 'Karma Chameleon' , ]
      predicted_score_original=predict(boosted, newdata=df_song, n.trees=10000)
      print(predicted_score_original)
      df_song$track_popularity
      
      View(df_song)
      
      df_song$decade = 20
      
      predicted_score_altered=predict(boosted, newdata=df_song, n.trees=10000)
      print(predicted_score_altered)

# group by artist, get mean of values and run PCA on that


# PCA analysis of features and use hue to show succesful songs. in this pca, lyrics siversity should be 1 or more feats

      
      
dim(df_popular)

names(df_popular)



df_artists_pca = df_popular %>% group_by(artist_name.x) %>% 
  summarise(track_popularity = mean(track_popularity), 
            track_danceability = mean(track_danceability), 
            track_energy = mean(track_energy), 
            track_instrumentalness = mean(track_instrumentalness), 
            track_tempo = mean(track_tempo),
            duration_minutes = mean(duration_minutes),
            unique_per_minute = mean(unique_per_minute), 
            words_per_unique = mean(words_per_unique),
            words_per_minute = mean(words_per_minute))

dim(df_artists_pca)     

pca=prcomp(df_artists_pca[,c(2:10)], scale=TRUE)

pca


#install.packages(“ggfortify")
library(ggfortify)


autoplot(pca, data = df_artists_pca[,c(2:10)], loadings = TRUE, loadings.label = TRUE,
         col=ifelse(df_artists_pca$track_popularity > 75,"blue","transparent"))

