library(dplyr)
library(ggplot2)
library(psych)
library(grid)
library(gridExtra)

# Read in the ratings dataframe and rename the columns
u1 <- "https://static.turi.com/datasets/millionsong/10000.txt"
song_df1 <- as.data.frame(read.table(u1, header = F, stringsAsFactors = F))
head(song_df1)
names(song_df1) <- c("user_id", "song_id", "listen_count")
head(song_df1)
nrow(song_df1)

#Read song  metadata
u2 <- "https://static.turi.com/datasets/millionsong/song_data.csv"
song_df2 <- as.data.frame(read.csv(u2, header = T, sep = ",", stringsAsFactors = F))
#song_df2<-song_df2 %>% distinct("song_id",  .keep_all = TRUE)
head(song_df2)
colnames(song_df2)
nrow(song_df2)

# Join data by song ID. Remove duplicate song ratings.

song_df2<-song_df2[!duplicated(song_df2$song_id),]
nrow(song_df2)
song_df <- inner_join(song_df1, song_df2, by = "song_id")
head(song_df)
nrow(song_df)

# Group and summarize joined dataframe by user ID
grouped_id <- song_df %>%
  select(user_id, listen_count) %>%
  group_by(user_id) %>%
  summarise(number_songs = n(), 
            mean_listen_count = mean(listen_count), 
            sum_listen_count = sum(listen_count))
head(grouped_id)

# Group and summarize joined dataframe by song
grouped_song <- song_df %>% 
  select(song_id, title, artist_name) %>% 
  group_by(title)
head(grouped_song)

# Number of unique users in the dataset
length(unique(song_df[,"user_id"])) 

# Count number of unique songs in the data set
length(unique(song_df[,"song_id"])) 
colnames(song_df)

# Total number of listens
sum(song_df$listen_count)

# High-level statistics on listeners

di <- describe(grouped_id)
di

# Compare total songs and listeners
ggplot(data = grouped_id, aes(number_songs)) + 
  geom_histogram(binwidth = 1) +
  labs(title = "How people listen: songs vs. listeners", x = "Unique songs", y = "Total listeners")

# Compare total songs and listeners below 100 songs
ggplot(data = grouped_id, aes(number_songs)) + 
  geom_histogram(breaks = seq(1, 100, by = 1)) +
  labs(title = "How people listen: songs vs. listeners", subtitle = "<100 songs (detail)", x = "Unique songs", y = "Total listeners")

# Compare total songs and total listens
ggplot(data = grouped_id, aes(x = number_songs, y = sum_listen_count)) +
  geom_point(color='darkblue') +
  geom_smooth(method = "loess", se = F) +
  xlim(c(0, 4000)) +
  ylim(c(0, 4000)) +
  labs(title = "How people listen: songs vs. listens", x = "Unique songs", y = "Total listens")

# Earliest and latest recordings (correcting for null values coded as 0)
min(song_df$year[which(song_df$year > 0)])
max(song_df$year[which(song_df$year > 0)])
head(song_df)
head(song_df, 2)

# Count number of unique songs in the data set
length(unique(song_df[,"song_id"])) 
colnames(song_df)

# Total number of listens
sum(song_df$listen_count)

# High-level statistics on songs
describe(song_df$listen_count)

# Compare total listens and unique listeners
song_df %>% 
  select(user_id, song_id, listen_count) %>% 
  group_by(song_id) %>% 
  summarise(total_listens = sum(listen_count), unique_listeners = n_distinct(user_id)) %>%
  ggplot(aes(x = total_listens, y = unique_listeners)) +
  geom_point(color='red') +
  geom_smooth(method = "loess", se = F) +
  xlim(c(0, 8000)) +
  ylim(c(0, 8000)) +
  labs(title = "How songs are listened to: unique listeners vs. total listens", x = "Total listens", y = "Unique listeners")

# Calculate ratings and filter dataframe for modeling

# Join total listen count to the full dataframe.
joined_song_df <- left_join(song_df, grouped_id, by = "user_id")
head(joined_song_df)

# Create a new column to hold a calculated implicit rating (as a number from 0 to 100) of user preference for a song.
final_song_df <- mutate(joined_song_df, rating = round((joined_song_df$listen_count / joined_song_df$sum_listen_count)*100, 2))
head(final_song_df)
tail(final_song_df)

# Filter out users with a single song rating. Include users who have a diverse set of ratings.
final_song_df <- filter(final_song_df, rating<100, mean_listen_count>2, number_songs>=15, year>0)
head(final_song_df)

hist(final_song_df$rating)

##########Last Decade Analysis#########################

################taking data for 10 years only##############
c=seq(2000,2010)

song_df3<-song_df[song_df$year %in% c,]

#No no records for years 2000 to 2010
nrow(song_df3)

#Sample data
head(song_df3)
##################################
#top 10 artists
top_artist<-group_by(song_df3,artist_name)
top_artist<-summarise(top_artist,listen_count=n())
top_10_artists<-head(arrange(top_artist,-listen_count),n=10)
top_10_artists

ggplot(data=top_10_artists, aes(reorder(artist_name,listen_count),listen_count))+
  geom_bar(colour="black",width=.4, stat="identity",fill="chartreuse4")+
  xlab("Artist Name") + ylab("Popularity") +
  ggtitle("Top 10 Artists from 2000 - 2010 ")+coord_flip()

#top 10 albums
top_release<-group_by(song_df3,release)
top_release<-summarise(top_release,listen_count=n())
top_10_albums<-head(arrange(top_release,-listen_count),n=10)
top_10_albums

ggplot(data=top_10_albums, aes(reorder(release,listen_count),listen_count))+
  geom_bar(colour="black",width=.4, stat="identity",fill="dodgerblue3")+
  xlab("Album Name") + ylab("Popularity") +
  ggtitle("Top 10 Albums from 2000 - 2010 ")+coord_flip()

#top 10 songs
top_song<-group_by(song_df3,title)
top_song<-summarise(top_song,listen_count=n())
top_10_songs<-head(arrange(top_song,-listen_count),n=10)

top_10_songs

ggplot(data=top_10_songs, aes(reorder(title,listen_count),listen_count))+
  geom_bar(colour="black",width=.4, stat="identity",fill="darkgoldenrod3")+
  xlab("Song Name") + ylab("Popularity") +
  ggtitle("Top 10 Songs from 2000 - 2010 ")+coord_flip()


#################Top 3 artists by year##############

top_artist_yearly<-group_by(song_df3,year,artist_name)
top_artist_yearly<-summarise(top_artist_yearly,listen_count=n())
#head(top10_artist_yearly[order(-top10_artist_yearly$listen_count),],n=10)

top3_artist<-top_n(top_artist_yearly,3,listen_count)
top3_artist<-arrange(top3_artist,year,-listen_count)

#Top 10 artists in each year
top3_artist

top3_artist$year<-as.character(top3_artist$year)

l=list()
k=1
cl=rainbow(11, s = 0.3)
#heat.colors(11, alpha=1)
for (m in seq(2000,2010)){
  data1<-filter(top3_artist,year==m)
  data1$year<-as.character(data1$year)
  
  l[[k]]<-ggplot(data=data1,aes(x=artist_name,listen_count))+
    geom_bar(colour="black",width=.4, stat="identity",fill=cl[k])+
    ggtitle(paste(m))+
    theme(axis.text.x = element_text(angle=90, vjust=0.6),text = element_text(size=15),axis.title.x=element_blank(),axis.title.y=element_blank())
  k=k+1
}
grid.arrange(grobs=l,nrow=3,ncol=4,bottom=textGrob("Artist Name",gp=gpar(fontsize=20)),left=textGrob("Popularity", rot=90,gp=gpar(fontsize=20)))

################Artist popularity by year##################
top_artist<-group_by(song_df3,artist_name)
top_artist<-summarise(top_artist,listen_count=n())
top_10_artists<-head(arrange(top_artist,-listen_count),n=5)

p=list()
j=1
co=c("gold","deepskyblue1","firebrick1","coral","seagreen2")
co1=c("gold4","dodgerblue4","firebrick4","coral4","seagreen4")
for (i in top_10_artists$artist_name){
  data<-filter(song_df3,artist_name==i)
  groupdata<-group_by(data,year,artist_name)
  year_trend<-summarize(groupdata,listen_count=n())
  year_trend$year<-as.factor(year_trend$year)
  p[[j]]<-ggplot(data=year_trend, aes(x=year,listen_count))+
    geom_line(group = 'character',size=1.2,col=co[j]) +
    geom_point(size=4,col=co1[j])+
    xlab("Year") + ylab("Listen Count") +
    ggtitle(paste(i," populatity over 10 years"))
  j=j+1
}
grid.arrange(grobs=p,nrow=3,ncol=2)



#######################USer Trend Analysis###########################

year_user<-distinct(song_df3,user_id,year)
group_data<-group_by(year_user,year)
user_trend<-summarize(group_data,user_count=n())
user_trend$year<-as.character(user_trend$year)

ggplot(data=user_trend, aes(x=year,user_count),group=year)+geom_line(group = 'character',size=1.5,col='midnightblue') +
  geom_point(size=5,col='orange')+
  xlab("YEAR") + ylab("No of Users") +
  ggtitle("User Count over 11 years")

########################total songs over years#########################

title_release<-distinct(song_df3,year,release,title)
group_data<-group_by(title_release,release,year)
release_song_trend<-summarize(group_data,song_count=n())
group_data<-group_by(release_song_trend,year)
release_song_trend<-summarize(group_data,total_songs=sum(song_count))
release_song_trend$year<-as.character(release_song_trend$year)

ggplot(data=release_song_trend, aes(x=year,total_songs),group=year)+geom_line(group='character',col = 'gray0',size=1.5) +
  geom_point(size=5,col='deepskyblue')+
  xlab("YEAR") + ylab("total songs released") +
  ggtitle("Songs released over 11 years")

########################total albums over years#########################

yearly_release<-distinct(song_df3,year,release)
group_data<-group_by(title_release,year)
albums_trend<-summarize(group_data,album_count=n())
albums_trend$year<-as.character(albums_trend$year)


ggplot(data=albums_trend, aes(x=year,album_count))+
  geom_bar(colour="black",width=.4, stat="identity",fill="#FF8888")+
  xlab("Year") + ylab("Album Count") +
  ggtitle("Trend of Album Count over 11 years ")


#########################Songs vs users per year########################
song_user_df <- inner_join(release_song_trend, user_trend, by = "year")
song_user_df $year<-as.factor(song_user_df$year)

ggplot(data=song_user_df ,aes(x=total_songs,user_count))+
  geom_smooth(method=lm,se=FALSE) +
  geom_point(size=4,col='blue')+
  xlab("Songs") + ylab("Users") +
  ggtitle("Songs vs Users over 10 years")

##############################Timeless Songs#####################

group_data<-group_by(song_df3,year,title)
song_count<-summarize(group_data,listen_count=n())


group_data<-group_by(song_count,title)
evergreen_songs<-summarize(group_data,year_count=n())
evergreen_songs<-filter(arrange(evergreen_songs,-year_count),year_count>=4)
evergreen_songs
evergreen_songs_data<-filter(song_count,title %in% evergreen_songs$title)
evergreen_songs_data$year<-as.character(evergreen_songs_data$year)

ggplot(data=evergreen_songs_data, aes(x=year,listen_count,group=title,fill=title))+
  geom_bar(colour="black",width=.7, stat="identity", position=position_dodge())+
  xlab("Year") + ylab("Listen Count") +
  ggtitle("Trend of timeless songs over 11 years ")








