########################
# Házi feladat 4       #
# Programozás I.       #
# 2016/17. II. félév   #
# Kónya Viktória       #
# 2017-04-28           #
########################

# 2. Feladat

# Adatbázis behívása
tweets <- read.csv2(file = "data/clinton_trump_tweets.csv")
View(tweets)
tweets<- as.data.frame(tweets)

# Nevek átílakítása
tweets$name <- c()
for (i in 1:length(tweets$handle)){
  if (tweets$handle[i]=="realDonaldTrump"){
    tweets$name[i]="Donald Trump"}
  else if (tweets$handle[i]=="HillaryClinton"){
    tweets$name[i]="Hillary Clinton"}
}

# Fig könyvtár létrehozása
dir.create("fig", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2.1. Összes tweet száma 
clinton <- nrow(tweets[tweets$name == "Hillary Clinton",])
trump <- nrow(tweets[tweets$name == "Donald Trump",])

# Clinton 3226, Trump 3218 üzenetet írt
table(clinton, trump)

# Oszlopdiagrammal ábrázolás
# segédváltozók
Candidate <- c("Hillary Clinton", "Donald Trump")
Tweet <- c(clinton, trump)
View(Candidate)

plot_1_2 <- data.frame(Candidate, Tweet)
plot_1_2$Candidate <- as.character(plot_1_2$Candidate)
View(plot_1_2)

# Ábra: összes tweet jelöltenként
library(ggplot2)

limit <- c("Hillary Clinton", "Donald Trump")

ggplot(plot_1_2, aes(x = Candidate, y = Tweet, fill = Candidate)) +
  scale_x_discrete(limits = limit)+
  scale_fill_manual(breaks = c("Hillary Clinton", "Donald Trump"), 
                    values=c("red", "blue"))+
  geom_bar(stat = "identity")+
  ggtitle("Candidate Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank())









ggsave("fig/tweet1.png", width = 5, height = 5, dpi = 100)

# ------------------------------------------------------------------------------
# 2.2. Nyelvek gyakoriságai

# Nyelvek megoszlása jelöltenként
kereszttabla<-table(tweets$lang, tweets$name)
kereszttabla

# Szűrés a nem angol és nem spanol nyelvekre
tweets$nyelvek<-c()

for (i in 1:length(tweets$lang)){
  if (tweets$lang[i] != "en" & tweets$lang[i] != "es"){
    tweets$nyelvek[i] <- 0 
  }
  else
    tweets$nyelvek[i] <- 1
}

View(tweets)

# Ellenőrzés
kereszttabla2<-table(tweets$nyelvek, tweets$name)
kereszttabla2

sub_temp <- data.frame(tweets$handle, tweets$text, tweets$lang, tweets$nyelvek)
sub_temp2 <- sub_temp[tweets$nyelvek == 0, ]
View(sub_temp2)
# Ez a 9 üzenet is szerintem angolul van

# Nyelv átírása angolra
for (i in 1:length(tweets$lang)){
  if (tweets$nyelvek[i] == 0){
    tweets$lang[i] <- "en" 
  }
}

# Itt már csak angol és spanyol nyelv van van
kereszttabla3<-table(tweets$lang, tweets$name)
kereszttabla3

# Nyelvek ábrázolása jelöltenként
clinton_en <- nrow(tweets[tweets$name == "Hillary Clinton" & tweets$lang == "en",])
trump_en <- nrow(tweets[tweets$name == "Donald Trump" & tweets$lang == "en",])
clinton_es <- nrow(tweets[tweets$name == "Hillary Clinton" & tweets$lang == "es",])
trump_es <- nrow(tweets[tweets$name == "Donald Trump" & tweets$lang == "es",])

# Ellenőrzés
t<-c(clinton_en, clinton_es, trump_en, trump_es)
t

# Segédváltozók
Language <- c("English", "Spanish", "English", "Spanish")
Tweet <- c(clinton_en, clinton_es, trump_en, trump_es)
Candidate <- c("Hillary Clinton", "Hillary Clinton", "Donald Trump", "Donald Trump")

Tweet

sub2 <- data.frame(Language, Tweet, Candidate)
View(sub2)


ggplot(sub2, aes(x = sub2$Candidate, y = sub2$Tweet, fill = Language)) +
  scale_x_discrete(limits = limit)+
  scale_fill_manual(breaks = c("English", "Spanish"), 
                    values=c("darkgrey", "cornflowerblue"))+
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Language of Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line("white"))

ggsave("fig/tweet2.png", width = 7, height = 5, dpi = 100)



# ------------------------------------------------------------------------------
# 2.3. Feladat

# Függvény meghívása
source("src/homework-04-functions.R")

#num_tweets()

# Clinton első 10
num_tweets("Hillary Clinton", 10)

# Trump első 10
num_tweets("Donald Trump", 15)

# Ez egy helytelen név lesz
num_tweets("Helytelen Név", 10)
