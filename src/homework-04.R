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




################################################################################
--------------------------------------------------------------------------------
################################################################################

# 3. Feladat


# 3. Feladat -------------------------------------------------------------------

rm(list=ls(all=TRUE))

### dataset behívása
require(fivethirtyeight)
data("hiphop_cand_lyrics")
View(hiphop_cand_lyrics)
as.data.frame(hiphop_cand_lyrics)

# Package-ek behívása
library(ggplot2)
library(gcookbook)
install.packages("cowplot")
devtools::install_github("wilkelab/cowplot")
library("cowplot")
library(gridExtra)


# Név szétbontása
hiphop_cand_lyrics$name2 <- strsplit(as.character(hiphop_cand_lyrics$candidate), split = " ") 
View(hiphop_cand_lyrics)
hiphop_cand_lyrics$name2[[1]][[1]]

for (i in 1:length(hiphop_cand_lyrics$name2)){
  hiphop_cand_lyrics$name2_pri[i] <- hiphop_cand_lyrics$name2[[i]][[2]]
}
View(hiphop_cand_lyrics)

# Fig könyvtár létrehozása
#dir.create("fig", showWarnings = FALSE)

### Külön subseteket készítettem a feladatrészekhez
hiphop_cand_lyrics$num <- c(1)
# 3.1. ábrához
subset1 <- aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri), sum)
as.data.frame(subset1)
View(subset1)
# 3.2. ábrához
subset2<-aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri, sentiment=hiphop_cand_lyrics$sentiment), sum)
as.data.frame(subset2)
View(subset2)
# 3.3. ábrához
subset3<-aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri, theme=hiphop_cand_lyrics$theme), sum)
as.data.frame(subset3)
View(subset3)

### 3.1. ábra
# nem jó a label
subset1$name <- as.factor(subset1$name)
levels(subset1$name)
subset1$name = factor(subset1$name,levels(subset1$name)[c(8, 4, 1,3,6,7,2,5)])
subset1$name <- factor(subset1$name, levels = rev(levels(subset1$name)))
levels(subset1$name)
View(subset1)


plot_1 <- ggplot(subset1[order(subset1$name),], aes(x = subset1$date, y = subset1$x, fill = subset1$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  scale_x_continuous( breaks=c(seq(1990,2016,by=5)),
                      minor_breaks=seq(1990,2016,by=5) 
  )+
  theme(plot.title = element_text(hjust = 0.5))


plot_1 + theme(legend.position="top") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs")

ggsave("fig/hiphop1.png", width = 8, height = 5, dpi = 100)

### 3.2. ábra

# Szétbontás érzelmek szerint subsetekre
subset21 <-subset(subset2, subset2$sentiment=="positive")
subset22 <-subset(subset2, subset2$sentiment=="negative")
subset23 <-subset(subset2, subset2$sentiment=="neutral")

View(subset21)
View(subset22)
View(subset23)

### 3.2.1. ábra
subset21$name <- as.factor(subset21$name)
levels(subset21$name)
subset21$name = factor(subset21$name,levels(subset21$name)[c(4, 2, 3,1)])
subset21$name <- factor(subset21$name, levels = rev(levels(subset21$name)))
levels(subset21$name)
View(subset21)

plot_21 <- ggplot(subset21[order(subset21$name),], aes(x = subset21$date, y = subset21$x, fill = subset21$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_21
plot_21 + theme(legend.position="top") 

### 3.2.2. ábra
subset22$name <- as.factor(subset22$name)
levels(subset22$name)
subset22$name = factor(subset22$name,levels(subset22$name)[c(6, 2, 1,4,5,3)])
subset22$name <- factor(subset22$name, levels = rev(levels(subset22$name)))
levels(subset22$name)
View(subset22)

plot_22 <- ggplot(subset22[order(subset22$name),], aes(x = subset22$date, y = subset22$x, fill = subset22$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_22
plot_22 + theme(legend.position="top")


### 3.2.3. ábra
subset23$name <- as.factor(subset23$name)
levels(subset23$name)
subset23$name = factor(subset23$name,levels(subset23$name)[c(5,3,1,2,4)])
subset23$name <- factor(subset23$name, levels = rev(levels(subset23$name)))
levels(subset23$name)
View(subset23)

plot_23 <- ggplot(subset23[order(subset23$name),], aes(x = subset23$date, y = subset23$x, fill = subset23$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" ))+
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_23
plot_23 + theme(legend.position="top") 

# 3.2.1.-3.2.3. egyben
#install.packages("cowplot")
#devtools::install_github("wilkelab/cowplot")
#library("cowplot")
#library(gridExtra)

#list=c("Positive","Negative", "Neutral")

plot_egyben<-plot_grid(plot_21, plot_22, plot_23, ncol=3,
                       labels=c("Positive","Negative", "Neutral"), label_size=12, align="hv")+
  theme(plot.title = element_text(hjust = 0.5))

plot_egyben + theme(legend.position="top") +
  ggtitle("Candidate mentions, by sentiment")

ggsave("fig/hiphop2.png", width = 20, height = 5, dpi = 100)


### 3.3. ábra
# Szétbontás téma szerint subsetekre
subset31 <-subset(subset3, subset3$theme=="money")
subset32 <-subset(subset3, subset3$theme=="hotel")
subset33 <-subset(subset3, subset3$theme=="political")

View(subset31)
View(subset32)
View(subset33)

### 3.3.1 ábra
subset31$name <- as.factor(subset31$name)
levels(subset31$name)
subset31$name = factor(subset21$name,levels(subset21$name)[c(2, 1)])
subset31$name <- factor(subset21$name, levels = rev(levels(subset21$name)))
levels(subset31$name)
View(subset31)

plot_31 <- ggplot(subset31[order(subset31$name),], aes(x = subset31$date, y = subset31$x, fill = subset31$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )

plot_31 + theme(legend.position="top") 


### 3.2.2. ábra
subset32$name <- as.factor(subset32$name)
levels(subset32$name)
subset32$name = factor(subset32$name,levels(subset32$name)[c(1)])
subset32$name <- factor(subset32$name, levels = rev(levels(subset32$name)))
levels(subset32$name)
View(subset32)

plot_32 <- ggplot(subset22[order(subset32$name),], aes(x = subset32$date, y = subset32$x, fill = subset32$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
    
  )+
  scale_y_continuous( limits = c(0,15), expand = c(0,0) )

plot_32
plot_32 + theme(legend.position="top") 

### 3.2.3. ábra
subset33$name <- as.factor(subset33$name)
levels(subset33$name)
subset33$name = factor(subset33$name,levels(subset33$name)[c(5,2,1,4,3)])
subset33$name <- factor(subset33$name, levels = rev(levels(subset33$name)))
levels(subset33$name)
View(subset33)

plot_33 <- ggplot(subset33[order(subset33$name),], aes(x = subset33$date, y = subset33$x, fill = subset33$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" ))+
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )

plot_33
plot_33 + theme(legend.position="top") 

# 3.3.1.-3.3.3. Egyben
plot_egyben2<-plot_grid(plot_31, plot_32, plot_33, ncol=3,
                        labels=c("Wealth","Trump Tower", "Politics"), label_size=12, align="hv")


plot_egyben2 + theme(legend.position="top") +
  ggtitle("Candidate mentions, by subject")

ggsave("fig/hiphop3.png", width = 20, height = 5, dpi = 100)







