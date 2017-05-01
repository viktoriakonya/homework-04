# Függvények 2.3. feladat


num_tweets <- function(nev = "Hillary Clinton", tweet_szam = 1){
  
  if (!identical(nev, "Hillary Clinton") && !identical(nev, "Donald Trump")) {
    print("Rossz a megadott név!")
  }
  
  else if (identical(nev, "Hillary Clinton") && tweet_szam > clinton){
    print(paste0("Hillary Clinton maximális tweet-jeinek száma: ", clinton))
  }
  else if (identical(nev, "Donald Trump") && tweet_szam > trump){
    print(paste0("Donald Trump maximális tweet-jeinek száma: ", trump))
  }
  
  # Tweetek száma alapján csökkenő sorrend
  else 
    for(i in 1:length(tweets$name)){
      tweets$score[i] <- (tweets$retweet_count+tweets$favorite_count)
    }
  
  tweets <- tweets[order(tweets$score, decreasing = TRUE), ]
  
  if (identical(nev, "Hillary Clinton")){
    tweets_sub1 <- subset(tweets, tweets$name == "Hillary Clinton")
    print(paste(nev, tweet_szam, "legkedveltebb tweetje: "))
    for (i in 1:tweet_szam){
      print(tweets_sub1$text[i])
    }
  }
  else if (identical(nev, "Donald Trump")){
    tweets_sub1 <- subset(tweets, tweets$name == "Donald Trump")
    print(paste(nev, tweet_szam, "legkedveltebb tweetje: "))
    for (i in 1:tweet_szam){
      print(tweets_sub1$text[i])
    }
  }
}

