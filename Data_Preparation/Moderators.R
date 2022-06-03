#Fiction
genres <- fread("Data/goodreads_genres.txt")
genres <- merge(x= genres, y = overlap, by = "book_id", allow.cartesian=TRUE)

genres <- genres[!duplicated(genres$asin), ]
genres[is.na(genres)] <- 0

GoodAma <- merge(x= GoodAma, y = genres[, c("fiction", "non-fiction", "asin")], by = "asin", allow.cartesian=TRUE)
miss_genre <- filter(GoodAma, fiction == 0, GoodAma$non-fiction == 0)
miss_genre$miss_genre <- 1
GoodAma <- merge(x= GoodAma, y = miss_genre[, c("asin", "miss_genre")], by = "asin", allow.cartesian=TRUE)

GoodAma <- GoodAma %>% 
  mutate(fiction.1 = if_else(fiction + `non-fiction` == 0, 0, 1))

GoodAmaFic<-subset(GoodAma, fiction.1 == 1)
GoodAmaFic$fiction2 <- GoodAmaFic$fiction.y > GoodAmaFic$`non-fiction.y`

#Variance
before_inv <- subset(GoodAma, afterinv == FALSE)
before_inv <- subset(before_inv, goodr == 1)
variance <- aggregate(x = before_inv$rating,              
                          by = list(before_inv$asin),             
                          FUN = var)

variance <- dplyr::rename(variance, asin = Group.1)
GoodAma <- merge(x= GoodAma, y = variance[ , c("x", "asin")], by="asin", allow.cartesian=TRUE)
GoodAma <- dplyr::rename(GoodAma, variance = x)
names(GoodAma)[7] <- 'variance'
GoodAma$varhigh <- GoodAma$variance > 0.8683

#Number of reviews
numberreviews <- count(before_inv, "asin")
GoodAma <- merge(x= GoodAma, y = numberreviews, by="asin", all.x=TRUE, allow.cartesian=TRUE)
GoodAma$volhigh <- GoodAma$freq > 558.3
