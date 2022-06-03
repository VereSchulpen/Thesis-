#library(data.table)
#library(dplyr)
#library(tidyr)

#Open files
overlap <- fread("Data/overlap_titles_amazon_gr.txt")

#Match Book_id to ASIN in Q&A data
QuestAns = subset(QuestAns, select = -c(Likes, Answer, Answer_Likes, Number_of_Comments_for_Answer) )
QuestAns = unique(QuestAns)
QuestAns <- dplyr::rename(QuestAns, book_id = Book_Id)
QuestAns <- merge(x= QuestAns, y = overlap, by = "book_id", allow.cartesian=TRUE)

#Find unique questions
QuestAns <- QuestAns %>% 
  group_by(asin) %>% 
  filter(cleaned_question_time == min(cleaned_question_time)) %>% 
  distinct

#Merge Amazon with Q&A
amazon_reviews <- merge(x= amazon_reviews, y = QuestAns[ , c("asin", "cleaned_question_time")], by = "asin",  )
amazon_reviews$afterinv <- amazon_reviews$reviewTime > amazon_reviews$cleaned_question_time

#Merge Goodreads with Q&A
goodreads_reviews <- merge(x= goodreads_reviews, y = overlap, by = "book_id", allow.cartesian=TRUE)
goodreads_reviews <- merge(x= goodreads_reviews, y = QuestAns[ , c("asin", "cleaned_question_time")], by = "asin",  )
goodreads_reviews$afterinv <- goodreads_reviews$date > goodreads_reviews$cleaned_question_time

#Identify Goodreads or Amazon
amazon_reviews$goodr <- 0
goodreads_reviews$goodr <- 1

#Create new dataframe with both Goodreads and Amazon data
GoodAma <- select (amazon_reviews,-c(verified, reviewerID, style, reviewerName, unixReviewTime))
goodreads_reviews <- select (goodreads_reviews, -c(book_id))
goodreads_reviews <- goodreads_reviews[ , c("asin", "rating", "date", "cleaned_question_time", "afterinv", "goodr")]
GoodAma <- dplyr::rename(GoodAma, rating = overall)
GoodAma <- dplyr::rename(GoodAma, date = reviewTime)
GoodAma <- rbind(GoodAma, goodreads_reviews)

