#install.packages('data.table)
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('stringr')

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)

#Part 1: Q&A 
#Open file
QuestAns <- fread('Data/2864_goodreads_com_book_full.csv')

#Drop NA Date_of_Question
QuestAns <- QuestAns %>% mutate_all(na_if,"")
QuestAns <- QuestAns %>% drop_na(Date_of_Question)

#Month to days
QuestAns$Month <- ifelse(str_detect(QuestAns$Date_of_Question, "months"), gsub("[a-zA-Z ]", "", QuestAns$Date_of_Question), "")
QuestAns$Month <- as.numeric(as.character(QuestAns$Month))
QuestAns$Month <- QuestAns$Month* 30

QuestAns$Day <- ifelse(str_detect(QuestAns$Date_of_Question, "day"), gsub("[a-zA-Z]", "", QuestAns$Date_of_Question), "")
QuestAns$Day <- as.numeric(as.character(QuestAns$Day))

#Subtract days from scraping date
QuestAns$datecorrection1 <- as.Date(QuestAns$Scrapting_Date) - QuestAns$Month
QuestAns$datecorrection2 <- as.Date(QuestAns$Scrapting_Date) - QuestAns$Day
QuestAns$datecorrection1and2 <- coalesce(QuestAns$datecorrection1, QuestAns$datecorrection2)

#Open file webarchive
web_archive <- fread('Data/2864_web_archive_org_one_year_level.csv')

#Take first 8 char of URL timestamp
web_archive$Url_Timestamp <-str_sub(web_archive$Url_Timestamp, 1, 8)

#URL timestamp to date format
web_archive$Url_Timestamp <- as.Date(web_archive$Url_Timestamp, "%Y%m%d")

#Months and year to days
web_archive$Month <- ifelse(str_detect(web_archive$Question_Timestamp, "months"), gsub("[a-zA-Z ]", "", web_archive$Question_Timestamp), "")
web_archive$Month <- as.numeric(as.character(web_archive$Month))
web_archive$Month <- web_archive$Month* 30

web_archive$Year <- ifelse(str_detect(web_archive$Question_Timestamp, "year"), 1, "")
web_archive$Year <- as.numeric(as.character(web_archive$Year))
web_archive$Year <- web_archive$Year* 365

web_archive$Day <- ifelse(str_detect(web_archive$Question_Timestamp, "day"), gsub("[a-zA-Z]", "", web_archive$Question_Timestamp), "")
web_archive$Day <- as.numeric(as.character(web_archive$Day))

web_archive$Daysago <- coalesce(web_archive$Month, web_archive$Year, web_archive$Day)

#Subtract days ago from scraping date
web_archive$exactqtimestamp <- web_archive$Url_Timestamp - web_archive$Daysago

#Merge data sets
QuestAns <- merge( x = QuestAns, y = web_archive, by="Book_Id", all.x = TRUE)
QuestAns$cleaned_question_time <- coalesce(QuestAns$datecorrection1and2, QuestAns$exactqtimestamp)

#Remove NA cleaned_question_time
QuestAns <- QuestAns[!is.na(QuestAns$cleaned_question_time),]

#Remove useless columns
QuestAns <- select (QuestAns,-c(6, Answer_Url, Date_of_Answer, Searched_Url.x, Isbn13.x, Scrapting_Date, Month.x, Day.x, datecorrection1, datecorrection2, datecorrection1and2, Title.y, Question.y, Date_of_Question.y, Url_Timestamp, Scraping_Date, Url, Searched_Url.y, Isbn13.y, Month.y, Year, Day.y, Daysago,exactqtimestamp, Question_Timestamp))

#Part 2: Reviews Goodreads
#Open file review data Goodreads
goodreads_reviews = fread("Data/goodreads_reviews.csv", select = c("book_id", "rating", "date_added"))

#Remove weekday
goodreads_reviews$date_added <- gsub('Mon', '',
                                gsub('Tue', '',
                                gsub('Wed', '',
                                gsub('Thu', '',
                                gsub('Fri', '',
                                gsub('Sat', '',
                                gsub('Sun', '', goodreads_reviews$date_added)))))))

#Break up string
goodreads_reviews$month <- substr(goodreads_reviews$date_added, 2, 7)
goodreads_reviews$year <- substr(goodreads_reviews$date_added, 24, 27)
goodreads_reviews$date <- paste(goodreads_reviews$year, goodreads_reviews$month)
goodreads_reviews <- select (goodreads_reviews,-c(year, month, date_added))

#Change month to number
goodreads_reviews$date <- gsub('Jan', '01',
                          gsub('Feb', '02',
                          gsub('Mar', '03',
                          gsub('Apr', '04',
                          gsub('May', '05',
                          gsub('Jun', '06',
                          gsub('Jul', '07', 
                          gsub('Aug', '08',
                          gsub('Sep', '09',
                          gsub('Oct', '10',
                          gsub('Nov', '11',
                          gsub('Dec', '12',     
                          goodreads_reviews$date))))))))))))

#Create date format
goodreads_reviews$date <- as.IDate(goodreads_reviews$date, "%Y%m%d")

#Part 3: Reviews Amazon
# Open file review data Amazon
amazon_reviews <- fread("Data/amazon_reviews.csv")

#Change date format
amazon_reviews$reviewTime <- gsub(",", "", amazon_reviews$reviewTime)
amazon_reviews$reviewTime <- gsub(" ", "/", amazon_reviews$reviewTime)
amazon_reviews$reviewTime <- as.IDate(amazon_reviews$reviewTime, "%m/%d/%y")