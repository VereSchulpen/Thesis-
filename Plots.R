hist_treat_GoodAma <- GoodAma$rating
hist_treat_GoodAma <-as.numeric(hist_treat_GoodAma)
hist(hist_treat_GoodAma, breaks = 5, freq = FALSE, main = "Rating Distribution", xlab = "rating")

hist_treat_Good <- goodreads_reviews$rating
hist_treat_Good <-as.numeric(hist_treat_Good)
hist(hist_treat_Good, breaks = 5, freq = FALSE, main = "Rating Distribution on Goodreads", xlab = "rating")

hist_treat_Ama <- amazon_reviews$overall
hist_treat_Ama <-as.numeric(hist_treat_Ama)
hist(hist_treat_Ama, breaks = 5, freq = FALSE, main = "Rating Distribution on Amazon", xlab = "rating")


barplot(table(QuestAns$Total_Number_of_Questions), main="Number of Questions per Book",
        xlab="Questions", ylab= "Frequency", col="grey")
QuestAns$Number_of_Answers[is.na(QuestAns$Number_of_Answers)] <- 0
barplot(table(QuestAns$Number_of_Answers), main="Number of Answers per Question",
        xlab="Answers", ylab= "Frequency", col="grey")

summary(QuestAns)