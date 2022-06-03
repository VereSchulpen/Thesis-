install.packages("zoo")
library(zoo)
GoodAma$timefixed <- as.yearmon(GoodAma$date)

goodreads <- subset(GoodAma, goodr == "1")
library(lfe)
before_after = felm(rating ~ afterinv | asin, goodreads$timefixed, data = goodreads)
summary(before_after)

before_after_var = felm(rating ~ afterinv + afterinv:varhigh| asin, goodreads$timefixed, data = goodreads)
summary(before_after_var)

before_after_vol = felm(rating ~ afterinv + afterinv:volhigh| asin, goodreads$timefixed, data = goodreads)

goodfic <- subset(GoodAmaFic, goodr == "1")

before_after_fic = felm(rating ~ afterinv + afterinv:fiction2| asin, goodfic$timefixed, data = goodfic)

install.packages("stargazer")
library(stargazer)
stargazer(before_after, before_after_var, before_after_vol, before_after_fic, type = "text")

