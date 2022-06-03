est_did = feols(rating ~ afterinv + i(days_bin, goodr) | asin + days_bin, GoodAmaPar)
iplot(est_did)

dif_in_dif = felm(rating ~ afterinv + goodr * afterinv | asin, GoodAma$timefixed, data = GoodAma)
summary(dif_in_dif)

dif_in_dif_varhigh = felm(rating ~ afterinv + goodr*afterinv + goodr:afterinv:varhigh | asin, GoodAma$timefixed, data = GoodAma)
summary(dif_in_dif_varhigh)

dif_in_dif_volhigh = felm(rating ~ afterinv + goodr*afterinv + goodr:afterinv:volhigh | asin, GoodAma$timefixed, data = GoodAma)
summary(dif_in_dif_varhigh)

dif_in_dif_fiction = felm(rating ~ afterinv + goodr*afterinv + goodr:afterinv:fiction2 | asin, GoodAmaFic$timefixed, data = GoodAmaFic)
summary(dif_in_dif_fiction)

stargazer(dif_in_dif, dif_in_dif_volhigh, dif_in_dif_varhigh, dif_in_dif_fiction, type = "text")