library(rtweet)
library(magrittr)

token <- get_token()

requests <- search_tweets(q = "(oxygen OR bed) AND (needed OR required)", type = "recent",
                        include_rts = FALSE,
                        geocode = "21.0,78.0,1900km",
                        n=100,
                        parse = TRUE)
