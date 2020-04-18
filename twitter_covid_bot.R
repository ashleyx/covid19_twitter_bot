library(rtweet)
library(magrittr)

token <- get_token()
get_last_tweet_id <- function(){
    get_timeline("AshleysBot2")$status_id[1]
}

post_tweet(status = "Testing normal tweet @theashleyxavier",
           token = token)

post_tweet(status = "Testing media atachment",
           token = token,
           in_reply_to_status_id = get_last_tweet_id(),
           media = "~/Pictures/vlad-tchompalov-NpQSAv29evU-unsplash.jpg")

