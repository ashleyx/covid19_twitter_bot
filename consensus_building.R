libraries <- c('rtweet','magrittr','readr','stringr','dplyr','lubridate')
if(!all(libraries %in% rownames(installed.packages()))){
    install.packages(libraries)
}
invisible(sapply(libraries, function(i){
    suppressPackageStartupMessages(library(i,character.only = TRUE))
    i
},USE.NAMES = FALSE) )

api_keys <- readLines("~/.twitter_keys")
token <- create_token(
    app = "R-programming_interface",
    consumer_key = api_keys[1],
    consumer_secret = api_keys[2],
    access_token = api_keys[3],
    access_secret = api_keys[4])

stack <- search_tweets(q = "\"wrYeG7RAYeYM7WxuXyXM\" AND (\"LOCK\" OR \"UNLOCK\")", type = "recent",
                       include_rts = FALSE,
                       n=1000,
                       parse = TRUE) %>% as.data.frame()

broadcast_format <- "^IGNORE THIS;(LOCK)|(UNLOCK);\\d+;\\d+;wrYeG7RAYeYM7WxuXyXM$"

stack %>%
    filter(str_detect(text,broadcast_format)) %>%
    arrange(created_at) %>% .$text %>%
    read_delim(file = ., delim = ";",col_names = c("ignore",'mode','start','end','key')) %>%
    select(mode, start,end)
sapply(stack$status_id, function(i) post_tweet(destroy_id = i))
