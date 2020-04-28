library(rtweet)
library(magrittr)

token <- get_token()
get_last_tweet_id <- function(){
    get_timeline("AshleysBot2")$status_id[1]
}
source("generate_plots.R")


post_tweet(status = paste0("Covid Monitoring for India: ",Sys.Date(),""),
           token = token)

list.files("plots", full.names = TRUE,
           pattern = Sys.Date() %>%  as.character()) %>%
    sapply(function(i){
        post_tweet(status = gsub(paste0("plots/",Sys.Date(),"_|.png"),"",x = i) %>%
                       gsub("_"," ",x=.) %>% paste0(collapse = ""),
                   token = token,
                   media = i,
                   in_reply_to_status_id = get_last_tweet_id())
    }) %>% invisible()

post_tweet(status = "Update Sucessful @theashleyxavier",
           token = token,
           in_reply_to_status_id = get_last_tweet_id())
