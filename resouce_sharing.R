library(rtweet)
library(magrittr)
library(readr)
library(stringr)
library(dplyr)

district_data <- read_tsv("GADM.tsv")
additional_districts <- read_lines("districts_extra.txt")
district_pattern <- paste0(c(tolower(district_data$DISTRICT),additional_districts),collapse ='|')
# state_pattern <- paste0(tolower(district_data$ST_NM) %>% unique(),collapse ='|')

if(!file.exists("processed_tweets.tsv")){
    file.create("processed_tweets.tsv")
    write("time\tstatus_id","processed_tweets.tsv")
}

api_keys <- readLines("~/.twitter_keys")
token <- create_token(
    app = "R-programming_interface",
    consumer_key = api_keys[1],
    consumer_secret = api_keys[2],
    access_token = api_keys[3],
    access_secret = api_keys[4])



# database update functions -----------------------------------------------
update_request_tweets <- function(){
    if(!all(c('request_timestamp','requests') %in% ls())){
        requests <- search_tweets(q = "(oxygen OR bed) AND (needed OR required) AND urgent", type = "recent",
                                  include_rts = FALSE,
                                  geocode = "21.0,78.0,1900km",
                                  n=1000,
                                  parse = TRUE) %>% as.data.frame()

        request_timestamp <<- Sys.time()
    }else if(as.numeric(Sys.time()- availability_timestamp, units = "mins") > 20){
        requests <- search_tweets(q = "(oxygen OR bed) AND (needed OR required) AND urgent", type = "recent",
                                  include_rts = FALSE,
                                  geocode = "21.0,78.0,1900km",
                                  n=1000,
                                  parse = TRUE) %>% as.data.frame()

        request_timestamp <<- Sys.time()
    }
}

update_available_tweets <- function(){
    if(!all(c('availability_timestamp','available') %in% ls())){
        available <<- search_tweets(q = "(oxygen OR bed) AND verified", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,1900km",
                                    n=10000,
                                    parse = TRUE) %>% as.data.frame()
        availability_timestamp <<- Sys.time()
    }else if(as.numeric(Sys.time()- availability_timestamp, units = "mins") > 20){
        available <<- search_tweets(q = "(oxygen OR bed) AND verified", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,1900km",
                                    n=10000,
                                    parse = TRUE) %>% as.data.frame()
        availability_timestamp <<- Sys.time()
    }
}


# processing request tweets -----------------------------------------------

find_best_response <- function(text){
    text <- tolower(text)
    req_district <- str_extract_all(text,district_pattern)[[1]] %>% unique()
    if(length(req_district) == 0){
        return(NA)
    }
    update_available_tweets()
    query_words <- c("bed",'icu',"ventilator",
                     "oxygen","refill","cylinder")
    avail_loc <- available %>% filter(any(str_detect(text,req_district)))
    if(nrow(avail_loc) == 0){
        return(NA)
    }
    req_queries <- query_words[sapply(query_words,function(i) str_detect(text,i))]
    scores <- avail_loc$text %>% sapply(function(i) sum(sapply(req_queries,function(j) str_detect(i,j))))
    if(all(scores == 0)){
        return(NA)
    }
    avail_loc %<>% filter(scores == max(scores))
    link <- paste0("https://twitter.com/",
                   avail_loc$user_id[1],
                   "/status/",
                   avail_loc$status_id)
    response <- paste0("Most recent tweet found for ",
                       paste0(req_queries,collapse ="/"),
                       " at ",
                       paste0(req_district,collapse = "/"),
                       " is: \n ",
                       link)
    if(nchar(response) <= 280){
        return(response)
    }else{
        return(link)
    }
}

while(TRUE){
    update_request_tweets()
    processed_tweets <- read_tsv("processed_tweets.tsv") %>% as.data.frame()
    requests %<>% filter(!(status_id %in% processed_tweets$status_id))
    i = 1
    count = 0
    while(i <= nrow(requests)){
        response <- find_best_response(requests$text[i])
        if(is.na(response)){
            i = i+1
        }else{
            write(paste0(Sys.time(),"\t",requests$status_id[i]),
                  "processed_tweets.tsv",append = TRUE)
            post_tweet(status = response,
                       token = token,
                       in_reply_to_status_id = requests$status_id[i],
                       auto_populate_reply_metadata = TRUE)
            i = i+1
            count = count + 1
        }
        flag = TRUE
        if(count >= 90){
            break
        }
        while(flag){
            count_processed <- processed_tweets %>% filter(time < (Sys.time() - 3600)) %>% nrow()
            if((count_processed + count) < 90){
                flag = FALSE
            }else{
                Sys.sleep(300)
            }
        }
    }
}
