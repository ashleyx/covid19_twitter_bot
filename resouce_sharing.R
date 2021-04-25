libraries <- c('rtweet','magrittr','readr','stringr','dplyr','lubridate')
if(!all(libraries %in% rownames(installed.packages()))){
    install.packages(libraries)
}
invisible(sapply(libraries, function(i){
    suppressPackageStartupMessages(library(i,character.only = TRUE))
    i
},USE.NAMES = FALSE) )
district_data <- read_tsv("GADM.tsv",
                          col_types = cols(
                              DISTRICT = col_character(),
                              ST_NM = col_character()
                          ))
#districts_extra is where i'm dumping all alternate spellings i find
additional_districts <- read_lines("districts_extra.txt") %>% unique()
district_pattern <- paste0(c(tolower(district_data$DISTRICT),additional_districts),
                           collapse ='\\b|\\b#?')
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
    if(!all(c('request_timestamp','requests') %in% ls(envir = globalenv()))){
        cat('\nPulling \'request\' tweets:data not found in env\n')
        requests <<- search_tweets(q = "(oxygen OR bed) AND (need OR require OR urgent)", type = "recent",
                                   include_rts = FALSE,
                                   geocode = "21.0,78.0,2200km",
                                   n=1000,
                                   parse = TRUE) %>% as.data.frame()

        request_timestamp <<- now(tz="Asia/Kolkata")
    }else if(as.numeric(now(tz="Asia/Kolkata")- request_timestamp, units = "mins") > 20){
        cat('\nPulling \'request\' tweets:data timeout since last pull\n')
        requests <<- search_tweets(q = "(oxygen OR bed) AND (need OR require OR urgent)", type = "recent",
                                   include_rts = FALSE,
                                   geocode = "21.0,78.0,2200km",
                                   n=1000,
                                   parse = TRUE) %>% as.data.frame()

        request_timestamp <<- now(tz="Asia/Kolkata")
    }
}

update_available_tweets <- function(){
    if(!all(c('availability_timestamp','available') %in% ls(envir = globalenv()))){
        cat('\nPulling \'available\' tweets:data not found in env\n')
        available <<- search_tweets(q = "(oxygen OR bed) AND (verified OR available)", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,2200km",
                                    n=2500,
                                    parse = TRUE) %>% as.data.frame()
        availability_timestamp <<- now(tz="Asia/Kolkata")
    }else if(as.numeric(now(tz="Asia/Kolkata")- availability_timestamp, units = "mins") > 60){
        cat('\nPulling \'available\' tweets:data timeout since last pull\n')
        available <<- search_tweets(q = "(oxygen OR bed) AND (verified OR available)", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,2200km",
                                    n=2500,
                                    parse = TRUE) %>% as.data.frame()
        availability_timestamp <<- now(tz="Asia/Kolkata")
    }
}


# processing request tweets -----------------------------------------------

find_best_response <- function(text){
    text <- tolower(text)
    req_district <- str_extract_all(text,district_pattern)[[1]] %>% unique()
    if(length(req_district) == 0){
        return(NA)
    }
    query_words <- c("bed",'icu',"ventilator",
                     "oxygen","refill","cylinder","concentrator")
    avail_loc <- available[sapply(available$text, function(i) any(str_detect(i,
                                                                             paste0('\\b',req_district,'\\b')))),]
    if(nrow(avail_loc) == 0){
        return(NA)
    }
    req_queries <- query_words[sapply(query_words,function(i) str_detect(text,i))]
    scores <- avail_loc$text %>% sapply(function(i) sum(sapply(req_queries,function(j) str_detect(i,j))))
    if(all(scores == 0)){
        return(NA)
    }
    avail_loc$text %<>% tolower()
    avail_loc %<>% filter(scores == max(scores,na.rm = TRUE),
                          str_detect(text,'need|require',negate = TRUE),
                          !(is.na(user_id) | user_id == 'NA'),
                          !(is.na(status_id) | status_id == 'NA'))
    if(is.na(avail_loc$user_id[1]) | is.na(avail_loc$user_id[1])){
        return(NA)
    }
    link <- paste0("https://twitter.com/",
                   avail_loc$user_id[1],
                   "/status/",
                   avail_loc$status_id[1])
    response <- paste0("Recent tweet found for '",
                       paste0(req_queries,collapse ="/"),
                       "' at '",
                       paste0(req_district,collapse = "/"),
                       "' : \n ",
                       link)
    if(nchar(response) <= 280){
        return(response)
    }else{
        return(link)
    }
}


# the persistent code -----------------------------------------------------


count_retires <- 0
while(TRUE){
    update_request_tweets()
    processed_tweets <- read_tsv("processed_tweets.tsv",
                                 col_types = cols(
                                     time = col_datetime(format =  "%Y-%m-%d %H:%M:%S"),
                                     status_id = col_character()
                                 )) %>% as.data.frame()
    processed_tweets$time <- force_tz(processed_tweets$time,tz = "Asia/Kolkata")
    requests %<>% filter(!(status_id %in% processed_tweets$status_id))
    i <-  1
    count_posted <- 0
    time_posted <- c()
    while(i <= nrow(requests)){
        update_available_tweets()
        response <- find_best_response(requests$text[i])
        count_posted <- count_posted - sum( as.numeric(now(tz="Asia/Kolkata") - time_posted, units = "mins") > 60)
        time_posted <- time_posted[as.numeric(now(tz="Asia/Kolkata") - time_posted,units = "mins") < 60]

        if(is.na(response)){
            i = i+1
        }else{
            post_tweet(status = response,
                       token = token,
                       in_reply_to_status_id = requests$status_id[i],
                       auto_populate_reply_metadata = TRUE)
            write(paste0(now(tz="Asia/Kolkata"),"\t",requests$status_id[i]),
                  "processed_tweets.tsv",append = TRUE)
            i <- i+1
            count_posted <- count_posted + 1
            time_posted <- c(time_posted,now(tz="Asia/Kolkata"))
            count_retires <- 0
            cat(as.character(count_posted),'\n')
        }
        if(count_posted >= 90){
            break
        }
        flag = TRUE
        while(flag){
            count_processed <-  sum(as.numeric(now(tz="Asia/Kolkata") - processed_tweets$time,units = "mins") < 60)
            if((count_processed + count_posted) <= 90){
                flag = FALSE
            }else{
                cat('\nHit posting limit, snoozing for 5 mins and retrying')
                cat('\nprocessed_count: ',as.character(count_processed),
                    '\t count_posted: ',as.character(count_posted))
                cat('\nCurrent request batch progress',as.character(100*i/nrow(requests)),'%\n')
                Sys.sleep(300)
                count_posted <- count_posted - sum( as.numeric(now(tz="Asia/Kolkata") - time_posted, units = "mins") > 60)
                time_posted <- time_posted[as.numeric(now(tz="Asia/Kolkata") - time_posted,units = "mins") < 60]
            }
        }
        if(i == nrow(requests)){
            cat('\nExhausted search on all pulled \'request\' tweets\n')
            count_retires <- count_retires +1
        }
        if(count_retires >= 3){
            cat('\nFound nothing to do in multiple retries, snoozing for 5 mins and retrying\n')
            Sys.sleep(300)
        }
    }
}
