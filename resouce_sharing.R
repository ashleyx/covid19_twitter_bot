# loading packages/libraries ----------------------------------------------

libraries <- c('rtweet','magrittr','readr','stringr','dplyr','lubridate')
if(!all(libraries %in% rownames(installed.packages()))){
    install.packages(libraries)
}
invisible(sapply(libraries, function(i){
    suppressPackageStartupMessages(library(i,character.only = TRUE))
    i
},USE.NAMES = FALSE) )



# top level switches and knobs ---------------------------------------------

dry_run <- FALSE
hourly_tweet_limit <- 95


# location matching things ------------------------------------------------

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

retry_on_fail <- function(fx){
    flag <- TRUE
    while(flag){
        tryCatch(expr = {
            fx()
            flag <- FALSE
        },error = function(e){
            print(e)
            Sys.sleep(60)
        })}
}

update_request_tweets <- function(){
    flag <- FALSE
    if(!all(c('request_timestamp','requests') %in% ls(envir = globalenv()))){
        cat('\nPulling \'request\' tweets:data not found in env\n')
        requests <<- search_tweets(q = "(oxygen OR bed) AND (need OR require OR urgent)", type = "recent",
                                   include_rts = FALSE,
                                   geocode = "21.0,78.0,2200km",
                                   n=1000,
                                   parse = TRUE) %>% as.data.frame()
        #%>%            arrange(created_at)
        flag <- TRUE
        request_timestamp <<- now(tz="Asia/Kolkata")
    }else if(as.numeric(now(tz="Asia/Kolkata")- request_timestamp, units = "mins") > 20){
        cat('\nPulling \'request\' tweets:data timeout since last pull\n')
        requests <<- search_tweets(q = "(oxygen OR bed) AND (need OR require OR urgent)", type = "recent",
                                   include_rts = FALSE,
                                   geocode = "21.0,78.0,2200km",
                                   n=1000,
                                   parse = TRUE) %>% as.data.frame()
        #%>%            arrange(created_at)
        flag <- TRUE
        request_timestamp <<- now(tz="Asia/Kolkata")
    }
    return(flag)
}

update_available_tweets <- function(){
    flag <- FALSE
    if(!all(c('availability_timestamp','available') %in% ls(envir = globalenv()))){
        cat('\nPulling \'available\' tweets:data not found in env\n')
        available <<- search_tweets(q = "(oxygen OR bed) AND (verified OR available)", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,2200km",
                                    n=2500,
                                    parse = TRUE) %>% as.data.frame()
        flag <- TRUE
        availability_timestamp <<- now(tz="Asia/Kolkata")
    }else if(as.numeric(now(tz="Asia/Kolkata")- availability_timestamp, units = "mins") > 60){
        cat('\nPulling \'available\' tweets:data timeout since last pull\n')
        available <<- search_tweets(q = "(oxygen OR bed) AND (verified OR available)", type = "recent",
                                    include_rts = FALSE,
                                    geocode = "21.0,78.0,2200km",
                                    n=2500,
                                    parse = TRUE) %>% as.data.frame()
        flag <- TRUE
        availability_timestamp <<- now(tz="Asia/Kolkata")
    }
    if(flag){
        write_tsv(x = available %>%
                      select(user_id:reply_count),
                  file = paste0("~/work/twtbot/","twtdump_",
                                as.character(now(tzone = "Asia/Kolkata"))))
    }
    return(flag)
}


# processing request tweets -----------------------------------------------

find_best_response <- function(text){
    tweet_post_count <- 0
    query_words <- c("bed",'icu',"ventilator",
                     "oxygen","refill","cylinder","concentrator",
                     "oxygen kit")
    strict_query_words <- c("icu","ventilator")
    if(any(str_detect(text,c("plasma","tocilizumab",
                             "remdesivir","remdisivir")))){
        return(NA)
    }

    text <- tolower(text)
    req_district <- str_extract_all(text,district_pattern)[[1]] %>% unique()
    if(length(req_district) == 0){
        return(NA)
    }
    avail_loc <- available[sapply(available$text, function(i) any(str_detect(i,
                                                                             paste0('\\b',req_district,'\\b')))),]
    if(nrow(avail_loc) == 0){
        return(NA)
    }
    req_queries <- query_words[sapply(query_words,function(i) str_detect(text,i))]
    if(any(strict_query_words %in% req_queries)){
        req_strict_queries <- strict_query_words[strict_query_words %in% req_queries]
        avail_loc <- avail_loc[sapply(avail_loc$text,function(i) str_detect(i,strict_query_words)),]
    }
    scores <- avail_loc$text %>% sapply(function(i) sum(str_detect(i,req_queries)))
    if(all(scores == 0,na.rm = TRUE)){
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

    search_link <- paste0("https://twitter.com/search?q=",
                          "%28%22",
                          paste0(req_district,collapse = "%22%20OR%20%22"),
                          "%22%29")
    req_strict_queries <- strict_query_words[strict_query_words %in% req_queries]
    if(any(strict_query_words %in% req_queries)){ #strict matching for strict_queries
        search_link <- paste0(search_link,
                              "%20AND%20%28%22",
                              paste0(req_strict_queries,collapse = "%22%20AND%20%22"),
                              "%22%29")
    }
    req_queries_alt <- req_queries[!(req_queries %in% strict_query_words)]
    if(length(req_queries_alt)>0){ # relaxed matching for regular query words
        search_link <- paste0(search_link,
                              "%20AND%20%28%22",
                              paste0(req_queries_alt,collapse = "%22%20OR%20%22"),
                              "%22%29")
    }
    search_link <- paste0(search_link,"%20AND%20%22available%22&f=live")#sort by latest
    search_link <- str_replace_all(search_link,"\\s","%20")
    # link <- paste0("https://twitter.com/",
    #                avail_loc$user_id[1:min(nrow(avail_loc),3)],
    #                "/status/",
    #                avail_loc$status_id[1:min(nrow(avail_loc), 3)], collapse = '\n')
    # response <- paste0("Recent tweets for '",
    #                    paste0(req_queries,collapse ="/"),
    #                    "' at '",
    #                    paste0(req_district,collapse = "/"),
    #                    "' : \n ",
    #                    link)
    # if(nchar(response) >= 280){
    #     link <- paste0("https://twitter.com/",
    #                    avail_loc$user_id[1:min(nrow(avail_loc), 2)],
    #                    "/status/",
    #                    avail_loc$status_id[1:min(nrow(avail_loc), 2)], collapse = '\n')
    #     response <- paste0("Recent tweets for '",
    #                        paste0(req_queries,collapse ="/"),
    #                        "' at '",
    #                        paste0(req_district,collapse = "/"),
    #                        "' : \n ",
    #                        link)
    # }
    # post_tweet(status = response,
    #            token = token,
    #            in_reply_to_status_id = requests$status_id[i],
    #            auto_populate_reply_metadata = TRUE)
    # write(paste0(now(tz="Asia/Kolkata"),"\t",requests$status_id[i]),
    #       "processed_tweets.tsv",append = TRUE)
    # tweet_post_count <- tweet_post_count +1

    search_response <- paste0("A twitter search link using '",
                              paste0(req_queries,collapse ="/"),
                              "' at '",
                              paste0(req_district,collapse = "/"),
                              "' is : ",
                              search_link,
                              "\nOn mobile switch to Latest tab to sort by new.",
                              "\n")

    if(not(dry_run)){

        post_tweet(status = search_response,
                   token = token,
                   in_reply_to_status_id = requests$status_id[i],
                   auto_populate_reply_metadata = TRUE)
        write(paste0(now(tz="Asia/Kolkata"),"\t",requests$status_id[i]),
              "processed_tweets.tsv",append = TRUE)
        tweet_post_count <- tweet_post_count +1

    }

    return(tweet_post_count)
}

# the persistently running code -----------------------------------------------------

count_retires <- 0
while(TRUE){

    processed_tweets <- read_tsv("processed_tweets.tsv",
                                 col_types = cols(
                                     time = col_datetime(format =  "%Y-%m-%d %H:%M:%S"),
                                     status_id = col_character()
                                 )) %>% as.data.frame()
    processed_tweets$time <- force_tz(processed_tweets$time,tz = "Asia/Kolkata")

    #snooze cycle: waiting for ~1 hour since last tweet to retry
    count_posted <-  sum(as.numeric(now(tz="Asia/Kolkata") - processed_tweets$time,units = "mins") < 60)
    while(count_posted > (hourly_tweet_limit/2)){
        snooze_duration <- as.numeric(max(processed_tweets$time) + 3600 - now(tz="Asia/Kolkata"),units = "secs")+60
        cat('\nSnoozing for ',as.character(snooze_duration %/% 60),' minutes to stay under hourly posting limit')
        Sys.sleep(snooze_duration)
        count_posted <-  sum(as.numeric(now(tz="Asia/Kolkata") - processed_tweets$time,units = "mins") < 60)
    }

    retry_on_fail(update_request_tweets)
    requests %<>% filter(!(status_id %in% processed_tweets$status_id))
    #TODO add consensus filtering here

    #setting up the while loop
    i <-  1
    count_posted <- sum(as.numeric(now(tz="Asia/Kolkata") - processed_tweets$time,units = "mins") < 60)
    time_posted <- processed_tweets$time[as.numeric(now(tz="Asia/Kolkata") - processed_tweets$time,units = "mins") < 60]

    while(count_posted <= hourly_tweet_limit & i <= nrow(requests)){
        retry_on_fail(update_available_tweets)

        count_posted <- count_posted - sum( as.numeric(now(tz="Asia/Kolkata") - time_posted, units = "mins") > 60)
        time_posted <- time_posted[as.numeric(now(tz="Asia/Kolkata") - time_posted,units = "mins") < 60]

        #finding and posting a response
        tryCatch(expr = {
            response <- find_best_response(requests$text[i])
        },
        error = function(e){
            print(e)
            response <- NA
        })
        if(is.na(response)){
            i = i+1
        }else{
            i <- i+1
            count_posted <- count_posted + response
            time_posted <- c(time_posted,rep(now(tz="Asia/Kolkata"),response))
            count_retires <- 0
        }
    }

    if(count_posted >= hourly_tweet_limit){ #This doesnt really serve a purpose
        cat('\nHit hourly posting limit ')
    }

    #alerting if requests are exhausted before posting limit; need to handle this case better
    if(i >= nrow(requests)){
        cat('\nEntire request batch has been parsed ')
        count_retires <- count_retires + 1
    }

    if(count_retires >= 3){
        cat('\nFound nothing to do in multiple retries, snoozing for 5 mins and retrying\n')
        Sys.sleep(300)
    }

}
