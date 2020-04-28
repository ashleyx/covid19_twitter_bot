chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) # stolen from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r

# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite","ggtext"),
        function(x){
            if(x %in% rownames(installed.packages())){
                suppressPackageStartupMessages(library(x, character.only = TRUE))
            }else{
                install.packages(x)
                suppressPackageStartupMessages(library(x, character.only = TRUE))
            }

            x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------------
# DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

# Data updates throughout the day. The total tally for previous day only refects post midnight

# This is the file used for most plots ----------------------------------------------------------------------

data_india_raw <- read_json("https://api.covid19india.org/raw_data.json",simplifyVector = TRUE)$raw_data %>%
    mutate(dateannounced = as.Date(dateannounced, "%d/%m/%y")) %>%
    na.omit()

data_india_district <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
    data_india_raw %>% filter(dateannounced <= d) %>%
        group_by(detecteddistrict) %>%
        summarise(confirmed = length(dateannounced)) %>%
        mutate(date = d)
}) %>%  Reduce(f = rbind)

data_india_state <- unique(data_india_raw$dateannounced) %>% lapply(function(d){
    data_india_raw %>% filter(dateannounced <= d) %>%
        group_by(detectedstate) %>%
        summarise(confirmed = length(dateannounced)) %>%
        mutate(date = d)
}) %>%  Reduce(f = rbind)


cat("https://api.covid19india.org/raw_data.json\n")
# This is the file used for testing numbers  ------------------------------
data_testing_raw <- read_json("https://api.covid19india.org/data.json",simplifyVector = TRUE)
suppressWarnings(data_testing_national <- data_testing_raw $tested %>%
                     transmute(total = as.integer(totalsamplestested),
                               positive = as.integer(totalpositivecases),
                               negative = total - positive,
                               date = as.Date(updatetimestamp,"%d/%m/%Y")) %>%  na.omit() %>%
                     group_by(date) %>%
                     filter( total == max(total)) %>% ungroup() %>%
                     transmute(date,
                               new_tests = total - lag(total),
                               new_positives = positive - lag(positive),
                               new_negatives = new_tests - new_positives) %>%
                     filter(date > Sys.Date()-24)
)
data_testing_state <- read_csv("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv") %>%
    mutate("Updated On" = as.Date(`Updated On`,"%d/%m/%Y")) %>%
    filter(`Updated On` > "2020-03-01")

cat("https://api.covid19india.org/data.json\n")
# This file is used for semi-log plot -------------------------------------

data_india <- read_json("https://api.covid19india.org/states_daily.json",simplifyVector = TRUE)$states_daily %>%
    melt(id.vars = c("status","date")) %>%
    transmute(state = variable,
              date = as.Date(date,"%d-%b-%y"),
              status,
              count = as.integer(value)) %>%
    filter(state != "tt") # removing total tally
data_india$count[is.na(data_india$count)] <- 0

state_mapping <- data.frame(abbrv = strsplit("an,ap,ar,as,br,ch,ct,dd,dn,dl,ga,gj,hr,hp,jk,jh,ka,kl,la,ld,mp,mh,ml,mn,mz,nl,or,py,pb,rj,sk,tn,tg,tr,up,ut,wb", split = ",")[[1]],
                            full = strsplit("Andaman and Nicobar Islands,Andhra Pradesh,Arunachal Pradesh,Assam,Bihar,Chandigarh,Chhattisgarh,Daman and Diu,Dadra and Nagar Haveli,Delhi,Goa,Gujarat,Haryana,Himachal Pradesh,Jammu and Kashmir,Jharkhand,Karnataka,Kerala,Lakshadweep,Ladakh,Madhya Pradesh,Maharashtra,Meghalaya,Manipur,Mizoram,Nagaland,Odisha,Puducherry,Punjab,Rajasthan,Sikkim,Tamil Nadu,Telangana,Tripura,Uttar Pradesh,Uttarakhand,West Bengal",split =",")[[1]])

data_india$state %<>% as.character() %>%
    sapply(function(i) state_mapping$full[state_mapping$abbrv ==i])

cat("https://api.covid19india.org/states_daily.json\n")
cat("All done")
