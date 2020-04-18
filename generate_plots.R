chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2","gghighlight","jsonlite","ggtext"),
        function(x){
            suppressPackageStartupMessages(library(x, character.only = TRUE))
            x
        },USE.NAMES = FALSE)

# data import -------------------------------------------------------------

# Data updates throughout the day. The total tally for current day only refects post midnight
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


# daily new cases ---------------------------------------------------------
last_plot <- data_india_raw %>% group_by(dateannounced) %>%
    summarise(count = length(dateannounced)) %>%
    filter(dateannounced > Sys.Date()-42) %>%
    ggplot(aes(x=dateannounced, y = count))+
    geom_histogram(stat="identity")+
    theme_bw() +
    scale_y_continuous(expand = c(-Inf,1))+
    scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/%Y",
                 expand = c(0,0))+
    xlab("Date (dd/mm/yyyy)") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(title = "Daily New Cases, India",
         caption = "Data: covid19india.org API")

ggsave(filename = paste0("plots/",Sys.Date(),"_daily_new_cases.png"),
       plot = last_plot ,
       height = 8, width = 8 )

# trends of cumulative for top states ---------------------------------------------------------
paste0("plots/",Sys.Date(),
       "_statewise_confirmed_",1:5,".png") %>%
    sapply(function(i){
        if(file.exists(i))file.remove(i)
    })

data_india_state %>% group_by(detectedstate) %>%
    filter(date == max(date)) %>%  filter(detectedstate != "") %>%
    arrange(confirmed) %>%  .$detectedstate %>%
    chunk2(5) %>%
    lapply(function(i){

        last_plot <- data_india_state %>% group_by(detectedstate) %>%
            filter(date > (Sys.Date()-28)) %>%
            ggplot(aes(x= date , y = confirmed,color = detectedstate, group = detectedstate))+
            geom_line(alpha = 0.6) +
            gghighlight(detectedstate %in% i,
                        use_group_by = FALSE,
                        max_highlight = 20) +
            theme_bw()+
            scale_y_continuous(expand = c(-Inf,1))+
            scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/%Y",
                         expand = c(0,0))+
            xlab("Date (dd/mm/yyyy)") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1,),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_blank()) +
            labs(title = "Cumulative Confirmed Cases",
                 caption = "Data: covid19india.org API")
        for(j in 1:5){
            if(!file.exists(paste0("plots/",Sys.Date(),
                                   "_statewise_confirmed_",j,".png"))){
                break
            }
        }
        ggsave(filename = paste0("plots/",Sys.Date(),
                                 "_statewise_confirmed_",j,".png"),
               plot = last_plot ,
               height = 8, width = 8 )

    })

# National percent new cases + doubling time ---------------------------------------------------------
last_plot <- data_india_state %>% group_by(date) %>%
    summarize(daily = sum(confirmed)) %>%
    mutate(total = cumsum(daily),
           percent_increase = round(100*daily/lag(total), digits = 0),
           doubling_time = round(log(x = 2,base = (1+percent_increase/100)), digits = 1)) %>%
    filter(date > Sys.Date()-21) %>%
    ggplot(aes(x= date , y = percent_increase)) + geom_histogram(stat = "identity") +
    geom_text(aes(x= date, y = percent_increase + 1, label = paste0(percent_increase)), color = "#850c0c") +
    geom_text(aes(x= date, y = percent_increase + 2, label = paste0("(",doubling_time,")")), color = "#30119e") +  theme_bw() +
    labs(title = "<span style='color:#30119e'>Effective Doubling Time (days)</span>",
         subtitle = "<span style='color:#850c0c'>Percentage increase over previous day total (%)</span> ",
         caption = "Data: covid19india.org API") +
    scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/%Y",
                 expand = c(0,0))+
    xlab("Date (dd/mm/yyyy)") +
    theme(plot.subtitle = element_markdown(),
          plot.title = element_markdown(),
          axis.text.x = element_text(angle = 90, hjust = 1,),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

ggsave(filename = paste0("plots/",Sys.Date(),"_national_doubling_time.png"),
       plot = last_plot ,
       height = 8, width = 8 )


# Plot on Testing Numbers -----------------------------------------------------------

testing_data <- read_json("https://api.covid19india.org/data.json",simplifyVector = TRUE)$tested %>%
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
last_plot <- testing_data  %>%
    melt(id.vars = "date") %>% filter(variable != "new_tests") %>%
    ggplot()+
    geom_histogram(aes(x=date , y= value, fill = variable),
                   stat = "identity") +
    geom_text(data = testing_data %>%
                  mutate(percent = round(100*new_positives/new_tests,1)),
              aes(x= date , y = new_tests+ 1000,
                  label = paste0(percent,"%"))) +
    scale_fill_grey() +
    scale_x_date(date_breaks = "3 days" , date_labels = "%d/%m/%Y")+
    labs(title = "New Tests at each ICMR Notification",
         subtitle = "% positive indicated as text",
         caption = "Data: covid19india.org API") +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1,),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position = "bottom",
                       legend.title = element_blank())

ggsave(filename = paste0("plots/",Sys.Date(),"_ICMR_testing.png"),
       plot = last_plot ,
       height = 10, width = 10 )

