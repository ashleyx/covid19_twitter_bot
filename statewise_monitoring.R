# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","reshape2","ggplot2","jsonlite"),
        function(x){
          if(!x %in% rownames(installed.packages())){
            install.packages(x)
          }
          suppressPackageStartupMessages(library(x, character.only = TRUE))

          x
        },USE.NAMES = FALSE)

#  -------------------------------------------------------- -----
# RAW DATA IMPORT -------------------------------------------------------------
#  -------------------------------------------------------------

if(!all(c("data_india_raw","data_state") %in% ls())){
  source("load_data.R")
}

dir.create(paste0("statewise_plot/",Sys.Date()),recursive = TRUE)
date_start <- as.Date("2020-03-29")

data_state %<>% group_by(status,state) %>%
  mutate(tally = cumsum(count)) %>% filter(status == "Confirmed",
                                           date > date_start)

#  -------------------------------------------------------------
#  Function to simulate  -------------------------------------------------------------
#  -------------------------------------------------------------

simulate_cases <- function(data_cumulative, date_predict, n_days,
                           estimation_window = 3,iters = 1000, simplify = TRUE ){

  # estimation of K = Rt/serial_interval --------------------------------------------------------

  K <- (log(data_cumulative$tally[data_cumulative$date == (date_predict - 1)]) -
          log(data_cumulative$tally[data_cumulative$date == (date_predict - estimation_window)]) ) /
    (estimation_window - 1)

  # Running simulations --------------------------------------------------------
  bootstrap <- matrix(0,nrow = (n_days + 1), ncol = iters) -> bootstrap_new_cases
  bootstrap[1,] <- data_cumulative$tally[data_cumulative$date == (date_predict - 1)]

  for(i in 2:(n_days+1)){
    expected <- bootstrap[i-1,] %>% sapply(function(j){
      j*exp(K) - j
    })

    bootstrap_new_cases[i,] <- expected %>%
      sapply(function(j){
        rpois(1,j)
      })
    bootstrap[i,] <- bootstrap[i-1,] + bootstrap_new_cases[i,]
  }

  if(n_days == 1){
    predicted <- data.frame(day = 1,
                            quantile(bootstrap_new_cases[-1,],probs= c(0.025,0.5,0.975)) %>%  t) %>%
      transmute(date = date_predict + day -1,
                lower = X2.5. ,
                mean = X50.,
                upper = X97.5.)
  }else{
    predicted <- data.frame(day = 1:n_days,
                            apply(bootstrap_new_cases[-1,],1,quantile,probs= c(0.025,0.5,0.975)) %>%  t) %>%
      transmute(date = date_predict + day -1,
                lower = X2.5. ,
                mean = X50.,
                upper = X97.5.)
  }


  if(simplify){
    return(predicted)
  }else{
    return(bootstrap_new_cases)
  }

}
#  -------------------------------------------------------------
#  Looping over states -------------------------------------------------------------
#  -------------------------------------------------------------

data_state %>% group_by(state) %>%
  summarise(n = max(tally)) %>% {
    .$state[.$n > 400]
  } %>% lapply(function(s){

    print(s)
    data <- data_state %>% filter(state == s)
    #  -------------------------------------------------------------
    #  Simulations -------------------------------------------------------------
    #  -------------------------------------------------------------

    predicted <- lapply((Sys.Date() - 1:7),function(d){
      simulate_cases(data_cumulative = data,
                     date_predict = d,
                     n_days = 1)

    }) %>%
      Reduce(f = bind_rows , x = .) %>%
      mutate(mean = NA) %>%
      bind_rows(simulate_cases(data_cumulative = data,
                               date_predict = Sys.Date(),
                               n_days = 7))
    #  -------------------------------------------------------------
    #  Plotting -------------------------------------------------------------
    #  -------------------------------------------------------------

    K <- (log(data$tally[data$date == (Sys.Date() - 1)]) -
            log(data$tally[data$date == (Sys.Date() - 7)]) ) /
      (7 - 1)


    final_plot <- data %>%  filter(status == "Confirmed") %>%
      ggplot(aes(x= date)) +
      geom_histogram(aes(y = count),stat = "identity", fill = "#999999")+
      geom_histogram(data = predicted,aes(x = date, y = mean), fill = "#CCCCCC", stat = "identity")+
      geom_errorbar(data = predicted, aes( ymin = lower, ymax = upper))+
      labs(title = s,
           subtitle = paste0("7 day average doubling time: ", round(log(2)/K, 1), " days"))+
      # geom_ribbon(data = predicted, aes( ymin = lower, ymax = upper), alpha = 0.3)+
      # geom_line(data = predicted,aes(x = date, y = mean), linetype = 2) +
      theme_bw()

    ggsave(paste0("statewise_plot/",Sys.Date(),"/",Sys.Date(),"_",gsub(" ","-",s),".png"),
           plot = final_plot,
           width = 7 , height = 7)

  }) %>%  invisible()


