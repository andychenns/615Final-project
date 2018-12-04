##limited to Bloomberg extracted data

clean.function <- function(raw.data){
  tmp <-rev(raw.data) %>% 
    mutate(Price = na.interpolation(PX_LAST, option = "linear")) %>% 
    mutate(log_return = c(diff(log(Price), lag=1),NA)) %>% 
    mutate(Date = as.Date(Date)) %>% 
    select(Date, Price, log_return) %>%
    head(-1)
}