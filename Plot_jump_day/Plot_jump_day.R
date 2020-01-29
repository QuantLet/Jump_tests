## install and load packages ##
libraries = c("data.table", "ggplot2", "scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )
## ##

## settings ##
Sys.setenv(LANG = "en") # set environment language to English
Sys.setlocale("LC_TIME", "en_US.UTF-8") # set timestamp language to English
## ##

## read data ##
DT_subs <- fread("bitstamp_may17_btcusd.csv")
## ##

### plot it ###
ggplot(data = DT_subs, mapping = aes(y = p, x = t, group = Symbol, colour = Symbol)) +
  geom_line() +
  plot_theme +
  labs(x = "Time", y = "Exchange rate [USD]" ,
       title = paste("BTC exchange rate (USD) / observed during",
                     as.character(lubridate::month(head(DT_subs$date, 1), label = TRUE, abbr = FALSE)),
                     paste(mday(head(DT_subs$date, 1)),  ",", sep = ""),
                     year(head(DT_subs$date, 1))
       )) +
  scale_x_datetime(limits = c(min = min(DT_subs$t), max = max(DT_subs$t)), expand=c(0.01,0.01),  labels = date_format("%H:%M")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
        plot.margin = margin(10, 10, 10, 85)
  )
