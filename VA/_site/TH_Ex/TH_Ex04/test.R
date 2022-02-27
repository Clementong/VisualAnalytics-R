
packages = c('plotly','Quandl','rvest','quantmod','tidyquant','rmarkdown','tidyr','tidyverse',
             'data.table','XML','xml2','httr','dplyr','knitr', 'ggHoriPlot', 'ggthemes', 'patchwork', 'ggTimeSeries')

for (p in packages) {
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}
#Reading the Data
stockprices.data <- read.csv('./data/stockprices.csv', header = TRUE)
# top 40 companies 
top_40 = c(stockprices.data$Symbol)

# https://giniceseah.netlify.app/posts/2021-06-18-scraping-of-financial-dataset/#tidyquant-package
from_date = "2019-12-31"
to_date = "2021-12-31"
period_type = "days"  # "days"/ "weeks"/ "months"/ "years"

main_df = data.table()
df_list = list()

for(i in top_40){
  
  benchmark_data_daily = tq_get(i,
                                get = "stock.prices",
                                from = from_date,
                                to = to_date) %>%tq_transmute(select  = NULL, 
                                                              mutate_fun = to.period, 
                                                              period  = period_type)
  
  benchmark_data_daily$Symbol = replicate(nrow(benchmark_data_daily), i)
  main_df = rbind(main_df,benchmark_data_daily )
}

main_df$date <- format(as.Date(main_df$date, format="%Y-%m-%d"))
main_df$Year <- format(as.Date(main_df$date, format="%Y-%m-%d"),"%Y")
main_df$month <- format(as.Date(main_df$date, format="%Y-%m-%d"),"%m")

write.csv(main_df,"./data/main_df.csv", row.names = FALSE)

main_df <- read.csv('./data/main_df.csv', header = TRUE)

# checking for missing values not present 
print(main_df[rowSums(is.na(main_df)) >= 0.7,])

# ensuring that all have the same time frame 
main_df_length <- main_df %>% group_by(Symbol) %>%
  summarise(n=n()) %>%
  ungroup()

# notice some are less than 504 

# 
symbol_list_names <- main_df_length %>% 
  select(Symbol) %>%
  filter(main_df_length$n >= 504)
symbol_list_names <- as.list(symbol_list_names$Symbol)
main_df <- subset(main_df, Symbol %in% symbol_list_names)

# Gghoriplot  

main_df$performance <- (main_df$close - main_df$open)/main_df$open * 100

p1 <- main_df %>%
  ggplot() +
  geom_horizon(aes(as.Date(date),
                   adjusted), origin = 'mean', horizonscale = 4, show.legend = TRUE)+
  scale_fill_hcl(palette = 'RdBu') +
  facet_grid(Symbol~.) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b%y") +
  ggtitle('Drastic Changes to the Performance of Stocks in March 2020 to April 2020',
          'Horiztonal Area Chart Representing the Stock Performance of the Top 40 Companies of Singapore from 1 Jan 2020 to 31 Dec 2021') +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7.2, angle = 0, hjust = 0),
    plot.title = element_text(size=10),
    plot.subtitle = element_text(size=8),
    legend.title = element_text(size=7),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    
  ) +
  guides(fill=guide_legend(title="Performance Value Gradient"),)
p1

