
# Loading the libraries 
packages = c('tidyverse', 'readxl', 'knitr', 'hms', 'kableExtra', 'readr')
for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# Loading the data 

orders <- read_xls("./data/Sample - Superstore.xls", sheet='Orders')
returns <- read_xls("./data/Sample - Superstore.xls", sheet='Returns')

join_table <- left_join(returns, orders,
                        by = c('Order ID' = 'Order ID'))

# Pareto chart Category | Frequency | Cumulative Freq | CumPercent

pareto_df <- join_table %>% 
                group_by(`Sub-Category`) %>% 
                  summarise('Returns'=n()) %>% 
                  arrange(desc(Returns)) %>%
                  mutate(cumfreq = cumsum(Returns), cumperc = cumfreq/nrow(join_table) * 100) %>%
                      ungroup() 


pareto_df$`Sub-Category` = ordered(pareto_df$`Sub-Category`, levels= unlist(pareto_df$`Sub-Category`, use.names = F))

# 
# brks <- seq(0, (322.6*10), 322.6)

# ggplot(pareto_df, aes(x=`Sub-Category`)) +
#   geom_bar(aes(y=Returns), fill='blue', stat="identity") +
#   geom_path(aes(y=cumfreq/scaleRight, group=1),colour="red", size=0.9) +
#   geom_point(aes(y=cumfreq/scaleRight, group=1),colour="red") +
#   scale_y_continuous(breaks=brks ,sec.axis = sec_axis(~.*scaleRight, name = "Cumulative (%)")) +
#   theme(axis.text.x = element_text(angle=90, vjust=0.5)) +
#   labs(title="Pareto Chart", subtitle="SNS Hyspec Background Contributions", x="Background Source", y="Frequnecy of Categories")
# 

N = sum(pareto_df$Returns)
y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
nr <- nrow(pareto_df)
Df_ticks <- data.frame(xtick0 = rep(nr +.6, 11), xtick1 = rep(nr +.8, 11), 
                       ytick = seq(0, N, N/10))


ggplot(pareto_df, aes(x=`Sub-Category`, y=Returns)) + 
  geom_bar(aes(y=Returns), fill='lightblue', stat="identity") +
  geom_path(aes(y=cumperc, group=1), color="black", size=0.9) +
  geom_point(aes(x=`Sub-Category`, y = cumperc,color="black") + # pch is typ of pointer 
      scale_y_continuous(breaks=seq(0, N, N/10))  +
      scale_x_discrete(breaks = pareto_df$`Sub-Category`) +
      guides(fill = FALSE, color = FALSE, scale="none") +
      annotate("rect", xmin = nr + 1, xmax = nr + 2, 
               ymin = -.03 * N, ymax = N * 1.02, fill = "white") + # create the space 
      annotate("text", x = nr +1.2, y = seq(0, N, N/10), label = y2, size = 2.5) + # create the labels
      geom_segment(x = nr + 0.6, xend = nr+0.6, y = -.02 * N, yend = N * 1.02 , color = "grey50") + # create the line 
      geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +  # create the ticks 
      
      labs(title="Pareto Chart - Returns by sub-category", subtitle="count of the number of returns by sub-category", 
           x="sub-category", y="absolute frequency") +
      theme_bw() +
      theme(axis.text.x = element_text(size=rel(0.8))
            ,axis.title.x = element_text(size=rel(1))
            ,axis.text.y = element_text(size=rel(1))
            ,axis.title.y = element_text(size=rel(1))
            )

# sec axis - build in https://ggplot2.tidyverse.org/reference/sec_axis.html
# segement : https://ggplot2.tidyverse.org/reference/geom_segment.html
# adjusting scale y : https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
# expand function of y : https://stackoverflow.com/questions/56712461/how-to-increase-the-padding-around-a-plot
# geom text : https://ggplot2.tidyverse.org/reference/annotate.html  
# Population Pyramid

respo_df <- read_csv("./data/respopagesextod2021.csv")
respo_df$AG <- factor(respo_df$AG, ordered=TRUE ,levels=c("0_to_4","5_to_9","10_to_14",
                                                          "15_to_19","20_to_24","25_to_29",
                                                          "30_to_34","35_to_39","40_to_44",
                                                          "45_to_49","50_to_54","55_to_59",
                                                          "60_to_64","65_to_69","70_to_74",
                                                          "75_to_79","80_to_84","85_to_89","90_and_over"))

# GET DATA
ag_df <- respo_df %>% 
  select(AG, Sex, Pop) %>%
  group_by(AG,Sex) %>% 
  summarise(Total = sum(Pop)) %>%
  arrange(Sex,AG) %>%
  ungroup()
  
# All males are negative so they go to the left
ag_df$Total <- ag_df$Total/1000
ag_df$Total <- ifelse(ag_df$Sex == "Males", -1*ag_df$Total, ag_df$Total)

# creating ticks 
N = max(ag_df$Total)

#y_label =
  
  
ggplot(ag_df, aes(x = AG, fill = Sex,
                 y = Total)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(n.breaks=18, labels=abs, limits = N * c(-1,1))+
  coord_flip() +
  scale_colour_manual(values = c("pink", "steelblue"),
                      aesthetics = c("colour", "fill")) +
  labs(title="Constrictive Population Pyramid of ", subtitle="Population by Age and Gender ", 
       x="Age Groups", y="Number of People (In Thousands)") +
  theme_bw()


ggplot(ag_df, aes(x = AG, y = Total, fill = Sex)) + 
  geom_bar(subset = subset(ag_df, Sex=="Females"), stat = "identity") + 
  geom_bar(subset = subset(ag_df, Sex=="Males"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-170000.0, 170000.0, 50000.0),
                     labels = paste0(as.character(c(seq(170, 0, -50), seq(50, 170, 50))),"k"))+
  coord_flip()
  theme_bw()



#detach(package:plyr) do this if summarise dont work
# ggplot(ag_df, aes(x = AG, y = Total, fill = Sex)) + 
#   geom_bar(data= subset(ag_df, Sex == "Females"), stat = "identity") + 
#   geom_bar(data= subset(ag_df,Sex == "Males"), stat = "identity") + 
#   scale_y_continuous(breaks=seq(-N,N,20000),labels=abs(seq(-N,N,20000)))+ # now this is the x when coord flips 
#   guides(fill = FALSE, color = FALSE, scale="none") +
#   coord_flip()+ 
#   scale_fill_brewer(palette = "Set1") + 
#   labs(title="Pareto Chart of returns by sub-category", subtitle="count of the number of returns by sub-category", 
#        x="sub-category", y="absolute frequency") +
#   theme_bw()