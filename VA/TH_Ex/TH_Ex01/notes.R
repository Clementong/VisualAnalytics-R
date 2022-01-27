# understanding geom segment
b <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

df <- data.frame(x1 = 6, x2 = 6, y1 = 0, y3 = 35)
b +
  geom_segment(aes(x = x1, y = y1, xend = x1, yend = y3, colour = "segment"  ), data = df) +
  annotate("text", 18.2, 8.2, label="(8, 8.03)", size=3)

# creating a factor variable:
Example <- rep(c(letters[1:2], LETTERS[1:3]), c(15, 39, 6, 42, 50))

# implementing the function:

  x <- Example
  title <- deparse(substitute(x))
  
  x <- data.frame(modality = na.omit(x))
  
  library(dplyr)
  
  Df <- x %>% group_by(modality) %>% summarise(frequency=n()) %>% 
    arrange(desc(frequency))
  
  Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
  
  Df <- Df %>% mutate(modality_int = as.integer(modality), 
                      cumfreq = cumsum(frequency), cumperc = cumfreq/nrow(x) * 100)
  nr <- nrow(Df)
  N <- sum(Df$frequency)
  
  # xtick 0 and xtick 1 is to create the ticks space 
  Df_ticks <- data.frame(xtick0 = rep(nr +.55, 11), xtick1 = rep(nr +.59, 11), 
                         ytick = seq(0, N, N/10))
  
  
  
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  
  library(ggplot2)
  
ggplot(Df, aes(x=modality, y=frequency)) + 
    geom_bar(stat="identity", aes(fill = modality_int)) +
    geom_line(aes(x=modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x=modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks=seq(0, N, N/10), limits=c(-.02 * N, N * 1.02)) + 
    scale_x_discrete(breaks = Df$modality) +
    guides(fill = FALSE, color = FALSE) + 
    annotate("rect", xmin = nr + .55, xmax = nr + 1, 
             ymin = -.02 * N, ymax = N * 1.02, fill = "white") + # create the space 
    annotate("text", x = nr + .8, y = seq(0, N, N/10), label = y2, size = 3.5) + # create the labels
    geom_segment(x = nr - 2, xend = nr - 2, y = -.02 * N, yend = N * 1.02, color = "grey50") + # create the line 
    geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +  # create the ticks 
    labs(title = paste0("Pareto Chart of ", title), y = "absolute frequency") +
    theme_bw()


# population pyramid

library(XML)
library(reshape2)
library(ggplot2)
library(plyr)


get_data <- function(country, year) {
  c1 <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="  
  c2 <- "&R=-1&C="
  url <- paste0(c1, year, c2, country)
  df <- data.frame(readHTMLTable(url))
  keep <- c(2, 4, 5)
  df <- df[,keep]  
  names(df) <- c("Age", "Male", "Female")
  cols <- 2:3
  df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df <- df[df$Age != 'Total', ]  
  df$Male <- -1 * df$Male
  df$Age <- factor(df$Age, levels = df$Age, labels = df$Age)
  
  df.melt <- melt(df, 
                  value.name='Population', 
                  variable.name = 'Gender', 
                  id.vars='Age' )
  
  return(df.melt)
}

nigeria <- get_data("NI", 2014)

n1 <- ggplot(nigeria, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
  geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()



# example 2 
#make this example reproducible
set.seed(1)

#create data frame
data_multiple <- data.frame(age = rep(1:100, 6),
                            gender = rep(c("M", "F"), each = 300),
                            country = rep(c("A", "B", "C"), each = 100, times = 2))

#add population variable
data_multiple$population <- round(1/sqrt(data_multiple$age)*runif(200, 10000, 15000), 0)

#view first six rows of dataset
head(data_multiple)

#  age gender country population
#1   1      M       A      11328
#2   2      M       A       8387
#3   3      M       A       7427
#4   4      M       A       7271
#5   5      M       A       4923
#6   6      M       A       5916

#create one population pyramid per country
ggplot(data_multiple, aes(x = age, fill = gender,
                          y = ifelse(test = gender == "M",
                                     yes = -population, no = population))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(data_multiple$population) * c(-1,1)) +
  labs(y = "Population Amount") + 
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #rotate x-axis labels
