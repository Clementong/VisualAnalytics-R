#	3	Create a data visualisation to segment kid drinks and other by nutrition indicators. 
#For the purpose of this task, starbucks_drink.csv should be used.

packages = c('ggiraph', 'plotly', 
             'DT', 'patchwork',
             'gganimate', 'tidyverse',
             'readxl', 'gifski', 'kableExtra', 'knitr', 'GGally', 'parcoords', 'parallelPlot', 'dplyr')

for (p in packages){
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# data reading
starbucks.data <- read.csv('./data/starbucks_drink.csv', header = TRUE)
starbucks.kids_and_other <- starbucks.data %>% 
                            filter(Category == "kids-drinks-and-other")


# process data
starbucks.kids_and_other.processed <- subset(starbucks.kids_and_other, select = -c(Category,Size, Whipped.Cream, Milk, Portion.fl.oz.) )

# full name of drink 
starbucks.kids_and_other.processed$Full.Name = paste(starbucks.kids_and_other$Name,starbucks.kids_and_other$Milk, starbucks.kids_and_other$Whipped.Cream, sep=" ", collapse=NULL)

# since size and proportion relates to the same meaning, with all the volume being consistent based on size, we don't need the size column.
# we need to normalize the nutritional values of each nutrition by oz. (e.g. Calories/oz.)
starbucks.kids_and_other.processed$Calories <- starbucks.kids_and_other$Calories/starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Calories.from.fat <- starbucks.kids_and_other$Calories.from.fat/starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Total.Fat.g. <- starbucks.kids_and_other$Total.Fat.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Saturated.fat.g. <- starbucks.kids_and_other$Saturated.fat.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Cholesterol.mg. <- starbucks.kids_and_other$Cholesterol.mg./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Sodium.mg. <- starbucks.kids_and_other$Sodium.mg./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Total.Carbohydrate.g. <- starbucks.kids_and_other$Total.Carbohydrate.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Dietary.Fiber.g. <- starbucks.kids_and_other$Dietary.Fiber.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Sugars.g. <- starbucks.kids_and_other$Sugars.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Protein.g. <- starbucks.kids_and_other$Protein.g./starbucks.kids_and_other$Portion.fl.oz.
starbucks.kids_and_other.processed$Caffeine.mg. <- strtoi(starbucks.kids_and_other$Caffeine.mg.)/starbucks.kids_and_other$Portion.fl.oz.

#parallel graph by drink
ggparcoord(data = starbucks.kids_and_other.processed, columns = c(2:13),
           groupColumn = 15,
           scale = "uniminmax",
           alphaLines = 0.2,
           boxplot = TRUE, 
           title = "Parallel Coordinates Plot of Starbucks Drinks") +
  facet_wrap(~ Cluster) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# parallel graph
parcoords(
  starbucks.kids_and_other.processed[,2:13],
  alpha = 0.3,
  alphaOnBrushed = 0.2,
  rownames = FALSE,
  reorderable = T,
  brushMode = '1D-axes',
  width = 700,
  height = 400)

histoVisibility <- rep(TRUE, ncol(starbucks.kids_and_other.processed))
parallelPlot(starbucks.kids_and_other.processed,
             categorical = unique(starbucks.kids_and_other.processed$Name),
             continuousCS = "YlOrRd",
             rotateTitle = TRUE,
             histoVisibility = histoVisibility)



# output to be present as PNG file
png(file = "KMeansExample.png")

starbucks.kids_and_other.nutritions <- subset(starbucks.kids_and_other.processed, select = -c(Name, Full.Name))
km <- kmeans(starbucks.kids_and_other.nutritions, centers = 5, nstart = 25)


starbucks.kids_and_other.processed$Cluster = km$cluster

# Visualize the clusters
fviz_cluster(km, data = starbucks.kids_and_other.nutritions)

# saving the file
dev.off()

install.packages("factoextra")
library(factoextra)
