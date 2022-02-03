packages = c('ggiraph', 'plotly', 
             'DT', 'patchwork',
             'gganimate', 'tidyverse',
             'readxl', 'gifski', 'gapminder')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}


pop_2010  <- read_csv('./data/respopagesextod2000to2010.csv')
pop_2020 <- read_csv('./data/respopagesextod2011to2020.csv')

# display using DT side by side 


# check for missing values
pop_2010[rowSums(is.na(pop_2010)) >= 0.7,]
pop_2020[rowSums(is.na(pop_2020)) >= 0.7,]

# checking the columns before a union 
names(pop_2010)
names(pop_2020)
# all same and also the data type is same 

# union these data set 
combined_pop <- union(pop_2010, pop_2020)
# DT table head

# checking combined data for missing values
unique(combined_pop$PA) # Not stated is found, exclude using dyplr mutate


# checking the number of rows 
print(nrow(pop_2010) + nrow(pop_2020)) 
print(nrow(combined_pop))
# Turned out same


# Insert, getting the 0 total
Location_pop <- combined_pop %>%
  select(PA, Pop) %>%
  group_by(PA) %>%
  summarise(total=sum(Pop)) %>%
  filter(total==0) %>%
  select(PA) %>%
  ungroup()

# get lsit of locations to exclude
no_pop_locations <- as.vector(Location_pop$PA)


# Replace at the part where combined_pop_group is made
combined_pop_grouped <- combined_pop %>% 
  select(PA,Time,AG, Sex, Pop) %>%
  group_by(PA,Time,AG,Sex) %>% 
  summarise(Total = sum(Pop)) %>%
  arrange(PA,Time,Sex,AG) %>%
  filter(PA != 'Not Stated') %>%
  filter(!PA %in% no_pop_locations) %>%
  ungroup()

# color dictionary
colour <- list("#4682B4","#FFC0CB")
names(colour) <- c("Males","Females") # access colour["Males"] 

# renaming table columns
names(combined_pop_grouped) <- c("Planning_Area","Year","Age_Group","Gender","Population")

# All males are negative so they go to the left
combined_pop_grouped$Total_Population <- combined_pop_grouped$Population
combined_pop_grouped$Total_Population <- ifelse(combined_pop_grouped$Gender == "Males"
                                                , -1*combined_pop_grouped$Total_Population, combined_pop_grouped$Total_Population)

# Tool-Tip
combined_pop_grouped$tooltips <- c(paste0("Gender = ", combined_pop_grouped$Gender
                                          , "\n Age Group = ", combined_pop_grouped$Age_Group
                                          , "\n Population = ", combined_pop_grouped$Population ))

combined_pop_grouped$Age_Group<-sub('_to_', ' to ', combined_pop_grouped$Age_Group)
combined_pop_grouped$Age_Group<-sub('_and_', ' and ', combined_pop_grouped$Age_Group)

# Similarly factorize age group
combined_pop_grouped$Age_Group <- factor(combined_pop_grouped$Age_Group, ordered=TRUE ,levels=c("0 to 4","5 to 9","10 to 14",
                                                                                                "15 to 19","20 to 24","25 to 29",
                                                                                                "30 to 34","35 to 39","40 to 44",
                                                                                                "45 to 49","50 to 54","55 to 59",
                                                                                                "60 to 64","65 to 69","70 to 74",
                                                                                                "75 to 79","80 to 84","85 to 89","90 and over"))




color_coding<- function(x){
  if(x > 0){
    return("blue")
    
  }else{
    return("pink")
  }
}

combined_pop_grouped$colors <- lapply(combined_pop_grouped$Total_Population, color_coding)

# For each of the dataframes , subset by planning area and create an interactive motion chart 
# with tool tips using population

Area <- unique(combined_pop_grouped$Planning_Area)[1:2]

Area_df <- combined_pop_grouped %>%
  filter(Planning_Area == Area)


library(plotly)   

maledf =  subset(Area_df, Gender == "Males")
femaledf =  subset(Area_df, Gender == "Females")



# fig <- plot_ly(Area_df)
# 
# fig <- fig %>% 
#   add_bars(y =~Age_Group, x = maledf$Total_Population)
# 
# fig <- fig %>% 
#   add_bars(y =~Age_Group, x = femaledf$Total_Population)
# 
# fig



fig <- Area_df %>% 
  plot_ly(x=~Total_Population, y=~Age_Group, frame=~Year,
          hovertext=~tooltips, hoverinfo='text',  mode='markers', 
          transforms=list(
        list(
          type = 'filter',
             target = ~Planning_Area,
             operation = '=',
             value = unique(Area_df$Planning_Area)[1]
        )
  )) %>%
  
  layout(
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 0,
        buttons = list(
          list(method = "restyle",
               args = list("transforms[0].value", unique(Area_df$Planning_Area)[1]),
               label = unique(Area_df$Planning_Area)[1]),
          list(method = "restyle",
               args = list("transforms[0].value", unique(Area_df$Planning_Area)[2]),
               label = unique(Area_df$Planning_Area)[2])
          )
        )
      )
    )

fig



p <- iris %>%
  plot_ly(
    type = 'scatter', 
    x = ~Sepal.Length, 
    y = ~Petal.Length,
    text = ~Species,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~Species,
        operation = '=',
        value = unique(iris$Species)[1]
      )
    )) %>% 
  layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[1]),
                 label = unique(iris$Species)[1]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[2]),
                 label = unique(iris$Species)[2]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[3]),
                 label = unique(iris$Species)[3])
          )
        )
      )
    )

p