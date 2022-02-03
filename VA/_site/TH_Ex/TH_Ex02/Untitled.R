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


# GET DATA
combined_pop_grouped <- combined_pop %>% 
  dplyr::select(PA,Time,AG, Sex, Pop) %>%
  group_by(PA,Time,AG,Sex) %>% 
  summarise(Total = sum(Pop)) %>%
  arrange(PA,Time,Sex,AG) %>%
  filter(PA != 'Not Stated') %>%
  ungroup()


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



# For each of the dataframes , subset by planning area and create an interactive motion chart 
# with tool tips using population

Areas <- unique(combined_pop_grouped$Planning_Area)

split_male_female_area_df <- function (df, area) {
  Area_df <- df %>%
    filter(Planning_Area == area) %>%
    dplyr::select(Year,Age_Group,Gender, Population,Total_Population, tooltips) 
  
  maledf =  subset(Area_df, Gender == "Males")
  femaledf =  subset(Area_df, Gender == "Females")
  output <- c(maledf, femaledf)
  return (output)
}

get_max_gender <- function(femaledf, maledf) {
  if ( max(maledf$Total_Population) > max(femaledf$Total_Population)) {
    return(maledf)
  } else { 
    return(femaledf)
  }
}

get_axis_range <- function(df) { 
  bw <- 2 * IQR(df$Total_Population) / length(df$Total_Population)^(1/3)
  bins <- ceiling((max(df$Total_Population) - min(df$Total_Population))/bw)
  window <- 0.10 * mean(df$Total_Population) 
  axis_range <- max(df$Total_Population) + window
  return (axis_range)
}

get_tick_text <- function(axis_range) {
  round_axis_range <- signif(axis_range, digits = 1)
  text_val <- seq(round_axis_range, 0, -2000)
  t_text <- c(text_val, text_val)
  return (t_text)
}

get_tick_val <- function(axis_range) {
  round_axis_range <- signif(axis_range, digits = 1)
  t_val <- c(seq(round_axis_range, 0, -2000), seq(-round_axis_range, 0, 2000))
  return (t_val)
}

generate_graph <- function(femaledf, maledf, axis_range, t_text, t_val, area) {
  fig <- plot_ly(frame = maledf$Year) %>%
    add_bars(
      orientation = 'h',
      x = ~maledf$Total_Population,
      y = ~maledf$Age_Group,
      name = 'Male',
      hoverinfo = 'x',
      marker = list( color = 'powderblue')
    ) %>%
    add_bars(
      orientation = 'h',
      x = ~femaledf$Total_Population,
      y = ~femaledf$Age_Group,
      hovertext = -femaledf$Total_Population,
      name = 'Female',
      hoverinfo = 'text',
      marker = list( color = 'pink')
    ) %>%
    layout(
      title=list(text = area),
      bargap = 0.1, 
      barmode = 'overlay',
      yaxis = list (title = 'Age Group'),
      xaxis = list(range=c(-axis_range, axis_range),
                   tickvals= t_val,
                   ticktext= t_text,
                   title='Total Population')
    )
  fig
}

for (area in Areas) {
  dfs <- split_male_female_area_df(combined_pop_grouped, area)
  femaledf <- dfs[1:6]
  maledf <- dfs[7:12]
  axis_range <- get_axis_range(get_max_gender(femaledf, maledf))
  t_text <- get_tick_text(axis_range)
  t_val <- get_tick_val(axis_range)
  generate_graph(femaledf, maledf, axis_range, t_text, t_val, area)
}



area <- select.list(Areas, title="Choose area")

dfs <- split_male_female_area_df(combined_pop_grouped, area)
femaledf <- dfs[1:6]
maledf <- dfs[7:12]
axis_range <- get_axis_range(get_max_gender(femaledf, maledf))
t_text <- get_tick_text(axis_range)
t_val <- get_tick_val(axis_range)
generate_graph(femaledf, maledf, axis_range, t_text, t_val, area)

