library(plotly)
library(MASS)

covmat <- matrix(c(0.8, 0.4, 0.3, 0.8), nrow = 2, byrow = T)
df <- mvrnorm(n = 10000, c(0,0), Sigma = covmat)
df <- as.data.frame(df)

colnames(df) <- c("x", "y")
fig <- plot_ly(df, x = ~x, y = ~y, alpha = 0.3)
fig <- fig %>% add_markers(marker = list(line = list(color = "black", width = 1)))
fig <- fig %>% layout(
  title = "Drop down menus - Plot type",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Scatter"),
        
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "2D Histogram")))
  ))

fig



df <- gapminder 
fig <- df %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )
fig <- fig %>% layout(
  xaxis = list(
    type = "log"
  )
)

fig


library(plotly)

df <- data.frame(x = runif(200), y = runif(200), z = runif(200), j = runif(200), k = rep(0.7, 200), i = rnorm(200,0.6,0.05))

create_buttons <- function(df, y_axis_var_names) {
  lapply(
    y_axis_var_names,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('y', list(df[, var_name])),
        label = sprintf('Show %s', var_name)
      )
    },
    df
  )
  
}

y_axis_var_names <- c('y', 'z', 'j', 'k', 'i')

p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = create_buttons(df, y_axis_var_names)
      )
    ))
p
