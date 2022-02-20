library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(tidyverse)

ec_data <- read_csv('ec_data_workshop.csv')

names(ec_data)[4] <- "Area_in_SQM"
names(ec_data)[5] <- "Transacted_Price"
names(ec_data)[6] <- "Unit_Price_PSM"
names(ec_data)[7] <- "Unit_Price_PSF"
names(ec_data)[11] <- "Sale_Type"
names(ec_data)[12] <- "Purchaser_Address_Indicator"
names(ec_data)[16] <- "Planning_Region"
names(ec_data)[17] <- "Planning_Area"
names(ec_data)[19] <- "Max_Floor"
names(ec_data)[37] <- "Prior_Transacted_Price"
names(ec_data)[39] <- "Profit"
names(ec_data)[40] <- "Num_Years_Owned"
names(ec_data)[49] <- "CPI"


ui <- dashboardPage( #1
  
  
  dashboardHeader(title = "Workshop 2"),
  
    
  sidebar <- dashboardSidebar( # 2
      
        sidebarMenu(
          menuItem("CI" ,tabName = "CI", icon = icon("dashboard")),
          menuItem("ANOVA",tabName = "ANOVA", icon = icon("globe")),
          menuItem("MLR", tabName = "MLR", icon = icon("chart-line"))
        )
        
    ), #DB page
    
  dashboardBody( # 3
    
      tabItems(
        
            tabItem(
              
                tabName = 'CI',
                titlePanel("95% Confidence Interval (assumption: large sample size and sigma is unknown"),
                tabsetPanel(
                  tabPanel(
                  ),
                  tabPanel(
                  )
                )# tab set panel 
              
            ) # tabitem
          
      )#tabItems
    
  )# dashbaord body
  
)# dashboard page

server <- function(input, output) { 
  
  
  output$summary1 <- renderTable( {
    
    region_1 <- ec_data %>% 
      dplyr::select(input$variable1, Planning_Region) %>%
      group_by(Planning_Region) %>%
      summarise(Min = min(!!sym(input$variable1), na.rm = TRUE), 
                Max = max(!!sym(input$variable1), na.rm = TRUE), 
                Mean = mean(!!sym(input$variable1), na.rm = TRUE),
                Sd = sd(!!sym(input$variable1), na.rm = TRUE),
                Median = median(!!sym(input$variable1), na.rm = TRUE)
      )
    
    region_2 <- ec_data %>%
      dplyr::count(Planning_Region)
    
    summary_planning_region <- merge(region_1, region_2, by = 'Planning_Region')
    summary_planning_region$lower_CI <- summary_planning_region$Mean - qnorm(0.975)* summary_planning_region$Sd/sqrt(summary_planning_region$n)
    summary_planning_region$upper_CI <- summary_planning_region$Mean + qnorm(0.975)* summary_planning_region$Sd/sqrt(summary_planning_region$n)
    return(summary_planning_region)
    
  })
  
  output$summary2 <- renderTable( {
    
    level_1 <- ec_data %>% 
      dplyr::select(input$variable2, Floor_Level) %>%
      group_by(Floor_Level) %>%
      summarise(Min = min(input$variable2, na.rm = TRUE), 
                Max = max(input$variable2, na.rm = TRUE), 
                Mean = mean(input$variable2, na.rm = TRUE),
                Sd = sd(input$variable2, na.rm = TRUE),
                Median = median(input$variable2, na.rm = TRUE)
      )
    
    level_2 <- ec_data %>%
      dplyr::count(Floor_Level)
    
    summary_floor_level <- merge(level_1, level_2, by = 'Floor_Level')
    summary_floor_level$lower_CI <- summary_floor_level$Mean - qnorm(0.975)* summary_floor_level$Sd/sqrt(summary_floor_level$n)
    summary_floor_level$upper_CI <- summary_floor_level$Mean + qnorm(0.975)* summary_floor_level$Sd/sqrt(summary_floor_level$n)
    return(summary_floor_level)
    
  })
  
  output$boxplot1 <- renderPlot({
    
    ggplot(ec_data, aes(y = !!sym(input$variable1), x = Planning_Region))+
      geom_boxplot()+
      labs(title = paste('Boxplots of', input$variable1, 'by Planning Region'),
           x = 'Planning Region', y = input$variable1
      )
  })
  
  output$boxplot2 <- renderPlot({
    
    ggplot(ec_data, aes(y = !!sym(input$variable2), x = Floor_Level))+
      geom_boxplot()+
      labs(title =  paste('Boxplots of', input$variable2, 'by Floor Level'),
           x = 'Floor Level', y = input$variable2)
  })
  
  output$ci_plot1 <- renderPlot({
    
    
    Level_1 <- ec_data %>%
      dplyr::select(input$variable1, Planning_Region) %>%
      group_by(Planning_Region) %>%
      summarize(Mean = mean(!!sym(input$variable1), 
                            na.rm = TRUE),
                Sd = sd(!!sym(input$variable1), 
                        na.rm = TRUE)
      )
    Level_2 <- ec_data %>%
      dplyr::count(Planning_Region)
    
    summary_planning_region <- merge(Level_1, Level_2, by = "Planning_Region")
    summary_planning_region$lower_CI <- summary_planning_region$Mean - qnorm(0.975)* summary_planning_region$Sd/sqrt(summary_planning_region$n)
    summary_planning_region$upper_CI <- summary_planning_region$Mean + qnorm(0.975)* summary_planning_region$Sd/sqrt(summary_planning_region$n)
    
    
    ggplot(summary_planning_region, aes(Mean, Planning_Region)) + 
      geom_point() +
      geom_errorbarh(aes(xmax = upper_CI, 
                         xmin = lower_CI, 
                         height = .2)
      )+
      labs(x =  input$variable1)
    
  })
  
  output$ci_plot2 <- renderPlot({
    
    Level_1 <- ec_data %>%
      dplyr::select(input$variable2, 
                    Floor_Level) %>%
      group_by(Floor_Level) %>%
      summarize(Mean = mean(!!sym(input$variable2), 
                            na.rm = TRUE),
                Sd = sd(!!sym(input$variable2), 
                        na.rm = TRUE)
      )
    
    Level_2 <- ec_data %>%
      dplyr::count(Floor_Level)
    
    summary_floor_level <- merge(Level_1, Level_2, by = "Floor_Level")
    summary_floor_level$lower_CI <- summary_floor_level$Mean - qnorm(0.975)* summary_floor_level$Sd/sqrt(summary_floor_level$n)
    summary_floor_level$upper_CI <- summary_floor_level$Mean + qnorm(0.975)* summary_floor_level$Sd/sqrt(summary_floor_level$n)
    
    ggplot(summary_floor_level, aes(Mean, Floor_Level)) + 
      geom_point() +
      geom_errorbarh(aes(xmax = upper_CI, 
                         xmin = lower_CI, 
                         height = .2)
      )+
      labs(x = input$variable2)
    
  })
  
}


shinyApp(ui, server)
