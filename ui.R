#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(plyr)
library(plotly)


source('exploration.R')


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Rizzo Analysis"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      h3('Game Info'),
      selectInput('season',
                  'Time of Year:',
                  choices = c("Spring", "Summer", "Fall"),
                  selected = c("Spring", "Summer", "Fall"),
                  multiple = TRUE,
                  selectize = FALSE),
      selectInput("month",
                   "Month:",
                   choices = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                   selected = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                   multiple = TRUE,
                  selectize = FALSE),
      checkboxGroupInput("balls",
                  "Ball Count:",
                  choices = c(0,1,2,3),
                  selected = c(0,1,2,3),
                  inline = TRUE),
      checkboxGroupInput("strikes",
                   "Strike Count:",
                   choices = c(0,1,2),
                   selected = c(0,1,2),
                   inline = TRUE),
      checkboxGroupInput("outs",
                  "Outs:",
                  choices = c(0,1,2),
                  selected = c(0,1,2),
                  inline = TRUE),
      checkboxGroupInput("inning",
                   "Inning:",
                   choices = c(1,2,3,4,5,6,7,8,9),
                   selected = c(1,2,3,4,5,6,7,8,9),
                   inline = TRUE),
      # checkboxGroupInput("onbase",
      #              "Runners On Base:",
      #              choices = c("1st" = 1, "2nd" = 2, "3rd" = 3),
      #              selected = c(1,2,3),
      #              inline = TRUE),
      hr(),
      h3('Pitcher Info'),
      checkboxGroupInput("pitcher_hand",
                  "Left/Right-Handed:",
                  choices = c("Right" = "R", "Left" = "L"),
                  selected = "L",
                  inline = TRUE),
      hr(),
      h3('Pitch Info'),
      selectInput("pitch_name",
                "Pitch Type",
                choices = unique(rizzo_df$pitch_name),
                selected = unique(rizzo_df$pitch_name),
                multiple = TRUE),
      sliderInput("release_speed",
                "Velocity:",
                min = min(rizzo_df$release_speed, na.rm = TRUE),
                max = max(rizzo_df$release_speed, na.rm = TRUE),
                value = c(min(rizzo_df$release_speed, na.rm = TRUE), max(rizzo_df$release_speed, na.rm = TRUE))),
      sliderInput("release_spin_rate",
                "Spin Rate:",
                min = min(rizzo_df$release_spin_rate, na.rm = TRUE),
                max = max(rizzo_df$release_spin_rate, na.rm = TRUE),
                value = c(min(rizzo_df$release_spin_rate, na.rm = TRUE), max(rizzo_df$release_spin_rate, na.rm = TRUE)))
    ),    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("strikezonePlot", height = '300px', width = '350px'),
      plotlyOutput("launchPlot", height = '300px', width = '300px'),
      img(src = 'anthony-rizzo.png'),
      plotlyOutput("spraychartPlot", height = '300px', width = '300px')
    )
  )
))

