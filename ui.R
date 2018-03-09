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
  fluidRow(
    sidebarPanel(
      h2('Pitching Info'),
      hr(),
      h3('Game Scenario'),
      # selectInput('season',
      #             'Time of Year:',
      #             choices = c("Spring", "Summer", "Fall"),
      #             selected = c("Spring", "Summer", "Fall"),
      #             multiple = TRUE,
      #             selectize = FALSE),
      fluidRow(
        column(
          width = 6,
          selectInput("month",
                      "Month:",
                      choices = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                      selected = c("March", "April", "May", "June", "July", "August", "September", "October", "November"),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 6,
          selectInput('year',
                      "Year:",
                      choices = seq(max(rizzo_df$game_year, na.rm = TRUE), min(rizzo_df$game_year, na.rm = TRUE), -1),
                      selected = seq(max(rizzo_df$game_year, na.rm = TRUE), min(rizzo_df$game_year, na.rm = TRUE), -1),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        )
      ),
      
      fluidRow(
        column(
          width = 4,
          selectInput("balls",
                      "Balls:",
                      choices = c(0,1,2,3),
                      selected = c(0,1,2,3),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 4,
          selectInput("strikes",
                      "Strikes:",
                      choices = c(0,1,2),
                      selected = c(0,1,2),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        ),
        column(
          width = 4,
          selectInput("outs",
                      "Outs:",
                      choices = c(0,1,2),
                      selected = c(0,1,2),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px')
        )
      ),
      fluidRow(
        column(
          width = 6,
          selectInput("homeaway",
                      "Top/Bottom:",
                      choices = c('Top', 'Bottom' = 'Bot'),
                      selected = c('Top', 'Bot'),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px',
                      size = 4)
        ),
        column(
          width = 6,
          selectInput("inning",
                      "Inning:",
                      choices = c(1,2,3,4,5,6,7,8,9),
                      selected = c(1,2,3,4,5,6,7,8,9),
                      multiple = TRUE,
                      selectize = FALSE,
                      width = '200px',
                      size = 4)  
        )
      ),
      # checkboxGroupInput("onbase",
      #              "Runners On Base:",
      #              choices = c("1st" = 1, "2nd" = 2, "3rd" = 3),
      #              selected = c(1,2,3),
      #              inline = TRUE),
      hr(),
      h3('Pitcher Info'),
      selectInput("pitcher_hand",
                  "Left/Right-Handed:",
                  choices = c("Right" = "R", "Left" = "L"),
                  selected = c("L","R"),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Pitch Selection'),
      fluidRow(
        column(
          width = 6,
          selectInput("pitch_name",
                      "Pitch Type",
                      choices = na.exclude(unique(rizzo_df$pitch_name)),
                      selected = na.exclude(unique(rizzo_df$pitch_name)),
                      multiple = TRUE,
                      selectize = FALSE,
                      #size = 3,
                      width = '200px')
        ),
        column(
          width = 6,
          sliderInput("release_speed",
                      "Velocity:",
                      min = min(rizzo_df$release_speed, na.rm = TRUE),
                      max = max(rizzo_df$release_speed, na.rm = TRUE),
                      value = c(min(rizzo_df$release_speed, na.rm = TRUE), max(rizzo_df$release_speed, na.rm = TRUE)),
                      width = '200px'),
          sliderInput("release_spin_rate",
                      "Spin Rate:",
                      min = min(rizzo_df$release_spin_rate, na.rm = TRUE),
                      max = max(rizzo_df$release_spin_rate, na.rm = TRUE),
                      value = c(min(rizzo_df$release_spin_rate, na.rm = TRUE), max(rizzo_df$release_spin_rate, na.rm = TRUE)),
                      width = '200px'),
          selectInput('strikezone',
                      'Strike Zone Location:',
                      choices = c(1,2,3,4,5,6,7,8,9,11,12,13,14),
                      selected = c(0,1,2,3,4,5,6,7,8,9,11,12,13,14),
                      selectize = FALSE,
                      multiple = TRUE,
                      width = '200px')
        )
      ),
      width = 3
    ),    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("strikezonePlot", height = '300px', width = '375px'),
      plotlyOutput("launchPlot", height = '300px', width = '500px'),
      #img(src = 'anthony-rizzo.png'),
      plotlyOutput("spraychartPlot", height = '300px', width = '375px'),
      width = 6
    ),
    
    sidebarPanel(
      h2('Outcomes Info'),
      hr(),
      h3('Contact Info'),
      selectInput('contact_info',
                  'Take/Swing/Contact',
                  choices = c('take', 'missed swing', 'foul', 'contact'),
                  selected = c('take', 'missed swing', 'foul', 'contact'),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Launch Info'),
      selectInput('ball_flight',
                  'Ball Flight',
                  choices = c('Barrel' = 6, 'Solid Contact' = 5, 'Flare/Burner' = 4, 'Poor Contact/Underneath' = 3, 'Poor Contact/Topped' = 2, 'Weak Contact' = 1, 'No Contact' = 0),
                  selected = c(0, 1, 2, 3, 4, 5, 6),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      hr(),
      h3('Field Placement'),
      # selectInput('field_zone',
      #             'Area of Field',
      #             choices = c(0,1,2,3,4,5,6,7,8,9),
      #             selected = c(0,1,2,3,4,5,6,7,8,9),
      #             multiple = TRUE,
      #             selectize = FALSE,
      #             width = '200px'),
      selectInput("spray_angle_cat",
                  "Spray Angle:",
                  choices = c('Opposite Field', 'Straight-Away', 'Pulled', 'No Contact' = NA),
                  selected = c('Opposite Field', 'Straight-Away', 'Pulled', NA),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      selectInput("infield_outfield",
                  'Depth:',
                  choices = c('Infield', 'Outfield', 'No Contact' = NA),
                  selected = c('Infield', 'Outfield', NA),
                  multiple = TRUE,
                  selectize = FALSE,
                  width = '200px'),
      width = 3
    )
  )
))

