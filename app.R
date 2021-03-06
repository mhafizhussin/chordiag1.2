#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readxl)
library(chorddiag)
library(dplyr)
library(bubbles)
library(formattable)
library(knitr)
library(ggplot2)
library(tidyr)


# Global Constant
color1 <- c('#f0a8a8', '#f0baa8','#f0cca8','#f0dea8', '#f0f0a8', '#def0a8',
            '#ccf0a8', '#baf0a8', '#a8f0a8', '#a8f0ba', '#a8f0cc', '#a8f0de', '#a8f0ec', '#a8f0f0', '#a8def0')
color2 <- c('#7ce9ce', '#7ce9e3', '#7ce9e9', '#7ccee9', '#7cb3e9', '#7c97e9',
            '#7c7ce9', '#977ce9', '#b37ce9', '#ce7ce9', '#e97ce9', '#e97cce', '#e97cb3', '#e97c97', '#e97c7c')

university <- c("UM","USM","UKM","UPM","UTM")
Gender <- c("MALE_STUDENT","FEMALE_STUDENT","MALE_STAFF","FEMALE_STAFF")
Student_Level <- c("PHD_STUDENT", "MASTER_STUDENT", "BACHELOR_STUDENT","DIPLOMA_STUDENT")
Nationality <- c("MALAYSIAN_STUDENT", "INTERNATIONAL_STUDENT")
Disability <- c("HEARING_DISABILITY","SPEECH_DISABILITY", "PHYSICAL_DISABILTY", "VISUAL_DISABILITY", "OTHERS_DISABILITY")
Staff_Qualification <- c("PHD_HOLDER", "MASTERS_HOLDER", "BACHELOR_HOLDER")
Staff_Position <- c("PROFESSORS","ASSOC_PROFS","LECTURERS")
choices_var <- c("Gender", "Student_Level", "Nationality", "Disability", "Staff_Qualification", "Staff_Position")

main_data <- read_excel("Main_Data.xlsx") # first tab
newcol <- c('University', 'Male 15', 'Female 15', 'PhD 15', 'Master 15', 'Bachelor 15', 'Diploma 15', 'Malaysian 15', 'International 15', 
  'Hearing 15', 'Speech 15', 'Physical 15', 'Visual 15', 'Others 15', 'Male Staff 15', 'Female Staff 15', 'PhD Staff 15', 
  'Masters Staff 15', 'Bachelor Staff 15', 'Professors 15', 'Assoc. Profs 15', 'Lecturers 15', 'Male 16', 'Female 16', 
  'PhD 16', 'Master 16', 'Bachelor 16', 'Diploma 16', 'Malaysian 16', 'International 16', 'Hearing 16', 'Speech 16', 
  'Physical 16', 'Visual 16', 'Others 16', 'Male Staff 16', 'Female Staff 16', 'PhD Staff 16', 'Masters Staff 16', 
  'Bachelor Staff 16', 'Professors 16', 'Assoc. Profs 16', 'Lecturers 16', 'Male 17', 'Female 17', 'PhD 17', 'Master 17', 
  'Bachelor 17', 'Diploma 17', 'Malaysian 17', 'International 17', 'Hearing 17', 'Speech 17', 'Physical 17', 'Visual 17', 
  'Others 17', 'Male Staff 17', 'Female Staff 17', 'PhD Staff 17', 'Masters Staff 17', 'Bachelor Staff 17', 'Professors 17', 
  'Assoc. Profs 17', 'Lecturers 17')
names(main_data) <- newcol

BubbleData <- read_excel("Main_Data.xlsx", sheet = "Overall")


header <- dashboardHeader(title = "Top 5 Universities")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("OVERVIEW", tabName = "OVERVIEW", icon = icon("info"), badgeLabel = "Time Series", badgeColor = "orange"),
    selectInput('input_table', "Select Data to Display",
                choices = university,
                multiple = FALSE,
                selected = 'UM',
                width = '98%'),
    
    menuItem("STUDENT", tabName = "STUDENT", icon = icon("bar-chart-o"), badgeLabel = "Chord Diagram", badgeColor = "green"),
    menuItem("STAFF", tabName = "STAFF", icon = icon("bar-chart-o"), badgeLabel = "Chord Diagram", badgeColor = "green"),
    menuItem("OVERALL", tabName = "OVERALL", icon = icon("bar-chart-o"), badgeLabel = "Bubble Diagram", badgeColor = "fuchsia"),
    menuItem("ABOUT", tabName = "ABOUT", icon = icon("info-circle", lib = "font-awesome"))
  )
)

body <- dashboardBody(
  
  tabItems(
    tabItem("OVERVIEW",
            fluidRow(
              valueBox(5, width = 12,"Selected Public Universities for this Project: UM, USM, UKM, UPM & UTM", icon = icon("education", lib = "glyphicon"))
              # valueBox(100 * 2, "Current Students", icon = icon("stats", lib = "glyphicon")),
              # valueBox(1500,"Number Of Staff", icon = icon("user", lib = "glyphicon"))
            ),
            
            fluidRow(
              box(
                title = "University Times Series Visualization", width = 12, height = 40, solidHeader = TRUE, status = "warning"
              )
            ),
            
            fluidRow(
              box(width = 12, background = "blue", 
                  selectInput("inputX","Select variable to display time series",choices = choices_var, multiple = FALSE,selected = "Gender" ))
            ),
            
            fluidRow(
              box(width = 12, height = 400, background = "navy", plotOutput("plot2", height = 380 ))
            ),
            
            fluidRow(
              box(
                title = "Data for Application", height = 800, width = 12, solidHeader = TRUE,
                status = "primary",
                formattableOutput("table")
              )
            )
    ),
    
    tabItem("STUDENT",
            fixedRow(
              box(title = "Student by Gender, Nationality, Level of Study and Disabilities", width = 12, height = 40, background = "navy")
            ),
            
            fixedRow(
              box(width = 12, background = "navy", 
                  selectInput('input_data', "Choose one variable to view the corresponding Chord Diagram",
                              choices = c("Gender","Nationality","Level of Study", "Disabilities"),
                              multiple = FALSE,
                              selected = 'Gender',
                              width = '98%')
              ),
              
              box(width = 12, status = "warning", 
                  "Mouseover to focus on each chord and see the information.")
            ),
            fluidPage(
              chorddiagOutput("distPlot", height = 600)
            )
    ),
    
    tabItem("STAFF",
            fixedRow(
              box(width = 12, height = 40, background = "navy", title = "Staff by Gender, Qualification and Position")
            ),
            
            fixedRow(
              box(width = 12, background = "navy", 
                  selectInput('input_data2', "Choose one variable to view the corresponding Chord Diagram",
                              choices = c("Gender","Education Qualification", "Academic Position"),
                              multiple = FALSE,
                              selected = 'Gender',
                              width = '98%')
              ),
              
              box(width = 12, status = "warning", 
                  "Mouseover to focus on each chord and see the information.")
            ),
            fluidPage(
              chorddiagOutput("staffPlot", height = 600)
            )
    ),
    
    tabItem("OVERALL",
            fluidRow(
              tabBox(
                title = "Bubble Chart",
                id = "tabset1", height = "1000px", width = 12,
                tabPanel("Student by States", "Tab 1 content",
                         sliderInput("animation1", "Slide to change year:", width = '98%',
                                     min = 2014, max = 2017,
                                     value = 2014, step = 1,
                                     animate = animationOptions(interval = 1500, loop = TRUE)),
                         bubblesOutput("overallPlot1", height = 600, width = '98%')
                ),
                tabPanel("Student by Fields", "Tab 2 Content",
                         sliderInput("animation2", "Slide to change year:", width = '98%',
                                     min = 2014, max = 2017,
                                     value = 2014, step = 1,
                                     animate = animationOptions(interval = 1500, loop = TRUE)),
                         bubblesOutput("overallPlot2", height = 600, width = '98%')
                )
              )
            )
    ),
    
    tabItem("ABOUT", box(width = 12, "ABOUT"))
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$plot2 <- renderPlot({
      print(paste0("Current Uni is: ", input$input_table))
      print(paste0("Current output is: ", input$inputX))
      
      
      if(input$input_table == "UM"){
        u_data <- read_excel("Main_Data.xlsx", sheet = "UM")
      } else if (input$input_table == "USM") {
        u_data <- read_excel("Main_Data.xlsx", sheet = "USM")
      } else if (input$input_table == "UKM") {
        u_data <- read_excel("Main_Data.xlsx", sheet = "UKM")
      } else if (input$input_table == "UPM") {
        u_data <- read_excel("Main_Data.xlsx", sheet = "UPM")
      } else {
        u_data <- read_excel("Main_Data.xlsx", sheet = "UTM")
      }
      
      transpose_data <- as.data.frame(t(u_data))
      colnames(transpose_data) <- u_data$VARIABLES
      transpose_data <- transpose_data[-1,]
      
      if (input$inputX == "Gender") {
        input_len = length(Gender)
        input_str = Gender
      } else if(input$inputX == "Nationality") {
        input_len = length(Nationality)
        input_str = Nationality
      } else if(input$inputX == "Student_Level") {
        input_len = length(Student_Level)
        input_str = Student_Level
      } else if(input$inputX == "Disability") {
        input_len = length(Disability)
        input_str = Disability
      } else if(input$inputX == "Staff_Qualification") {
        input_len = length(Staff_Qualification)
        input_str = Staff_Qualification
      } else {
        input_len = length(Staff_Position)
        input_str = Staff_Position
      }
      
      year <- rep(as.numeric(c("2015","2016","2017")),times = length(input_len))
      newData <- transpose_data %>% select(input_str) %>% gather(key = variable, value = value)
      newData$year <- year
      newData$value <- as.numeric(newData$value)
      
      # Multiple line plot
      ggplot(newData, aes(x = year, y = value)) + 
        geom_line(aes(color = variable), size = 1) +
        scale_color_manual(values = rainbow(input_len, alpha = NULL)[sample(input_len)]) +
        theme_minimal()
      
    })
    
    output$table <- renderFormattable({
      # read from Main_data
      
      if (input$input_table == "UM") {
        This_Table <- read_excel("Main_Data.xlsx", sheet = "UM")
      } else if (input$input_table == "USM") {
        This_Table <- read_excel("Main_Data.xlsx", sheet = "USM")
      } else if (input$input_table == "UKM") {
        This_Table <- read_excel("Main_Data.xlsx", sheet = "UKM")
      } else if (input$input_table == "UPM") {
        This_Table <- read_excel("Main_Data.xlsx", sheet = "UPM")
      } else {
        This_Table <- read_excel("Main_Data.xlsx", sheet = "UTM")
      }
      
      formattable(This_Table, list(VARIABLES = color_tile("lightgreen", "lightgreen"),
                                   '2017' = color_tile("pink", "pink")))
    })
    
    output$distPlot <- renderChorddiag({
      
      
      # local constant
      gender_variables <-c('Male 15', 'Male 16', 'Male 17','Female 15', 'Female 16', 'Female 17')
      
      citizen_variables <- c('Malaysian 15','Malaysian 16','Malaysian 17',
                             'International 15', 'International 16', 'International 17')
      
      level_variables <- c('Diploma 15', 'Diploma 16', 'Diploma 17', 'Bachelor 15', 'Bachelor 16', 'Bachelor 17',
                           'Master 15', 'Master 16', 'Master 17', 'PhD 15', 'PhD 16', 'PhD 17')
      
      disable_variables <- c('Hearing 15', 'Hearing 16', 'Hearing 17','Speech 15', 'Speech 16', 'Speech 17', 
                             'Physical 15', 'Physical 16', 'Physical 17',
                             'Visual 15', 'Visual 16', 'Visual 17','Others 15', 'Others 16', 'Others 17')
      
      
      # select data
      gender_data <- main_data[,gender_variables]
      citizen_data <- main_data[,citizen_variables]
      level_data <- main_data[,level_variables]
      disable_data <- main_data[,disable_variables]
      
      gender.mat <- as.matrix(gender_data)
      citizen.mat <- as.matrix(citizen_data)
      level.mat <- as.matrix(level_data)
      disable.mat <- as.matrix(disable_data)
      
      rownames(gender.mat) <- university
      rownames(citizen.mat) <- university
      rownames(level.mat) <- university
      rownames(disable.mat) <- university
      
      
      if(input$input_data =="Gender"){
        chorddiag(gender.mat, type = "bipartite", palette2 = "OrRd", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else if(
        input$input_data =="Nationality"){
        chorddiag(citizen.mat, type = "bipartite", palette2 = "Greens", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      } else if(
        input$input_data =="Level of Study"){
        chorddiag(level.mat, type = "bipartite", palette2 = "Blues", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else{
        chorddiag(disable.mat, type = "bipartite", palette2 = "Spectral", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }
    })
    
    output$staffPlot <- renderChorddiag({
      
      # local constant
      gender_var <- c('Male Staff 15', 'Male Staff 16', 'Male Staff 17', 
                      'Female Staff 15', 'Female Staff 16', 'Female Staff 17')
      
      level_var <- c('PhD Staff 15', 'PhD Staff 16', 'PhD Staff 17', 
                     'Masters Staff 15', 'Masters Staff 16', 'Masters Staff 17', 
                     'Bachelor Staff 15', 'Bachelor Staff 16', 'Bachelor Staff 17')
      
      position_var <- c('Professors 15', 'Professors 16', 'Professors 17', 
                        'Assoc. Profs 15', 'Assoc. Profs 16', 'Assoc. Profs 17', 
                        'Lecturers 15', 'Lecturers 16', 'Lecturers 17')
      
      gender_staff <- main_data[,gender_var]
      level_staff <- main_data[,level_var]
      position_staff <- main_data[,position_var]
      
      gender_staffMat <- as.matrix(gender_staff)
      level_staffMat <- as.matrix(level_staff)
      position_staffMat <- as.matrix(position_staff)
      
      rownames(gender_staffMat) <- university
      rownames(level_staffMat) <- university
      rownames(position_staffMat) <- university
      
      
      if(input$input_data2 == "Gender"){
        chorddiag(gender_staffMat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else if(input$input_data2 == "Education Qualification"){
        chorddiag(level_staffMat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else {
        chorddiag(position_staffMat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }
      
    })
    
    output$overallPlot1 <- renderBubbles({
      
      state <- BubbleData$STATES
      valueB2014 <- BubbleData$STUDENTS_BY_STATES_2014
      valueB2015 <- BubbleData$STUDENTS_BY_STATES_2015
      valueB2016 <- BubbleData$STUDENTS_BY_STATES_2016
      valueB2017 <- BubbleData$STUDENTS_BY_STATES_2017
      
      if(input$animation1 == 2014) {
        bubbles(value = valueB2014, label = state, tooltip = valueB2014,
                color = color1)
      } else if(input$animation1 == 2015) {
        bubbles(value = valueB2015, label = state, tooltip = valueB2015,
                color = color1)
      } else if(input$animation1 == 2016) {
        bubbles(value = valueB2016, label = state, tooltip = valueB2016,
                color = color1)
      } else {
        bubbles(value = valueB2017, label = state, tooltip = valueB2017,
                color = color1)
      }
    })
    
    output$overallPlot2 <- renderBubbles({
      
      field <- BubbleData$FIELD_OF_STUDY
      valueF2014 <- BubbleData$STUDENTS_BY_FIELDS_2014
      valueF2015 <- BubbleData$STUDENTS_BY_FIELDS_2015
      valueF2016 <- BubbleData$STUDENTS_BY_FIELDS_2016
      valueF2017 <- BubbleData$STUDENTS_BY_FIELDS_2017
      
      if(input$animation2 == 2014) {
        bubbles(value = valueF2014, label = field, tooltip = valueF2014,
                color = color2)
      } else if(input$animation2 == 2015) {
        bubbles(value = valueF2015, label = field, tooltip = valueF2015,
                color = color2)
      } else if(input$animation2 == 2016) {
        bubbles(value = valueF2016, label = field, tooltip = valueF2016,
                color = color2)
      } else {
        bubbles(value = valueF2017, label = field, tooltip = valueF2017,
                color = color2)
      }
    })
  }
)