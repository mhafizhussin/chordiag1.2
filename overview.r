library(shinydashboard)
library(shiny)
library(readxl)
library(chorddiag)
library(dplyr)
library(streamgraph)
library(bubbles)

color1 <- c("#6600FF","#0066FF","#CCFF00","#00CCFF","#FF00CC","#66FF00","#00FF66","#00FF00","#FFCC00",
            "#CC00FF","#0000FF","#FF0000","#FF6600","#00FFCC","#FF0066")
color2 <- c("#CC00FF","#0000FF","#FF0000","#FF6600","#00FFCC","#FF0066",
            "#6600FF","#0066FF","#CCFF00","#00CCFF","#FF00CC","#66FF00","#00FF66","#00FF00","#FFCC00")

header <- dashboardHeader(title = "SAMPLE")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("OVERVIEW", tabName = "OVERVIEW"),
    menuItem("STUDENT", tabName = "STUDENT"),
    menuItem("STAFF", tabName = "STAFF"),
    menuItem("OVERALL", tabName = "OVERALL")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("OVERVIEW",
      fluidRow(
        valueBox(15, "Public Universities", icon = icon("education", lib = "glyphicon")),
                     
        valueBox(100 * 2, "Current Students", icon = icon("stats", lib = "glyphicon")),
                     
        valueBox(1500,"Number Of Staff", icon = icon("user", lib = "glyphicon"), color = "fuchsia")
      ),
      
      fluidRow(
        box(
          title = "Graph of Enrolment, Intake and Output for 2017", height = 500,width = 12, solidHeader = TRUE,
          status = "primary",
          plotOutput("plot2", height = 400)
        )
      ),
      
      fluidRow(
        box(
          title = "Graph 2", height = 500, width = 12, solidHeader = TRUE,
          status = "primary"
        )
      )
    ),
    
    tabItem("STUDENT",
      fluidRow(
        infoBox("UM", 1, icon = icon("education", lib = "glyphicon"), color = "blue"),
        infoBox("UKM", 2, icon = icon("education", lib = "glyphicon"), color = "yellow"),
        infoBox("USM", 3, icon = icon("education", lib = "glyphicon"), color = "green"),
        infoBox("UTM", 4, icon = icon("education", lib = "glyphicon"), color = "black"),
        infoBox("UPM", 5, icon = icon("education", lib = "glyphicon"), color = "lime"),
        infoBox("TOTAL", 6, icon = icon("education", lib = "glyphicon"), color = "red")
      ),
      
      fluidPage(
        br(),
        br(),
        
        selectInput('input_data', "Input Test",
                    choices = c("Gender","Nationality","Field of Study", "Disabilities"),
                    multiple = FALSE,
                    selected = 'Gender',
                    width = '98%'),
        
        chorddiagOutput("distPlot", height = 600)
      )
    ),
    
    tabItem("STAFF",
      fluidRow(
        infoBox("UM", 1, icon = icon("education", lib = "glyphicon"), color = "blue"),
        infoBox("UKM", 2, icon = icon("education", lib = "glyphicon"), color = "yellow"),
        infoBox("USM", 3, icon = icon("education", lib = "glyphicon"), color = "green"),
        infoBox("UTM", 4, icon = icon("education", lib = "glyphicon"), color = "black"),
        infoBox("UPM", 5, icon = icon("education", lib = "glyphicon"), color = "lime"),
        infoBox("TOTAL", 6, icon = icon("education", lib = "glyphicon"), color = "red")
      ),
      fluidPage(
        br(),
        br(),
        
        selectInput('input_data2', "Input Test 2",
                    choices = c("Gender","Education Qualification", "Academic Position"),
                    selected = 'Gender',
                    width = '98%'),
        chorddiagOutput("staffPlot", height = 600)
      )
    ),
    
    tabItem("OVERALL",
      fluidRow(
        tabBox(
          title = "Bubble Chart",
          id = "tabset1", height = "1000px", width = 12,
          tabPanel("Student by States", "Tab 1 content",
                   sliderInput("animation1", "Looping Animation:",
                              min = 2014, max = 2017,
                              value = 2014, step = 1,
                              animate = animationOptions(interval = 1500, loop = TRUE)),
                   bubblesOutput("overallPlot1", height = 600)),
          tabPanel("Student by Fields", "Tab 2 Content",
                   sliderInput("animation2", "Looping Animation:",
                               min = 2014, max = 2017,
                               value = 2014, step = 1,
                               animate = animationOptions(interval = 1500, loop = TRUE)),
                   bubblesOutput("overallPlot2", height = 600)
          )
        )
      )
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$plot2 <- renderPlot({
      barplot(c(7,5,8,20,3),
              names.arg=c("Category 1","Category 2","Category 3","Category 4","Category 5"),
              ylim=c(0,25),
              col=c("beige","orange","lightgreen","lightblue","yellow"),
              ylab="Count of items")
    })
    
    output$distPlot <- renderChorddiag({
      
      # main_data
      main_data <- read_excel("Main_Data.xlsx")
      
      # constant
      university <- c("UM","USM","UKM","UPM","UTM")
      gender_variables <-c("MALE_2015","MALE_2016","MALE_2017",
                           "FEMALE_2015","FEMALE_2016","FEMALE_2017")
      citizen_variables <- c("MALAYSIAN_2015","MALAYSIAN_2016","MALAYSIAN_2017",
                             "INTERNATIONAL_2015","INTERNATIONAL_2016","INTERNATIONAL_2017")
      level_variables <- c("DIPLOMA_2015","DIPLOMA_2016","DIPLOMA_2017",
                           "BACHELOR_2015","BACHELOR_2016","BACHELOR_2017",
                           "MASTER_2015","MASTER_2016","MASTER_2017",
                           "PHD_2015","PHD_2016","PHD_2017")
      disable_variables <- c("HEARING_DIS_2015","HEARING_DIS_2016","HEARING_DIS_2017",
                             "SPEECH_DIS_2015","SPEECH_DIS_2016","SPEECH_DIS_2017",
                             "LEGS_DIS_2015","LEGS_DIS_2016","LEGS_DIS_2017",
                             "SPEECH_DIS_2015","SPEECH_DIS_2016","SPEECH_DIS_2017",
                             "ARMS_DIS_2015","ARMS_DIS_2016","ARMS_DIS_2017",
                             "PARALYTICS_DIS_2015","PARALYTICS_DIS_2016","PARALYTICS_DIS_2017",
                             "VISUAL_DIS_2015","VISUAL_DIS_2016","VISUAL_DIS_2017",
                             "OTHERS_DIS_2015","OTHERS_DIS_2016","OTHERS_DIS_2017")
      
      
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
        chorddiag(gender.mat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else if(
        input$input_data =="Nationality"){
        chorddiag(citizen.mat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      } else if(
        input$input_data =="Field of Study"){
        chorddiag(level.mat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }else{
        chorddiag(disable.mat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
      }
    })
    
    output$staffPlot <- renderChorddiag({
      # main_data
      main_data2 <- read_excel("Main_Data.xlsx")
      
      # constant
      university <- c("UM","USM","UKM","UPM","UTM")
      gender_var <- c("MALE_STAFF_2015", "MALE_STAFF_2016", "MALE_STAFF_2017",
                      "FEMALE_STAFF_2015","FEMALE_STAFF_2016","FEMALE_STAFF_2017")
      level_var <- c("PHD_STAFF_2015", "PHD_STAFF_2016", "PHD_STAFF_2017",
                     "MASTERS_STAFF_2015", "MASTERS_STAFF_2016", "MASTERS_STAFF_2017",
                     "BACHELOR_STAFF_2015", "BACHELOR_STAFF_2016", "BACHELOR_STAFF_2017")
      position_var <- c("PROFESSORS_2015", "PROFESSORS_2016", "PROFESSORS_2017",
                        "ASSOC_PROFS_2015", "ASSOC_PROFS_2016", "ASSOC_PROFS_2017",
                        "LECTURERS_2015", "LECTURERS_2016", "LECTURERS_2017")
      
      gender_staff <- main_data2[,gender_var]
      level_staff <- main_data2[,level_var]
      position_staff <- main_data2[,position_var]
      
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
      BubbleData <- read_excel("Main_Data.xlsx", sheet = "Overall")
      state <- BubbleData$STATES
      valueB2015 <- BubbleData$STUDENTS_BY_STATES_2015
      valueB2016 <- BubbleData$STUDENTS_BY_STATES_2016
      valueB2017 <- BubbleData$STUDENTS_BY_STATES_2017
      
      if(input$animation1 == 2014) {
        #bubbles(value = valueB2015, label = state, tooltip = valueB2015,
         #     color = rainbow(15, alpha=NULL)[sample(15)])
      } else if(input$animation1 == 2015) {
        bubbles(value = valueB2015, label = state, tooltip = valueB2015,
                color = color1)#rainbow(15, alpha = NULL)[sample(15)])
      } else if(input$animation1 == 2016) {
        bubbles(value = valueB2016, label = state, tooltip = valueB2016,
                color = color1)#rainbow(15, alpha = NULL)[sample(15)])
      } else {
        bubbles(value = valueB2017, label = state, tooltip = valueB2017,
                color = color1)#rainbow(15, alpha = NULL)[sample(15)])
      }
    })
    
    output$overallPlot2 <- renderBubbles({
      BubbleData2 <- read_excel("Main_Data.xlsx", sheet = "Overall")
      field <- BubbleData2$FIELD_OF_STUDY
      #valueF2014 <- BubbleData2$STUDENTS_BY_FIELDS_2014
      valueF2015 <- BubbleData2$STUDENTS_BY_FIELDS_2015
      valueF2016 <- BubbleData2$STUDENTS_BY_FIELDS_2016
      valueF2017 <- BubbleData2$STUDENTS_BY_FIELDS_2017
      
      if(input$animation2 == 2014) {
        #bubbles(value = valueB2015, label = state, tooltip = valueB2015,
        #     color = rainbow(15, alpha=NULL)[sample(15)])
      } else if(input$animation2 == 2015) {
        bubbles(value = valueF2015, label = field, tooltip = valueF2015,
                color = color2)#rainbow(15, alpha = NULL)[sample(15)])
      } else if(input$animation2 == 2016) {
        bubbles(value = valueF2016, label = field, tooltip = valueF2016,
                color = color2)#rainbow(15, alpha = NULL)[sample(15)])
      } else {
        bubbles(value = valueF2017, label = field, tooltip = valueF2017,
                color = color2)#rainbow(15, alpha = NULL)[sample(15)])
      }
    })
  }
)