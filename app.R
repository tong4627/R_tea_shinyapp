library(shiny)
#library(tidyverse)
library(magrittr)
library(readr)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

non <- tibble("流水號" = "該關鍵字無效，請重新輸入。",
              "課程名稱" = "該關鍵字無效，請重新輸入。",
              "授課老師" = "該關鍵字無效，請重新輸入。",
              "課程大綱" = "該關鍵字無效，請重新輸入。")

course_data <- read_csv("partial_100_course_data.csv") %>% bind_rows(.,non)
fcm_course <- read_csv("fcm_course_tibble_100_0104.csv")
dfm_course <- read_csv("dfm_course_tibble_100_0104.csv")

feat_tags <- function (fcm, dfm, keyword, occurrence = 2){
  
  
  if (!keyword %in% fcm_course$feat_names){
    
    return(nrow(course_data))
    
  } else {
    
    filter_class <- function (tags = NULL){
      if (is.null(tags)){
        return(nrow(course_data))
        
      } else {
        #建立回傳值的空vector
        ans <- vector()
        
        for (i in seq_along(tags)) {
          #製造欄位名稱
          score_name <- paste("score_", i)
          #創造代數不然下下行有時候會有問題 ## 這個還是有問題
          strange <- dfm[[keyword]] * 0.5 + dfm[[tags[i]]] * 0.5
          #測試是不是這邊出錯
          #print(strange)
          
          #創造加權平均的分數欄位
          new_data <- mutate(dfm, score_name = strange) %>% 
            #重新排序由大到小
            arrange(desc(score_name)) %>% 
            head(5) %>%
            filter(score_name > 0) %>%
            .$document
          
          ans <- c(ans, new_data)
        }
        return(unique(ans))
      }
    }
    
    frame <- fcm %>% select(feat_names, keyword)
    
    for ( i in names(frame)[2:length(frame)]){
      result_tags <- frame %>% 
        filter(.[[i]] >= occurrence) %>% # 找出大於等於 occurrence 的數值
        arrange(desc(.[[i]])) %>%
        .$feat_names
      
      if (length(result_tags) <= 10 && length(result_tags) > 0){
        return(filter_class(tags = result_tags[1:length(result_tags)]))
      } else if (length(result_tags) > 10){
        return(filter_class(result_tags[1:10]))
      } else if (length(result_tags) == 0){
        return(filter_class(NULL))
      }
    }
  }
}



ui <- fluidPage(
  shiny::tags$head(
    shiny::tags$style(HTML("
      h2 {
        font-weight: 900;
        line-height: 1.1;
        color: #8B4513;
      }

    "))
  ),
  useShinyjs(),
  setBackgroundImage(src = "board.jpg", shinydashboard = FALSE),
  theme = shinytheme("united"),
  titlePanel(h3("R 分解茶", style = "color:black;font-weight:bold")),
  fluidRow(
    div(style="display:inline-block", textInput("keyword", label = NULL, placeholder = "請輸入一個感興趣的主題，例如：音樂")),
    div(style="display:inline-block", actionButton("search", label="search")),
    
    h2(textOutput("class_no")),
    h2(textOutput("class_name")),
    h2(textOutput("teacher")),
    
    br(),
    div(style="display:inline-block", actionButton("dislike", label="dislike", icon=icon("times"))),
    div(style="display:inline-block", actionButton("like", label="like",icon=icon("heart"))),
    align="center"
    #onclick ="window.open('http://google.com', '_blank')"
    #onclick="location.href='http://google.com'"
  )
)


server <- function(input, output, session) {
  v <- reactiveValues(data = 0)
  
  observeEvent(input$search, {
    v$data = 1
  })
  
  observeEvent(input$search, {
    v$keyword <- input$keyword
    v$class_no <- feat_tags(fcm_course, dfm_course, keyword = v$keyword, occurrence = 10)
    v$non <- non[1, ]
    v$course_return <- course_data[v$class_no, ]
  })
  
  observeEvent(input$like, {
    if (v$data != 0) {
      class_url <- paste0("'https://nol.ntu.edu.tw/nol/coursesearch/myschedule.php?add=", v$course_return$流水號[v$data],"'")
      open_url <- sprintf("window.open(%s, '_blank')", class_url)
      runjs(open_url)
      v$data <- v$data + 1
    }
  })
  
  observeEvent(input$dislike, {
    v$data <- v$data + 1
  })
  
  output$class_no <- renderText({
    paste0("課程流水號：", v$course_return$流水號[v$data])
  })
  
  output$class_name <- renderText({
    paste0("課程名稱　：", v$course_return$課程名稱[v$data])
  })
  
  output$teacher <- renderText({
    paste0("授課老師　：", v$course_return$授課老師[v$data])
  })
  
}

shinyApp(ui, server)
