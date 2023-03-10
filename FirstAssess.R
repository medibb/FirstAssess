####초진기록지 ####
# UI 정의
ui <- fluidPage(
  titlePanel("통증환자 초진기록지"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "이름"),
      selectInput("gender", "성별", choices = c("남성", "여성")),
      radioButtons("gender", "성별", choices = c("남성", "여성")),
      numericInput("age", "나이", value = 30, min = 0, max = 120),
      selectInput("region", "지역", choices = c("서울", "경기", "인천")),
      dateInput("date", "방문일", value = Sys.Date())
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("현재 증상", 
                 textInput("symptom", "현재 증상", value = ""),
                 sliderInput("severity", "증상 심각도", min = 1, max = 10, value = 5),
                 checkboxInput("chronic", "만성적인 증상인가요?", value = FALSE),
                 sliderInput("pain_intensity", "통증 강도", min = 0, max = 10, value = 5),
                 sliderInput("pain_duration", "통증 지속시간", min = 0, max = 24, value = 1),
                 checkboxInput("pain_location_head", "두통"),
                 checkboxInput("pain_location_neck", "목 통증"),
                 checkboxInput("pain_location_back", "등 통증"),
        ),
        tabPanel("과거력", 
                 checkboxGroupInput("medical_history", "과거력", 
                                    choices = c("고혈압", "당뇨병", "심장질환", "간질환", "신장질환", "암", "기타"), 
                                    selected = character(0))
        ),
        tabPanel("치료 및 복용 중인 약물",
                 textInput("treatment", "치료 및 복용 중인 약물", value = "")
        ),
        tabPanel("기타 사항",
                 textInput("note", "기타 사항", value = "")
        )
      )
    )
  ),
  actionButton("save", "저장"),
)


# 서버 정의
server <- function(input, output, session) {
  # 데이터를 저장하는 함수
  save_data <- function(data, filename) {
    write.csv(data, file = filename, row.names = FALSE)
  }
  
  # 데이터를 저장할 경로
  save_path <- "pain_data.csv"
  
  # 저장 버튼 클릭 이벤트
  observeEvent(input$save, {
    # 데이터를 생성합니다.
    data <- data.frame(
      name = input$name,
      age = input$age,
      gender = input$gender,
      pain_intensity = input$pain_intensity,
      pain_duration = input$pain_duration,
      pain_location = paste(
        ifelse(input$pain_location_head, "두통", ""),
        ifelse(input$pain_location_neck, "목 통증", ""),
        ifelse(input$pain_location_back, "등 통증", ""),
        sep = ", "
      )
    )
    # 데이터를 저장합니다.
    save_data(data, save_path)
    # 저장 완료 메시지를 출력합니다.
    showModal(modalDialog("저장되었습니다."))
  })
}

# Shiny 애플리케이션 실행
shinyApp(ui, server)



#### 그림그리기 ####
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Pain Diagram"),
  sidebarLayout(
    sidebarPanel(
      actionButton("clear", "Clear"),
      br(),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      plotOutput("plot1", click = "plot_click")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load pain diagram image
  img_url <- "./pain_diagram.png"
  img <- readPNG(img_url)
  
  # Initialize variables
  clicked_points <- reactiveValues()
  
  # Draw plot with image
  output$plot1 <- renderPlot({
    plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
    rasterImage(img, xleft = 0.5, ybottom = 0.5, xright = 9.5, ytop = 9.5)
    
    # Add points if clicked by user
    if (!is.null(clicked_points$x)) {
      points(clicked_points$x, clicked_points$y, col = "red", pch = 16)
    }
    
    # Clear points if button is clicked
    observeEvent(input$clear,{
      clicked_points$x <- NULL
      clicked_points$y <- NULL
    })
    
    # Save image if button is clicked
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("pain_diagram_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        png(file)
        plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
        rasterImage(img, xleft = 0.5, ybottom = 0.5, xright = 9.5, ytop=9.5)
        if (!is.null(clicked_points$x)) {
          points(clicked_points$x,
                 clicked_points$y,
                 col="red",
                 pch=16)
        }
        dev.off()
      }
      
    )
    
  })
  
  # Record clicks on plot and store in reactiveValues object 
  observeEvent(input$plot_click,{
    click_x <- round(input$plot_click$x)
    click_y <- round(input$plot_click$y)
    if(click_x <8 & click_x>2 & click_y <8 & click_y>2){
      isolate({
        clicked_points$x<-c(clicked_points$x,
                            input$plot_click$x)
        clicked_points$y<-c(clicked_points$y,
                            input$plot_click$y)}
      )
    }
    
  })
}

# Run the application 
shinyApp(ui=ui , server=server )

