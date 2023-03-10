library(shiny)
library(png)

# Define UI
ui <- fluidPage(
  tags$head(
  tags$meta(charset = "utf-8")
  ),
  titlePanel("통증환자 초진기록지"),
  textInput("ID", "ID이름"),
  sidebarLayout(
    sidebarPanel("이 문진표에 포함된 모든 질문은 엄격히 비밀이 유지되며 귀하의 의료 기록의 일부가 됩니다.", width =4,
      tabsetPanel(
        tabPanel("통증의 양상",
                 selectInput("cc", "제일 아픈 곳(우측 그림에도 표시)",
                             choices = c("목", "어깻죽지", "어깨", "팔", "팔꿈치", "손목/손", "허리", "엉덩이", "허벅지", "무릎","종아리", "발")),
                 selectInput("comobid", "그외 아픈 곳(우측 그림에도 표시)",
                             choices = c("목", "어깻죽지", "어깨", "팔", "팔꿈치", "손목/손", "허리", "엉덩이", "허벅지", "무릎","종아리", "발"), multiple = TRUE),
                 selectInput("onset", "통증이 언제 생겼나요?", choices = c("1주일 내", "1달 내", "3달 내", "6개월 이상")),
                 dateInput("date", "통증이 생긴 날이 기억나면 선택해주세요", value = Sys.Date()),
                 selectInput("mode", "통증이 어떻게 생겼나요?", choices = c("갑자기", "서서히", "모르겠다")),
        ),
        tabPanel("일상생활",
                 radioButtons("lifestyle", "집에서 생활할 때", choices = c("좌식생활(바닥에 앉고 바닥에서 잡니다.)", "입식생활(의자에 앉고 침대에서 잡니다.)",
                                                          "입식과 좌식 반반")),
                 textInput("exercise", "현재 하고있는 운동?"),
                 selectInput("injHx", "최근 1달 내 시술(주사)", choices = c("있다", "없다")),
                 br(),
                downloadButton("downloadData", "그림저장"),
                actionButton("save", "저장"),
                
        )
      )
      ),
    mainPanel(height = "1000px",
      sliderInput("nrs", "현재의 통증 강도를 0 ~ 10 점사이로 표시해 주세요 (10 - 죽을만큼아픔, 절단통 / 9 - 산통 / 5 - 중간 / 0 - 통증없음)",
      min = 0, max = 10, value = 5, width = '100%'),
      plotOutput("plot1", click = "plot_click"),
      actionButton("clear", "다시 그리기")
    )
  )
)



# Define server logic
server <- function(input, output) {
  
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
      ID = input$ID,
      cc = input$cc,
      onset = input$onset,
      date = input$date,
      mode = input$mode,
      lifestyle = input$lifestyle,
      exercise = input$exercise,
      injHx = input$injHx,
      nrs = input$nrs
    )
    # 데이터를 저장합니다.
    save_data(data, save_path)
    # 저장 완료 메시지를 출력합니다.
    showModal(modalDialog("저장되었습니다."))
  })
  
  
  # Load pain diagram image
  img_url <- "./pain_diagram.png"
  img <- readPNG(img_url)
  
  # Initialize variables
  clicked_points <- reactiveValues()
  
  # Clear points if button is clicked
  observeEvent(input$clear,{
    clicked_points$x <- NULL
    clicked_points$y <- NULL
  })
  
  # Draw plot with image
  output$plot1 <- renderPlot({
    plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
    rasterImage(img, xleft = 0, ybottom = 0.5, xright = 10, ytop=10)

    
    # Add points if clicked by user
    if (!is.null(clicked_points$x)) {
      points(clicked_points$x, clicked_points$y, col = "red", pch = 16)
    }
    
    output$nameOutput <- renderText({
      paste("당신의 이름은", input$name, "입니다.")
    })
    

    # Save image if button is clicked
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("pain_diagram_", Sys.Date(), ".png")
      },
      
      content = function(file) {
        png(file)
        plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "")
        rasterImage(img, xleft = 0, ybottom = 0.5, xright = 10, ytop=10)
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
    isolate({
      clicked_points$x<-c(clicked_points$x,
                          input$plot_click$x)
      clicked_points$y<-c(clicked_points$y,
                          input$plot_click$y)}
    )

  })
}

# Run the application 
shinyApp(ui=ui , server=server )
