library(shiny)
library(ggplot2)

# UI 정의
ui <- fluidPage(
  # 환자 정보 입력 폼
  textInput("name", "환자 이름"),
  numericInput("age", "환자 나이", value = 30, min = 0, max = 120),
  radioButtons("gender", "성별", choices = c("남성", "여성")),
  
  # 통증 정보 입력 폼
  sliderInput("pain_intensity", "통증 강도", min = 0, max = 10, value = 5),
  sliderInput("pain_duration", "통증 지속시간", min = 0, max = 24, value = 1),
  fileInput("image", "Select an Image File"),
  checkboxInput("pain_location_head", "두통"),
  checkboxInput("pain_location_neck", "목 통증"),
  checkboxInput("pain_location_back", "등 통증"),
  
  # 저장 버튼
  actionButton("save", "저장"),
  
  # 그림 출력
  plotOutput("plot")
)

# 서버 정의
server <- function(input, output, session) {
  # 데이터를 저장하는 함수
  save_data <- function(data, filename) {
    write.csv(data, file = filename, row.names = FALSE)
  }
  
  # 그림을 출력하는 함수
  draw_plot <- function(data) {
    ggplot(data, aes(x = pain_duration, y = pain_intensity)) +
      geom_point(aes(color = pain_location)) +
      labs(title = "통증환자 초진기록지", x = "통증 지속시간", y = "통증 강도")
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
  
  # 그림을 출력합니다.
  output$plot <- renderPlot({
    # 데이터를 불러옵니다.
    data <- read.csv(save_path)
    # 그림을 그립니다.
    draw_plot(data)
  })
}

# Shiny 애플리케이션 실행
shinyApp(ui, server)



# UI 구성
ui <- fluidPage(
  titlePanel("통증환자 초진기록지"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "성별", choices = c("남성", "여성")),
      numericInput("age", "나이", value = 30),
      selectInput("region", "지역", choices = c("서울", "경기", "인천")),
      dateInput("date", "방문일", value = Sys.Date())
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("현재 증상", 
                 textInput("symptom", "현재 증상", value = ""),
                 sliderInput("severity", "증상 심각도", min = 1, max = 10, value = 5),
                 checkboxInput("chronic", "만성적인 증상인가요?", value = FALSE)
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
      ),
      br(),
      actionButton("submit", "저장")
    )
  )
)





# 데이터를 저장하는 함수
save_data <- function(data, filename) {
  write.csv(data, file = filename, row.names = FALSE)
}

# 데이터를 읽어오는 함수
load_data <- function(data) {
  read.csv(data)
}

# Server 구성
server <- function(input, output) {
  observeEvent(input$submit, {
    # 입력한 데이터를 저장합니다.
    data <- list(
      gender = input$gender,
      age = input$age,
      region = input$region,
      date = input$date,
      symptom = input$symptom,
      severity = input$severity,
      chronic = input$chronic,
      medical_history = input$medical_history,
      treatment = input$treatment,
      note = input$note
    )
    save_data(data)

  })
}

# 앱 실행
shinyApp(ui = ui, server = server)


# 서버 정의
server <- function(input, output, session) {
  # 데이터를 저장하는 함수
  save_data <- function(data, filename) {
    write.csv(data, file = filename, row.names = FALSE)
  }
  
  # 데이터를 불러오는 함수
  read_data <- function(filename) {
    read.csv(filename)
  }
  
  # 데이터를 저장할 경로
  save_path <- "data.csv"
  
  # 저장 버튼 클릭 이벤트
  observeEvent(input$save, {
    # 데이터를 저장합니다.
    save_data(mtcars, save_path)
    # 저장 완료 메시지를 출력합니다.
    showModal(modalDialog("저장되었습니다."))
  })
  
  # 불러오기 버튼 클릭 이벤트
  observeEvent(input$load, {
    # 데이터를 불러옵니다.
    data <- read_data(save_path)
    # 데이터를 출력합니다.
    output$table <- renderDataTable(data)
    # 불러오기 완료 메시지를 출력합니다.
    showModal(modalDialog("불러왔습니다."))
  })
}



# 데이터를 저장하는 함수
save_data <- function(data) {
  write.csv(data, file = data, row.names = FALSE)
}

# 데이터를 읽어오는 함수
load_data <- function(data) {
  read.csv(data)

# Server 구성
server <- function(input, output) {
  observeEvent(input$submit, {
    # 입력한 데이터를 저장합니다.
    data <- list(
      gender = input$gender,
      age = input$age,
      region = input$region,
      date = input$date,
      symptom = input$symptom,
      severity = input$severity,
      chronic = input$chronic,
      medical_history = input$medical_history,
      treatment = input$treatment,
      note = input$note
    )
    save_data(data)
    # 저장된 데이터를 출력합니다.
    output$report <- renderPrint({
      paste("성별: ", input$gender, "\n",
             "나이: ", input$age, "\n",
             "지역: ", input$region, "\n",
             "방문일: ", input$date, "\n\n",
             "현재 증상: ", input$symptom, "\n",
             "증상 심각도: ", input$severity, "\n",
             "만성적인 증상: ", ifelse(input$chronic, "예", "아니오"), "\n",
             "과거력: ", paste(input$medical_history, collapse = ", "), "\n",
             "치료 및 복용 중인 약물: ", input$treatment, "\n",
             "기타 사항: ", input$note)
    })
  })
}



}

# Server 구성
server <- function(input, output) {
  observeEvent(input$submit, {
    # 입력한 데이터를 저장합니다.
    data <- list(
      gender = input$gender,
      age = input$age,
      region = input$region,
      date = input$date,
      symptom = input$symptom,
      severity = input$severity,
      chronic = input$chronic,
      medical_history = input$medical_history,
      treatment = input$treatment,
      note = input$note
    )
    save_data(data)
  })
  
  output$report <- renderPrint({
    # 저장된 데이터를 읽어옵니다.
    data <- load_data()
    # 읽어온 데이터를 출력합니다.
    paste0("성별: ", data$gender, "\n",
           "나이: ", data$age, "\n",
           "지역: ", data$region, "\n",
           "방문일: ", data$date, "\n\n",
           "현재 증상: ", data$symptom, "\n",
           "증상 심각도: ", data$severity, "\n",
           "만성적인 증상: ", ifelse(data$chronic, "예", "아니오"), "\n",
           "과거력: ", paste(data$medical_history, collapse = ", "), "\n",
           "치료 및 복용 중인 약물: ", data$treatment, "\n",
           "기타 사항: ", data$note)
  })
}
