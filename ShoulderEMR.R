library(shiny)
library(png)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "utf-8")
  ),
  titlePanel("어깨통증환자 기록지"),
  textInput("ID", "ID이름"),
  sidebarLayout(
    sidebarPanel( width =4,
                 tabsetPanel(
                   tabPanel("Subjectives",
                            selectInput("onset","onset" , choices = c("1주일 내", "1달 내", "3달 내", "6개월 이상")),
                            dateInput("date", "onset_date", value = Sys.Date()),
                            selectInput("mode", "mode", choices = c("갑자기", "서서히", "잘 모르게")),
                            radioButtons("side", "side", choices = c("Rt.", "Lt."),inline = TRUE),
                            selectInput("area", "area",
                                        choices = c("lateral","ant.","post." )),
                            sliderInput("nrs", "Present_nrs",
                                        min = 0, max = 10, value = 5),
                            sliderInput("nrsR", "resting_nrs",
                                        min = 0, max = 10, value = 5),
                            sliderInput("nrsS", "severe_nrs",
                                        min = 0, max = 10, value = 5),
                            textInput("agg", "aggfactor"),
                            textInput("exercise", "운동"),
                            textInput("job", "직업"),
                   ),
                   tabPanel("ROM",
                            textInput("Flx","Flx"),
                            textInput("Abd","Abd"),
                            textInput("ES","ES"),
                            textInput("IS","IS"),
                            textInput("ER","ER")
                   ),
                   tabPanel("Objectives",
                            radioButtons("ERpain", "ER pain", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Fpain", "Flexion pain", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Abpain", "Abduction pain", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Hawkins", "Hawkin's sign", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Neer", "Neer test", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Emptycan", "Empty can", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("bellypress", "bellypress", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("Hadd", "Horizontal adduction", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("ACJtd", "AC Joint td", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("parc", "painful arc", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("instability", "instability test", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("apprehension", "apprehension test", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                            radioButtons("spurling", "spurling test", choices = c("-", "+","++"), selected = "-",inline = TRUE),
                   )
                 )
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("Shoulder xray & USG",
                 selectInput("xray", "xray",
                             choices = c("no_specific_abnormal_findings","calcific deposits")),
                 radioButtons("LBTS", "LBTS", choices = c("mild", "moderate","severe","no"), selected = "no",inline = TRUE),
                 selectInput("SSc", "SSc", choices = c("no_definite_abnormality","thickening","hypoattenuation", "calcific deposit","cortical_irregularity",
                                                       "sup. tendon","inf. tendon"), selected = "no_definite_abnormality", multiple = TRUE),
                 radioButtons("AC", "AC", choices = c("no_definite_abnormality", "narrowing","osteophyte"), selected = "no_definite_abnormality",inline = TRUE),
                 selectInput("SST", "SST", choices = c("no_definite_abnormality","thickening","hypoattenuation", "calcific deposit","cortical_irregularity",
                                                       "ant. tendon","post. tendon"), selected = "no_definite_abnormality", multiple = TRUE),
                 radioButtons("Shortaxis", "Shortaxis", choices = c("no_definite_abnormality", "cleft", "sagging"), selected = "no_definite_abnormality",inline = TRUE),
                 selectInput("SASD", "SASD", choices = c("no_definite_abnormality", "thickened","effusion"), selected = "no_definite_abnormality", multiple = TRUE),
                 selectInput("IST", "IST", choices = c("no_definite_abnormality","thickening","hypoattenuation", "calcific deposit","cortical_irregularity",
                                                         "ant. tendon","post. tendon"), selected = "no_definite_abnormality", multiple = TRUE),
                 selectInput("inj", "inj", choices = c("IA inj : total 20cc, tam1@", "rupture(-)", "leaking(-)", "SASD inj : total 10cc, tam1@","Barbotage"), selected = "-")
                 ),
        tabPanel("Assessment & Plan",
                 radioButtons("Assess", "Assess", choices = c("# Painful stiff shoulder, clinically adhesive capsulitis","# Rotator cuff tendinopathy",
                                                      "# Calcific tendinitis on rotator cuff"), selected = "# Painful stiff shoulder, clinically adhesive capsulitis",inline = FALSE),
                 selectInput("Plan", "Plan", choices = c("# impinge ser","# Scapular Stabilizing Exercise - stage 1","# ROM exercise with posteror stretch",
                 "# NSAIDs","# RTC in 6wks"), selected = "# RTC in 6wks", multiple = TRUE),
                 br(),
                 actionButton("save", "저장"),
                 )
      )
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
  save_path <- "pain_data.txt"
  
  # 저장 버튼 클릭 이벤트
  observeEvent(input$save, {
    # 데이터를 생성합니다.
    data <- paste("S)", "onset:", input$onset,"ago", "(날짜:", input$date, ")", "\n",
                  "mode: ", input$mode,"생긴 통증", "\n",
                  "area: ", input$side, "Shoulder","에",input$area, "생긴", "\n",
                  "통증nRS: ", input$nrs, "\n",
                  "resting nRS: ", input$nrsR, "\n",
                  "severe nRS: ", input$nrsS, "\n",
                  "Agg factor: ", input$agg, "\n",
                  "운동: ", input$exercise, "\n",
                  "직업: ", input$job, "\n",
                  "ROM(Flx-Abd-ES-IS-ER) :", input$Flx,"-",input$Abd,"-",input$ES,"-",input$IS,"-",input$ER,"\n",
                  "ER pain (", input$ERpain, ")", "\n",
                  "flexion & abduction pain (", input$Fpain, " / ", input$Abpain,")", "\n",
                  "Hawkin's / Neer / Empty-can / belly press: (", input$Hawkins, " / ", input$Neer, " / ", input$Emptycan, " / ",input$bellypress ,")", "\n",
                  "horizontal add (", input$Hadd, ")", "\n",
                  "AC joint Td (", input$ACJtd, ")", "\n",
                  "painful arc / resistive abduction (", input$parc,")", "\n",
                  "instability test / apprehension test (", input$instability, "/", input$apprehension, ")", "\n",
                  "spurling (", input$spurling, ")","\n",
                  "\n",
                  "o)", "Shoulder xray :", input$xray, "\n",
                  "\n",
                  "Shoulder USG",
                  "LBTS :", input$LBTS, "effusion","\n",
                  "SSc:", input$SSc,"\n",
                  "AC :", input$AC,"\n",
                  "SST :",input$SST,"\n",
                  "Shortaxis:",input$Shortaxis,"\n",
                  "SASD bursa :", input$SASD,"\n",
                  "IST :", input$IST,"\n",
                  "inj :", input$inj,"\n",
                  "\n",
                  "A)", input$Assess,"\n",
                  "\n",
                  "P)", input$Plan
                  )
    # 데이터를 저장합니다.
    save_data(data, save_path)
    writeClipboard(data)
    # 저장 완료 메시지를 출력합니다.
    showModal(modalDialog("dismiss"))
    })

}

# Run the application 
shinyApp(ui=ui , server=server )
