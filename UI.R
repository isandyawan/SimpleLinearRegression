shinyUI(fluidPage(
  titlePanel("Program Analisis Regresi"),
  
  sidebarLayout(
    sidebarPanel(
      code(textOutput("error")),
      p("Ignatius Sandyawan",align ="center"),
      p("2KS1/15.8657", align="center"),
      selectInput("separator", label = h3("Jenis Delimiter"), 
                  choices = list("Koma (,)" = 1, "Semicolon (;)" = 2),selected = 2), 
      helpText("File harus berekstensi .csv . Jika error saat upload, coba ganti delimiter"),
      fileInput("file", label = h3("Inputkan File"),accept = c("text/csv",".csv")),
      uiOutput("var_dep_select"),
      uiOutput("var_indep_select"),
      uiOutput("analisis_button")
    ),
    
    mainPanel(
      tabsetPanel(id="panel",
        tabPanel("Korelasi Variabel",value="kor", 
                 helpText("Pilih variabel independen dengan korelasi yang kuat dengan variabel independen"),
                 h1("Plot Korelasi"),
                 plotOutput("cor_plot"),
                 h2("Korelasi"),
                 htmlOutput("cor_value")),
        tabPanel("Model Regresi",value="model", 
                 textOutput("kesimpulan"),
                 verbatimTextOutput("regresi"),
                 plotOutput("plotFitted")),
        tabPanel("Uji Asumsi",value="asumsi",
                 h3(textOutput("judul_homoskedas")),
                 verbatimTextOutput("hasil_homoskedas"),
                 textOutput("kesimpulan_homoskedas"),
                 htmlOutput("saran_homoskedas"),
                 h3(textOutput("judul_autokol")),
                 verbatimTextOutput("hasil_autokol"),
                 textOutput("kesimpulan_autokol"),
                 htmlOutput("saran_autokol"),
                 h3(textOutput("judul_normalitas")),
                 verbatimTextOutput("hasil_normalitas"),
                 textOutput("kesimpulan_normalitas"),
                 htmlOutput("saran_normalitas"),
                 h3(textOutput("judul_multikol")),
                 verbatimTextOutput("hasil_multikol"),
                 textOutput("kesimpulan_multikol"),
                 htmlOutput("saran_multikol")
                 )
      )
      
      
    )
  )
))