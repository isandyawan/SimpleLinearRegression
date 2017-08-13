library("lmtest")
library("VIF")
library("car")

shinyServer(
  function(input, output, session) {
    filedata <- reactive({
      infile <- input$file
      if (is.null(infile)) {
        return(NULL)
      }
      if(infile$type!="text/csv"){
        output$error<-renderText("Format file tidak didukung, silakan ulangi upload")
        return(NULL)
      }
      switch(input$separator,
             "1"=read.csv(infile$datapath),
             "2"=read.csv2(infile$datapath)
      )
    })
    output$var_dep_select <- renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      for(i in 1:length(items)){
        if(!is.numeric(df[3,names(df[i])])){
          remove<-df[names(df[i])]
          items<-names(df[which(!df %in% remove)])
        }
      }
      names(items)=items
      selectInput("var_dep", "Variabel Dependen:",items,selected = 3)
    })
    
    output$var_indep_select<-renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      for(i in 1:length(items)){
        if(!is.numeric(df[3,names(df[i])])){
          remove<-df[names(df[i])]
          items<-names(df[which(!df %in% remove)])
        }
      }
      names(items)=items
      remove<-var_dep()
      items<-items[-which(items %in% remove)]
      checkboxGroupInput("var_indep", label = h3("Variabel Independen"), 
                         choices = items)
    })
    
    output$analisis_button<-renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      actionButton("analisis", label = "Analisis")
    })
    
    var_dep<-reactive({
      input$var_dep  
    })
    
    regresi<-reactive({
      dependent<-var_dep()
      data<-filedata()
      #y<-data.matrix(data[dependent])
      variabel<-paste(input$var_indep,collapse = "+")
      y<-paste0(dependent,"~")
      return(lm(paste0(y,variabel),data = filedata()))
    })
    
    observeEvent(input$analisis, {
      if(is.null(input$var_indep)){
        output$error<-renderText("Galat : Tidak ada variable independen terpilih")
        return(NULL)
      }
      updateTabsetPanel(session, "panel",
                        selected = "model")
      output$regresi<-renderPrint(summary(regresi()))
      output$plotFitted<-renderPlot(plot(regresi(),which=1))
    })
    
    cor_value<-reactive({
      data<-filedata()
      jmlcol<-ncol(data)
      text<-""
      dependent<-var_dep()
      if(!is.numeric(data[3,dependent])){
        return(NULL)
      }
      for(i in 1:jmlcol){
        if (is.numeric(data[,i])){
          if (dependent!=names(data[i]))
            text[i]<-paste("Korelasi",dependent,"dengan",colnames(data[i]),":",cor(data[dependent],data[i]))
        }
      }
      return(paste0(text,collapse = "<br/>"))
    })
    
    output$cor_plot<-renderPlot({
      df <-filedata()
      if (is.null(df)) return(NULL)
      pairs(df)
    })
    
    output$cor_value<-renderUI({
      df <-filedata()
      if (is.null(df)) return(NULL)
      return(HTML(cor_value()))
    })
    
    output$kesimpulan<-renderText({
      kesimpulan<-"Kesimpulan :"
      cocok<-TRUE
      summary<-coef(summary(regresi()))
      f<-summary(regresi())$fstatistic
      if(f[1]<qf(0.95,f[2],f[3])){
        return(paste(kesimpulan,"Jadi dengan tingkat signifikansi 5% tidak ada cukup bukti yang mendukung bahwa ada pengaruh dari variabel independen terhadap variabel dependen secara simultan"))
      }
      pvaluet<-summary[,"Pr(>|t|)"]
      for(i in 2:length(pvaluet)){
        if(pvaluet[i]>0.05){
          cocok<-FALSE
          kesimpulan<-paste(kesimpulan,"variabel",names(pvaluet[i]),"tidak berpengaruh terhadap model.")
        }
      }
      if(cocok){
        return(paste(kesimpulan,"Jadi dengan tingkat signifikansi 5% terdapat cukup bukti yang mendukung bahwa ada pengaruh dari variabel independen terhadap variabel dependen"))
      }else{
        return(paste(kesimpulan,"Jadi dengan tingkat signifikansi 5% tidak ada cukup bukti yang mendukung bahwa ada pengaruh dari variabel independen terhadap variabel dependen secara parsial"))
      }
    })
    
    ####Get Nilai Uji Asumsi####
    homoskedas<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      return(bptest(regresi()))
    })
    autokol<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      return(dwtest(regresi()))
    })
    normalitas<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      residual<-resid(regresi())  
      return(shapiro.test(residual))
    })
    multikol<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      return(vif(regresi()))
    })
    
    ifmulti<-reactive({
      df <-filedata()
      if (is.null(df)) return(NULL)
      multi<-FALSE
      for(i in 1:length(multikol())){
        if(multikol()[i]>5) multi<-TRUE
      }
      return(multi)
    })
    
    
    ####Uji Asumsi####
    #Homoskedastisitas
    output$judul_homoskedas<-renderText("Asumsi Homoskedastisitas")
    output$hasil_homoskedas<-renderPrint(homoskedas())
    output$kesimpulan_homoskedas<-renderText(
      if(homoskedas()$p.value>0.05)
        return("Dengan tingkat signifikansi 5% tidak ada cukup bukti yang mendukung bahwa terjadi heteroskedastisitas")
      else return("Dengan tingkat signifikansi 5% ada cukup bukti yang mendukung bahwa terjadi heteroskedastisitas")
    )
    output$saran_homoskedas<-renderUI(
      if(homoskedas()$p.value>0.05) return(NULL)
      else{
        saran<-NULL
        saran[1]<-"Saran, lakukan salah satu metode di bawah ini :"
        saran[2]<-"1. Lakukan metode generalized least square"
        saran[3]<-"2. Transformasikan model dengan ln"
        saran[4]<-"3. Transformasikan model dengan 1/Xi"
        saran[5]<-"4. Transformasikan model dengan E(Yi)"
        saran_total<-paste0(saran,collapse = "<br/>")
        return(HTML(saran_total))  
      }
    )
    
    #Autokorelasi
    output$judul_autokol<-renderText("Asumsi Autokorelasi")
    output$hasil_autokol<-renderPrint(autokol())
    output$kesimpulan_autokol<-renderText(
      if(autokol()$p.value>0.05)
        return("Dengan tingkat signifikansi 5% tidak ada cukup bukti yang mendukung bahwa terjadi autokorelasi")
      else return("Dengan tingkat signifikansi 5% ada cukup bukti yang mendukung bahwa terjadi autokorelasi")
    )
    output$saran_autokol<-renderUI(
      if(autokol()$p.value>0.05) return(NULL)
      else{
        sarana<-NULL
        sarana[1]<-"Saran, lakukan salah satu metode di bawah ini :"
        sarana[2]<-"1. Tambahkan variabel independen baru"
        sarana[3]<-"2. Gunakan variabel independen yang telah ditransformasi"
        sarana[4]<-"3. Lakukan prosedur Cochrane-Orcutt"
        sarana[5]<-"4. Lakukan prosedur Hildreth_Lu"
        sarana_total<-paste0(sarana,collapse = "<br/>")
        return(HTML(sarana_total))  
      }
    )
    
    #Normalitas    
    output$judul_normalitas<-renderText("Asumsi Normalitas")
    output$hasil_normalitas<-renderPrint(normalitas())
    output$kesimpulan_normalitas<-renderText(
      if(normalitas()$p.value>0.05)
        return("Dengan tingkat signifikansi 5% tidak ada cukup bukti yang mendukung bahwa residual tidak berdistribusi normal")
      else
        return("Dengan tingkat signifikansi 5% ada cukup bukti yang mendukung bahwa residual tidak berdistribusi normal")
    )
    output$saran_normalitas<-renderUI(
      if(normalitas()$p.value>0.05) return(NULL)
      else{
        sarann<-NULL
        sarann[1]<-"Saran, lakukan salah satu metode di bawah ini :"
        sarann[2]<-"1. Buang data yang outlier"
        sarann[3]<-"2. Transformasikan model dengan ln"
        sarann_total<-paste0(sarann,collapse = "<br/>")
        return(HTML(sarann_total))  
      }
    )
    
    #Multikolinieritas
     output$judul_multikol<-renderText("Asumsi Multikolinieritas")
     output$hasil_multikol<-renderPrint(multikol())
     output$kesimpulan_multikol<-renderText(
       if(ifmulti())
         return("Terjadi multikolinieritas pada model")
       else
         return("Tidak terjadi multikolinieritas pada model")
     )
     output$saran_multikol<-renderUI(
       if(!ifmulti()) return(NULL)
       else{
         saranm<-NULL
         saranm[1]<-"Saran, lakukan salah satu metode di bawah ini :"
         saranm[2]<-"1. Cari informasi apriori yang cocok"
         saranm[3]<-"2. Hubungkan data cross-sectional dan data time series"
         saranm[4]<-"3. Mengeluarkan satu atau beberapa variabel bebas"
         saranm_total<-paste0(saranm,collapse = "<br/>")
         return(HTML(saranm_total))
       }
     )
     

  })
  