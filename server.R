
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shinydashboard)
library(ggplot2)
library(data.table)
library(stringr)
library(plotly)
library(plyr)
library(dplyr)
library(shiny)


shinyServer(function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  
  drawerfile <- reactive({
    
    # check if the file input is null
    if(is.null(input$file1)|is.null(input$file2)) { return(NULL) }
    
    # input file
    tmpfile1 <- fread(input$file1$datapath)
    tmpfile2 <- fread(input$file2$datapath)
    
    # Manipulating stocking file
    tmpfile1 <- tmpfile1[!is.na(價格)]
    tmpfile1 <- tmpfile1[選項 ==""]
    tmpfile1[is.na(成本價),成本價 := 價格 * 0.33]
    tmpfile1[,商品名稱 := str_replace_all
             (商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>% trimws ]
    
    # Manipulating sales file
    tmpfile2 <- tmpfile2[,!duplicated(colnames(tmpfile2)),with = FALSE]
    tmpfile2[,`:=`(訂單狀態 = first(訂單狀態),送貨狀態 = first(送貨狀態))
             ,by = .(訂單號碼)]
    tmpfile2 <- tmpfile2[!(訂單狀態 == "已取消"|送貨狀態 == "已退貨"
                |送貨狀態 == "退貨中")]
    saledata <- tmpfile2 %>% group_by(商品名稱) %>% 
      summarise(sales = sum(數量)) %>% setDT
    saledata[,商品名稱 := str_replace_all
               (商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>% trimws ]
    
    # Merge two file
    finaldata <- merge(saledata,tmpfile1,by = "商品名稱")
    
    # Classificaion
    finaldata[grepl("戒指|對戒|戒組|尾戒|關節戒|連指戒|情侶戒|三件戒|開口戒",finaldata$商品名稱),"category"]<-"戒指"
    finaldata[grepl("耳環|耳針|耳扣|耳夾",finaldata$商品名稱),"category"]<-"耳環"
    finaldata[grepl("項鍊|鎖骨鍊|頸鍊|頸圈",finaldata$商品名稱),"category"]<-"項鍊"
    finaldata[grepl("手鍊|手環|手鐲",finaldata$商品名稱),"category"]<-"手鍊"
    finaldata[grepl("髮飾|髮帶|髮圈|髮夾|髮箍",finaldata$商品名稱),"category"]<-"髮飾"
    finaldata[grepl("手錶",finaldata$商品名稱),"category"]<-"手錶"
    finaldata[grepl("刺青貼紙",finaldata$商品名稱),"category"]<-"刺青貼紙"
    finaldata[grepl("墨鏡",finaldata$商品名稱),"category"]<-"墨鏡"
    finaldata[grepl("腳鍊",finaldata$商品名稱),"category"]<-"腳鍊"
    finaldata[is.na(finaldata$category),"category"]<-"其它"
    finaldata[,category := as.factor(category)]
    
    # Assign manufacturer region
    finaldata[,商店貨號 := sapply(商店貨號,function(k) 
      {strsplit(k,"-")} %>% .[[1]] %>% .[1]) %>% as.character]
    finaldata[substr(商店貨號,1,1) == "K" | 商店貨號 == "IHNS",region := "Korea"]
    finaldata[is.na(region),region := "China"]
    
    # Last Manipulating on calculating main feature
    finaldata[,totalcost := sales * 成本價]
    colnames(finaldata)[grep("庫存",colnames(finaldata))] <- "stock"
    finaldata[,stockcost := stock * 成本價]
    data.table(finaldata)
    })
  
  # Creating reactive object for stock checking
    stocker <- reactive({
  # Check the presence of input file
    if(is.null(input$file1)) {return(NULL)}
  
  # Input file
    juicyfile <- fread(input$file1$datapath)
    
  # Manipulating stocking file
    juicyfile <- juicyfile[!is.na(價格)]
    juicyfile <- juicyfile[選項 ==""]
    juicyfile[is.na(成本價),成本價 := 價格 * 0.33]
    juicyfile <- juicyfile[選項 == ""]
    juicyfile[,商品名稱 := str_replace_all
             (商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>% trimws ]
    
  # Classification
    juicyfile[grepl("戒指|對戒|戒組|尾戒|關節戒|連指戒|情侶戒|三件戒|開口戒",juicyfile$商品名稱),"category"]<-"戒指"
    juicyfile[grepl("耳環|耳針|耳扣|耳夾",juicyfile$商品名稱),"category"]<-"耳環"
    juicyfile[grepl("項鍊|鎖骨鍊|頸鍊|頸圈",juicyfile$商品名稱),"category"]<-"項鍊"
    juicyfile[grepl("手鍊|手環|手鐲",juicyfile$商品名稱),"category"]<-"手鍊"
    juicyfile[grepl("髮飾|髮帶|髮圈|髮夾|髮箍",juicyfile$商品名稱),"category"]<-"髮飾"
    juicyfile[grepl("手錶",juicyfile$商品名稱),"category"]<-"手錶"
    juicyfile[grepl("刺青貼紙",juicyfile$商品名稱),"category"]<-"刺青貼紙"
    juicyfile[grepl("墨鏡",juicyfile$商品名稱),"category"]<-"墨鏡"
    juicyfile[grepl("腳鍊",juicyfile$商品名稱),"category"]<-"腳鍊"
    juicyfile[is.na(juicyfile$category),"category"]<-"其它"
    
  # Assigning Region
    juicyfile[,商店貨號 := sapply(商店貨號,function(k) 
      {strsplit(k,"-")} %>% .[[1]] %>% .[1]) %>% as.character]
    juicyfile[substr(商店貨號,1,1) == "K" | 商店貨號 == "IHNS",region := "Korea"]
    juicyfile[is.na(region),region := "China"]
  
  # Calculating main feature
    colnames(juicyfile)[grep("庫存",colnames(juicyfile))] <- "stock"
    juicyfile[,stockcost := stock * 成本價]
    juicyfile <- juicyfile[!stock == 0]
    })
  
     # Ploting first panel
    output$bycategory <- renderPlotly({
      if (is.null(stocker())) {return(NULL)}
    kkbox  <- stocker() %>% group_by(category,region) %>% summarise(stockcost = sum(stockcost)) %>%
      ggplot(aes(category,stockcost,fill = region)) + geom_bar(stat = "identity",position = "dodge") +
        geom_text(aes(label = stockcost),position = position_dodge(width = 1)) + theme_bw()
    ggplotly(kkbox)
    })
    
     # Ploting second panel
    output$byregion <- renderPlotly({
      if (is.null(stocker())) {return(NULL)}
    applemusic <- stocker() %>% group_by(category,region) %>% summarise(num = n() ) %>%
      ggplot(aes(category,num,fill = region)) + geom_bar(stat = "identity",position = "dodge") +
      geom_text(aes(label = num),position = position_dodge(width = 1)) + theme_bw()
    ggplotly(applemusic)
    })
    
    # Ploting third panel
    output$byboth <- renderPlotly({
      if (is.null(drawerfile())) {return(NULL)}
    spotify <- drawerfile() %>% group_by(category,region) %>% 
      summarise(totalcost = sum(totalcost)) %>% ggplot(aes(category,totalcost,fill = region)) +
        geom_bar(stat = "identity",position = "dodge") + geom_text(aes(label = totalcost),position = position_dodge(width = 1)) + 
          theme_bw()
    ggplotly(spotify)
    })
    
    # Ploting fourth panel
    output$byN <- renderPlotly({
      if (is.null(drawerfile())) {return(NULL)}
      googlemusic <- drawerfile() %>% group_by(category,region) %>% summarise(num = n()) %>%
        ggplot(aes(category,num,fill = region)) + geom_bar(stat = "identity",position = "dodge") +
          geom_text(aes(label = num),position = position_dodge(width = 1)) +
            theme_bw()
    ggplotly(googlemusic)
    })
})