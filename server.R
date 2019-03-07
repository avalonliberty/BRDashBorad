
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
  
  
  sales_plot_filefile <- reactive({
    
    # check if the file input is null
    if(is.null(input$stock_file_path) | is.null(input$sales_file_path)) {
      return(NULL)
      }
    
    # input file
    stock_file <- fread(input$stock_file_path$datapath)
    sales_file <- fread(input$sales_file_path$datapath)
    
    # Manipulating stocking file
    stock_file <- stock_file[!is.na(價格)]
    stock_file <- stock_file[選項 == ""]
    stock_file[is.na(成本價), 成本價 := 價格 * 0.33]
    stock_file %>%
      `[`(j = 商品名稱 :=
            str_replace_all(商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>%
            trimws )
    
    # Manipulating sales file
    sales_file <- sales_file %>%
      `[`(j = !duplicated(colnames(sales_file)), with = FALSE)
    sales_file %>%
      `[`(j = `:=`(訂單狀態 = first(訂單狀態),
                   送貨狀態 = first(送貨狀態)) ,by = .(訂單號碼))
    sales_file <- sales_file[!(訂單狀態 == "已取消" |
                               送貨狀態 == "已退貨" |
                               送貨狀態 == "退貨中")]
    saledata <- sales_file %>%
      group_by(商品名稱) %>% 
      summarise(sales = sum(數量)) %>%
      setDT
    saledata %>%
      `[`(j = 商品名稱 :=
            str_replace_all(商品名稱,"(\\[.*?\\])|\\【.*?\\】","") %>%
            trimws)
    
    # Merge two file
    final_data <- merge(saledata, stock_file, by = "商品名稱")
    
    # Classificaion
    final_data %>%
      `[`(grepl("戒指 | 對戒 | 戒組 | 尾戒 | 關節戒 | 連指戒 | 情侶戒 |
                三件戒 | 開口戒", 商品名稱), category := "戒指")
    final_data %>%
      `[`(grepl("耳環 | 耳針 | 耳扣 | 耳夾", 商品名稱), category := "耳環")
    final_data %>%
      `[`(grepl("項鍊 | 鎖骨鍊 | 頸鍊 | 頸圈", 商品名稱), category := "項鍊")
    final_data %>%
      `[`(grepl("手鍊 | 手環 | 手鐲", 商品名稱), category := "手鍊")
    final_data %>%
      `[`(grepl("髮飾 | 髮帶 | 髮圈 | 髮夾 | 髮箍", 商品名稱),
          category := "髮飾")
    final_data[grepl("手錶", 商品名稱), category := "手錶"]
    final_data[grepl("刺青貼紙", 商品名稱), category := "刺青貼紙"]
    final_data[grepl("墨鏡", 商品名稱), category := "墨鏡"]
    final_data[grepl("腳鍊", 商品名稱), category := "腳鍊"]
    final_data[is.na(category), category := "其它"]
    final_data[, category := as.factor(category)]
    
    # Assign manufacturer region
    final_data %>%
      `[`(j = 商店貨號 := sapply(商店貨號,function(k) 
      {strsplit(k, "-")} %>%
        .[[1]] %>%
        .[1]) %>%
        as.character)
    final_data %>%
      `[`(substr(商店貨號,1,1) == "K" | 商店貨號 == "IHNS",
          region := "Korea")
    
    # Last Manipulating on calculating main feature
    final_data[, totalcost := sales * 成本價]
    colnames(final_data)[grep("庫存", colnames(final_data))] <- "stock"
    final_data[, stockcost := stock * 成本價]
    data.table(final_data)
    })
  
  # Creating reactive object for stock checking
    stock_plot_file <- reactive({
  # Check the presence of input file
    if(is.null(input$stock_file_path)) {
      return(NULL)
      }
  
  # Input file
    processed_stock_file <- fread(input$stock_file_path$datapath)
    
  # Manipulating stocking file
    processed_stock_file <- processed_stock_file[!is.na(價格)]
    processed_stock_file <- processed_stock_file[選項 == ""]
    processed_stock_file[is.na(成本價), 成本價 := 價格 * 0.33]
    processed_stock_file <- processed_stock_file[選項 == ""]
    processed_stock_file %>%
      `[`(j = 商品名稱 := 
            str_replace_all(商品名稱, "(\\[.*?\\])|\\【.*?\\】","") %>%
            trimws)
    
  # Classification
    processed_stock_file %>%
      `[`(grepl("戒指 | 對戒 | 戒組 | 尾戒 | 關節戒 | 連指戒 | 情侶戒 |
                三件戒 | 開口戒", 商品名稱), category := "戒指")
    processed_stock_file %>%
      `[`(grepl("耳環 | 耳針 | 耳扣 | 耳夾", 商品名稱), category := "耳環")
    processed_stock_file %>%
      `[`(grepl("項鍊 | 鎖骨鍊 | 頸鍊 | 頸圈", 商品名稱), category := "項鍊")
    processed_stock_file %>%
      `[`(grepl("手鍊 | 手環 | 手鐲", 商品名稱), category := "手鍊")
    processed_stock_file %>%
      `[`(grepl("髮飾 | 髮帶 | 髮圈 | 髮夾 | 髮箍", 商品名稱),
          category := "髮飾")
    processed_stock_file[grepl("手錶", 商品名稱), category := "手錶"]
    processed_stock_file[grepl("刺青貼紙", 商品名稱), category := "刺青貼紙"]
    processed_stock_file[grepl("墨鏡", 商品名稱), category := "墨鏡"]
    processed_stock_file[grepl("腳鍊", 商品名稱), category := "腳鍊"]
    processed_stock_file[is.na(category), category := "其它"]
    processed_stock_file[, category := as.factor(category)]
    
  # Assigning Region
    processed_stock_file %>%
      `[`(j = 商店貨號 := sapply(商店貨號,function(k) 
      {strsplit(k, "-")} %>%
        .[[1]] %>%
        .[1]) %>%
        as.character)
    processed_stock_file %>%
      `[`(substr(商店貨號, 1, 1) == "K" | 商店貨號 == "IHNS", region := "Korea")
  
  # Calculating main feature
    stock_in_name <- grep("庫存", colnames(processed_stock_file))
    colnames(processed_stock_file)[stock_in_name] <- "stock"
    processed_stock_file[, stockcost := stock * 成本價]
    processed_stock_file <- processed_stock_file[!stock == 0]
    })
  
     # Ploting first panel
    output$stock_by_category <- renderPlotly({
      if (is.null(stock_plot_file())) {
        return(NULL)
        }
    stock_by_category_canvas  <- stock_plot_file() %>%
      group_by(category, region) %>%
      summarise(stockcost = sum(stockcost)) %>%
      ggplot(aes(category, stockcost, fill = region)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = stockcost), position = position_dodge(width = 1)) +
      theme_bw()
      ggplotly(stock_by_category_canvas)
    })
    
     # Ploting second panel
    output$stock_by_region <- renderPlotly({
      if (is.null(stock_plot_file())) {
        return(NULL)
        }
    stock_by_region_canvas <- stock_plot_file() %>%
      group_by(category, region) %>%
      summarise(num = n()) %>%
      ggplot(aes(category, num, fill = region)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = num), position = position_dodge(width = 1)) +
      theme_bw()
      ggplotly(stock_by_region_canvas)
    })
    
    # Ploting third panel
    output$cost_by_region_category <- renderPlotly({
      if (is.null(sales_plot_filefile())) {
        return(NULL)
        }
    cost_plot_canvas <- sales_plot_filefile() %>%
      group_by(category,region) %>% 
      summarise(totalcost = sum(totalcost)) %>%
      ggplot(aes(category, totalcost, fill = region)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = totalcost), position = position_dodge(width = 1)) + 
      theme_bw()
      ggplotly(cost_plot_canvas)
    })
    
    # Ploting fourth panel
    output$sales_by_region_category <- renderPlotly({
      if (is.null(sales_plot_filefile())) {
        return(NULL)
        }
      sales_plot_canvas <- sales_plot_filefile() %>%
        group_by(category, region) %>%
        summarise(num = n()) %>%
        ggplot(aes(category, num, fill = region)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = num), position = position_dodge(width = 1)) +
        theme_bw()
        ggplotly(sales_plot_canvas)
    })
})