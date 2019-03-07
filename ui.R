library(shinydashboard)
library(plotly)
library(shiny)

# Writing dashboard header
  header <- dashboardHeader(title = "Bonny&Read Stocking Report",
                            titleWidth = 300)

# Writing dashboard sidebar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Stocking Report", tabName = "Stocking",
               icon = icon("bar-chart-o"), badgeLabel = "new",
               badgeColor = "red")
    ),
  # upload the stocking file
    fileInput("stock_file_path", "上傳庫存檔案"),
  
  # upload the sales file
    fileInput("sales_file_path", "上傳銷售檔案")
  )
  
# Writing dashboard body
  body <- dashboardBody(
    fluidRow(
      box(title = "類別-庫存成本柱狀圖",
          plotlyOutput("stock_by_category"),
          status = "primary"),
      box(title = "庫存產品柱狀圖",
          plotlyOutput("stock_by_region"))
      ),
    
    fluidRow(
      box(title = "銷售成本柱狀圖", plotlyOutput("cost_by_region_category"),
      footer = "以地區與類別分類的銷售成本圖"),
      box(title = "銷售產品個數柱狀圖", plotlyOutput("sales_by_region_category"))
      )
    )
  
# Creating dashboardpage
dashboardPage(header, sidebar, body, skin = "yellow")