library(shinydashboard)
library(plotly)
library(shiny)

# Writing dashboard header
  header <- dashboardHeader(title = "Bonny&Read Stocking Report",titleWidth = 300)

# Writing dashboard sidebar
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Stocking Report",tabName = "Stocking",icon = icon("bar-chart-o")
                ,badgeLabel = "new",badgeColor = "red")
    ),
  # upload the stocking file
    fileInput("file1","上傳庫存檔案"),
  
  # upload the sales file
    fileInput("file2","上傳銷售檔案")
  )
  
# Writing dashboard body
  body <- dashboardBody(
    fluidRow(
      box(title = "類別-成本柱狀圖",plotlyOutput("bycategory")),
      box(title = "地區-成本柱狀圖",plotlyOutput("byregion"))
    ),
    fluidRow(
      box(title = "銷售成本柱狀圖",plotlyOutput("byboth")),
      box(title = "產品個數柱狀圖",plotlyOutput("byN"))
  )
    )
  
# Creating dashboardpage
dashboardPage(header,sidebar,body,skin = "yellow")