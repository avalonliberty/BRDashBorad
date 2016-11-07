# Writing dashboard header
  header <- dashboardHeader(title = "Bonny&Read Stock Report")

# Writing dashboard sidebar
  sidebar <- dashboardSidebar()
# Writing dashboard body
  body <- dashboardBody()
  
# Run the dashboard
  dashboardPage(header,sidebar,body)