dashboardPage(
  skin = "blue",
  title = "SMARTIBRE - NMECSYS",
  header = dashboardHeader(title = div("SMARTIBRE", style = "font-family: sans-serif; font-weight:bold"),
                           dropdownMenuOutput("notif_user")
                           ),
                           
  sidebar = dashboardSidebar(),
  body = dashboardBody()
)
