bs4DashPage(
    enable_preloader = FALSE,
    sidebar_collapsed = FALSE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
        title = NULL,
        expand_on_hover = TRUE,
        fixed = FALSE,
        skin = "light",
        bs4SidebarMenu(
            bs4SidebarMenuItem(
                text = "Inicio",
                tabName = "inicio",
                icon =  "tachometer-alt"
            ),
            bs4SidebarMenuItem(
                text = "Síntomas",
                tabName = "sintomas",
                icon = "stethoscope"
            ),
            bs4SidebarMenuItem(
                text = "Social",
                tabName = "Social",
                icon = "users"
            ),            
            bs4SidebarMenuItem(
                text = "Sistema de Salud",
                tabName = "sistema",
                icon = "hospital-o"
            ),
            bs4SidebarMenuItem(
                text = "Acerca de",
                tabName = "acerca",
                icon = "question-circle"
            )
        )
    ),
    body = bs4DashBody(
        tags$head(tags$link(rel="shortcut icon", href="fa.png")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/movid19.css")),
        tags$script(src = "js/movid19.js"),
        bs4TabItems(
            bs4TabItem(
                tabName = "inicio",
                fluidRow(
                    bs4CardHC(highchartOutput("chart2")),
                    bs4CardHC(highchartOutput("chart"))
                    )
                ),
            bs4TabItem(
                tabName = "acerca",
                verbatimTextOutput("input")
                )
            ),
        
        )
    )
