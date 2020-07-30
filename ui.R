bs4DashPage(
    enable_preloader = TRUE,
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
        title = tags$small("MOVID19"),
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
                text = "Sistema de Salud",
                tabName = "sistema",
                icon = "hospital-o"
            ),
            bs4SidebarMenuItem(
                text = "Informe III",
                tabName = "economia",
                icon = "area-chart"
            )
        )
    ),
    body = bs4DashBody(
        tags$head(tags$link(rel="shortcut icon", href="fa.png")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/movid19.css")),
        tags$script(src = "js/custom19.js"),
        bs4TabItems(
            bs4TabItem(
                tabName = "inicio",
                bs4CardHC(highchartOutput("chart"))
                )
            )
        )
    )
