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
                text = "Acceso sistema de salud",
                tabName = "sistema",
                icon = "hospital-o"
            ),
            bs4SidebarMenuItem(
                text = "Prácticas sociales",
                tabName = "Social",
                icon = "users"
            ),
            bs4SidebarMenuItem(
                text = "Percepción de riesgo",
                tabName = "percepcion",
                icon = "exclamation-triangle"
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
                    bs4Card(
                        title = "Respuestas",
                        collapsible = FALSE,
                        width = 8,
                        highchartOutput("inc_respuestas")
                        ),
                    bs4Card(
                        title = "Género",
                        collapsible = FALSE,
                        width = 4,
                        highchartOutput("inc_genero")
                        )
                    )
                ),
            bs4TabItem(
                tabName = "sintomas",
                fluidRow(
                    bs4Card(
                        title = "Síntomas",
                        collapsible = FALSE,
                        width = 6,
                        selectizeInput(
                            "snt_opt", NULL, 
                            choices = OPTS_SINTOMAS,
                            selected = "snt_fiebre",
                            multiple = TRUE, 
                            width = "100%",
                            options = list(maxItems = 5)
                            ),
                        highchartOutput("snt_hc_tlsnt")
                    ),
                    bs4Card(
                        title = "Sospechoso",
                        collapsible = FALSE,
                        width = 6,
                        highchartOutput("snt_hc_sospc")
                    ),
                    bs4Card(
                        title = "Contacto estrecho",
                        collapsible = FALSE,
                        width = 6,
                        highchartOutput("snt_hc_contc")
                    ),
                    bs4Card(
                        title = "Confirmados y probables",
                        collapsible = FALSE,
                        width = 6,
                        highchartOutput("snt_hc_confm")
                    ),
                )
            )
        )
    )
)
