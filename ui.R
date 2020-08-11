bs4DashPage(
    enable_preloader = TRUE,
    loading_duration = 1.5,
    loading_background = "#00A1D5",
    sidebar_collapsed = TRUE,
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
                # icon =  "tachometer-alt",
                icon = "virus"
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
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-virus"), " Inicio")
                    ),
                    bs4Card(
                        width = 8,
                        title = "Respuestas",
                        highchartOutput("inc_respuestas")
                        ),
                    bs4Card(
                        width = 4,
                        title = "Género",
                        highchartOutput("inc_genero")
                        )
                    )
                ),
            bs4TabItem(
                tabName = "sintomas",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-stethoscope"), " Síntomas")
                    ),
                    bs4Card(
                        title = "Síntomas",
                        selectizeInput(
                            "snt_opt", NULL, 
                            choices = OPTS_SINTOMAS,
                            selected = "snt_fiebre",
                            multiple = TRUE, 
                            width = "100%",
                            options = list(maxItems = 5)
                            ),
                        # 345 de altura debido al selectInput anterior
                        highchartOutput("snt_hc_tlsnt", height = 345)
                    ),
                    bs4Card(
                        title = "Sospechoso",
                        highchartOutput("snt_hc_sospc")
                    ),
                    bs4Card(
                        title = "Contacto estrecho",
                        highchartOutput("snt_hc_contc")
                    ),
                    bs4Card(
                        title = "Confirmados y probables",
                        highchartOutput("snt_hc_confm")
                    ),
                )
            ),
            bs4TabItem(
                tabName = "sistema",
                fluidRow(
                    column(12,
                        tags$h2(tags$i(class = "fa fa-hospital-o"), " Sistema de Salud"),   
                        selectizeInput("ssd_opt", "Seleccione variable para desagregar", 
                                       choices = OPTS_DESAGREGAR, width = "100%")
                    ),
                    bs4Card(
                        title = "ssd_hc_medex",
                        highchartOutput("ssd_hc_cslta") 
                    ),
                    bs4Card(
                        title = "ssd_hc_examn",
                        highchartOutput("ssd_hc_examn") 
                    )
                )
            )
        )
    )
)
