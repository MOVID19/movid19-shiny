bs4DashPage(
    enable_preloader = FALSE,
    loading_duration = 1.5,
    loading_background = "white",
    sidebar_collapsed = TRUE,
    navbar = bs4DashNavbar(),
# sidebar -----------------------------------------------------------------
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
                tabName = "social",
                icon = "users"
            ),
            bs4SidebarMenuItem(
                text = "Percepción de riesgo",
                tabName = "percepcion",
                icon = "exclamation-triangle"
            ),
            bs4SidebarMenuItem(
                text = "Participantes",
                tabName = "participantes",
                icon = "user-friends"
            ),
            bs4SidebarMenuItem(
                text = "Mapas",
                tabName = "mapas",
                icon = "map-marked-alt"
            ),
            bs4SidebarMenuItem(
                text = "Acerca de",
                tabName = "acerca",
                icon = "question-circle"
            )
        )
    ),
# body --------------------------------------------------------------------
    body = bs4DashBody(
        
        useSweetAlert(theme = "minimal"),
        use_cicerone(),
        tags$head(tags$link(rel="shortcut icon", href="fa.png")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/movid19.css")),
        tags$script(src = "js/movid19.js"),
        
        bs4TabItems(
# inicio ------------------------------------------------------------------
            bs4TabItem(
                tabName = "inicio",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-virus"), " Inicio"),
                        tags$hr()
                    ),
                    
                    valueBoxOutput("vb_casos", width = 3),
                    valueBoxOutput("vb_fallc", width = 3),
                    valueBoxOutput("vb_resps", width = 3),
                    valueBoxOutput("vb_partc", width = 3),
                    
                    bs4Card(
                        width = 12,
                        title = "Respuestas",
                        highchartOutput("inc_respuestas", height = "100%")
                        ),
                    # bs4Card(
                    #     width = 4,
                    #     title = "Género",
                    #     highchartOutput("inc_genero")
                    #     )
                    )
                ),
# sintomas ----------------------------------------------------------------
            bs4TabItem(
                tabName = "sintomas",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-stethoscope"), " Síntomas"),
                        tags$hr()
                    ),
                    column(
                        12,
                        column(
                            6,
                            selectizeInput("ssd_opt", "Seleccione variable para desagregar", 
                                           choices = OPTS_DESAGREGAR, width = "100%")
                            
                        )
                    ),
                    bs4Card(
                        title = "Síntomas durante la última semana",
                        width = 12,
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
                        title = "Casos sospechosos",
                        selectizeInput(
                            "snt_sos", NULL, 
                            choices = OPTS_SOSPECHOSO_DEFINICION, 
                            width = "100%"
                        ),
                        highchartOutput("snt_hc_sospc", height = 345)
                    ),
                    bs4Card(
                        title = "Contacto estrecho",
                        highchartOutput("snt_hc_contc")
                    ),
                    # bs4Card(
                    #     title = "Confirmados y probables",
                    #     highchartOutput("snt_hc_confm")
                    # ),
                )
            ),
# sistema salud -----------------------------------------------------------
            bs4TabItem(
                tabName = "sistema",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-hospital-o"), " Sistema de Salud"),
                        tags$hr()
                        ),
                    column(
                        12,
                        column(
                            6,
                            selectizeInput("ssd_opt2", "Seleccione variable para desagregar", 
                                           choices = OPTS_DESAGREGAR, width = "100%")
                            
                            )
                        ),
                    bs4Card(
                        title = "Consulta médica",
                        highchartOutput("ssd_hc_cslta") 
                    ),
                    bs4Card(
                        title = "Exámenes",
                        highchartOutput("ssd_hc_examn") 
                    ),
                    bs4Card(
                        title = "Confirmación a través de examen COVID-19",
                        highchartOutput("ssd_hc_posit") 
                    ),
                    bs4Card(
                        title = "Días de espera consulta",
                        highchartOutput("ssd_hc_ctads") 
                    ),
                    bs4Card(
                        title = "Días de espera entre toma examen y resultados",
                        highchartOutput("ssd_hc_exesp") 
                    ),
                    bs4Card(
                        title = "Días de espera entre sínotmas y resultados",
                        highchartOutput("ssd_hc_ctaex") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Razones para no consultar a profesional o no realizarse exámen",
                        fluidRow(
                            column(
                                6,
                                selectizeInput(
                                    "razones_opt", NULL,
                                    choices = OPTS_RAZONES,
                                    multiple = FALSE,
                                    width = "100%"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                6,
                                highchartOutput("ssd_hc_s3con", height = 345)
                            ),
                            column(
                                6,
                                highchartOutput("ssd_hc_s8exm", height = 345)
                            )
                        )
                       
                    ),
                )
            ),

# prácticas sociales ------------------------------------------------------
            bs4TabItem(
                tabName = "social",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-users"), " Prácticas Sociales"),
                        tags$hr()
                    ),
                    bs4Card(
                        width = 12,
                        title = "Promedio frecuencia de salida",
                        highchartOutput("pcsoc_frec_salida") 
                    ),
                    bs4Card(
                        title = "Proporción de personas que salen al menos 2 veces por semana",
                        highchartOutput("pcsoc_prop2") 
                    ),
                    bs4Card(
                        title = "Proporción de personas que no salen",
                        highchartOutput("pcsoc_nosalen") 
                    ),
                )
            ),
# percepción riesgo -------------------------------------------------------
            bs4TabItem(
                tabName = "percepcion",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-exclamation-triangle"), " Percepción de riesgo"),
                        tags$hr()
                    ),
                    bs4Card(
                        width = 12,
                        title = "Proporción de personas que consideran el COVID19 un problema de alto riesgo",
                        highchartOutput("persgo_alto") 
                    )
                )
            ),

# participantes -----------------------------------------------------------
            bs4TabItem(
                tabName = "participantes",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-user-friends"), " Participantes"),
                        tags$hr()
                    ),
                    bs4Card(
                        width = 6,
                        title = "Sexo",
                        highchartOutput("part_sexo") 
                    ),
                    bs4Card(
                        width = 6,
                        title = "Edad",
                        highchartOutput("part_edad") 
                    ),
                    bs4Card(
                        width = 6,
                        title = "Previsión",
                        highchartOutput("part_prev") 
                    ),
                    bs4Card(
                        width = 6,
                        title = "Educación",
                        highchartOutput("part_educ") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Ocupación",
                        highchartOutput("part_ocup") 
                    ),
                )
            ),
# mapas -------------------------------------------------------------------
            bs4TabItem(
                tabName = "mapas",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-map-marked-alt"), " Mapas"),
                        tags$hr()
                    ),
                    column(
                        12,
                        tags$iframe(
                            src = "https://visualizaciones-movid.netlify.app",
                            frameborder="0",
                            style="overflow:hidden;overflow-x:hidden;overflow-y:hidden;width:100%;top:0px;left:0px;right:0px;bottom:0px",
                            height="700px", 
                            width="100%"
                            )
                    )
                )
            )
# end bs4DashPage ---------------------------------------------------------
        )
    )

)
