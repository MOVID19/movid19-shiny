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
            id = "current_tab",
            bs4SidebarMenuItem(
                text = "MOVID-19",
                tabName = "inicio",
                # icon =  "tachometer-alt",
                icon = "virus"
            ),
            bs4SidebarMenuItem(
                text = "Síntomas COVID-19",
                tabName = "sintomas",
                icon = "stethoscope"
            ),
            bs4SidebarMenuItem(
                text = "Acceso sistema de salud",
                icon = "hospital",
                startExpanded = FALSE,
                bs4SidebarMenuSubItem(
                    text = "COVID-19",
                    tabName = "sistema_covid",
                    icon = "ambulance"
                ),
                bs4SidebarMenuSubItem(
                    text = "No COVID-19",
                    tabName = "sistema_nocovid",
                    icon = "pills"
                )
            ),
            bs4SidebarMenuItem(
                text = "Prácticas sociales",
                tabName = "social",
                icon = "users"
            ),
            bs4SidebarMenuItem(
                text = "Percepciones",
                tabName = "percepcion",
                icon = "exclamation-triangle"
            ),
            bs4SidebarMenuItem(
                text = "Mapas",
                tabName = "mapas",
                icon = "map-marked-alt"
            ),
            bs4SidebarMenuItem(
                text = "Participantes",
                tabName = "participantes",
                icon = "user-friends"
            ),
            bs4SidebarMenuItem(
                text = "Ayuda",
                tabName = "ayuda",
                icon = "question-circle",
                expandedName = "ayuda"
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
                        tags$h2(tags$i(class = "fa fa-virus"), " MOVID-19"),
                        tags$hr()
                    ),
                    column(
                        12,
                        HTML("Con este visualizador podrás conocer los principales
                            resultados del Monitoreo Nacional de Síntomas y Prácticas 
                            COVID-19 en Chile (MOVID-19). Te invitamos a explorar 
                            cada sección y conocer la evolución de los distintos 
                            aspectos de la pandemia en nuestro país con datos 
                            actualizados de nuestra encuesta. Recuerda seguir 
                            contestando o inscribirte  <a href='https://encuestacovid.uchile.cl/' target='_blank'>aquí</a>."
                        ),
                        tags$hr()
                    ),
                    valueBoxOutput("vb_casos", width = 3),
                    valueBoxOutput("vb_fallc", width = 3),
                    valueBoxOutput("vb_resps", width = 3),
                    valueBoxOutput("vb_partc", width = 3),
                    bs4Card(
                        width = 6,
                        title = "Proporción de personas que salen al menos 2 veces por semana",
                        highchartOutput("inicial")
                    ),
                    bs4Card(
                        width = 6,
                        title = "Acceso a consulta médica",
                        highchartOutput("inicial_2")
                    )
                ),
                
            ),
            # sintomas ----------------------------------------------------------------
            bs4TabItem(
                tabName = "sintomas",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-stethoscope"), " Síntomas COVID-19"),
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
                    bs4Card(
                        title = "Casos probables",
                        highchartOutput("snt_hc_confm")
                    ),
                    bs4Card(
                        title = "Síntomas durante la última semana",
                        width = 6,
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
                )
            ),
            # sistema salud -----------------------------------------------------------
            bs4TabItem(
                tabName = "sistema_covid",
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
                        width = 12,
                        title = "Acceso a consulta médica",
                        fluidRow(
                            column(6,highchartOutput("ssd_hc_cslta") ),
                            column(6, highchartOutput("ssd_hc_cslta_b"))
                        )
                    ),
                    bs4Card(
                        width = 12,
                        title = "Acceso a examen diagnóstico",
                        fluidRow(
                            column(6, highchartOutput("ssd_hc_examn")),
                            column(6, highchartOutput("ssd_hc_examn_b"))    
                        )
                    ),
                    bs4Card(
                        title = "Tasa de test diagnóstico",
                        highchartOutput("ssd_hc_posit") 
                    ),
                    bs4Card(
                        title = "Positividad de los test diagnósticos",
                        highchartOutput("ssd_hc_posit2") 
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
                        width = 12,
                        title = "Razones entregadas entre las personas que declaran síntomas y decidieron no consultar",
                        selectizeInput(
                            "razones_opt",
                            NULL,
                            choices = OPTS_RAZONES,
                            multiple = FALSE,
                            width = "100%"
                        ),
                        highchartOutput("ssd_hc_s3con", height = 345)
                    ),
                    bs4Card(
                        width = 12,
                        title = "Razones entregadas entre las personas que teniendo indicado realizarse un test diagnóstico no se lo realizaron",
                        selectizeInput(
                            "razones_opt2",
                            NULL,
                            choices = OPTS_RAZONES2,
                            selected = c("espera", "nodisp", "nograve", "nosabia", "nimporta"),
                            multiple = TRUE,
                            width = "100%"
                        ),
                        highchartOutput("ssd_hc_s8exm")
                    ),
                )
            ),
            
            # no covid19 --------------------------------------------------------------
            bs4TabItem(
                tabName = "sistema_nocovid",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-pills"), " Nuevos problemas de salud en pandemia"),
                        tags$hr()
                    ),
                    column(
                        12,
                        column(
                            6,
                            selectizeInput("ssd_opt3", "Seleccione variable para desagregar", 
                                           choices = OPTS_DESAGREGAR, width = "100%")
                            
                        ),
                    ),
                    bs4Card(
                        title = "Nuevo problema de salud",
                        highchartOutput("ssd_hc_cslta_nocovid") 
                    ),
                    bs4Card(
                        title = "Acceso a consulta médica",
                        highchartOutput("ssd_hc_cslta_b_nocovid") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Tipo de prestación que fue pospuesta",
                        selectizeInput(
                            "razones_opt4",
                            NULL,
                            choices = OPTS_RAZONES4,
                            selected = c("consulta", "examen", "insumos", "vacuna"),
                            multiple = TRUE,
                            width = "100%"
                        ),
                        highchartOutput("ssd_hc_nc3_pq_nocovid") 
                    )
                ),
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-lungs-virus"), " Personas con enfermedades crónicas"),
                        tags$hr()
                    ),
                    bs4Card(
                        width = 6,
                        title = "Acceso a consulta médica",
                        highchartOutput("ssd_hc_cslta_cronico") 
                    ),
                    bs4Card(
                        width = 6,
                        title = "Posponer consulta médica",
                        highchartOutput("ssd_hc_cslta_cronico_posponer") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Razones de no acceso a consulta médica",
                        selectizeInput(
                            "razones_opt3",
                            NULL,
                            choices = OPTS_RAZONES3,
                            selected = c("temor", "cancela", "costo", "sistlleno"),
                            multiple = TRUE,
                            width = "100%"
                        ),
                        highchartOutput("ssd_hc_crn3_pq_cronico") 
                    )
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
                        title = "Frecuencia promedio de salidas según tipo de práctica",
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
                    bs4Card(
                        width = 12,
                        title = "Proporción de personas que se transportan según medio de movilización",
                        highchartOutput("pcsoc_transporte") 
                    ),
                )
            ),
            # percepciones ------------------------------------------------------------
            bs4TabItem(
                tabName = "percepcion",
                fluidRow(
                    column(
                        12,
                        tags$h2(tags$i(class = "fa fa-exclamation-triangle"), " Percepciones"),
                        tags$hr()
                    ),
                    bs4Card(
                        width = 12,
                        title = "El COVID-19 es un problema de alto riesgo y/o amenaza",
                        highchartOutput("persgo_alto") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Yo y mis cercanos cumplimos con las recomendaciones de cuidado frente a COVID19",
                        highchartOutput("pergdo_cump")
                    ),
                    bs4Card(
                        width = 12,
                        title = "Las autoridades sanitarias han puesto el bienestar de la población por sobre sus propios intereses",
                        highchartOutput("peleg_bienestar")
                    ),
                    bs4Card(
                        width = 12,
                        title = "Aunque a veces no estemos de acuerdo con las autoridades sanitarias y sus medidas, es nuestro deber seguir sus indicaciones al pie de la letra",
                        highchartOutput("peleg_obedecer")    
                    ),
                    bs4Card(
                        width = 12,
                        title = "En Chile, hay enormes desigualdades frente a la pandemia: algunas personas están mucho más protegidas y tienen mejor acceso a salud que otras.",
                        highchartOutput("peleg_desigualdad")    
                    ),
                    bs4Card(
                        width = 12,
                        title = "Mientras la pandemia no esté completamente controlada, no debiera permitirse ningún tipo de protesta o movilización social.",
                        highchartOutput("peleg_represion")    
                        
                    )
                    
                    
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
                        width = 6,
                        title = "Ocupación",
                        highchartOutput("part_ocup") 
                    ),
                    bs4Card(
                        width = 6,
                        title = "Región",
                        highchartOutput("part_region") 
                    ),
                    bs4Card(
                        width = 12,
                        title = "Respuestas",
                        highchartOutput("part_respuestas", height = "100%")
                    ),
                )
            )
            # end bs4DashPage ---------------------------------------------------------
        )
    )
    
)