ui <- navbarPage(
    title = "Ísland og COVID19", 
    theme = shinytheme(theme = "flatly"),
    ##### Smitafjöldi #####
    tabPanel(
        title = "Þróun",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "continent",
                    label = "Heimsálfa",
                    choices = unique(d$continent),
                    multiple = TRUE, 
                    selectize = TRUE,
                    selected = "Europe"
                ),
                uiOutput("countries"),
                div(
                    actionButton(inputId = "selectall", label = "Velja/Afvelja öll lönd"), 
                    class = "center", align = "middle"
                ),
                uiOutput("countries_to_choose"),
                selectInput(
                    inputId = "x_var",
                    label = "Sýna þróun eftir",
                    choices = c("Dagsetningu" = "dags", "Dögum síðan skilyrði að neðan var náð" = "skyl"),
                    multiple = FALSE,
                    selected = "Dagsetningu"
                ),
                selectInput(
                    inputId = "y_var",
                    label = "Breyta",
                    choices = c("Smit" = "total_cases", "Dauðsföll" = "total_deaths"),
                    multiple = FALSE,
                    selected = "Smit"
                ),
                fluidRow(
                    column(
                        6,
                        selectInput(
                            inputId = "filtervar",
                            label = "Sýna gögn þar sem",
                            choices = c("Fjöldi tilvika", "Tíðni tilvika per milljón"),
                            multiple = FALSE,
                            selected = "Fjöldi tilvika"
                        )
                    ),
                    column(
                        6,
                        numericInput(
                            inputId = "filtervalue", label = "Er hærri en", 
                            min = 0, max = 100, value = 0
                        )
                    )
                ),
                selectInput(
                    inputId = "scale",
                    label = "Kvarði",
                    choices = c("Upprunalegur", "Logra"),
                    multiple = FALSE, 
                    selected = "Upprunalegur"
                ),
                div(
                    actionButton(inputId = "gobutton1", label = "Birta", width = "120px"), 
                    class = "center", align = "middle"
                ),
                HTML(sidebar_info)
            ), 
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Fjöldi", 
                             plotlyOutput("euro_plot_n", height = "600px"),
                             plotlyOutput("euro_plot_n_daily", height = "600px")
                             ),
                    tabPanel("Tíðni", plotlyOutput("euro_plot_p", height = "600px"))
                )
            )
        )
    ),
    ##### Aukning #####
    tabPanel(
        title = "Aukning",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "continent_samanburdur",
                    label = "Heimsálfa",
                    choices = unique(d$continent),
                    multiple = TRUE, 
                    selectize = TRUE,
                    selected = "Europe"
                ),
                uiOutput("countries_to_choose_samanburdur"),
                conditionalPanel(
                    condition="input.aukningtab==1",
                    selectInput(
                        inputId = "tegund_samanburdur",
                        label = "Hvernig er tími valinn í reikninga?",
                        choices = c("Dagsetningu" = "dags", "Dögum síðan skilyrði að neðan var náð" = "skyl"),
                        multiple = FALSE, 
                        selectize = FALSE,
                        selected = "Dagsetning"
                    ),
                    uiOutput("param_selection_samanburdur")
                ),
                div(
                    actionButton(inputId = "gobutton_samanburdur", label = "Birta", width = "120px"),
                    class = "center", align = "middle"
                ),
                HTML(sidebar_info)
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Höfðatala",
                        value = 1,
                        plotlyOutput("lmer_plot", height = "800px")
                    ),
                    tabPanel(
                        "Vikuleg smit",
                        value = 2,
                        plotlyOutput("viku_plot", height = "600px")
                    ),
                    id = "aukningtab"
                ),
                
            )
        )
    ),
    ##### Töfluyfirlit #####
    tabPanel(
        title = "Töfluyfirlit",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "continent_table",
                    label = "Heimsálfa",
                    choices = unique(d$continent),
                    multiple = TRUE, 
                    selectize = TRUE,
                    selected = "Europe"
                ),
                uiOutput("countries_table"),
                div(
                    actionButton(inputId = "selectall_table", label = "Velja/Afvelja öll lönd"), 
                    class = "center", align = "middle"
                ),
                uiOutput("countries_to_table"),
                selectInput(
                    inputId = "sort_col", label = "Raða eftir",
                    choices = c("Landi", "Tilfellum", "Smitatíðni", "Dauðsföll", "Dánartíðni", "Fyrsta smiti"), 
                    selected = "Landi"
                ),
                div(
                    actionButton(inputId = "gobutton2", label = "Birta", width = "120px"), 
                    class = "center", align = "middle"
                ),
                HTML(sidebar_info)
            ),
            mainPanel(
                tableOutput("summary_table"),
                conditionalPanel(
                    condition = "input.gobutton2>0",
                    downloadButton(outputId = "table_download", label = "Sækja gögn")
                )
            )
        )
    ),
    ##### Forspá #####
    tabPanel(
        title = "Spá",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "tegund_forspa",
                    label = "Sjá spá fyrir",
                    choices = c("Uppsafnaðan fjölda" = "cumulative", "Virkan fjölda" = "active")
                ),
                selectInput(
                    inputId = "breyta_forspa",
                    label = "Sjá spá fyrir fjölda",
                    choices = c("Greindra smita" = "cases", "Á spítala" = "hospital", "Á gjörgæslu" = "icu")
                ),
                selectInput(
                    inputId = "byage_forspa",
                    label = "Birta eftir aldurs hópum?",
                    choices = c("Aldursskipting", "Heild"), 
                    selected = "Heild"
                ),
                fluidRow(
                    column(
                        6,
                        dateInput(
                            "date_from_forspa", 
                            label = "Frá",
                            value = Sys.Date() - 14, 
                            min = min(d_spa$date), 
                            max = Sys.Date() + 21
                        )
                    ),
                    column(
                        6,
                        dateInput(
                            "date_to_forspa", 
                            label = "Til",
                            value = Sys.Date() + 21, 
                            min = "2020-03-02", 
                            max = Sys.Date() + 21
                        )
                    )
                ),
                div(
                    actionButton(inputId = "gobutton_forspa", label = "Birta", width = "120px"), 
                    HTML("<br><br>"),
                    class = "center", align = "middle"
                ),
                HTML("<br>"),
                HTML(sidebar_info)
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Myndrit",
                        plotOutput(outputId = "myndrit", height = "600px")
                    )
                    ,
                    tabPanel(
                        "Tafla",
                        HTML("<br>"),
                        downloadButton("downloadData_forspa", label = "Sækja töflu"),
                        dataTableOutput(outputId = "tafla")
                    )
                )
            )
        )
    ),
    ##### Fróðleikur #####
    tabPanel(
        title = "Fróðleikur", 
        sidebarLayout(
            sidebarPanel(
                h6("Höfundur:"),
                h6("Brynjófur Gauti Jónsson,"),
                h6("Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands"),
                div(img(src = "hi_hvs_horiz.png", width = "80%"), align = "middle", class = "center"),
                h6("Byggt á daglega uppfærðum gögnum ECDC sem fást í hlekk að neðan"),
                a("Hlekkur á gögn", href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"),
                h6(sidast_uppfaert),
                a("Allan kóða má nálgast hér", href = "https://github.com/bgautijonsson/covid19")  
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs", 
                    tabPanel(
                        "Lögmál smárra talna", 
                        includeHTML("www/LawOfSmallNumbers.html")
                    )
                )
            )
        )
    ) 
)
