server <- function(input, output, session) {
    # Þróun ----
    observe({
        updateCheckboxGroupInput(
            session = session,
            inputId = "countries",
            selected = if (input$selectall %% 2L == 1L) {
                get_count_per_cont(input$continent)
            } else {
                default_countries
            }
        )
    })
    output$countries <- renderUI({
        req(input$continent)
        selectInput(
            inputId = "countries",
            label = "Lönd",
            choices = get_count_per_cont(input$continent),
            multiple = TRUE, 
            selectize = TRUE,
            selected = if ("Evrópa" %chin% input$continent) default_countries
        )
    })
    output$countries_to_choose <- renderUI({
        req(input$countries)
        selectInput(
            inputId = "chosen",
            label = "Samanburðarland",
            choices = input$countries,
            selectize = TRUE,
            selected = if ("Ísland" %chin% input$countries) {
                "Ísland"
            }  else {
                input$countries[1]
            }
        )
    })
    
    throun_df <- reactive({
        req(input$countries, input$chosen)
        y_var_total      <- paste0("total_", input$y_var)
        y_var_p          <- paste0("total_", input$y_var, "_per100k")
        y_var_n_daily    <- paste0("new_", input$y_var)
        y_var_n_weekly   <- paste0("new_", input$y_var, "_lw")
        y_var_p_biweekly <- paste0("new_", input$y_var, "_l2w_per100k")

        if (input$filtervar == "ft") {
            filtervar <- y_var_total
            filtervalue <- input$filtervalue 
        } else if (input$filtervar == "ftpm") { 
            filtervar <- "total_cases_per100k"
            filtervalue <- input$filtervalue / 10
        } else if (input$filtervar == "dags") {
            filtervar <- "date"
            filtervalue <- input$filtervalue_dags
        }
        d[input$countries
          ][get(filtervar) > filtervalue,
            .(
                chosen = fifelse(country == input$chosen, "comp", "rest"),
                y_var_n = get(y_var_total),
                y_var_n_daily = get(y_var_n_daily),
                y_var_p = get(y_var_p),
                y_var_n_weekly = get(y_var_n_weekly),
                y_var_p_biweekly = get(y_var_p_biweekly),
                death_rate,
                days = rowid(country),
                country,
                date
            )]
    })
    
    # Fjöldi graf
    plot_n <- eventReactive(input$gobutton1, {
        ylab <- switch(
            input$y_var,
            "cases" = "greindra smita",
            "deaths" = "skráðra dauðsfalla", 
            "vaccines" = "fullkláraðra bólusetninga"
        )
        ggplotly(
            general_plot(
                throun_df(),
                yvar = "y_var_n",
                ylab = ylab,
                xdags = input$x_var != "skyl", 
                logscale = input$scale == "Logra"
            ),
            tooltip = "text"
        )
    })
    output$plot_n <- renderPlotly({
        plot_n()
    })
    
    # Fjöldi undanfarna viku
    plot_n_weekly <- eventReactive(input$gobutton1, {
        ylab <- switch(
            input$y_var,
            "cases" = "nýgreindra smita undanfarna viku",
            "deaths" = "skráðra dauðsfalla undanfarna viku", 
            "vaccines" = "fullkláraðra bólusetninga undanfarna viku"
        )
        ggplotly(
            general_plot(
                throun_df(),
                yvar = "y_var_n_weekly",
                ylab = ylab,
                xdags = input$x_var != "skyl", 
                logscale = input$scale == "Logra"
            ),
            tooltip = "text"
        )
    })
    output$plot_n_weekly <- renderPlotly({
        plot_n_weekly()
    })

    # Fjöldi per 100k
    plot_p <- eventReactive(input$gobutton1, {
        ylab <- switch(
            input$y_var,
            "cases" = "smitaðra á hverja 100.000 íbúa",
            "deaths" = "dauðsfalla á hverja 100.000 íbúa", 
            "vaccines" = "fullkláraðra bólusetninga á hverja 100.000 íbúa"
        )
        ggplotly(
            general_plot(
                throun_df(),
                yvar = "y_var_p",
                ylab = ylab,
                xdags = input$x_var != "skyl", 
                logscale = input$scale == "Logra"
            ),
            tooltip = "text"
        )
    })
    output$plot_p <- renderPlotly({
        plot_p()
    })

    # Fjöldi undanfarnar 2 vikur per 100k
    plot_p_biweekly <- eventReactive(input$gobutton1, {
          ylab <- switch(
              input$y_var,
              "cases" = "smitaðra síðustu tvær vikurnar (per 100.000 íbúa)",
              "deaths" = "dauðsfalla síðustu tvær vikurnar (per 100.000 íbúa)", 
              "vaccines" = "fullkláraðra bólusetninga síðustu tvær vikurnar (per 100.000 íbúa)"
          )
          ggplotly(
              general_plot(
                  throun_df(),
                  yvar = "y_var_p_biweekly",
                  ylab = ylab,
                  xdags = input$x_var != "skyl", 
                  logscale = input$scale == "Logra"
              ),
              tooltip = "text"
          )
    })
    output$plot_p_biweekly <- renderPlotly({
        plot_p_biweekly()
    })

    # Dánartíðni
    plot_death_rate <- eventReactive(input$gobutton1, {
          ggplotly(
              general_plot(
                  throun_df(),
                  yvar = "death_rate",
                  ylab = 'Dánartíðni (hlutfall smitaðra sem hafa látist)',
                  xdags = input$x_var != "skyl", 
                  logscale = input$scale == "Logra",
                  perc = TRUE
              ),
              tooltip = "text"
          )
    })
    output$plot_death_rate <- renderPlotly({
        plot_death_rate()
    })




    ##### Aukning LMER #####
    output$countries_to_choose_samanburdur <- renderUI({
        req(input$continent_samanburdur)
        selectInput(
            inputId = "chosen_samanburdur", 
            label = "Samanburðarland", 
            choices = get_count_per_cont(input$continent_samanburdur),
            selectize = TRUE,
            selected = if ("Evrópa" %in% input$continent_samanburdur) "Ísland"
        )
    })
    
    output$param_selection_samanburdur <- renderUI({
        req(input$tegund_samanburdur)
        if (input$tegund_samanburdur == "dags") {
            h4("Veldu tímabil til að bera saman aukningu í tíðni smita")
            fluidRow(
                column(
                    6,
                    dateInput(
                        "date_from_samanburdur", 
                        label = "Frá",
                        value = date_range[2] - 31L, 
                        min = date_range[1], 
                        max = date_range[2] - 2L
                    )
                ),
                column(
                    6,
                    dateInput(
                        "date_to_samanburdur", 
                        label = "Til",
                        value = date_range[2], 
                        min = "2020-03-08", 
                        max = date_range[2]
                    )
                )
            )
        } else {
            fluidRow(
                column(
                    6,
                    selectInput(
                        inputId = "type_filt_samanburdur",
                        label = "Sýna gögn þar sem",
                        choices = c("Fjöldi tilvika", "Tíðni tilvika per milljón"),
                        multiple = FALSE,
                        selected = "Fjöldi tilvika")
                ),
                column(
                    6,
                    numericInput(
                        inputId = "filtervalue_samanburdur",
                        label = "Er hærri en", 
                        min = 0, 
                        max = 1000, 
                        value = 50
                    )
                )
            )
        }
    })
    
    lmer_plot <- eventReactive(input$gobutton_samanburdur, {
        req(input$continent_samanburdur)
        d <- d %>% 
            filter(country %chin% get_count_per_cont(input$continent_samanburdur)) %>%
            mutate(days = as.integer(date - min(date)))
        if (input$tegund_samanburdur == "dags") {
            d <- d %>% 
                filter(date %between% c(input$date_from_samanburdur, input$date_to_samanburdur))
            ekki_byrjud <- d %>% 
                group_by(country) %>% 
                summarise(ekki_med_case = any(total_cases_per100k == 0)) %>% 
                filter(ekki_med_case) %>% 
                pull(country)
            d <- d %>% filter(!country %in% ekki_byrjud)
        } else {
            if (input$type_filt_samanburdur == "Fjöldi tilvika") {
                filter_var <- "total_cases"
                filter_value <- input$filtervalue_samanburdur
            }
            else {
                filter_var <- "total_cases_per100k"
                filter_value <- input$filtervalue_samanburdur / 1000
            }
            d <- d %>% filter(!!sym(filter_var) >= filter_value)
        }
        m <- lmer(
            log(total_cases_per100k) ~ days + (days | country),
            data = d,
            control = lmerControl(optimizer = "bobyqa")
        )
        temp <- as_tibble(coef(m)$country, rownames = "country") %>%
            select(-`(Intercept)`) %>%
            mutate(
                col = fifelse(country == input$chosen_samanburdur, "blue", "grey"),
                change = exp(days) - 1,
                country = factor(reorder(country, change)),
            )
        evo_chosen <- temp %>% filter(col == "blue") %>% pull(round(change, 3))
        mean_evo <- exp(fixef(m)[2]) - 1
        p <- temp %>%
            ggplot(aes(x = country, y = change, text = with(temp, sprintf('%s: %.02f%%', country, change*100)))) +
            geom_point(aes(col = col), show.legend = FALSE) +
            geom_segment(aes(xend = country, yend = 0, col = col), show.legend = FALSE) +
            geom_hline(yintercept = exp(summary(m)$coefficients[2, 1]) - 1, lty = 2) +
            geom_text(
                aes(label = "Meðalaukning", x = 2, y = mean_evo*1.25, text = ''), 
                size = 4
            ) +
            scale_y_continuous(
                labels = percent, 
                breaks = pretty_breaks(5),
                expand = expansion(mult = 0.02)
            ) +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            scale_colour_manual(values = c(blue = "blue", grey = "grey")) +
            coord_flip() +
            labs(title = "Dagleg aukning á tíðni tilfella á völdu tímabili") +
            theme(axis.title = element_blank(), text = element_text(size = 12)) +
            background_grid(major = "none", minor = "none")
        # Label Chosen
        if (any(temp$col == "blue")) {
            p <- p + 
                geom_text(
                    data = tibble(),
                    aes(label = percent(evo_chosen), x = input$chosen_samanburdur, y = evo_chosen + 0.06*max(temp$change), text = ''), 
                    col = "blue", 
                    size = 4
                )
        }       
        if (nrow(temp) > 60) {
            p <- p + theme(axis.text.y = element_text(size = 5))
        }
        ggplotly(p, tooltip = 'text')
    })
    
    output$lmer_plot <- renderPlotly({
        lmer_plot()
    })
    
    ##### Vikuleg Smit #####
    vikuleg_aukning <- reactive({
        # req() ?
        # input$gobutton_samanburdur
        d %>% 
            #arrange(country, date) %>% 
            filter(country %chin% get_count_per_cont(input$continent_samanburdur)) %>%
            group_by(country) %>% 
            mutate(
                weekly_cases = fifelse(is.na(new_cases_lw), cumsum(new_cases), new_cases_lw),
                chosen = fifelse(country == input$chosen_samanburdur, "comp", "rest")
            ) %>%
            ungroup()
    })
    
    vikulegt_plot <- eventReactive(input$gobutton_samanburdur, {
        p <- vikuleg_aukning() %>%
            ggplot(
                aes(
                    total_cases, weekly_cases, 
                    group = country, alpha = chosen
                )
            ) +
            geom_abline(intercept = 0, slope = 1, lty = 2, size = 1,
                        col = "grey", alpha = 0.5) +
            geom_line() +
            scale_x_log10(breaks = c(1, 3, 10, 30, 
                                     1e2, 3e2, 1e3, 3e3,
                                     1e4, 3e4, 1e5, 3e5,
                                     1e6, 3e6, 1e7),
                          labels = label_number(accuracy = 1, big.mark = "\U202F")) +
            scale_y_log10(breaks = c(1, 3, 10, 30, 
                                     1e2, 3e2, 1e3, 3e3,
                                     1e4, 3e4, 1e5, 3e5,
                                     1e6),
                          labels = label_number(accuracy = 1, big.mark = "\U202F")) +
            scale_colour_manual(values = c(comp = "Blue", rest = "Black")) +
            coord_cartesian(xlim = c(1, 1e7),
                            ylim = c(1, 1e6)) +
            labs(title = "Vikuleg smit eftir löndum",
                 x = "Heildarfjöldi smita",
                 y = "Nýgreind smit undanfarna viku")
        ggplotly(p)
    })
    
    output$viku_plot <- renderPlotly({
        vikulegt_plot()
    })
    
    ##### Töfluyfirlit #####
    observe({
        updateCheckboxGroupInput(
            session = session,
            inputId = "countries_table",
            selected = if (input$selectall_table %% 2L == 1L) {
                get_count_per_cont(input$continent_table)
            } else {
                default_countries
            }
        )
    })

    output$countries_table <- renderUI({
        req(input$continent_table)
        selected <- if ("Evrópa" %in% input$continent_table) default_countries
        selectInput(
            inputId = "countries_table",
            label = "Lönd",
            choices = get_count_per_cont(input$continent_table),
            multiple = TRUE,
            selectize = TRUE,
            selected = selected
        )
    })
    
    output$countries_to_table <- renderUI({
        req(input$countries_table)
        selectInput(
            inputId = "chosen_table",
            label = "Samanburðarland",
            choices = input$countries_table,
            selectize = TRUE,
            selected = if ("Ísland" %in% input$countries_table) {
                "Ísland"
            }  else {
                input$countries_table[1]
            }
        )
    })
    
    summary_table <- eventReactive(input$gobutton2, {
      d[input$countries_table
        ][, .SD[.N], country
         ][, .(Land = country,
              Fólksfjöldi = count_pop[country],
              Tilfelli = total_cases, 
              `Smitatíðni` = total_cases_per100k / 100000,
              Dauðsföll = total_deaths,
              `Dánartíðni (per smit)` = death_rate,
              `Dánartíðni (per 100.000)` = total_deaths_per100k,
              `Fullkláraðar bólusetningar` = total_vaccines,
              `Bólusetningatíðni` = total_vaccines_per100k / 100000  
            )]
    })
    
    output$summary_table <- renderDT({
        req(input$chosen_table)
        datatable(
            summary_table(),
            extensions = 'Buttons',
            rownames= FALSE,
            options = list(
                dom = 'Bfrtip',
                buttons = c('csv', 'excel', 'pdf'),
                pageLength = 120,
                lengthChange = FALSE,
                language = list(
                  decimal = ",", thousands = ".", 
                  url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Icelandic.json'
                )
            )
        ) %>%
            formatPercentage(c("Smitatíðni", "Dánartíðni (per smit)", "Bólusetningatíðni"), 2) %>%
            formatRound(c("Dánartíðni (per 100.000)"), digits = 2) %>%
            formatStyle(
              target = 'row', columns = 'Land',  
              backgroundColor = styleEqual(input$chosen_table, c("#b3cde3"))
            )
    })
}