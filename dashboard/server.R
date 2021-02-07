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
        if (input$y_var == "total_cases") {
            y_var_p = "case_rate"
            y_var_n_daily = "new_cases"
            y_var_n_weekly = "cases_n_weekly"
            y_var_p_biweekly = "cases_p_biweekly"
        } else {
            y_var_p = "death_rate"
            y_var_n_daily = "new_deaths"
            y_var_n_weekly = "deaths_n_weekly"
            y_var_p_biweekly = "deaths_p_biweekly"
        }
        req(input$countries, input$chosen)
        if (input$filtervar == "ft") {
            filtervar <- input$y_var
            filtervalue <- input$filtervalue 
        } else if (input$filtervar == "ftpm") { 
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 10
        } else if (input$filtervar == "dags") {
            filtervar <- "date"
            filtervalue <- input$filtervalue_dags
        }
        out <- d %>%
            filter(
                country %chin% input$countries, 
                !!sym(filtervar) > filtervalue
            ) %>%
            mutate(
              chosen = fifelse(country == input$chosen, "comp", "rest"),
              y_var_n = !!sym(input$y_var),
              y_var_n_daily = !!sym(y_var_n_daily),
              y_var_p = !!sym(y_var_p),
              y_var_n_weekly = !!sym(y_var_n_weekly),
              y_var_p_biweekly = !!sym(y_var_p_biweekly)
            )
        # data.table::rowid() does same as group_by() + row_number()    
        if (input$x_var == "skyl") mutate(out, days = rowid(country)) else out
    })
    
    # Fjöldi graf
    euro_plot_n <- eventReactive(input$gobutton1, {
        
        if (input$y_var == "total_cases") {
            yvar <- "greindra smita"
        } else {
            yvar <- "skráðra dauðsfalla"
        }
        
        if (input$x_var == "skyl") {
            p <- throun_df() %>%
                ggplot(
                    aes(
                        days, y_var_n, 
                        col = chosen,
                        group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", y_var_n)
                    )
                ) +
                labs(
                    title = "Þróun fjölda greindra smita",
                    subtitle = "Sýnd eftir dögum frá öðru smiti hvers lands",
                    x = "Dagar síðan skilyrði var náð",
                    y = "Fjöldi greindra smita"
                )
        } else { 
            # Eftir dagsetningu
            p <- throun_df() %>%
                ggplot(
                    aes(
                        date, y_var_n, 
                        col = chosen,
                        group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", y_var_n),
                    )
                ) +
                scale_x_date(labels = date_format("%d/%m"), breaks = pretty_breaks(8)) +
                labs(
                    title = paste("Þróun fjölda", yvar),
                    subtitle = "Sýnd eftir dagsetningu",
                    y = paste("Fjöldi", yvar)
                ) +
                theme(axis.title.x = element_blank())
        }
        p <- p + 
            geom_line(show.legend = FALSE) +
            scale_colour_manual(values = c(comp = "Blue", rest = "Black"))
        p <- p +
            if (input$scale == "Logra") {
                scale_y_log10(labels = label_number(accuracy = 1, big.mark = "\U202F"))
            } else {
                scale_y_continuous(labels = label_number(accuracy = 1, big.mark = "\U202F"))
            }
        ggplotly(p, tooltip = "text")
    })
    
    output$euro_plot_n <- renderPlotly({
        euro_plot_n()
    })
    
    euro_plot_n_daily <- eventReactive(input$gobutton1, {
        
        if (input$y_var == "total_cases") {
            yvar <- "nýgreindra smita undanfarna viku"
        } else {
            yvar <- "skráðra dauðsfalla undanfarna viku"
        }
        
        if (input$x_var == "skyl") {
            p <- throun_df() %>%
                ggplot(
                    aes(
                        days, y_var_n_weekly, 
                        col = chosen, group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", y_var_n_weekly)
                    )
                ) +
                labs(
                    title = paste("Þróun fjölda", yvar),
                    subtitle = "Sýnd eftir dagsetningu",
                    x = "Dagar síðan skilyrði var náð",
                    y = paste("Fjöldi", yvar)
                )
        } else { 
            # Eftir dagsetningu
            p <- throun_df() %>%
                ggplot(
                    aes(
                        date, y_var_n_weekly, 
                        col = chosen, group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", y_var_n_weekly)
                    )
                ) +
                scale_x_date(labels = date_format("%d/%m"), breaks = pretty_breaks(8)) +
                labs(
                    title = paste("Þróun fjölda", yvar),
                    subtitle = "Sýnd eftir dagsetningu",
                    y = paste("Fjöldi", yvar)
                ) +
                theme(axis.title.x = element_blank())
        }
        p <- p + 
            geom_line(show.legend = FALSE) +
            scale_colour_manual(values = c(comp = "Blue", rest = "Black"))
        p <- p +
            if (input$scale == "Logra") {
                scale_y_log10(labels = label_number(accuracy = 1, big.mark = "\U202F"))
            } else {
                scale_y_continuous(labels = label_number(accuracy = 1, big.mark = "\U202F"))
            }
        ggplotly(p, tooltip = "text")
    })
    
    output$euro_plot_n_daily <- renderPlotly({
        euro_plot_n_daily()
    })
    
    # Tíðni graf
    euro_plot_p <- eventReactive(input$gobutton1, {
        if (input$y_var == "total_cases") {
            yvar <- "smitaðra"
        } else {
            yvar <- "dauðsfalla"
        }
        
        
        if (input$x_var == "skyl") {
            p <- throun_df() %>%
                ggplot(
                    aes(
                        days, y_var_p,
                        col = chosen, group = country,
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", round(y_var_p, 2))
                    )
                ) +
                labs(
                    title = ifelse(
                        input$y_var == "total_cases", 
                        "Þróun tíðni greindra smita (per 100.000 íbúa)",
                        "Þróun dánartíðni (hlutfall smitaðra sem látast)"
                    ),
                    subtitle = ifelse(
                        input$y_var == "total_cases",
                        "Sýnd sem fjöldi á hverja 100.000 íbúa eftir dögum frá öðru smiti hvers lands",
                        "Sýnd sem fjöldi skráðra dauðsfalla gegn fjölda greindra smita"),
                    y = ifelse(input$y_var == "total_cases", 
                               "Fjöldi smitaðra á hverja 100.000 íbúa",
                               "Dánartíðni (hlutfall smitaðra sem hafa látist)"),
                    x = "Dagar síðan skilyrði var náð"
                )
        } else {
            # Eftir dagsetningu
            p <- throun_df() %>%
                ggplot(
                    aes(
                        date, y_var_p, 
                        col = chosen, group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", round(y_var_p, 2))
                    )
                ) +
                scale_x_date(labels = date_format("%d/%m"), breaks = pretty_breaks(8)) +
                labs(
                    title = ifelse(
                        input$y_var == "total_cases", 
                        "Þróun tíðni greindra smita (per 100.000 íbúa)",
                        "Þróun dánartíðni (hlutfall smitaðra sem hafa látist)"
                    ),
                    subtitle = ifelse(
                        input$y_var == "total_cases",
                        "Sýnd sem fjöldi á hverja 100.000 íbúa eftir dögum frá öðru smiti hvers lands",
                        "Sýnd sem fjöldi skráðra dauðsfalla gegn fjölda greindra smita"),
                    y = ifelse(input$y_var == "total_cases", 
                               "Fjöldi smitaðra á hverja 100.000 íbúa",
                               "Dánartíðni (Fjöldi látinna / fjöldi smitaðra)"),
                    x = "Dagar síðan skilyrði var náð"
                ) +
                theme(axis.title.x = element_blank())
        }
        p <- p +
            geom_line(show.legend = FALSE) +
            scale_colour_manual(values = c(comp = "Blue", rest = "Black"))
        p <- p +
            if (input$scale == "Logra") {
                scale_y_log10(labels = label_number(big.mark = "\U202F", decimal.mark = ","))
            } else {
                scale_y_continuous(labels = label_number(accuracy = 1, big.mark = "\U202F", decimal.mark = ","))
            }
        ggplotly(p, tooltip = "text")
    })
    
    output$euro_plot_p <- renderPlotly({
        euro_plot_p()
    })
    
    euro_plot_p_biweekly <- eventReactive(input$gobutton1, {
        # Only available for total cases.
        if (input$x_var == "skyl") {
            p <- throun_df() %>%
                ggplot(
                    aes(
                        days, y_var_p_biweekly,
                        col = chosen, group = country,
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", round(y_var_p_biweekly, 2))
                    )
                ) +
                labs(
                    title = "Greind smit síðustu tvær vikurnar (per 100.000 íbúa)",
                    y = "Fjöldi smitaðra á hverja 100.000 íbúa",
                    x = NULL
                )
        } else {
            # Eftir dagsetningu
            p <- throun_df() %>%
                ggplot(
                    aes(
                        date, y_var_p_biweekly, 
                        col = chosen, group = country, 
                        text = paste0(country, ", ", format(date, "%d/%m"), "<br>", round(y_var_p_biweekly, 2))
                    )
                ) +
                scale_x_date(labels = date_format("%d/%m"), breaks = pretty_breaks(8)) +
                labs(
                    title = "Greind smit síðustu tvær vikurnar (per 100.000 íbúa)",
                    y = "Fjöldi smitaðra á hverja 100.000 íbúa",
                    x = NULL
                ) +
                theme(axis.title.x = element_blank())
        }
        p <- p +
            geom_line(show.legend = FALSE) +
            scale_colour_manual(values = c(comp = "Blue", rest = "Black"))
        p <- p +
            if (input$scale == "Logra") {
                scale_y_log10(labels = label_number(big.mark = "\U202F", decimal.mark = ","))
            } else {
                scale_y_continuous(labels = label_number(accuracy = 1, big.mark = "\U202F", decimal.mark = ","))
            }
        ggplotly(p, tooltip = "text")
    })

    output$euro_plot_p_biweekly <- renderPlotly({
        euro_plot_p_biweekly()
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
                summarise(ekki_med_case = any(case_rate == 0)) %>% 
                filter(ekki_med_case) %>% 
                pull(country)
            d <- d %>% filter(!country %in% ekki_byrjud)
        } else {
            if (input$type_filt_samanburdur == "Fjöldi tilvika") {
                filter_var <- "total_cases"
                filter_value <- input$filtervalue_samanburdur
            }
            else {
                filter_var <- "case_rate"
                filter_value <- input$filtervalue_samanburdur / 1000
            }
            d <- d %>% filter(!!sym(filter_var) >= filter_value)
        }
        m <- lmer(
            log(case_rate) ~ days + (days | country),
            data = d,
            control = lmerControl(optimizer = "bobyqa")
        )
        temp <- as_tibble(coef(m)$country, rownames = "country") %>%
            select(-`(Intercept)`) %>%
            mutate(
                col = if_else(country == input$chosen_samanburdur, "blue", "grey"),
                change = exp(days) - 1,
                country = factor(reorder(country, change)),
            )
        evo_chosen <- temp %>% filter(col == "blue") %>% pull(round(change, 3))
        mean_evo <- exp(fixef(m)[2]) - 1
        p <- temp %>%
            ggplot(aes(country, change)) +
            geom_point(aes(col = col), show.legend = FALSE) +
            geom_segment(aes(xend = country, yend = 0, col = col), show.legend = FALSE) +
            geom_hline(yintercept = exp(summary(m)$coefficients[2, 1]) - 1, lty = 2) +
            # Meðalaukning
            geom_text(
                aes(label = "Meðalaukning", x = 2, y = mean_evo + 0.01), 
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
                    aes(label = percent(evo_chosen), x = input$chosen_samanburdur, y = evo_chosen + 0.02), 
                    col = "blue", 
                    size = 4
                )
        }       
        if (nrow(temp) > 60) {
            p <- p + theme(axis.text.y = element_text(size = 5))
        }
        ggplotly(p, tooltip = c("x", "y", "country"))
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
            filter(continent %in% input$continent_samanburdur, total_cases > 0) %>%
            group_by(country) %>% 
            mutate(
                weekly_cases = fifelse(is.na(cases_n_weekly), cumsum(new_cases), cases_n_weekly),
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
      d[country %chin% input$countries_table, .SD[.N], country
        ][, .(Land = country,
              Fólksfjöldi = count_pop[country],
              Tilfelli = total_cases, 
              `Smitatíðni` = total_cases / count_pop[country],
              Dauðsföll = total_deaths,
              `Dánartíðni (per smit)` = total_deaths/total_cases,
              `Dánartíðni (per 100.000)` = total_deaths / count_pop[country] * 100000,
              `Bólusetningar` = total_vaccines,
              `Bólusetningatíðni` = total_vaccines / count_pop[country]
            )]
    })
    
    output$summary_table <- renderDataTable({
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