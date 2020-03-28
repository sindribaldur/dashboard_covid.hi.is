server <- function(input, output, session) {
    ##### Smitafjöldi #####
    observe({
        toselect <-
            if (input$selectall %% 2 == 1) {
                unique(d[d$continent %in% input$continent, "country"])
            } else {
                selected = c("Denmark", "Norway", "Finnland", "Sweden", "Iceland")
            }
        updateCheckboxGroupInput(
            session = session,
            inputId = "countries",
            selected = toselect
        )
    })
    
    output$countries <- renderUI({
        req(input$continent)
        selected <- 
            if ("Europe" %in% input$continent) {
                c("Denmark", "Norway", "Finnland", "Sweden", "Iceland")
            }
        selectInput(
            inputId = "countries",
            label = "Lönd",
            choices = unique(d[d$continent %in% input$continent, "country"]),
            multiple = TRUE, 
            selectize = TRUE,
            selected = selected
        )
    })
    
    output$countries_to_choose <- renderUI({
        req(input$countries)
        selectInput(
            inputId = "chosen",
            label = "Samanburðarland",
            choices = input$countries,
            selectize = TRUE,
            selected = if ("Iceland" %in% input$countries) {
                "Iceland"
            }  else {
                input$countries[1]
            }
        )
    })
    
    euro_plot_n <- eventReactive(input$gobutton1, {
        req(input$countries, input$chosen)
        if (input$filtervar == "Fjöldi tilvika") {
            filtervar <- "total_cases"
            filtervalue <- input$filtervalue 
        } else {
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- d %>% 
                filter(
                    country %in% input$countries, 
                    continent %in% input$continent,
                    !!sym(filtervar) > filtervalue
                ) %>% 
                group_by(country) %>% 
                mutate(
                  days = row_number(),
                  chosen = if_else(country == input$chosen, "chosen", "other")
                ) %>% 
                ungroup() %>% 
                ggplot(
                    aes(days, total_cases, col = chosen, size = chosen, alpha = chosen, group = country)
                ) +
                geom_line(show.legend = FALSE) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(
                    title = "Þróun fjölda smitaðra á Íslandi og annars staðar",
                    subtitle = "Sýnd eftir dögum frá öðru smiti hvers lands",
                    x = "Dagar",
                    y = "Fjöldi smitaðra"
                )
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- d %>% 
                filter(
                    country %in% input$countries, 
                    continent %in% input$continent,
                    !!sym(filtervar) > filtervalue
                ) %>% 
                mutate(
                    days_from_first = as.integer(date - min(date)),
                    chosen = ifelse(country == input$chosen, "chosen", "other")
                ) %>% 
                ggplot(
                    aes(date, total_cases, col = chosen, size = chosen, alpha = chosen, group = country)
                ) +
                geom_line(show.legend = FALSE) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(
                    title = "Þróun fjölda smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd eftir dagsetningu",
                     y = "Fjöldi smitaðra"
                ) +
                theme(axis.title.x = element_blank())
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        }
    })
    
    output$euro_plot_n <- renderPlotly({
        euro_plot_n()
    })
    
    output$euro_plot_n_info <- renderText({
        data <- nearPoints(
            d %>% filter(country %in% input$countries), 
            input$euro_plot_n_click, 
            threshold = 40, addDist = TRUE,
            xvar = "days", 
            yvar = "total_cases"
        )
        data <- data[which.min(data$dist_), ]
        if (length(data$country) == 0) return("Smelltu á línu til að sjá hvaða landi hún tilheyrir")
        today <- Sys.Date()
        timi <- as.integer(today - data$date)
        out <- paste0(data$country, ": Fjöldi var ", round(data$total_cases, 3), " fyrir ", timi, " dögum")
        out
    })
    
    ##### Europe Rates #####
    euro_plot_p <- eventReactive(input$gobutton1, {
        req(input$countries, input$chosen)
        if (input$filtervar == "Fjöldi tilvika") {
            filtervar <- "total_cases"
            filtervalue <- input$filtervalue 
        }
        else { 
            filtervar <- "case_rate"
            filtervalue <- input$filtervalue / 1000
        }
        if (input$x_var == "Dögum síðan skilyrði að neðan var náð") {
            p <- d %>% 
                filter(
                    country %in% input$countries, 
                    continent %in% input$continent,
                    !!sym(filtervar) > filtervalue
                ) %>% 
                group_by(country) %>% 
                mutate(
                    days = row_number(),
                    chosen = ifelse(country == input$chosen, "chosen", "other")
                ) %>% 
                ungroup() %>% 
                ggplot(
                    aes(days, case_rate, col = chosen, size = chosen, alpha = chosen, group = country)
                ) +
                geom_line(show.legend = FALSE) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(
                    title = "Þróun tíðni smitaðra á Íslandi og annars staðar",
                    subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                    x = "Dagar",
                    y = "Fjöldi smitaðra (per 1000 íbúar)"
                )
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        } else {
            p <- d %>% 
                filter(
                    country %in% input$countries, 
                    continent %in% input$continent,
                    !!sym(filtervar) > filtervalue
                ) %>% 
                mutate(chosen = ifelse(country == input$chosen, "chosen", "other")) %>% 
                ggplot(
                    aes(date, case_rate, col = chosen, size = chosen, alpha = chosen, group = country)
                ) +
                geom_line(show.legend = FALSE) +
                scale_x_date(date_labels = "%d. %B", breaks = pretty_breaks(8)) +
                scale_colour_manual(values = c("Blue", "Black")) +
                scale_size_manual(values = c(1.2, 0.8)) + 
                scale_alpha_manual(values = c(1, 0.3)) +
                labs(
                     title = "Þróun tíðni smitaðra á Íslandi og annars staðar",
                     subtitle = "Sýnd sem fjöldi per 1000 íbúar eftir dögum frá öðru smiti hvers lands",
                     y = "Fjöldi smitaðra (per 1000 íbúar)"
                ) +
                theme(axis.title.x = element_blank())
            if (input$scale == "Logra") p <- p + scale_y_log10()
            ggplotly(p, tooltip = c("x", "y", "country"))
        }
    })

    output$euro_plot_p <- renderPlotly({
        euro_plot_p()
    })
    
    output$euro_plot_p_info <- renderText({
        data <- nearPoints(
            d %>% filter(country %in% c(input$countries)),
            input$euro_plot_p_click, 
            threshold = 40, 
            addDist = TRUE,
            xvar = "days", 
            yvar = "case_rate"
        )
        data <- data[which.min(data$dist_), ]
        if (length(data$country) == 0) return ("Smelltu á línu til að sjá hvaða landi hún tilheyrir")
        today <- Sys.Date()
        timi <- as.integer(today - data$date)
        out <- paste0(data$country, ": Tíðni var ", round(data$case_rate, 3), " per 1000 íbúa fyrir ", timi, " dögum")
        out
    })
    
    ##### Europe LMER #####
    output$countries_to_choose_samanburdur <- renderUI({
        req(input$continent_samanburdur)
        if ("Europe" %in% input$continent_samanburdur) {
            selectInput(
                inputId = "chosen_samanburdur", 
                label = "Samanburðarland", 
                choices = d %>% 
                    filter(continent == input$continent_samanburdur) %>% 
                    pull(country) %>% 
                    unique(),
                selectize = TRUE,
                selected =  "Iceland"
            )
        } else {
            selectInput(
                inputId = "chosen_samanburdur", 
                label = "Samanburðarland", 
                choices = d %>% 
                    filter(continent == input$continent_samanburdur) %>% 
                    pull(country) %>% 
                    unique(),
                    selectize = TRUE
            )
        }
    })
    
    output$param_selection_samanburdur <- renderUI({
        req(input$tegund_samanburdur)
        if (input$tegund_samanburdur == "Dagsetning") {
            h4("Veldu tímabil til að bera saman aukningu í tíðni smita")
            fluidRow(
                column(
                    6,
                    dateInput(
                        "date_from_samanburdur", 
                        label = "Frá",
                        value = "2020-03-04", 
                        min = "2020-03-02", 
                        max = max(d$date) - 3
                    )
                ),
                column(
                    6,
                    dateInput(
                        "date_to_samanburdur", 
                        label = "Til",
                        value = max(d$date), 
                        min = "2020-03-08", 
                        max = max(d$date)
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
                        max = 100, 
                        value = 50
                    )
                )
            )
        }
    })
    
    lmer_plot <- eventReactive(input$gobutton_samanburdur, {
        req(input$continent_samanburdur)
        if (input$tegund_samanburdur == "Dagsetning") {
            d <- d %>% 
                filter(
                    date >= input$date_from_samanburdur,
                    date <= input$date_to_samanburdur,
                    continent %in% input$continent_samanburdur
                ) %>% 
                mutate(days = as.integer(date - min(date)))
            n_obs <- length(unique(d$country))
        } else {
            if (input$type_filt_samanburdur == "Fjöldi tilvika") {
                filter_var <- "total_cases"
                filter_value <- input$filtervalue_samanburdur
            }
            else {
                filter_var <- "case_rate"
                filter_value <- input$filtervalue_samanburdur / 1000
            }
            d <- d %>% 
                filter(
                    continent %in% input$continent_samanburdur,
                    !!sym(filter_var) >= filter_value
                ) %>% 
                mutate(days = as.integer(date - min(date)))
            n_obs <- length(unique(d$country))
        }
        m <- lmer(
            log(case_rate) ~ days + (days | country), 
            data = d,
            control = lmerControl(optimizer = "bobyqa")
        )
        evo <- coef(m)[[1]][, 2, drop = FALSE] %>% exp()
        evo <- evo[order(evo), , drop = FALSE]
        which_chosen <- which(rownames(evo) == input$chosen_samanburdur)
        evo_chosen <- evo[which_chosen, ]
        evo_chosen <- round(evo_chosen - 1, 3)
        mean_evo <- exp(fixef(m)[2]) - 1
        p <- tibble(country = rownames(evo), change = evo[, 1]) %>%
            mutate(
                country = factor(reorder(country, change)),
                col = if_else(country == input$chosen_samanburdur, "blue", "grey")
            ) %>%
            ggplot(aes(country, change - 1)) +
            geom_point(aes(col = col), show.legend = FALSE) +
            geom_segment(aes(xend = country, yend = 0, col = col), show.legend = FALSE) +
            geom_hline(yintercept = exp(summary(m)$coefficients[2, 1]) - 1, lty = 2) +
            # Meðalaukning
            geom_text(
                data = tibble(), 
                aes(label = "Meðalaukning", x = 2, y = mean_evo + 0.05), size = 4
            ) +
            # Label Chosen
            geom_text(
                data = tibble(),
                aes(label = percent(evo_chosen), x = which_chosen, y = evo_chosen + 0.02), 
                col = "blue", 
                size = 4
            ) +
            scale_y_continuous(
                labels = percent, 
                breaks = pretty_breaks(5),
                expand = expansion(mult = 0.02)
            ) +
            scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
            scale_colour_manual(values = c("blue", "grey", "red")) +
            coord_flip() +
            labs(title = "Dagleg aukning á tíðni tilfella (per 1000 íbúa) á völdu tímabili") +
            theme(axis.title = element_blank(), text = element_text(size = 12)) +
            background_grid(major = "none", minor = "none")
        if (n_obs > 60) {
          p <- p + theme(axis.text.y = element_text(size = 5))
        }
        ggplotly(p, tooltip = c("x", "y", "country"))
    })
    
    output$lmer_plot <- renderPlotly({
        lmer_plot()
    })
    
    ##### Europe Table #####
    output$countries_to_table <- renderUI({
        req(input$countries)
        if ("Iceland" %in% input$countries) {
            selectInput(
                inputId = "chosen_table", 
                label = "Samanburðarland", 
                choices = input$countries_table,
                selectize = TRUE,
                selected =  "Iceland"
            )
        } else {
            selectInput(
                inputId = "chosen_table", 
                label = "Samanburðarland", 
                choices = input$countries_table,
                selectize = TRUE,
                selected =  input$countries_table[1]
            )
        }
    })
    
    summary_table <- eventReactive(input$gobutton2, {
        req(input$sort_col)
        cols <- list(
            "Landi" = "country", 
            "Tilfellum" = "cases", 
            "Tíðni" = "incidence", 
            "Fyrsta smiti" = "days"
        )
        out <- d %>% 
            filter(country %in% input$countries_table) %>% 
            group_by(country) %>%
            summarise(
                cases = max(total_cases),
                incidence = round(cases / max(pop) * 1000, 4),
                days = as.integer(max(date) - min(date)),
                first = min(date)
            ) %>% 
            arrange(desc(!!sym(cols[[input$sort_col]])))
        if (input$sort_col == "Landi") {
            out <- out %>% 
                arrange(!!sym(cols[[input$sort_col]]))
        } else {
            out <- out %>% 
                arrange(desc(!!sym(cols[[input$sort_col]])))
        }
        names(out) <- c("Land", "Tilfelli", "Tíðni (per 1000)", "Dagar frá fyrsta smiti", "Dagsetning fyrsta smits")
        icel <- which(out$Land == input$chosen_table)
        out 
    })
    
    output$summary_table <- function() {
        out <- summary_table()
        icel <- which(out$Land == input$chosen_table)
        out %>% 
            kable(format = "html", align = c("l", rep("c", ncol(.) - 1))) %>% 
            kable_styling(bootstrap_options = c("striped", "hover")) %>% 
            row_spec(icel, bold = TRUE, background = "#b3cde3")
    }
    
    output$table_download <- downloadHandler(
        filename = function() {
            paste0("tafla_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write_xlsx(summary_table(), file)
        }
    )
    
    ##### Forspá #####
    out_gogn <- eventReactive(input$gobutton_forspa, {
        out <- d_spa %>% 
            filter(
              type == input$tegund_forspa,
              name == input$breyta_forspa,
              date >= input$date_from_forspa,
              date <= input$date_to_forspa
            )
        if (input$byage_forspa == "Heild") {
            out <- out %>% 
                filter(age == "total") %>% 
                select(
                    -type, -age, -name,
                    Dagsetning = date, 
                    "Líklegasta spá" = median, 
                    "Svartsýn spá" = upper
                )
        } else {
            out <- out %>% 
                filter(age != "total") %>% 
                select(
                    -name, -type,
                    Dagsetning = date, 
                    Aldur = age, 
                    "Líklegasta spá" = median, 
                    "Svartsýn spá" = upper
                )
        }
        out
    })
    output$downloadData_forspa <- downloadHandler(
        filename = function() {
            paste0(
                Sys.Date(),
                "_covid_",
                input$tegund_forspa, "_",
                input$breyta_forspa, "_",
                if (input$byage_forspa != "Heild") "eftir_aldri" else "",
                "_spagildi.xlsx"
            )
        },
        content = function(file) {write_xlsx(out_gogn(), file)}
    )
    output$tafla <- renderDataTable({
        datatable(
            out_gogn(),
            rownames= FALSE,
            options = list(dom = 't', pageLength = nrow(out_gogn()))
        )
    })
}