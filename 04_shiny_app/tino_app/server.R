server <- function(input, output, session) {

    # Define color palette
    pal <- colorFactor(
        palette = c("#AAAAAA", "#F4A0A0", "#C0392B", "#111111"),
        levels  = c(0, 1, 2, 3)
    )

    # Map
    #----

    # (pre) select data by selected date
    selected_date <- reactiveVal(unique_dates_US[1])

    # select the date
    lapply(seq_along(unique_dates_US), function(i) {
        observeEvent(input[[paste0("btn_date_", i)]], {
            selected_date(unique_dates_US[i]) }) })

    map_data <- reactive({
        shp_distr |>
            left_join(impacts |> filter(timestep == selected_date()),
                      by = c("BEZNAME" = "district")) })

    # create leaflet map
    output$heat_map <- renderLeaflet({
        leaflet(map_data()) |>
            addPolygons(
                fillColor   = ~pal(risk),
                fillOpacity = 0.8,
                color       = "#FFFFFF",
                weight      = 1.2,
                layerId     = ~BEZNAME,
                label       = ~paste0(BEZNAME, ": risk = ", risk),
                highlightOptions = highlightOptions(
                    weight       = 2,
                    color        = "#555555",
                    fillOpacity  = 0.9,
                    bringToFront = TRUE
                )
            ) |>
            addLegend(
                position = "topright",
                colors   = c("#AAAAAA", "#F4A0A0", "#C0392B", "#111111"),
                labels   = c("No", "Small", "Medium", "Large"),
                title    = "Heat-mortality risk",
                opacity  = 0.8
            )
    })

    # clicked district data
    clicked_district_data <- reactiveVal(NULL)

    observeEvent(input$heat_map_shape_click, {
        click <- input$heat_map_shape_click
        req(click$id)

        district_all_dates <- impacts |>
            filter(district == click$id)

        clicked_district_data(district_all_dates)
        cat("District:", click$id, "\n")
    })

    #----

    # Address search
    #----

    # debounce so we don't fire on every keystroke
    address_debounced <- debounce(reactive(input$address_input), 600)

    # query Nominatim for suggestions
    address_results <- reactive({
        req(nchar(address_debounced()) > 4)

        url <- paste0(
            "https://nominatim.openstreetmap.org/search?q=",
            utils::URLencode(address_debounced()),
            "&countrycodes=ch&format=json&addressdetails=1&limit=5"
        )

        response <- httr::GET(url, httr::user_agent("heat_risk_app"))
        jsonlite::fromJSON(httr::content(response, as = "text"))
    })

    # render clickable suggestions
    output$address_suggestions <- renderUI({
        res <- address_results()
        req(nrow(res) > 0)

        tags$div(
            style = "border: 1px solid #ddd; border-radius: 6px; overflow: hidden;",
            lapply(seq_len(nrow(res)), function(i) {
                actionButton(
                    inputId = paste0("addr_btn_", i),
                    label   = res$display_name[i],
                    style   = "width: 100%; text-align: left; border-radius: 0;
                               border: none; border-bottom: 1px solid #eee;
                               background: white; color: #333; font-size: 13px;
                               white-space: normal; height: auto; padding: 8px 12px;"
                )
            })
        )
    })

    # when user clicks a suggestion, extract district and look up data
    selected_address_data <- reactiveVal(NULL)
    # selected_address_data <- impacts |> filter(district == "District de Monthey")

    observe({
        res <- address_results()
        req(nrow(res) > 0)

        lapply(seq_len(nrow(res)), function(i) {
            observeEvent(input[[paste0("addr_btn_", i)]], {
                nominatim_district <- res$address$county[i]

                cat("Nominatim county:", nominatim_district, "\n")

                matched <- shp_distr |>
                    st_drop_geometry() |>
                    filter(stringr::str_detect(BEZNAME,
                                               stringr::fixed(nominatim_district,
                                                              ignore_case = TRUE))) |>
                    pull(BEZNAME) |>
                    first()

                if (!is.na(matched)) {
                    district_all_dates <- impacts |> filter(district == matched)
                    selected_address_data(district_all_dates)
                    cat("Matched to:", matched, "\n")
                } else {
                    cat("No match found for:", nominatim_district, "\n")
                }
                updateTextInput(session, "address_input", value = "")
            }, ignoreInit = TRUE)
        })
    })

    #----

    # Barplot
    #----

    # (pre) select data by selected Bezirk
    output$risk_plot <- renderPlot({
        req(selected_address_data())

        selected_address_data() |>
        # selected_address_data |>
            mutate(timestep = as.Date(timestep)) |>
            ggplot(aes(x = timestep, y = 1)) +
            geom_col(aes(fill = factor(risk)), color = "black", width = 0.9,
                     show.legend = FALSE, alpha = 0.8) +
            scale_fill_manual(values = c(
                "0" = "#AAAAAA",
                "1" = "#F4A0A0",
                "2" = "#C0392B",
                "3" = "#111111"
            )) +
            lims(y = c(0,1)) +
            scale_x_date(date_labels = "%d.%m.%y", date_breaks = "1 day") +
            # scale_y_continuous(
            #     breaks = c(0, 1, 2, 3),
            #     labels = c("No", "Small", "Medium", "Large"),
            #     limits = c(0, 3)
            # ) +
             labs(
                title = paste0("Heat-mortality risk: ", selected_address_data()$district[1]),
            ) +
            theme_minimal() +
            theme(
                axis.text.x  = element_text(angle = 0, hjust = .5, vjust = 1, size = 13),
                plot.title   = element_text(size = 13, face = "bold"),
                axis.title = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # axis.ticks.x = element_line(),
                plot.margin  = margin(t = 10, r = 10, b = 40, l = 10),
            )
    })


    observeEvent(selected_address_data(), {
        req(selected_address_data())

        matched_district <- selected_address_data()$district[1]

        # get geometry of matched district
        matched_geom <- map_data() |>
            filter(BEZNAME == matched_district)

        leafletProxy("heat_map") |>
            clearGroup("highlight") |>
            addPolygons(
                data        = matched_geom,
                fillColor   = "transparent",
                fillOpacity = 0,
                color       = "#FFD700",
                weight      = 3.5,
                opacity     = 1,
                group       = "highlight"
            )
    })

}
