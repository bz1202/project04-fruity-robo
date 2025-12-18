library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)

# Data prep ---------------------------------------------------------------
raw_data <- readRDS("data/merged_data.rds")

explorer_data <- raw_data %>%
  mutate(
    price_usd = as.numeric(price) / 100,
    positive = as.numeric(positive),
    negative = as.numeric(negative),
    total_reviews = positive + negative,
    positive_rate = ifelse(total_reviews > 0, round(positive / total_reviews * 100, 1), NA),
    playtime_hours = as.numeric(average_forever) / 60,
    release_date = mdy(release_date),
    release_year = year(release_date)
  ) %>%
  filter(!is.na(price_usd), !is.na(positive_rate)) %>%
  select(
    name, developer, publisher, price_usd, positive_rate, total_reviews,
    playtime_hours, ccu, owners, release_year, tags, overall_review
  )

year_range <- range(explorer_data$release_year, na.rm = TRUE)
price_range <- range(explorer_data$price_usd, na.rm = TRUE)
headline_metrics <- list(
  median_price = median(explorer_data$price_usd, na.rm = TRUE),
  median_rating = median(explorer_data$positive_rate, na.rm = TRUE),
  median_play = median(explorer_data$playtime_hours, na.rm = TRUE),
  total_games = nrow(explorer_data)
)

metric_card <- function(label, value, suffix = "", accent = "#2563eb") {
  div(
    class = "metric-card",
    style = paste0("border-top: 3px solid ", accent, ";"),
    p(class = "metric-label", label),
    h3(class = "metric-value", paste0(value, suffix))
  )
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;600;700&family=Inter:wght@400;500;600&display=swap"),
    tags$style(HTML("
      :root {
        --ink: #0f172a;
        --muted: #475569;
        --accent: #2563eb;
        --accent-2: #22c55e;
        --panel: #0b1224;
        --bg: #f5f7fb;
      }
      body { font-family: 'Space Grotesk','Inter','Segoe UI',sans-serif; background: var(--bg); color: var(--ink); }
      .container-fluid { max-width: 1200px; }
      .hero { background: linear-gradient(135deg, rgba(37,99,235,0.14), rgba(34,197,94,0.12));
              border: 1px solid rgba(37,99,235,0.15); border-radius: 18px; padding: 22px; margin: 14px 0 18px;
              box-shadow: 0 18px 38px rgba(15,23,42,0.14); color: #0f172a; }
      .hero h4 { margin-top: 0; font-weight: 700; letter-spacing: -0.2px; }
      .panel { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 14px; padding: 16px; box-shadow: 0 12px 32px rgba(15,23,42,0.12); }
      .control-row { gap: 12px; }
      .metric-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px; margin: 14px 0; }
      .metric-card { background: #0b1224; color: #e2e8f0; border-radius: 12px; padding: 14px 16px; border: 1px solid rgba(255,255,255,0.08); }
      .metric-label { margin: 0; font-size: 0.95rem; color: #cbd5e1; }
      .metric-value { margin: 4px 0 0; font-weight: 700; letter-spacing: -0.3px; }
      .nav-tabs>li>a { font-weight: 600; color: var(--muted); }
      .nav-tabs>li.active>a { color: var(--accent); }
      .well { background: #eef2ff; border: 1px solid #c7d2fe; }
    "))
  ),
  titlePanel("Steam Game Explorer"),
  div(
    class = "hero",
    h4("Find your launch lane, price, and pitch"),
    p("Scan 28k+ Steam games by price, sentiment, and release year to spot the pricing sweet spot, the least crowded launch windows, and the tags that carry premium perception."),
    div(
      class = "metric-grid",
      metric_card("Median Price", paste0("$", round(headline_metrics$median_price, 2)), accent = "#f8c660"),
      metric_card("Median Positive %", paste0(round(headline_metrics$median_rating, 1), "%"), accent = "#22c55e"),
      metric_card("Median Playtime", paste0(round(headline_metrics$median_play, 1), " hrs"), accent = "#2563eb"),
      metric_card("Games Indexed", format(headline_metrics$total_games, big.mark = ","), accent = "#7c3aed")
    )
  ),
  fluidRow(
    class = "control-row",
    column(3, class = "panel",
           sliderInput("price", "Price (USD)", min = floor(price_range[1]), max = ceiling(price_range[2]),
                       value = price_range, step = 1),
           sliderInput("rating", "Positive Rating (%)", min = 0, max = 100, value = c(60, 100), step = 1)
    ),
    column(3, class = "panel",
           sliderInput("year", "Release Year", min = year_range[1], max = year_range[2],
                       value = year_range, step = 1),
           selectInput("playtime_bucket", "Playtime Focus",
                       choices = c("All" = "all", "Short (<2h)" = "short", "Standard (2-20h)" = "standard", "Long (20h+)" = "long"),
                       selected = "all")
    ),
    column(3, class = "panel",
           textInput("search", "Search title/producer", placeholder = "type name, developer, publisher"),
           checkboxInput("hide_free", "Hide free games", value = FALSE)
    ),
    column(3, class = "panel",
           p(strong("Tips")),
           tags$ul(
             tags$li("Drag sliders to narrow price & sentiment windows"),
             tags$li("Use table filters for fine-grain search"),
             tags$li("Hover bubbles for detailed tooltips")
           )
    )
  ),
  br(),
  tabsetPanel(
    tabPanel(
      "Game Explorer",
      br(),
      div(class = "panel",
          h4("Browse & filter"),
          p(class = "text-muted", "Use the table filters to refine by publisher, year, or price tiers."),
          DTOutput("game_table")
      )
    ),
    tabPanel(
      "Price vs Rating",
      br(),
      div(class = "panel",
          h4("Does price hurt ratings?"),
          p(class = "text-muted", "Bubble = review volume, color = playtime intensity. Filters on the left apply."),
          plotlyOutput("price_rating_plot", height = "520px")
      )
    ),
    tabPanel(
      "Release Timeline",
      br(),
      div(class = "panel",
          h4("Launch crowding vs sentiment"),
          p(class = "text-muted", "Bar = game count per year, line = average positive rating."),
          plotlyOutput("timeline_plot", height = "520px")
      )
    ),
    tabPanel(
      "Tag Performance",
      br(),
      div(class = "panel",
          h4("Top tags by price & sentiment"),
          p(class = "text-muted", "Size = game count (>=100 titles), color = avg rating."),
          plotlyOutput("tag_plot", height = "520px")
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  filtered_data <- reactive({
    d <- explorer_data %>%
      filter(
        price_usd >= input$price[1], price_usd <= input$price[2],
        positive_rate >= input$rating[1], positive_rate <= input$rating[2],
        release_year >= input$year[1], release_year <= input$year[2]
      )

    if (input$hide_free) d <- d %>% filter(price_usd > 0)

    if (input$playtime_bucket != "all") {
      d <- d %>%
        filter(case_when(
          input$playtime_bucket == "short" ~ playtime_hours < 2,
          input$playtime_bucket == "standard" ~ playtime_hours >= 2 & playtime_hours <= 20,
          input$playtime_bucket == "long" ~ playtime_hours > 20,
          TRUE ~ TRUE
        ))
    }

    if (nzchar(input$search)) {
      key <- tolower(input$search)
      d <- d %>%
        filter(
          str_detect(tolower(name), key) |
          str_detect(tolower(developer), key) |
          str_detect(tolower(publisher), key)
        )
    }

    d
  })

  output$game_table <- renderDT({
    d <- filtered_data()
    datatable(
      d %>%
        select(
          Game = name,
          `Price ($)` = price_usd,
          `Positive %` = positive_rate,
          Reviews = total_reviews,
          `Avg Playtime (hrs)` = playtime_hours,
          `CCU` = ccu,
          `Owners` = owners,
          `Year` = release_year
        ),
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE, dom = "Bfrtip"),
      rownames = FALSE
    )
  })

  output$price_rating_plot <- renderPlotly({
    d <- filtered_data() %>%
      filter(total_reviews >= 50, price_usd > 0, price_usd < 100)
    req(nrow(d) > 0)

    if (nrow(d) > 4000) d <- d %>% slice_sample(n = 4000)

    plot_ly(
      d,
      x = ~price_usd, y = ~positive_rate,
      size = ~log10(total_reviews + 1),
      color = ~log10(playtime_hours + 1),
      text = ~paste0(
        "<b>", name, "</b><br>",
        "Price: $", round(price_usd, 2), "<br>",
        "Rating: ", positive_rate, "%<br>",
        "Reviews: ", format(total_reviews, big.mark = ","), "<br>",
        "Avg Playtime: ", round(playtime_hours, 1), " hrs<br>",
        "CCU: ", format(ccu, big.mark = ",")
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.65)
    ) %>%
      layout(
        title = "Price vs Positive Rating (bubble = review volume, color = playtime)",
        xaxis = list(title = "Price (USD)"),
        yaxis = list(title = "Positive Rating (%)"),
        coloraxis = list(colorbar = list(title = "Log Playtime"))
      )
  })

  output$timeline_plot <- renderPlotly({
    d <- filtered_data() %>%
      filter(!is.na(release_year)) %>%
      group_by(release_year) %>%
      summarise(
        count = n(),
        avg_rating = mean(positive_rate, na.rm = TRUE),
        avg_price = mean(price_usd, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(release_year)
    req(nrow(d) > 0)

    plot_ly(d, x = ~release_year) %>%
      add_bars(y = ~count, name = "Game Count", marker = list(color = "#2563eb")) %>%
      add_lines(y = ~avg_rating, name = "Avg Rating (%)", yaxis = "y2",
                line = list(color = "#f59e0b", width = 3)) %>%
      layout(
        title = "Releases per Year",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Games"),
        yaxis2 = list(
          title = "Avg Positive Rating (%)",
          overlaying = "y", side = "right",
          range = c(0, 100)
        ),
        legend = list(x = 0.05, y = 0.95)
      )
  })

  output$tag_plot <- renderPlotly({
    # Use full dataset for stability, not only filtered
    tag_stats <- explorer_data %>%
      filter(!is.na(tags)) %>%
      separate_rows(tags, sep = ",") %>%
      mutate(tag = str_trim(tags)) %>%
      filter(tag != "+", tag != "") %>%
      group_by(tag) %>%
      summarise(
        count = n(),
        avg_price = round(mean(price_usd, na.rm = TRUE), 2),
        avg_rating = round(mean(positive_rate, na.rm = TRUE), 1),
        avg_playtime = round(mean(playtime_hours, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      filter(count >= 100) %>%
      arrange(desc(count)) %>%
      slice_head(n = 40)
    req(nrow(tag_stats) > 0)

    plot_ly(
      tag_stats,
      x = ~avg_price,
      y = ~avg_rating,
      size = ~count,
      color = ~avg_rating,
      text = ~paste0(
        "<b>", tag, "</b><br>",
        "Games: ", format(count, big.mark = ","), "<br>",
        "Avg Price: $", avg_price, "<br>",
        "Avg Rating: ", avg_rating, "%<br>",
        "Avg Playtime: ", avg_playtime, " hrs"
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(
        opacity = 0.8,
        sizemode = "area",
        sizeref = 0.08,
        line = list(color = "#0f172a", width = 0.4)
      ),
      colors = colorRamp(c("#f8c660", "#34d399", "#2563eb"))
    ) %>%
      layout(
        title = "Tag Performance (size = game count, color = rating)",
        xaxis = list(title = "Average Price ($)"),
        yaxis = list(title = "Average Positive Rating (%)")
      )
  })
}

shinyApp(ui, server)
