library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(viridis)        # for scale_fill_viridis_c()
library(maps)           # for map_data()
library(countrycode)    # to get ISO3 codes

# Load CIA Factbook data from JSON
cia <- fromJSON("data.json")

# Prepare world map data and join with CIA data via ISO3 codes
world_map <- map_data("world") %>% 
  mutate(ISO3 = countrycode(region, "country.name", "iso3c", nomatch = NA))
map_df <- left_join(world_map, cia, by = "ISO3")

# Named vectors for user-friendly selectInput labels
var_choices <- c(
  "Education expend. (% GDP)"      = "expenditure",
  "Youth unemployment rate (%)"    = "youth_unempl_rate",
  "Net migration rate (per 1,000)" = "net_migr_rate",
  "Population growth rate (%)"     = "pop_growth_rate",
  "Electricity from fossil fuels (%)" = "electricity_fossil_fuel",
  "Life expectancy at birth (years)"  = "life_expectancy"
)
size_choices <- c(
  "Population" = "population",
  "Area (km²)" = "area"
)

# Define UI
ui <- fluidPage(
  titlePanel("Exploring 2020 World Data"),
  p("A Shiny app for univariate and multivariate exploration of CIA Factbook indicators."),

  tabsetPanel(
    # Univariate analysis tab
    tabPanel("Univariate analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("uni_var", "Select variable:", choices = var_choices),
          actionButton("view_raw", "View raw data"),
          DTOutput("raw_tbl")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Map", plotlyOutput("uni_map")),
            tabPanel("Global analysis",
              plotlyOutput("uni_box"),
              plotlyOutput("uni_hist"),
              plotlyOutput("uni_dens")
            ),
            tabPanel("By continent",
              plotlyOutput("uni_box_cont"),
              plotlyOutput("uni_dens_cont")
            )
          )
        )
      )
    ),

    # Multivariate analysis tab
    tabPanel("Multivariate analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("x_var", "X axis:", choices = var_choices),
          selectInput("y_var", "Y axis:", choices = var_choices),
          selectInput("size_var", "Point size by:", choices = size_choices)
        ),
        mainPanel(
          plotlyOutput("multi_scatter")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive expression for selected univariate variable
  uni_col <- reactive({ input$uni_var })

  # Render raw data table on button click (first 15 rows)
  observeEvent(input$view_raw, {
    df <- cia %>%
      select(country, continent, value = !!uni_col()) %>%
      rename(!!names(var_choices)[var_choices == uni_col()] := value)
    output$raw_tbl <- renderDT(head(df, 15), options = list(pageLength = 15))
  })

  # Univariate choropleth map
  output$uni_map <- renderPlotly({
    p <- ggplot(map_df, aes(long, lat, group = group, fill = .data[[uni_col()]])) +
      geom_polygon(colour = "white") +
      scale_fill_viridis_c(name = names(var_choices)[var_choices == uni_col()]) +
      theme_void()
    ggplotly(p, tooltip = c("region", uni_col()))
  })

  # Global univariate boxplot
  output$uni_box <- renderPlotly({
    p <- ggplot(cia, aes(y = .data[[uni_col()]])) +
      geom_boxplot() +
      labs(y = names(var_choices)[var_choices == uni_col()])
    ggplotly(p)
  })

  # Global univariate histogram
  output$uni_hist <- renderPlotly({
    p <- ggplot(cia, aes(.data[[uni_col()]])) +
      geom_histogram() +
      labs(x = names(var_choices)[var_choices == uni_col()])
    ggplotly(p)
  })

  # Global univariate density plot
  output$uni_dens <- renderPlotly({
    p <- ggplot(cia, aes(.data[[uni_col()]])) +
      geom_density() +
      labs(x = names(var_choices)[var_choices == uni_col()])
    ggplotly(p)
  })

  # Univariate analyses by continent: boxplot
  output$uni_box_cont <- renderPlotly({
    p <- ggplot(cia, aes(x = continent, y = .data[[uni_col()]], fill = continent)) +
      geom_boxplot() +
      labs(y = names(var_choices)[var_choices == uni_col()])
    ggplotly(p)
  })

  # Univariate analyses by continent: density
  output$uni_dens_cont <- renderPlotly({
    p <- ggplot(cia, aes(.data[[uni_col()]], color = continent)) +
      geom_density() +
      labs(x = names(var_choices)[var_choices == uni_col()])
    ggplotly(p)
  })

  # Multivariate scatterplot with LOESS smoothing
  output$multi_scatter <- renderPlotly({
    xcol <- input$x_var; ycol <- input$y_var; sz   <- input$size_var
    px <- names(var_choices)[var_choices == xcol]
    py <- names(var_choices)[var_choices == ycol]
    ps <- names(size_choices)[size_choices == sz]

    p <- ggplot(cia, aes(x = .data[[xcol]], y = .data[[ycol]],
                         size = .data[[sz]], color = continent)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE,
                  aes(x = .data[[xcol]], y = .data[[ycol]]),
                  inherit.aes = FALSE) +
      labs(x = px, y = py, size = ps) +
      theme_minimal()
    ggplotly(p)
  })

}

# Run the application
shinyApp(ui, server)
