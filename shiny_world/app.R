library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(viridis)
library(maps)
library(countrycode)

# Load CIA Factbook data
cia <- fromJSON("data.json")

# Prepare map dataframe joined with CIA data
world_map <- map_data("world") %>%
  mutate(ISO3 = countrycode(region, "country.name", "iso3c", nomatch = NA))
map_df <- left_join(world_map, cia, by = "ISO3")

# Named vectors for selectInput labels
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

# UI definition
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
            tabPanel("Map", plotlyOutput("uni_map", width = "90%", height = "60vh")),
            tabPanel("Global analysis",
              fluidRow(
                # Histogram + density on left
                column(6, plotlyOutput("uni_hist_den")),
                # Boxplot on right
                column(6, plotlyOutput("uni_box"))
              )
            ),
            tabPanel("Analysis per continent",
              fluidRow(
                # Swap: density left, boxplot right
                column(6, plotlyOutput("uni_dens_cont")),
                column(6, plotlyOutput("uni_box_cont"))
              )
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

# Server logic
server <- function(input, output, session) {
  uni_col <- reactive({ input$uni_var })

  # Render raw data table
  observeEvent(input$view_raw, {
    df <- cia %>%
      select(country, continent, value = .data[[uni_col()]]) %>%
      rename(!!names(var_choices)[var_choices == uni_col()] := value)
    output$raw_tbl <- renderDT(head(df, 15), options = list(pageLength = 15))
  })

  # Choropleth map
  output$uni_map <- renderPlotly({
    p <- ggplot(map_df, aes(long, lat, group = group, fill = .data[[uni_col()]])) +
      geom_polygon(colour = "white") +
      scale_fill_viridis_c(name = names(var_choices)[var_choices == uni_col()]) +
      coord_quickmap(
        xlim = c(-10, 10),
        ylim = c(-10, 15)
      ) +
      coord_fixed(ratio = 1.3) +
      labs(
        x = "long",
        y = "lat",
        fill = names(var_choices)[var_choices == uni_col()]
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill="white", colour=NA),
        panel.border     = element_rect(colour="black", fill=NA, size=1),
        panel.grid.major = element_line(color="grey80", size=0.3),
        axis.text        = element_text(color="grey20")
      )
    ggplotly(p, tooltip = c("region", uni_col()))
  })

  # Global boxplot (right side)
  output$uni_box <- renderPlotly({
    p <- ggplot(cia, aes(y = .data[[uni_col()]])) +
      geom_boxplot(fill = "white", alpha = 0.6, color = "black") +
      labs(y = names(var_choices)[var_choices == uni_col()]) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "grey90", color = NA))
    ggplotly(p)
  })

  # Global histogram + density (left side)
  output$uni_hist_den <- renderPlotly({
    p <- ggplot(cia, aes(x = .data[[uni_col()]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "grey80", color = "grey70") +
      geom_density(fill = "steelblue", alpha = 0.4) +
      labs(x = names(var_choices)[var_choices == uni_col()], y = "Density") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "grey90", color = NA))
    ggplotly(p)
  })

  # Continent boxplot (right side)
  output$uni_box_cont <- renderPlotly({
    p <- ggplot(cia, aes(x = continent, y = .data[[uni_col()]], fill = continent)) +
      geom_boxplot() +
      labs(x = NULL, y = names(var_choices)[var_choices == uni_col()]) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "grey90", color = NA))
    ggplotly(p)
  })

  # Continent density plots (left side)
  output$uni_dens_cont <- renderPlotly({
    p <- ggplot(cia, aes(x = .data[[uni_col()]], color = continent, fill = continent)) +
      geom_density(alpha = 0.4) +
      labs(x = names(var_choices)[var_choices == uni_col()], y = "Density") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "grey90", color = NA))
    ggplotly(p)
  })

  # Multivariate scatter with LOESS
  output$multi_scatter <- renderPlotly({
    xcol <- input$x_var; ycol <- input$y_var; sz <- input$size_var
    p <- ggplot(cia, aes(x = .data[[xcol]], y = .data[[ycol]], size = .data[[sz]], color = continent)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE, color = "black", inherit.aes = FALSE,
                  aes(x = .data[[xcol]], y = .data[[ycol]])) +
      labs(x = names(var_choices)[var_choices==xcol],
           y = names(var_choices)[var_choices==ycol],
           size = names(size_choices)[size_choices==sz]) +
      theme_minimal()
    ggplotly(p)
  })
}

# Launch app
shinyApp(ui, server)