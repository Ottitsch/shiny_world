# 🌍 Exploring 2020 World Data - Shiny App

This Shiny web application provides interactive visualizations of global socioeconomic indicators from the **CIA World Factbook 2020**. It allows users to explore and analyze a range of development-related metrics across countries and continents.

## 🚀 Live App

this app is currently deployed at [shiny world](https://ottitsch.shinyapps.io/shiny_world/)

## 📊 Features

### Univariate Analysis
- **Interactive World Map**: View a choropleth map of a selected variable, with color gradients indicating relative values.
- **Global Distributions**: Visualize the overall distribution of a variable with histogram + density plots and boxplots.
- **Continent Comparison**: Compare variable distributions across continents using grouped density plots and boxplots.
- **Raw Data Viewer**: Inspect up to 15 rows of raw data for the selected variable.

### Multivariate Analysis
- **Interactive Scatterplot**: Select two indicators to plot on the X and Y axes, and adjust point size by either population or area.
- **Color by Continent**: Countries are color-coded by continent for easy interpretation.
- **Smoothed Trends**: LOESS trend lines show patterns within continents.

## 📁 Project Structure

```
📦 your-shiny-app/
├── app.R           # Main Shiny app file
├── data.json       # Data source: CIA World Factbook (2020)
├── README.md       # Project overview and instructions
```
## 📎 Requirements

Ensure the following R packages are installed:

```r
install.packages(c(
  "shiny", "jsonlite", "dplyr", "ggplot2", "plotly",
  "DT", "viridis", "maps", "countrycode"
))
```

## 📂 Data

The dataset (`data.json`) contains the following metrics for most world countries:
- Education expenditure (% of GDP)
- Youth unemployment rate (%)
- Net migration rate (per 1,000)
- Electricity from fossil fuels (%)
- Area (km²)
- Population growth rate (%)
- Life expectancy at birth (years)
- Population
- Region and subregion information




