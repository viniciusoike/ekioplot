> EKIO Visual Identity System for R Data Visualization - Enhanced Edition

[![R-CMD-check](https://github.com/ekio/ekioplot/workflows/R-CMD-check/badge.svg)](https://github.com/ekio/ekioplot/actions)

## Overview

`ekioplot` is a comprehensive R package that implements EKIO's visual identity system, making it effortless for analysts and economists to create publication-ready charts following EKIO's design principles:

- **Minimalist Clarity**: Every element serves a purpose
- **Purposeful Color**: Strategic use of color to highlight and guide  
- **Professional Standards**: Consistent typography and spacing
- **Economic Focus**: Optimized for business and economic analysis
- **Enhanced Typography**: Smart font management with Google Fonts integration
- **Extensive Color System**: 35+ palettes including scientific and mapping options
- **Custom Color Indexing**: Advanced palette customization for precise control

## Installation

```r
# Install dependencies
install.packages(c("ggplot2", "scales", "extrafont", "showtext"))

# Install from GitHub
devtools::install_github("ekio/ekioplot")

# Load fonts (one-time setup)
library(ekioplot)
load_ekio_fonts(install_missing = TRUE)
```

## Quick Start

```r
library(ekioplot)
library(ggplot2)

# Basic usage with enhanced features
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_ekio_d("categorical") +
  theme_ekio("modern_premium") +
  labs(
    title = "Vehicle Fuel Efficiency Analysis", 
    subtitle = "Weight vs MPG by engine configuration",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  )
```

## Enhanced Features

### 🎨 Extensive Color System

**35+ Professional Palettes:**
- **6 EKIO Sequential**: Modern Premium, Academic Authority, Sophisticated Unique, etc.
- **6 Categorical Variants**: Including warm, cool, mixed, and muted options
- **6 Scientific Palettes**: Viridis, Inferno, Plasma, Hokusai1/2, Okabe-Ito
- **6 Mapping Palettes**: RdBu, BrBG, PuBuGn, YlOrRd, Greens, Blues
- **3 Custom EKIO**: Midnight Steel, Midnight Warm, Asphalt Gradient

```r
# Explore all available palettes
list_ekio_palettes("all")
list_ekio_palettes("scientific")

# Visual palette exploration
show_ekio_palette("hokusai1")
show_ekio_palette("categorical_warm", show_names = TRUE)
```

### 🔤 Smart Font Management

Automatic font detection with Google Fonts integration:

```r
# Load EKIO fonts with Google Fonts fallback
load_ekio_fonts(install_missing = TRUE)

# Import specific fonts
import_lato()
import_fira_code()
import_roboto_condensed()

# Check font availability
check_ekio_fonts(detailed = TRUE)
```

### 🎯 Custom Color Indexing

Advanced palette customization for non-obvious color orders:

```r
# Use specific colors from sequential palette
ggplot(data, aes(x = var1, y = var2, color = category)) +
  geom_point() +
  scale_color_ekio_d("modern_premium", indices = c(9, 7, 5, 4)) +
  theme_ekio()

# Perfect for when you need: color_pal[c(9, 7, 5, 4)]
```

## Strategic Color Selection

### EKIO Themes for Different Contexts

- **Modern Premium** (Primary): 80% of use cases, standard business presentations
- **Academic Authority**: Government reports, white papers, institutional analysis
- **Sophisticated Unique**: International clients, environmental studies  
- **Institutional Oxford**: Banking, traditional corporate environments
- **Professional Deep**: Technical documentation, serious economic analysis
- **Premium Steel**: Executive presentations, high-stakes proposals

### Scientific & Mapping Palettes

```r
# For choropleth maps
ggplot(brazil_data, aes(fill = gdp_index)) +
  geom_sf() +
  scale_fill_ekio_c("RdBu") +
  theme_ekio("academic_authority")

# For scientific visualization  
ggplot(data, aes(x, y, color = temperature)) +
  geom_point() +
  scale_color_ekio_c("viridis") +
  theme_ekio("institutional_oxford")
```

## Advanced Typography

Font hierarchy optimized for economic analysis:

- **Primary**: Avenir (macOS default) → Lato (Google) → Helvetica Neue
- **Monospace**: Fira Code (Google) → Monaco → Courier
- **Condensed**: Roboto Condensed (Google) → Arial Narrow
- **Automatic Detection**: Works across macOS, Windows, Linux

---

*EKIO Analytics - Transforming Data into Professional Insights*
