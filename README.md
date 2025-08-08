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
# Install from GitHub
devtools::install_github("viniciusoike/ekioplot")

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

---

*EKIO Analytics - Transforming Data into Professional Insights*
