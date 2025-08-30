# EKIO Plot

[![R-CMD-check](https://github.com/viniciusoike/ekioplot/workflows/R-CMD-check/badge.svg)](https://github.com/viniciusoike/ekioplot/actions)

## Overview

`ekioplot` is a comprehensive R package implementing EKIO's visual identity system for data visualization. The package provides themes, color palettes, and font management to create professional charts following EKIO design principles.

### Key Features

- **Professional Themes**: Six distinct theme variations optimized for different contexts
- **Smart Font Management**: Automatic font detection with safe fallbacks
- **Comprehensive Color System**: 35+ curated palettes for various chart types
- **Ready-to-Use Charts**: Pre-built functions for common visualization patterns
- **GT Table Themes**: Professional table formatting for reports

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

# Basic usage
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_color_ekio_d("modern_premium") +
  theme_ekio() +
  labs(
    title = "Vehicle Fuel Efficiency Analysis",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  )
```

## Theme Styles

The package includes six professional theme variations:

```r
# Available theme styles
theme_ekio("modern_premium")      # Default blue theme
theme_ekio("academic_authority")  # Deep blue for academic work
theme_ekio("sophisticated_unique") # Teal for distinctive presentations
theme_ekio("institutional_oxford") # Oxford blue for formal reports  
theme_ekio("professional_deep")   # Deep blue for business contexts
theme_ekio("premium_steel")       # Steel blue for premium feel
```

## Color Palettes

```r
# Explore available palettes
list_ekio_palettes("all")
show_ekio_palette("modern_premium")

# Apply palettes
scale_color_ekio_d("categorical")
scale_fill_ekio_c("sequential_blue")
```

## Font Management

Smart font detection with automatic fallbacks:

```r
# Load EKIO fonts (optional)
load_ekio_fonts()

# Check available fonts
check_ekio_fonts()
```

---

*EKIO*
