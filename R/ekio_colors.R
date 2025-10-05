# Enhanced Color System for EKIO Package
# Includes mapping palettes, custom palettes, and extended options

#' Enhanced EKIO Color System
#'
#' Extended version of ekio_colors with mapping palettes, scientific palettes,
#' and custom EKIO combinations
#'
#' @param palette Character string specifying which palette to return
#' @return Named vector of colors
#' @importFrom stats setNames
#' @examples
#' # EKIO palettes
#' ekio_colors("modern_premium")
#'
#' # Mapping palettes
#' ekio_colors("RdBu")
#' ekio_colors("viridis")
#'
#' # Scientific palettes
#' ekio_colors("hokusai1")
#'
#' # Custom EKIO combinations
#' ekio_colors("midnight_steel")
#'
#' @export
ekio_colors <- function(palette = "all") {
  # Core EKIO colors (enhanced)
  core_colors <- c(
    # EKIO Brand Colors
    "midnight_blue" = "#2c3e50",
    "wet_asphalt" = "#34495e",
    "asbestos" = "#7f8c8d",
    "concrete" = "#95a5a6",
    "clouds" = "#ecf0f1",
    "peter_river" = "#3498db",
    "belize_hole" = "#2980b9",
    "turquoise" = "#1abc9c",
    "green_sea" = "#16a085",
    "orange" = "#f39c12",
    "carrot" = "#e67e22",
    "alizarin" = "#e74c3c",
    "pomegranate" = "#c0392b",
    "amethyst" = "#9b59b6",
    "wisteria" = "#8e44ad",

    # Basic colors
    "black" = "#000000",
    "white" = "#ffffff",
    "offwhite" = "#fefefe"
  )

  # EKIO Sequential Palettes (from documentation)
  modern_premium <- c(
    "#f4f7fb",
    "#e3ecf5",
    "#d2e1ef",
    "#a0bfdb",
    "#6d9dc7",
    "#2C6BB3",
    "#265c9a",
    "#204d81",
    "#1a3e68"
  )

  academic_authority <- c(
    "#f2f4f7",
    "#e0e6ed",
    "#ced8e3",
    "#9fb4cc",
    "#7090b5",
    "#0F3A65",
    "#0d3256",
    "#0b2a47",
    "#092238"
  )

  sophisticated_unique <- c(
    "#f6fafa",
    "#e8f4f4",
    "#d9efef",
    "#b8dee0",
    "#96ced1",
    "#5F9EA0",
    "#528a8c",
    "#467678",
    "#396264"
  )

  institutional_oxford <- c(
    "#f3f6fa",
    "#e2eaf4",
    "#d1deee",
    "#9bbdd9",
    "#659cc4",
    "#1E5F9F",
    "#1a5289",
    "#164573",
    "#12385d"
  )

  professional_deep <- c(
    "#f2f4f7",
    "#e0e6ed",
    "#ced8e3",
    "#9fb4cc",
    "#7090b5",
    "#0F3A65",
    "#0d3256",
    "#0b2a47",
    "#092238"
  )

  premium_steel <- c(
    "#f4f7fa",
    "#e1eaf2",
    "#cddde9",
    "#a5c4d4",
    "#7da9c4",
    "#4682B4",
    "#3a6fa0",
    "#2f5c8c",
    "#244978"
  )

  # Custom EKIO Combinations using midnight blue + wet asphalt
  midnight_steel <- c(
    "#f4f7fa",
    "#e8eef3",
    "#d3dee8",
    "#a8bdd2",
    "#7d9cbd",
    "#2c3e50",
    "#34495e",
    "#273445",
    "#1a252f"
  )

  midnight_warm <- c(
    "#faf7f4",
    "#f3ede8",
    "#e8ddd3",
    "#d2bda8",
    "#bd9d7d",
    "#2c3e50",
    "#4a3728",
    "#3d2d1f",
    "#2f2315"
  )

  asphalt_gradient <- c(
    "#f8f9fa",
    "#ecf0f1",
    "#d5dbdb",
    "#95a5a6",
    "#7f8c8d",
    "#34495e",
    "#2c3e50",
    "#242f3d",
    "#1c252f"
  )

  # RColorBrewer-inspired palettes
  RdBu <- c(
    "#67001f",
    "#b2182b",
    "#d6604d",
    "#f4a582",
    "#fddbc7",
    "#f7f7f7",
    "#d1e5f0",
    "#92c5de",
    "#4393c3",
    "#2166ac",
    "#053061"
  )

  BrBG <- c(
    "#543005",
    "#8c510a",
    "#bf812d",
    "#dfc27d",
    "#f6e8c3",
    "#f5f5f5",
    "#c7eae5",
    "#80cdc1",
    "#35978f",
    "#01665e",
    "#003c30"
  )

  PuBuGn <- c(
    "#fff7fb",
    "#ece2f0",
    "#d0d1e6",
    "#a6bddb",
    "#67a9cf",
    "#3690c0",
    "#02818a",
    "#016c59",
    "#014636"
  )

  YlOrRd <- c(
    "#ffffcc",
    "#ffeda0",
    "#fed976",
    "#feb24c",
    "#fd8d3c",
    "#fc4e2a",
    "#e31a1c",
    "#bd0026",
    "#800026"
  )

  Greens <- c(
    "#f7fcf5",
    "#e5f5e0",
    "#c7e9c0",
    "#a1d99b",
    "#74c476",
    "#41ab5d",
    "#238b45",
    "#006d2c",
    "#00441b"
  )

  Blues <- c(
    "#f7fbff",
    "#deebf7",
    "#c6dbef",
    "#9ecae1",
    "#6baed6",
    "#4292c6",
    "#2171b5",
    "#08519c",
    "#08306b"
  )

  # Viridis palettes
  viridis <- c(
    "#440154",
    "#482677",
    "#3f4a8a",
    "#31678e",
    "#26838f",
    "#1f9d8a",
    "#6cce5a",
    "#b6de2b",
    "#fee825"
  )

  inferno <- c(
    "#000004",
    "#1b0c41",
    "#4a0c6b",
    "#781c6d",
    "#a52c60",
    "#cf4446",
    "#ed6925",
    "#fb9b06",
    "#f7d03c",
    "#fcffa4"
  )

  plasma <- c(
    "#0d0887",
    "#46039f",
    "#7201a8",
    "#9c179e",
    "#bd3786",
    "#d8576b",
    "#ed7953",
    "#fb9f3a",
    "#fdca26",
    "#f0f921"
  )

  # Scientific palettes from MetBrewer (Hokusai-inspired)
  hokusai1 <- c(
    "#2d70b4",
    "#388bc0",
    "#5ba3c7",
    "#7db8cd",
    "#9ecbd2",
    "#bfdbd7",
    "#dde9dc",
    "#f4f4e1",
    "#f1e7b3",
    "#edd985",
    "#e9cb57",
    "#e5bd29"
  )

  hokusai2 <- c(
    "#abc9c8",
    "#72aeb6",
    "#4692b0",
    "#2f70a1",
    "#134b73",
    "#0a3351",
    "#032539",
    "#1d1f2e",
    "#302f36",
    "#47433d",
    "#5e5445",
    "#756548"
  )

  # Okabe-Ito colorblind-friendly palette
  okabe_ito <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#000000"
  )

  # Categorical palettes for discrete data (enhanced)
  categorical_main <- c(
    "#2C6BB3",
    "#1abc9c",
    "#f39c12",
    "#e74c3c",
    "#9b59b6",
    "#95a5a6",
    "#34495e"
  )

  categorical_extended <- c(
    "#2c3e50",
    "#3498db",
    "#1abc9c",
    "#f39c12",
    "#e74c3c",
    "#9b59b6",
    "#95a5a6",
    "#16a085"
  )

  # Creative categorical combinations
  categorical_warm <- c(
    "#e74c3c",
    "#f39c12",
    "#e67e22",
    "#d35400",
    "#c0392b",
    "#a93226",
    "#922b21"
  )

  categorical_cool <- c(
    "#2C6BB3",
    "#3498db",
    "#1abc9c",
    "#16a085",
    "#2980b9",
    "#2471a3",
    "#1f618d"
  )

  categorical_mixed <- c(
    "#2c3e50",
    "#e74c3c",
    "#f39c12",
    "#1abc9c",
    "#9b59b6",
    "#3498db",
    "#95a5a6",
    "#34495e",
    "#e67e22",
    "#16a085"
  )

  categorical_muted <- c(
    "#7f8c8d",
    "#95a5a6",
    "#bdc3c7",
    "#d5dbdb",
    "#ecf0f1",
    "#34495e",
    "#2c3e50"
  )

  # ---- Additional categorical palettes ----

  cool_spectrum <- c(
    "#003f5c",
    "#2f4b7c",
    "#665191",
    "#a05195",
    "#d45087",
    "#f95d6a"
  )

  muted_elegant <- c(
    "#264653",
    "#2A9D8F",
    "#E76F51",
    "#E9C46A"
  )

  vibrant_professional <- c(
    "#E63946",
    "#F77F00",
    "#06AED5",
    "#073B4C",
    "#118AB2"
  )

  academic_classic <- c(
    "#0173B2",
    "#DE8F05",
    "#029E73",
    "#CC78BC",
    "#CA9161",
    "#949494"
  )

  earth_tones <- c(
    "#8B4513",
    "#CD853F",
    "#556B2F",
    "#BC8F8F",
    "#2F4F4F"
  )

  tech_modern <- c(
    "#0066CC",
    "#00C896",
    "#FF6B35",
    "#5B2C6F",
    "#FFD23F",
    "#1A1A1A"
  )

  nordic_minimal <- c(
    "#4C72B0",
    "#55A868",
    "#C44E52",
    "#8172B2"
  )

  corporate_standard <- c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd"
  )

  # Accent palettes (1-2 strong + 3-5 muted colors)
  accent_blue <- c(
    "#2C6BB3",  # Strong: modern premium blue
    "#3498db",  # Strong: peter river
    "#ecf4f9",  # Muted: very light blue
    "#d1e5f0",  # Muted: light blue
    "#a8c8e1",  # Muted: medium light blue
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  accent_teal <- c(
    "#1abc9c",  # Strong: turquoise
    "#16a085",  # Strong: green sea
    "#e8f6f3",  # Muted: very light teal
    "#c7eae5",  # Muted: light teal
    "#a1d9d0",  # Muted: medium light teal
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  accent_orange <- c(
    "#f39c12",  # Strong: orange
    "#e67e22",  # Strong: carrot
    "#fdf2e7",  # Muted: very light orange
    "#f7e6d1",  # Muted: light orange
    "#f0d0a0",  # Muted: medium light orange
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  accent_red <- c(
    "#e74c3c",  # Strong: alizarin
    "#c0392b",  # Strong: pomegranate
    "#fbeaea",  # Muted: very light red
    "#f5c6cb",  # Muted: light red
    "#e8999f",  # Muted: medium light red
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  accent_purple <- c(
    "#9b59b6",  # Strong: amethyst
    "#8e44ad",  # Strong: wisteria
    "#f4f1f6",  # Muted: very light purple
    "#e8d5f0",  # Muted: light purple
    "#d1ade0",  # Muted: medium light purple
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  # Alternative red tones (warmer/cooler variants)
  accent_red_warm <- c(
    "#d35400",  # Strong: warm orange-red
    "#e67e22",  # Strong: carrot (warm)
    "#fdeee6",  # Muted: very light warm red
    "#f8d3b0",  # Muted: light warm red
    "#f2a365",  # Muted: medium warm red
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  accent_red_cool <- c(
    "#a93226",  # Strong: cool deep red
    "#922b21",  # Strong: darker cool red
    "#f7e6e6",  # Muted: very light cool red
    "#eab3b3",  # Muted: light cool red
    "#d17a7a",  # Muted: medium cool red
    "#7f8c8d",  # Muted: neutral gray
    "#95a5a6"   # Muted: concrete
  )
  
  # Red to Blue diverging palette (alternative to RdBu)
  red_blue_diverging <- c(
    "#a93226",  # Deep red (strong)
    "#c0392b",  # Red (strong)
    "#e74c3c",  # Light red
    "#f5b7b1",  # Very light red
    "#ecf0f1",  # Neutral center
    "#aed6f1",  # Very light blue
    "#5dade2",  # Light blue
    "#2980b9",  # Blue (strong)
    "#1f618d"   # Deep blue (strong)
  )

  # Neutrals (enhanced)
  neutrals <- c(
    "white" = "#ffffff",
    "offwhite" = "#fefefe",
    "light_gray" = "#f8f9fa",
    "silver" = "#ecf0f1",
    "silver_sand" = "#bdc3c7",
    "gray" = "#7f8c8d",
    "dark_gray" = "#2c3e50",
    "black" = "#000000"
  )

  # Return based on palette selection
  result <- switch(
    palette,
    # Core EKIO
    "all" = core_colors,
    "primary" = core_colors[1:5],
    "core" = core_colors,

    # EKIO Sequential
    "modern_premium" = setNames(modern_premium, paste0("mp_", 1:9)),
    "academic_authority" = setNames(academic_authority, paste0("aa_", 1:9)),
    "sophisticated_unique" = setNames(sophisticated_unique, paste0("su_", 1:9)),
    "institutional_oxford" = setNames(institutional_oxford, paste0("io_", 1:9)),
    "professional_deep" = setNames(professional_deep, paste0("pd_", 1:9)),
    "premium_steel" = setNames(premium_steel, paste0("ps_", 1:9)),

    # Custom EKIO combinations
    "midnight_steel" = setNames(midnight_steel, paste0("ms_", 1:9)),
    "midnight_warm" = setNames(midnight_warm, paste0("mw_", 1:9)),
    "asphalt_gradient" = setNames(asphalt_gradient, paste0("ag_", 1:9)),

    # Categorical
    "categorical" = categorical_main,
    "categorical_extended" = categorical_extended,
    "categorical_warm" = categorical_warm,
    "categorical_cool" = categorical_cool,
    "categorical_mixed" = categorical_mixed,
    "categorical_muted" = categorical_muted,
    "cool_spectrum" = setNames(cool_spectrum, paste0("cs_", 1:6)),
    "muted_elegant" = setNames(muted_elegant, paste0("me_", 1:4)),
    "vibrant_professional" = setNames(vibrant_professional, paste0("vp_", 1:5)),
    "academic_classic" = setNames(academic_classic, paste0("ac_", 1:6)),
    "earth_tones" = setNames(earth_tones, paste0("et_", 1:5)),
    "tech_modern" = setNames(tech_modern, paste0("tm_", 1:6)),
    "nordic_minimal" = setNames(nordic_minimal, paste0("nm_", 1:4)),
    "corporate_standard" = setNames(corporate_standard, paste0("cst_", 1:5)),

    # RColorBrewer
    "RdBu" = setNames(RdBu, paste0("RdBu_", 1:11)),
    "BrBG" = setNames(BrBG, paste0("BrBG_", 1:11)),
    "PuBuGn" = setNames(PuBuGn, paste0("PuBuGn_", 1:9)),
    "YlOrRd" = setNames(YlOrRd, paste0("YlOrRd_", 1:9)),
    "Greens" = setNames(Greens, paste0("Greens_", 1:9)),
    "Blues" = setNames(Blues, paste0("Blues_", 1:9)),

    # Viridis
    "viridis" = setNames(viridis, paste0("viridis_", 1:9)),
    "inferno" = setNames(inferno, paste0("inferno_", 1:10)),
    "plasma" = setNames(plasma, paste0("plasma_", 1:10)),

    # Scientific
    "hokusai1" = setNames(hokusai1, paste0("hok1_", 1:12)),
    "hokusai2" = setNames(hokusai2, paste0("hok2_", 1:12)),
    "okabe_ito" = setNames(okabe_ito, paste0("oi_", 1:8)),

    # Accent palettes
    "accent_blue" = setNames(accent_blue, paste0("ab_", 1:7)),
    "accent_teal" = setNames(accent_teal, paste0("at_", 1:7)),
    "accent_orange" = setNames(accent_orange, paste0("ao_", 1:7)),
    "accent_red" = setNames(accent_red, paste0("ar_", 1:7)),
    "accent_red_warm" = setNames(accent_red_warm, paste0("arw_", 1:7)),
    "accent_red_cool" = setNames(accent_red_cool, paste0("arc_", 1:7)),
    "accent_purple" = setNames(accent_purple, paste0("ap_", 1:7)),
    
    # Diverging palettes
    "red_blue_diverging" = setNames(red_blue_diverging, paste0("rbd_", 1:9)),

    # Neutrals
    "neutrals" = neutrals,
    "extended" = c(core_colors, neutrals),

    # Default case - return NULL to trigger error
    NULL
  )

  # Check if palette was found
  if (is.null(result)) {
    stop(
      "Palette '",
      palette,
      "' not found. Use list_ekio_palettes() to see available options.",
      call. = FALSE
    )
  }

  return(result)
}
