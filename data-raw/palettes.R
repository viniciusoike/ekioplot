# Build R/sysdata.rda from inst/ekio-palettes.yaml.
#
# inst/ekio-palettes.yaml is the single source of truth for EKIO brand color.
# Edit that file, then run this script. tests/testthat/test-palette-data.R
# guards against the built object drifting from the YAML.

library(yaml)

yaml_path <- "inst/ekio-palettes.yaml"
spec <- yaml::read_yaml(yaml_path)

shades <- as.character(seq(100, 900, by = 100))

# ---- Scales ----

.ekio_scales <- lapply(spec$scales, function(s) {
  v <- unlist(s)
  names(v) <- as.character(names(s))
  v[shades]
})

# ---- Validation ----

stopifnot(
  "every scale must have exactly 9 shades" =
    all(lengths(.ekio_scales) == 9),
  "every scale must be named 100..900" =
    all(vapply(.ekio_scales, function(x) identical(names(x), shades), logical(1))),
  "every scale value must be a 6-digit hex code" =
    all(grepl("^#[0-9A-Fa-f]{6}$", unlist(.ekio_scales)))
)

# Ramps must run light to dark without reversing.
lum <- function(x) {
  rgb <- grDevices::col2rgb(x)
  as.numeric(0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ])
}
for (nm in names(.ekio_scales)) {
  if (any(diff(lum(.ekio_scales[[nm]])) >= 0)) {
    stop("scale '", nm, "' is not monotonically darkening", call. = FALSE)
  }
}

# ---- Token resolution ----

resolve <- function(x) {
  vapply(
    x,
    function(tok) {
      if (startsWith(tok, "#")) {
        return(toupper(tok))
      }
      parts <- strsplit(tok, ".", fixed = TRUE)[[1]]
      if (length(parts) != 2 || !parts[1] %in% names(.ekio_scales)) {
        stop("unknown token '", tok, "'", call. = FALSE)
      }
      hex <- .ekio_scales[[parts[1]]][[parts[2]]]
      if (is.null(hex)) stop("unknown shade in token '", tok, "'", call. = FALSE)
      hex
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.ekio_palettes <- lapply(names(spec$palettes), function(group) {
  lapply(spec$palettes[[group]], function(pal) {
    out <- resolve(pal)
    # Sequential palettes are the scales themselves, so they keep shade names;
    # everything else is an ordered set where position carries no shade meaning.
    if (group == "sequential") names(out) <- shades
    out
  })
})
names(.ekio_palettes) <- names(spec$palettes)

# Sequential palettes must match their scale exactly.
for (nm in names(.ekio_palettes$sequential)) {
  stopifnot(identical(
    unname(.ekio_palettes$sequential[[nm]]),
    unname(.ekio_scales[[nm]])
  ))
}

# A diverging palette's neutral pivot must be its lightest color, so the
# visual center of the scale lands on the data's zero rather than one slot
# off it.
for (nm in names(.ekio_palettes$diverging)) {
  pal <- .ekio_palettes$diverging[[nm]]
  mid <- (length(pal) + 1) / 2
  if (mid %% 1 != 0) {
    stop("diverging palette '", nm, "' must have an odd length", call. = FALSE)
  }
  l <- lum(pal)
  if (l[mid] != max(l)) {
    stop(
      "diverging palette '", nm, "': pivot is not the lightest color ",
      "(pivot ", round(l[mid]), ", lightest ", round(max(l)),
      " at position ", which.max(l), ")",
      call. = FALSE
    )
  }
}

usethis::use_data(.ekio_scales, .ekio_palettes, internal = TRUE, overwrite = TRUE)
