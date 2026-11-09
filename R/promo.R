#' Configure a package announcement carousel
#'
#' @param package,version Package name and version, displayed on every page.
#' @param logo Optional path to a PNG or JPEG logo.
#' @param accent,ink Hex colours for accents and text.
#' @param font,mono_font Installed font family names for prose and code.
#' @param docs_url Optional HTTPS documentation URL.
#' @param install Installation code; defaults to `install.packages(package)`.
#' @return A brand configuration for [render_promo()].
#' @export
promo_brand <- function(
  package,
  version,
  logo = NULL,
  accent = "#00605E",
  ink = "#152A44",
  font = "Lato",
  mono_font = "DejaVu Sans Mono",
  docs_url = NULL,
  install = sprintf('install.packages("%s")', package)
) {
  for (arg in c(
    "package",
    "version",
    "accent",
    "ink",
    "font",
    "mono_font",
    "install"
  )) {
    promo_text(get(arg), arg)
  }
  for (arg in c("accent", "ink")) {
    if (!grepl("^#[0-9a-fA-F]{6}$", get(arg))) {
      cli::cli_abort("{.arg {arg}} must be a six-digit hex colour.")
    }
  }
  for (arg in c("font", "mono_font")) {
    if (!grepl("^[[:alnum:] -]+$", get(arg))) {
      cli::cli_abort("{.arg {arg}} must be a font family name.")
    }
  }
  promo_image_path(logo, "logo")
  promo_text(docs_url, "docs_url", optional = TRUE)
  if (!is.null(docs_url) && !grepl("^https://[^[:space:]]+$", docs_url)) {
    cli::cli_abort("{.arg docs_url} must be an HTTPS URL.")
  }
  return(structure(
    list(
      package = package,
      version = version,
      logo = logo,
      accent = accent,
      ink = ink,
      font = font,
      mono_font = mono_font,
      docs_url = docs_url,
      install = install
    ),
    class = "ekio_promo_brand"
  ))
}

#' Construct announcement cards
#'
#' @param title Short sentence case title.
#' @param subtitle Optional short description (required on a cover).
#' @param chart Optional PNG or JPEG chart path.
#' @param alt Alternative text, required with a chart.
#' @param code,output Optional literal code and output strings. Text is escaped.
#' @param note Optional short note.
#' @param badge Optional short feature label, displayed above the title.
#' @return An `ekio_promo_card` for [render_promo()]. Absent sections are omitted.
#' @export
promo_cover <- function(title, subtitle) {
  promo_text(subtitle, "subtitle")
  return(promo_card("cover", title, subtitle = subtitle))
}

#' @rdname promo_cover
#' @export
promo_feature <- function(
  title,
  subtitle = NULL,
  chart = NULL,
  alt = NULL,
  code = NULL,
  output = NULL,
  note = NULL,
  badge = NULL
) {
  if (all(vapply(list(chart, code, output, note), is.null, logical(1)))) {
    cli::cli_abort(
      "Supply feature content: {.arg chart}, {.arg code}, {.arg output}, or {.arg note}."
    )
  }
  promo_image_path(chart, "chart")
  if (!is.null(chart)) {
    promo_text(alt, "alt")
  }
  return(promo_card(
    "feature",
    title,
    subtitle,
    chart,
    alt,
    code,
    output,
    note,
    badge
  ))
}

#' @rdname promo_cover
#' @export
promo_closing <- function(title, subtitle = NULL) {
  return(promo_card("closing", title, subtitle))
}

promo_text <- function(value, arg, optional = FALSE) {
  if (optional && is.null(value)) {
    return(invisible(NULL))
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be one nonempty string.",
      call = rlang::caller_env()
    )
  }
  return(invisible(NULL))
}

promo_image_path <- function(path, arg) {
  promo_text(path, arg, optional = TRUE)
  if (
    !is.null(path) &&
      (!file.exists(path) ||
        !grepl("\\.(png|jpe?g)$", path, ignore.case = TRUE))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must name an existing PNG or JPEG file.",
      call = rlang::caller_env()
    )
  }
  return(invisible(NULL))
}

promo_card <- function(
  type,
  title,
  subtitle = NULL,
  chart = NULL,
  alt = NULL,
  code = NULL,
  output = NULL,
  note = NULL,
  badge = NULL
) {
  promo_text(title, "title")
  for (arg in c("subtitle", "alt", "code", "output", "note", "badge")) {
    promo_text(get(arg), arg, optional = TRUE)
  }
  return(structure(
    list(
      type = type,
      title = title,
      subtitle = subtitle,
      chart = chart,
      alt = alt,
      code = code,
      output = output,
      note = note,
      badge = badge
    ),
    class = "ekio_promo_card"
  ))
}

promo_escape <- function(text) {
  for (pair in list(
    c("&", "&amp;"),
    c("<", "&lt;"),
    c(">", "&gt;"),
    c('"', "&quot;"),
    c("'", "&#39;")
  )) {
    text <- gsub(pair[1], pair[2], text, fixed = TRUE)
  }
  return(text)
}

promo_image <- function(path, alt) {
  rlang::check_installed("jsonlite", reason = "to embed carousel images")
  mime <- if (grepl("\\.png$", path, ignore.case = TRUE)) {
    "image/png"
  } else {
    "image/jpeg"
  }
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  return(sprintf(
    '<img src="data:%s;base64,%s" alt="%s">',
    mime,
    jsonlite::base64_enc(bytes),
    promo_escape(alt)
  ))
}

#' Render and export a package announcement carousel
#'
#' Creates a self-contained HTML document with 1080 by 1350 pixel pages.
#' Chrome waits for fonts and images before printing all pages. Export fails
#' if content overflows; shorten the affected card instead of shrinking text.
#' No network fonts are requested; install the configured fonts locally.
#'
#' @param cards Nonempty list of cards made with [promo_cover()],
#'   [promo_feature()], and [promo_closing()].
#' @param brand Configuration from [promo_brand()].
#' @param html Destination HTML file.
#' @param pdf Destination PDF; defaults to the HTML path with `.pdf` extension.
#'   Use `NULL` to write HTML only.
#' @param png_dir Optional directory for one PNG per PDF page at 96 dpi.
#'   Requires PDF export. PNGs use the same HTML layout as the PDF.
#' @return Invisibly, a list of absolute `html`, `pdf`, and `png` paths.
#' @export
render_promo <- function(
  cards,
  brand,
  html,
  pdf = sub("\\.[^.]+$", ".pdf", html),
  png_dir = NULL
) {
  if (
    !is.list(cards) ||
      !length(cards) ||
      !all(vapply(cards, inherits, logical(1), "ekio_promo_card"))
  ) {
    cli::cli_abort("{.arg cards} must be a nonempty list of promo cards.")
  }
  if (!inherits(brand, "ekio_promo_brand")) {
    cli::cli_abort("Use {.fn promo_brand} for {.arg brand}.")
  }
  promo_text(html, "html")
  promo_text(pdf, "pdf", optional = TRUE)
  promo_text(png_dir, "png_dir", optional = TRUE)
  if (!is.null(png_dir) && is.null(pdf)) {
    cli::cli_abort("{.arg png_dir} requires PDF export.")
  }
  if (
    !is.null(pdf) &&
      identical(
        normalizePath(html, mustWork = FALSE),
        normalizePath(pdf, mustWork = FALSE)
      )
  ) {
    cli::cli_abort(
      "{.arg html} and {.arg pdf} must use different paths; include file extensions."
    )
  }
  block <- function(tag, class, value) {
    if (is.null(value)) {
      return("")
    }
    value <- promo_escape(value)
    if (class == "code") {
      # Highlight full comment lines only; leave strings and inline hashes alone.
      value <- gsub(
        "(?m)^([ \t]*#[^\n]*)",
        '<span class="comment">\\1</span>',
        value,
        perl = TRUE
      )
    }
    return(sprintf(
      '<%s class="%s">%s</%s>',
      tag,
      class,
      value,
      tag
    ))
  }
  logo <- if (is.null(brand$logo)) {
    ""
  } else {
    promo_image(brand$logo, paste(brand$package, "logo"))
  }
  pages <- vapply(
    seq_along(cards),
    function(i) {
      card <- cards[[i]]
      chart <- if (is.null(card$chart)) {
        ""
      } else {
        paste0('<figure>', promo_image(card$chart, card$alt), '</figure>')
      }
      cta <- if (card$type != "closing") {
        ""
      } else {
        paste0(
          block("pre", "code", brand$install),
          if (!is.null(brand$docs_url)) {
            sprintf(
              '<a href="%s">%s</a>',
              promo_escape(brand$docs_url),
              promo_escape(brand$docs_url)
            )
          }
        )
      }
      return(paste0(
        '<section class="page ',
        card$type,
        if (!is.null(card$chart)) " has-chart",
        '" id="page-',
        i,
        '">',
        '<header>',
        logo,
        '<span>',
        promo_escape(brand$package),
        ' <small>',
        promo_escape(brand$version),
        '</small></span></header>',
        '<main>',
        block("span", "badge", card$badge),
        block("h1", "title", card$title),
        block("p", "subtitle", card$subtitle),
        chart,
        block("pre", "code", card$code),
        block("pre", "output", card$output),
        block("p", "note", card$note),
        cta,
        '</main>',
        '<footer><span>',
        sprintf('%02d / %02d', i, length(cards)),
        '</span></footer></section>'
      ))
    },
    character(1)
  )
  css <- paste0(
    '
@page { size: 1080px 1350px; margin: 0; }
* { box-sizing: border-box; }
body { margin: 0; color: ',
    brand$ink,
    '; background: #FBFBF6; font-family: "',
    brand$font,
    '", sans-serif; }
.page { width: 1080px; height: 1350px; padding: 56px 64px 40px; display: flex; flex-direction: column; break-after: page; }
.page:last-child { break-after: auto; }
header { display: flex; align-items: center; gap: 18px; font-size: 34px; font-weight: 700; margin-bottom: 36px; }
header img { width: 46px; height: 53px; object-fit: contain; }
small { font-size: 28px; font-weight: 400; margin-left: 12px; }
main { flex: 1; min-height: 0; display: flex; flex-direction: column; }
main > * { flex-shrink: 0; }
.feature:not(.has-chart) main { justify-content: center; }
.badge { align-self: flex-start; font-size: 24px; font-weight: 700; background: #E9EEEC; padding: 8px 14px; border-radius: 6px; margin-bottom: 18px; }
h1 { font-size: 48px; line-height: 1.08; letter-spacing: -1.3px; margin: 0 0 20px; }
p { font-size: 30px; line-height: 1.35; margin: 0 0 24px; }
.subtitle { color: #465563; }
figure { flex: 1; min-height: 0; margin: 12px 0 4px; background: #FFFFFF; }
figure img { width: 100%; height: 100%; display: block; object-fit: contain; }
pre { font: 25px/1.38 "',
    brand$mono_font,
    '", monospace; white-space: pre; overflow-wrap: normal; margin: 20px 0; padding: 24px; border-radius: 12px; }
.code { background: #152A44; color: #FBFBF6; }
.code .comment { color: #A9BBCB; }
.output { background: #E9EEEC; }
.note { font-size: 27px; padding-left: 20px; border-left: 5px solid ',
    brand$accent,
    '; }
footer { display: flex; justify-content: flex-end; padding-top: 20px; font-size: 24px; border-top: 2px solid #CED0D4; }
.cover main, .closing main { display: flex; flex-direction: column; justify-content: center; gap: 26px; }
.cover main { gap: 48px; }
.cover main > * { margin: 0; }
.cover h1 { font-size: 88px; }
.closing h1 { font-size: 72px; max-width: 870px; }
.cover .subtitle, .closing .subtitle { font-size: 40px; line-height: 1.35; }
.cover .subtitle { font-size: 44px; }
.cover { border-top: 16px solid ',
    brand$accent,
    '; }
a { font-size: 29px; color: ',
    brand$accent,
    '; overflow-wrap: anywhere; }
@media print { body { print-color-adjust: exact; -webkit-print-color-adjust: exact; } }
'
  )
  dir.create(dirname(html), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    paste0(
      '<!doctype html><html lang="en"><head><meta charset="utf-8"><title>',
      promo_escape(paste(brand$package, brand$version)),
      '</title><style>',
      css,
      '</style></head><body>',
      paste(pages, collapse = "\n"),
      '</body></html>'
    ),
    html,
    useBytes = TRUE
  )
  png <- character()
  if (!is.null(pdf)) {
    rlang::check_installed(
      c("chromote", "jsonlite"),
      reason = "to export carousel PDFs"
    )
    session <- chromote::ChromoteSession$new()
    on.exit(session$close(), add = TRUE)
    session$Page$navigate(paste0(
      "file://",
      utils::URLencode(normalizePath(html))
    ))
    ready <- session$Runtime$evaluate(
      "(async () => { await document.fonts.ready; await Promise.all(Array.from(document.images, async img => { await img.decode(); })); return true; })()",
      awaitPromise = TRUE
    )
    if (!is.null(ready$exceptionDetails)) {
      cli::cli_abort("Could not load carousel fonts or images.")
    }
    check <- session$Runtime$evaluate(
      "Array.from(document.querySelectorAll('.page')).map(p => { const m=p.querySelector('main'), f=p.querySelector('footer'); return {id:p.id, bad:m.scrollHeight>m.clientHeight+2 || m.getBoundingClientRect().bottom>f.getBoundingClientRect().top+2 || Array.from(p.querySelectorAll('pre, h1, p, a')).some(e=>e.scrollWidth>e.clientWidth+2)}; }).filter(p=>p.bad).map(p=>p.id).join(', ')",
      returnByValue = TRUE
    )$result$value
    if (nzchar(check)) {
      cli::cli_abort("Content overflows on {check}. Shorten those cards.")
    }
    dir.create(dirname(pdf), recursive = TRUE, showWarnings = FALSE)
    result <- session$Page$printToPDF(
      printBackground = TRUE,
      preferCSSPageSize = TRUE
    )
    writeBin(jsonlite::base64_dec(result$data), pdf)
    pdf <- normalizePath(pdf)
    if (!is.null(png_dir)) {
      dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
      png <- file.path(png_dir, sprintf("page-%02d.png", seq_along(cards)))
      session$Emulation$setDeviceMetricsOverride(
        # Leave room for the vertical scrollbar beside the fixed-width page.
        width = 1100,
        height = 1350,
        deviceScaleFactor = 1,
        mobile = FALSE
      )
      for (i in seq_along(cards)) {
        session$screenshot(
          png[i],
          selector = paste0("#page-", i),
          region = "border",
          delay = 0
        )
      }
      png <- normalizePath(png)
    }
    cli::cli_inform("Exported {length(cards)} carousel pages to {.file {pdf}}.")
  }
  return(invisible(list(html = normalizePath(html), pdf = pdf, png = png)))
}
