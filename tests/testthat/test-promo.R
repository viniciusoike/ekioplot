test_that("cards and brands validate their inputs", {
  expect_s3_class(promo_cover("Title", "Subtitle"), "ekio_promo_card")
  expect_s3_class(promo_feature("Title", code = "1 + 1"), "ekio_promo_card")
  expect_s3_class(promo_closing("Try it"), "ekio_promo_card")
  expect_error(promo_cover(NA_character_, "Subtitle"), "title")
  expect_error(promo_feature("Title"), "content")
  expect_error(
    promo_feature("Title", chart = "missing.png", alt = "Plot"),
    "chart"
  )
  expect_error(promo_feature("Title", code = c("a", "b")), "code")
  expect_error(
    promo_brand("pkg", "1", docs_url = "javascript:alert(1)"),
    "docs_url"
  )
  expect_error(promo_brand("pkg", "1", accent = "red;display:none"), "accent")
})

test_that("HTML escapes content, embeds assets and omits absent sections", {
  brand <- promo_brand("pkg", "1.0", docs_url = "https://example.org")
  cards <- list(
    promo_cover("A < B", "An example"),
    promo_feature("Code", code = "x < 2 & TRUE", output = "<dbl>"),
    promo_closing("Install")
  )
  path <- tempfile(fileext = ".html")
  result <- render_promo(cards, brand, path, pdf = NULL)
  html <- paste(readLines(path), collapse = "\n")
  expect_length(gregexpr('<section class="page ', html, fixed = TRUE)[[1]], 3)
  expect_match(html, "A &lt; B", fixed = TRUE)
  expect_match(html, "x &lt; 2 &amp; TRUE", fixed = TRUE)
  expect_match(html, "&lt;dbl&gt;", fixed = TRUE)
  expect_no_match(html, '<figure')
  expect_no_match(html, 'class="note"')
  expect_match(html, '@page { size: 1080px 1350px; margin: 0; }', fixed = TRUE)
  expect_identical(result$html, normalizePath(path))
  expect_error(render_promo(list("bad"), brand, path, pdf = NULL), "cards")
})

test_that("PDF and PNG export preserve page count and dimensions", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("pdftools")
  skip_if_not_installed("jsonlite")
  skip_if(is.null(chromote::find_chrome()), "Chrome is unavailable")
  folder <- tempfile()
  dir.create(folder)
  cards <- list(
    promo_cover("Title", "Subtitle"),
    promo_feature("Code", code = "1 + 1"),
    promo_closing("Try it")
  )
  result <- render_promo(
    cards,
    promo_brand("pkg", "1.0"),
    file.path(folder, "index.html"),
    png_dir = file.path(folder, "pages")
  )
  expect_equal(pdftools::pdf_info(result$pdf)$pages, 3)
  expect_length(result$png, 3)
  expect_equal(
    dim(pdftools::pdf_render_page(result$pdf, dpi = 96))[2:3],
    c(1080, 1350),
    tolerance = 1 / 1000
  )
  expect_equal(sum(file.exists(result$png)), 3)
  for (path in result$png) {
    con <- file(path, "rb")
    header <- readBin(con, "raw", n = 24)
    close(con)
    dimensions <- vapply(
      list(header[17:20], header[21:24]),
      function(bytes) {
        return(sum(as.integer(bytes) * 256^(3:0)))
      },
      numeric(1)
    )
    expect_equal(dimensions, c(1080, 1350))
  }
  expect_error(
    render_promo(
      list(promo_feature(
        "Overflow",
        code = paste(rep("x", 70), collapse = "\n")
      )),
      promo_brand("pkg", "1"),
      file.path(folder, "overflow.html")
    ),
    "overflows"
  )
})


test_that("image cards embed charts and logos and escape alternative text", {
  skip_if_not_installed("jsonlite")
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 20, height = 20)
  grid::grid.rect()
  grDevices::dev.off()
  destination <- tempfile(fileext = ".html")
  render_promo(
    list(promo_feature("Chart", chart = path, alt = "A < B", note = "A & B")),
    promo_brand("pkg", "1", logo = path),
    destination,
    pdf = NULL
  )
  html <- paste(readLines(destination), collapse = "\n")
  expect_length(gregexpr("data:image/png;base64,", html, fixed = TRUE)[[1]], 2)
  expect_match(html, 'alt="A &lt; B"', fixed = TRUE)
  expect_match(html, 'class="note">A &amp; B', fixed = TRUE)
  expect_no_match(html, 'class="code"')
  expect_no_match(html, 'class="output"')
})

test_that("feature badges are optional, validated and escaped", {
  expect_error(promo_feature("Title", code = "1", badge = ""), "badge")
  path <- tempfile(fileext = ".html")
  render_promo(
    list(
      promo_feature("New", code = "1", badge = "New < 2"),
      promo_feature("Existing", code = "2")
    ),
    promo_brand("pkg", "1"),
    path,
    pdf = NULL
  )
  html <- paste(readLines(path), collapse = "\n")
  expect_length(gregexpr('class="badge"', html, fixed = TRUE)[[1]], 1)
  expect_match(html, 'class="badge">New &lt; 2</span>', fixed = TRUE)
  expect_no_match(html, "R package /")
  expect_match(html, "02 / 02", fixed = TRUE)
})

test_that("feature layouts fill chart space and balance text-only cards", {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome is unavailable")
  chart <- tempfile(fileext = ".png")
  grDevices::png(chart, width = 600, height = 600)
  grid::grid.rect()
  grDevices::dev.off()
  path <- tempfile(fileext = ".html")
  render_promo(
    list(
      promo_feature(
        "Chart and code",
        chart = chart,
        alt = "Square",
        code = "1"
      ),
      promo_feature(
        "Chart and note",
        chart = chart,
        alt = "Square",
        code = "1",
        note = "A note"
      ),
      promo_feature("Code and output", code = "1 + 1", output = "2"),
      promo_cover("A cover", "A subtitle")
    ),
    promo_brand("pkg", "1"),
    path
  )
  session <- chromote::ChromoteSession$new()
  on.exit(session$close(), add = TRUE)
  session$Page$navigate(paste0("file://", normalizePath(path)))
  geometry <- session$Runtime$evaluate(
    "(async () => { await document.fonts.ready; await Promise.all(Array.from(document.images, i => i.decode())); return Array.from(document.querySelectorAll('main')).map(m => { const r=m.getBoundingClientRect(), first=m.firstElementChild.getBoundingClientRect(), last=m.lastElementChild.getBoundingClientRect(), fig=m.querySelector('figure'), img=m.querySelector('figure img'); return {height:fig?.clientHeight ?? 0, contentGap:last.top-first.bottom, bottomGap:r.bottom-last.bottom, topGap:first.top-r.top, fit:img ? getComputedStyle(img).objectFit : ''}; }); })()",
    awaitPromise = TRUE,
    returnByValue = TRUE
  )$result$value
  for (i in 1:2) {
    expect_gt(geometry[[i]]$height, 600)
    expect_lt(geometry[[i]]$bottomGap, 40)
    expect_identical(geometry[[i]]$fit, "contain")
  }
  expect_equal(geometry[[3]]$topGap, geometry[[3]]$bottomGap, tolerance = 2)
  expect_lt(geometry[[4]]$contentGap, 80)
  expect_equal(geometry[[4]]$topGap, geometry[[4]]$bottomGap, tolerance = 30)
})

test_that("code highlights only full comment lines and preserves literal text", {
  code <- paste(
    "# rebase each series < 100 & keep groups",
    "  # keep indentation",
    'label <- "# literal"',
    "x <- 1 # inline comment",
    sep = "\n"
  )
  path <- tempfile(fileext = ".html")
  render_promo(
    list(promo_feature("Rebase series", code = code, output = "# output")),
    promo_brand("pkg", "1"),
    path,
    pdf = NULL
  )
  html <- paste(readLines(path), collapse = "\n")
  expect_match(
    html,
    '<span class="comment"># rebase each series &lt; 100 &amp; keep groups</span>',
    fixed = TRUE
  )
  expect_match(
    html,
    '<span class="comment">  # keep indentation</span>',
    fixed = TRUE
  )
  expect_length(gregexpr('<span class="comment">', html, fixed = TRUE)[[1]], 2)
  expect_match(html, 'label &lt;- &quot;# literal&quot;', fixed = TRUE)
  expect_match(html, 'x &lt;- 1 # inline comment', fixed = TRUE)
  expect_match(html, '<pre class="output"># output</pre>', fixed = TRUE)
  expect_match(html, '.code .comment { color: #A9BBCB; }', fixed = TRUE)
})

test_that("shared typography uses smaller titles and aligned compact logos", {
  path <- tempfile(fileext = ".html")
  render_promo(
    list(promo_cover("Title", "Subtitle")),
    promo_brand("pkg", "1"),
    path,
    pdf = NULL
  )
  html <- paste(readLines(path), collapse = "\n")
  expect_match(html, 'h1 { font-size: 48px;', fixed = TRUE)
  expect_match(html, '.cover h1 { font-size: 88px; }', fixed = TRUE)
  expect_match(html, '.closing h1 { font-size: 72px;', fixed = TRUE)
  expect_match(html, 'header img { width: 46px; height: 53px;', fixed = TRUE)
  expect_match(html, 'align-items: center;', fixed = TRUE)
})
