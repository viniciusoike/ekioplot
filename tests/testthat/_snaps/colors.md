# accent palettes keep their main color while varying in size

    Code
      ekio_pal("accent_blue", n = 1)
    Condition
      Error in `ekio_pal()`:
      ! `n` for "accent_blue" must be between 2 and 6.

---

    Code
      ekio_pal("accent_orange", n = 7)
    Condition
      Error in `ekio_pal()`:
      ! `n` for "accent_orange" must be between 2 and 6.

# palette arguments require valid scalar values

    Code
      ekio_pal(c("full", "blue"))
    Condition
      Error in `ekio_pal()`:
      ! `palette` must be a single string.

---

    Code
      ekio_pal("full", n = 2.5)
    Condition
      Error in `ekio_pal()`:
      ! `n` must be `NULL` or a single non-negative whole number.

---

    Code
      ekio_pal("full", n = NA_real_)
    Condition
      Error in `ekio_pal()`:
      ! `n` must be `NULL` or a single non-negative whole number.

---

    Code
      ekio_pal("full", reverse = NA)
    Condition
      Error in `ekio_pal()`:
      ! `reverse` must be `TRUE` or `FALSE`.

---

    Code
      list_ekio_palettes(c("all", "accent"))
    Condition
      Error in `list_ekio_palettes()`:
      ! `type` must be a single string.
