# header() requires sfm argument [plain]

    Code
      header(name = "Test")
    Condition
      Error in `header()`:
      ! The `sfm` argument is required.

# header() requires sfm argument [ansi]

    Code
      header(name = "Test")
    Condition
      [1m[33mError[39m in `header()`:[22m
      [1m[22m[33m![39m The `sfm` argument is required.

# clean_language() rejects invalid language [plain]

    Code
      clean_language("python")
    Condition
      Error in `clean_language()`:
      ! Invalid `language` value.
      x Received "python".
      > Use `'Julia'` or `'R'`.

---

    Code
      clean_language("cpp")
    Condition
      Error in `clean_language()`:
      ! Invalid `language` value.
      x Received "cpp".
      > Use `'Julia'` or `'R'`.

# clean_language() rejects invalid language [ansi]

    Code
      clean_language("python")
    Condition
      [1m[33mError[39m in `clean_language()`:[22m
      [1m[22m[33m![39m Invalid `language` value.
      [31mx[39m Received [34m"python"[39m.
      > Use `'Julia'` or `'R'`.

---

    Code
      clean_language("cpp")
    Condition
      [1m[33mError[39m in `clean_language()`:[22m
      [1m[22m[33m![39m Invalid `language` value.
      [31mx[39m Received [34m"cpp"[39m.
      > Use `'Julia'` or `'R'`.

