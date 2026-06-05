# clean_language() rejects invalid language [plain]

    Code
      clean_language("python")
    Condition
      Error in `clean_language()`:
      x Invalid `language` value.
      i Received "python".
      > Use `'Julia'` or `'R'`.

---

    Code
      clean_language("cpp")
    Condition
      Error in `clean_language()`:
      x Invalid `language` value.
      i Received "cpp".
      > Use `'Julia'` or `'R'`.

# clean_language() rejects invalid language [ansi]

    Code
      clean_language("python")
    Condition
      [1m[33mError[39m in `clean_language()`:[22m
      [1m[22m[31mx[39m Invalid `language` value.
      [36mi[39m Received [34m"python"[39m.
      > Use `'Julia'` or `'R'`.

---

    Code
      clean_language("cpp")
    Condition
      [1m[33mError[39m in `clean_language()`:[22m
      [1m[22m[31mx[39m Invalid `language` value.
      [36mi[39m Received [34m"cpp"[39m.
      > Use `'Julia'` or `'R'`.

