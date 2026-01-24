# clean_language() error messages [plain]

    Code
      clean_language("python")
    Condition
      Error in `clean_language()`:
      ! Invalid `language` value.
      x Received "python".
      > Use `'Julia'` or `'R'`.

# clean_language() error messages [ansi]

    Code
      clean_language("python")
    Condition
      [1m[33mError[39m in `clean_language()`:[22m
      [1m[22m[33m![39m Invalid `language` value.
      [31mx[39m Received [34m"python"[39m.
      > Use `'Julia'` or `'R'`.

# clean_type() error messages [plain]

    Code
      clean_type(123)
    Condition
      Error in `clean_type()`:
      ! Invalid `type` argument.
      x Must be <character>.

# clean_type() error messages [ansi]

    Code
      clean_type(123)
    Condition
      [1m[33mError[39m in `clean_type()`:[22m
      [1m[22m[33m![39m Invalid `type` argument.
      [31mx[39m Must be [34m<character>[39m.

