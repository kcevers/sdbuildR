## Plan: Remove Delay/Smooth implementations and surface warning

TL;DR - Remove all internal conversion and special-case handling for Insight Maker `Delay`, `Smooth`, `delay()`/`smooth()` and the `DelayN`/`SmoothN` family, and rely on the existing `syntax5` unsupported-function reporting in `convert_builtin_functions_IM()` to inform users. Do not modify `notes/` or `old/`.

Steps
1. Rely on `convert_builtin_functions_IM` in `R/insightmaker_conv_eqn.R` to detect any Delay/Smooth constructs via `syntax5` (tokens `Delay`, `Smooth`, `delay`, `smooth`, `DelayN`, `SmoothN`) and emit a single clear warning or error using the existing unsupported-function reporting. Do not add an early scanner in `R/insightmaker_to_sfm.R`.
2. Edit `R/insightmaker_conv_eqn.R` to disable the converters `conv_delay` and `conv_delayN` so no `delay`/`smooth` code is emitted. Ensure these names remain in `syntax_df_unsupp` so they are detected as unsupported.
3. Remove the `conv_delay` and `conv_delayN` function definitions from `R/insightmaker_conv_eqn.R` (and clean up any now-unused roxygen `@inheritParams` references).
4. Make target-specific code explicit: remove or simplify the commented/partial handling in `R/julia_conv_eqn.R` and `R/assemble_script_julia.R` so they don't attempt any delay/past special-casing.
5. Add a test `tests/testthat/test_insightmaker_delay.R` asserting `insightmaker_to_sfm()` emits the expected warning or error when encountering Delay/Smooth.
6. Run `devtools::check()` and the new test; fix any minor issues (roxygen warnings, unused imports) as needed.

Notes
- `get_syntax_IM()` already marks Delay/Smooth/DelayN/SmoothN as `syntax5`.
- The existing `convert_builtin_functions_IM()` will `cli::cli_inform()` about `syntax5` matches; depending on desired severity you can change those informs to `cli::cli_warn()` or `cli::cli_abort()`.
- User requested to remove `DelayN`/`SmoothN` support as well; plan will delete the conversion helpers.
- Do not change files under `notes/` or any `old/` folder.

Verification
- `devtools::check()` exits cleanly (no unrelated failures).
- `testthat::test_file("tests/testthat/test_insightmaker_delay.R")` asserts the warning or error.
- Manual run: call `insightmaker_to_sfm()` on a sample `.InsightMaker`/.json file containing Delay/Smooth and confirm the message lists offending expressions and model/file.

Next step: I will remove the `conv_delay` and `conv_delayN` functions from `R/insightmaker_conv_eqn.R` and update the todo list status.