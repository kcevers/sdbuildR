# ---------------------------------------------------------------------------
# Demo: layout controls for plot.stockflow()
#
#   direction -> overall flow direction      ("LR" / "TB" / "RL" / "BT")
#   align     -> line nodes up ACROSS the flow axis   ({rank=same; ...})
#   order     -> sequence nodes ALONG the flow axis   (soft, invisible edges)
#
# predator_prey is used instead of SIR: its two stocks (predator, prey) are not
# directly connected by flows, so the layout controls have a visible effect.
#   stocks    : predator, prey
#   flows     : predator_births, predator_deaths, prey_births, prey_deaths
#   constants : alpha, beta, delta, gamma
#
# Run interactively (RStudio / VS Code) so the diagrams render in the viewer.
# ---------------------------------------------------------------------------

library(sdbuildR)

sfm <- stockflow("predator_prey")


# ===========================================================================
# 0. BASELINE -- no layout hints
# ===========================================================================
plot(sfm, show_constants = FALSE)


# ===========================================================================
# 1. direction -- overall flow direction
# ===========================================================================
plot(sfm, direction = "LR", show_constants = FALSE) # default: left -> right
plot(sfm, direction = "TB", show_constants = FALSE) # top -> bottom
plot(sfm, direction = "RL", show_constants = FALSE) # right -> left


# ===========================================================================
# 2. align -- line nodes up ACROSS the flow axis
# ===========================================================================

# Stack the two stocks in the same column (vertically, since direction = LR)
plot(sfm, align = c("predator", "prey"), show_constants = FALSE)

# Several groups at once (works for flows too, not only stocks)
plot(sfm,
  align = list(
    c("predator", "prey"),
    c("prey_births", "predator_births")
  ),
  show_constants = FALSE
)


# ===========================================================================
# 3. order -- sequence nodes ALONG the flow axis (soft hint)
# ===========================================================================

# Nudge prey ahead of predator. A soft hint: the real flows still win where
# they conflict, so the layout is never broken.
plot(sfm, order = c("prey", "predator"), show_constants = FALSE)

# A longer chain
plot(sfm,
  order = c("prey_births", "prey", "prey_deaths"),
  show_constants = FALSE
)


# ===========================================================================
# 4. order WITHIN an align group -- fix the order inside a column
#
# align puts the stocks in one column; order fixes which sits on top.
# ===========================================================================
plot(sfm,
  align = c("predator", "prey"),
  order = c("prey", "predator"),
  show_constants = FALSE
)


# ===========================================================================
# 5. EVERYTHING TOGETHER -- direction + align + order
# ===========================================================================
plot(sfm,
  direction = "TB",
  align = c("predator", "prey"),
  order = c("predator", "prey"),
  show_constants = FALSE
)
