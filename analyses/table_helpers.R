# analyses/table_helpers.R — Shared LaTeX table helpers
#
# Source this file after source("config.R") in any analyses/ script that
# produces LaTeX tables via kableExtra. Provides two canonical helpers:
#
#   write_tex()      — standard single-page table (hold_position + scale_down)
#   write_tex_long() — longtable for tables that span multiple pages
#
# Do not define these functions locally in individual scripts.

library(kableExtra)

# -----------------------------------------------------------------------------
# write_tex — standard kableExtra LaTeX table
# -----------------------------------------------------------------------------

write_tex <- function(tbl, path, caption, label, fmt = 4, escape = TRUE) {
  kbl(tbl,
      format            = "latex",
      booktabs          = TRUE,
      caption           = paste0(caption, " \\label{", label, "}"),
      digits            = fmt,
      escape            = escape,
      caption.placement = "top") |>
    kableExtra::kable_styling(
      latex_options = c("hold_position", "scale_down")
    ) |>
    footnote(
      general       = "Sources: UN Comtrade; Taiwan ITA. Author's calculations.",
      general_title = "",
      threeparttable = TRUE,
      escape        = FALSE
    ) |>
    save_kable(path)
  message("Saved: ", path)
}

# -----------------------------------------------------------------------------
# write_tex_long — longtable for multi-page tables
# -----------------------------------------------------------------------------

write_tex_long <- function(tbl, path, caption, label, fmt = 4, escape = TRUE) {
  kbl(tbl,
      format            = "latex",
      booktabs          = TRUE,
      longtable         = TRUE,
      caption           = paste0(caption, " \\label{", label, "}"),
      digits            = fmt,
      escape            = escape,
      caption.placement = "top") |>
    kableExtra::kable_styling(
      latex_options = c("hold_position", "repeat_header")
    ) |>
    footnote(
      general       = "Sources: UN Comtrade; Taiwan ITA. Author's calculations.",
      general_title = "",
      threeparttable = TRUE,
      escape        = FALSE
    ) |>
    save_kable(path)
  message("Saved: ", path)
}
