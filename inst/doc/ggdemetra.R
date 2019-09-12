## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 7,
  out.width = "100%"
)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(ggplot2)
library(ggdemetra)
p_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(title = "Seasonal adjustment of the French industrial production index",
         x = NULL, y = NULL)
p_ipi_fr

## ----include=FALSE-------------------------------------------------------
library(RJDemetra)
sa <- jx13(ipi_c_eu[, "FR"])

## ------------------------------------------------------------------------
spec <- RJDemetra::x13_spec("RSA3", tradingdays.option = "WorkingDays")
p_ipi_fr +
    geom_sa(color = "red",
            spec = spec)

## ------------------------------------------------------------------------
p_sa <- p_ipi_fr +
    geom_sa(component = "y_f", linetype = 2, message = FALSE,
            spec = spec) + 
    geom_sa(component = "sa", color = "red") +
    geom_sa(component = "sa_f", color = "red", linetype = 2)
p_sa

## ------------------------------------------------------------------------
p_sa + geom_outlier(geom = "label")

## ------------------------------------------------------------------------
p_sa + 
    geom_outlier(geom = "label_repel",
                 vjust = 4,
                 ylim = c(NA, 65), force = 10,
                 arrow = arrow(length = unit(0.03, "npc"),
                               type = "closed", ends = "last"))

## ------------------------------------------------------------------------
p_sa + 
    geom_outlier(geom = "label_repel",
                 first_date = 2009,
                 vjust = 4,
                 ylim = c(NA, 65), force = 10,
                 arrow = arrow(length = unit(0.03, "npc"),
                               type = "closed", ends = "last"))

## ------------------------------------------------------------------------
p_sa + 
    geom_arima(geom = "label",
               x_arima = -Inf, y_arima = -Inf, 
               vjust = -1, hjust = -0.1)

## ------------------------------------------------------------------------
diagnostics <- c("diagnostics.combined.all.summary", "diagnostics.qs", "diagnostics.ftest")
p_sa + 
    geom_diagnostics(diagnostics = diagnostics,
                     ymin = 58, ymax = 72, xmin = 2010,
                     table_theme = gridExtra::ttheme_default(base_size = 8))

## ------------------------------------------------------------------------
diagnostics <- c(`Combined test` = "diagnostics.combined.all.summary",
                 `Residual qs-test (p-value)` = "diagnostics.qs",
                 `Residual f-test (p-value)` = "diagnostics.ftest")
p_sa + 
    geom_diagnostics(diagnostics = diagnostics,
                     ymin = 58, ymax = 72, xmin = 2010,
                     table_theme = gridExtra::ttheme_default(base_size = 8))

## ------------------------------------------------------------------------
p_diag <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR))  +
    geom_diagnostics(diagnostics = diagnostics,
                     spec = spec,
                     table_theme = gridExtra::ttheme_default(base_size = 8),
                     message = FALSE) + 
    theme_void()
    
gridExtra::grid.arrange(p_sa, p_diag,
             nrow = 2, heights  = c(4, 1.5))

