## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(7,4)*1.4,
  out.width = "100%",
  warning = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
#  ipi_c_eu_df <- ts2df(ipi_c_eu)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(ggplot2)
library(ggdemetra)
p_ipi_fr <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR)) +
    geom_line() +
    labs(title = "Seasonal adjustment of the French industrial production index",
         x = NULL, y = NULL)
p_ipi_fr

## ----include=FALSE------------------------------------------------------------
library(RJDemetra)
sa <- jx13(ipi_c_eu[, "FR"])

## -----------------------------------------------------------------------------
spec <- RJDemetra::x13_spec("RSA3", tradingdays.option = "WorkingDays")
p_ipi_fr +
    geom_sa(color = "#155692",
            spec = spec)

## -----------------------------------------------------------------------------
p_sa <- p_ipi_fr +
    geom_sa(component = "y_f", linetype = 2, message = FALSE,
            spec = spec) + 
    geom_sa(component = "sa", color = "#155692") +
    geom_sa(component = "sa_f", color = "#155692", linetype = 2)
p_sa

## -----------------------------------------------------------------------------
p_sa + geom_outlier(geom = "label")

## -----------------------------------------------------------------------------
p_sa + 
    geom_outlier(geom = "label_repel",
                 ylim = c(NA, 65), 
                 arrow = arrow(length = unit(0.03, "npc"),
                               type = "closed", ends = "last"))

## -----------------------------------------------------------------------------
p_sa + 
    geom_outlier(geom = "label_repel",
                 first_date = 2009,
                 ylim = c(NA, 65), 
                 arrow = arrow(length = unit(0.03, "npc"),
                               type = "closed", ends = "last"))

## -----------------------------------------------------------------------------
p_sa + 
    geom_arima(geom = "label",
               x_arima = -Inf, y_arima = -Inf, 
               vjust = -1, hjust = -0.1)

## -----------------------------------------------------------------------------
diagnostics <- c("diagnostics.combined.all.summary", "diagnostics.qs", "diagnostics.ftest")
p_sa + 
    geom_diagnostics(diagnostics = diagnostics,
                     ymin = 58, ymax = 72, xmin = 2010,
                     table_theme = gridExtra::ttheme_default(base_size = 8))

## -----------------------------------------------------------------------------
diagnostics <- c(`Combined test` = "diagnostics.combined.all.summary",
                 `Residual qs-test (p-value)` = "diagnostics.qs",
                 `Residual f-test (p-value)` = "diagnostics.ftest")
p_sa + 
    geom_diagnostics(diagnostics = diagnostics,
                     ymin = 58, ymax = 72, xmin = 2010,
                     table_theme = gridExtra::ttheme_default(base_size = 8))

## -----------------------------------------------------------------------------
p_diag <- ggplot(data = ipi_c_eu_df, mapping = aes(x = date, y = FR))  +
    geom_diagnostics(diagnostics = diagnostics,
                     spec = spec, frequency = 12,
                     table_theme = gridExtra::ttheme_default(base_size = 8)) + 
    theme_void()
    
gridExtra::grid.arrange(p_sa, p_diag,
             nrow = 2, heights  = c(4, 1.5))

## ----mod----------------------------------------------------------------------
mod <- RJDemetra::x13(ipi_c_eu[,"UK"], spec)

## ----init-ggplot--------------------------------------------------------------
init_ggplot(mod) + 
    geom_line(color =  "#F0B400") +
    geom_sa(color =  "#155692") +
    geom_arima(geom = "label",
               x_arima = -Inf, y_arima = -Inf, 
               vjust = -1, hjust = -0.1)

## ----sa-init------------------------------------------------------------------
data <- ts.union(raw(mod), raw(mod, forecast = TRUE),
                 trendcycle(mod), trendcycle(mod, forecast = TRUE),
                 seasonaladj(mod), seasonaladj(mod, forecast = TRUE))
colnames(data) <- c("y", "y_f",
                    "t", "t_f",
                    "sa", "sa_f")
ggplot(data = ts2df(data), mapping = aes(x = date)) +
    geom_line(mapping = aes(y = y), color =  "#F0B400", na.rm = TRUE) +
    geom_line(mapping = aes(y = y_f), color =  "#F0B400", na.rm = TRUE, linetype = 2) +
    geom_line(mapping = aes(y = t), color =  "#1E6C0B", na.rm = TRUE) +
    geom_line(mapping = aes(y = t_f), color =  "#1E6C0B", na.rm = TRUE, linetype = 2) +
    geom_line(mapping = aes(y = sa), color =  "#155692", na.rm = TRUE) +
    geom_line(mapping = aes(y = sa_f), color =  "#155692", na.rm = TRUE, linetype = 2) +
    theme_bw()

## ----ggsiratio----------------------------------------------------------------
ggsiratioplot(mod)

## ----autoplot-----------------------------------------------------------------
autoplot(mod)

