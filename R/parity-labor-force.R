##----------------------------------------------------------------
##              Parity Podcast --- AllRise Media LLC             -
##               OECD -- composition of labor force              -
##             email: cathyanddeborah@parity_podcast             -
##----------------------------------------------------------------
#libraries ----
library(dplyr)
library(tidyr)
#load files ----
file <- "./inst/extdata/DP_LIVE_08122021132606364.csv"
df <- readr::read_csv(file = file) |> janitor::clean_names()
# clean files ----
oecd_labor_force <-
     df |>
     filter(frequency == "A") |>
     group_by(location) |>
     filter(time == max(time)) |>
     select(time, location, indicator, subject, value) |>
     pivot_wider(
          id_cols = time:indicator,
          values_from = value,
          names_from = subject
     ) |>
     mutate(men = `/`(MEN, TOT) |> round(3)) |>
     mutate(women = `/`(WOMEN, TOT) |> round(3)) |>
     mutate(diff = men - women) |>
     select(-MEN, -WOMEN, -TOT) |>
     pivot_longer(
          cols = c(men, women),
          names_to = "gender",
          values_to = "pct"
     ) |>
     na.omit() |>
     mutate(pct = pct * 100) |>
     arrange(diff) |>
     ungroup() |>
     slice_head(n = 30)
# create factor variable ----
oecd_labor_force$usa <- 0
oecd_labor_force$usa[which(oecd_labor_force$location == "USA")] <- 1
oecd_labor_force$usa <- factor(oecd_labor_force$usa)
# plot with ggplot ----
library(ggplot2)
p <- ggplot(
     oecd_labor_force,
     aes(reorder(location, -diff),
          y = pct,
          group = gender,
          shape = gender,
          color = usa,
          text = paste0("country: ", location)
     )
)
p <- p + geom_point(size = 2.0)
p <- p + scale_color_manual(values = c("black", "#C80404"))
p <- p + scale_y_continuous(
     name = "",
     limits = c(40, 60),
     breaks = c(40, 45, 50, 55, 60),
     labels = c(paste0(seq(40, 60, by = 5), "%"))
)
p <- p + scale_x_discrete(name = "")
p <- p + guides(color = FALSE)
p <- p + labs(title = "OECD Composition (%) of Labor Force by Gender")
p <- p + theme_minimal()
p <- p + theme(legend.position = "none")
p
# convert to plotly object ----
ggplotly(p, tooltip = c("text", "y", "shape"))
# push to plotly
api_create(
     x = last_plot(),
     filename = "oecd-gender-labor-force",
     fileopt = c("overwrite"),
     sharing = c("public")
)
