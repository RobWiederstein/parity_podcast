##---------------------------------------------------------------
##              Parity Podcast -- AllRise Media LLC             -
##                 OECD Labor Force by Gender US                -
##             email: cathyanddebora@parity_podcast             -
##---------------------------------------------------------------
# libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
# load files ----
file <- "./inst/extdata/DP_LIVE_08122021132606364.csv"
df <- readr::read_csv(file = file) |> janitor::clean_names()
# clean data ----
us_workforce <-
	df |>
	filter(frequency == "A") |>
	filter(location == "USA") |>
	filter(subject %in% c('MEN', 'WOMEN')) |>
	mutate(time = as.integer(time)) |>
	filter(time %in% seq(1955, 2020, by = 5)) |>
	select(-measure, -frequency, -flag_codes) |>
	rename(gender = subject,
	       year = time,
	       workforce = value) |>
	mutate(workforce = `/`(workforce, 1000))
# plot data ----
p <- ggplot(us_workforce,
	    aes(x = year,
	        y = workforce,
	        #group = gender,
	        color = gender))
p <- p + geom_line()
p <- p + geom_point()
p <- p + scale_color_manual(values = c("black", "#C80404"))
p <- p + scale_y_continuous(name = "",
			    limits = c(0, 100),
			    breaks = c(0, 33, 66, 100),
			    labels = c("0m", "33m", "66m", "100m"))
p <- p + scale_x_continuous(name = "")
p <- p + labs(title = "US Workforce by Gender in millions")
p <- p + theme_minimal()
p <- p + theme(legend.position = "none")
p
#plotly ----
ggplotly(p, tooltip = c('x', 'y', 'gender'))
api_create(
	x = last_plot(),
	filename = "us-labor-force",
	fileopt = c("overwrite"),
	sharing = c("public")
)
