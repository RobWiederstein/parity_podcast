##----------------------------------------------------------------
##              Parity Podcast --- AllRise Media LLC             -
##                 OECD -- SIGI to GDP per capita                -
##             email: cathyanddeborah@parity_podcast             -
##----------------------------------------------------------------
#libraries ----
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
# load sigi ----
file <- "./inst/extdata/DP_SIGI_08122021234040422.csv"
sigi <- read_csv(file = file) |> clean_names()
df.sigi <-
	sigi |>
	filter(subject == 'TOT') |>
	select(-flag_codes) |>
	rename(sigi_score = value)
# load gdp ----
file <- "./inst/extdata/GDP_PER_CAPITA_08122021235405125.csv"
gdp <- readr::read_csv(file = file) |> janitor::clean_names()
df.gdp <-
	gdp |>
	filter(time == max(time)) |>
	select(location, value) |>
	mutate(value = round(value, 0)) |>
	rename(gdp_per_capita_usd = value)
# join dataframes ---
df <-
	dplyr::left_join(df.sigi, df.gdp, by = 'location') |>
	na.omit()
# final clean ----
gdp_sigi <-
	df |>
	mutate(usa = 0)
# factor for color ----
gdp_sigi$usa[which(gdp_sigi$location == "USA")] <- 1
gdp_sigi$usa <- factor(gdp_sigi$usa)
# plot ----
p <- ggplot(gdp_sigi, aes(gdp_per_capita_usd,
		    sigi_score,
		    group = usa,
		    color = usa,
		    text = paste0("country: ", location))
)
p <- p + scale_color_manual(values = c("black", "red"))
p <- p + geom_point()
p <- p + geom_smooth(method = "lm")
p <- p + theme_minimal()
p <- p + labs(title = "OECD GDP per capita to SIGI")
p <- p + theme(legend.position = "none")
p
# plotly ----
ggplotly(p, tooltip = c("text", "x", "y"))
