##----------------------------------------------------------------
##              Parity Podcast --- AllRise Media LLC             -
##                    OECD -- Gender Wage Gap                    -
##             email: cathyanddeborah@parity_podcast             -
##----------------------------------------------------------------
# Defined begin ----
#OECD gender wage gap The gender wage gap is defined as the difference between
#median earnings of men and women relative to median earnings of men. Data refer
#to full-time employees on the one hand and to self-employed on the other.
#Gender gaps are pervasive in all walks of economic life and imply large losses
#in terms of foregone productivity and living standards to the individuals
#concerned and the economy.
# Defined end ----
# libraries ----
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggthemes)
library(plotly)
# load ----
file <- "./inst/extdata/GENDER_EMP_07122021224435264.csv"
df <- readr::read_csv(file = file)
# clean ----
df.1 <-
	df |>
	select_if(function(x) !(all(is.na(x)) | all(x==""))) |>
	select(COU, Country, IND, SEX, AGE, TIME, Value) |>
	clean_names() |>
	filter(ind == "EMP9_5") |>
	filter(time == 2018)  |>
	mutate(group = 0) |>
	arrange(desc(value)) |>
	rename(wage_gap = value) |>
	slice_head(n = 10)
# factor variable for colour ----
df.1$group[which(df.1$cou == "USA")] <- 1
df.1$group <- factor(df.1$group)
# plot ----
p <-
	df.1 |>
	ggplot() +
	aes(wage_gap,
	    y = reorder(cou, wage_gap),
	    group = group,
	    color = group,
	    text = paste0("Country: ", country)) +
	geom_point(size = 3.0) +
	scale_colour_manual(values=c("black", "#C80404")) +
	labs(title = "Top OECD Countries by Gender Wage Gap",
	     caption = "OECD - 2018",
	     y = "",
	     x = "") +
	scale_x_continuous(breaks = c(0, 10, 20, 30),
			   limits = c(0, 40),
			   labels = c("0%", "10%", "20%", "30%")) +
	theme_minimal() +
	theme(legend.position="none")
p
# plotly ----
ggplotly(p, tooltip = c("text", "x"))
# push plot -----
api_create(
	x = last_plot(),
	filename = "oecd-gender-wage-gap",
	fileopt = c("overwrite"),
	sharing = c("public")
)
