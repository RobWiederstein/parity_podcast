file <- "./inst/extdata/EAG_GRAD_ENTR_SHARE_08122021211019514.csv"
df <- readr::read_csv(file = file)

df.1 <-
	df |>
	filter(`Education level` == "Bachelor’s or equivalent level") |>
	filter(`Statistical unit` == "Graduates") |>
	filter(COUNTRY == "USA") |>
	filter(YEAR != 9999) |>
	filter(`Mobility status` == "Total") |>
	rename(country_abbr = COUNTRY)
df.2 <-
	df.1 |>
	select(country_abbr, Country, Indicator, `Education level`,
	       `Reference sector`, `Statistical unit`, Gender, YEAR,
	       Value) |>
	janitor::clean_names() |>
	rename(grad_rate = value)

p <- ggplot(df.2, aes(year,
		      grad_rate,
		      color = gender))
p <- p + geom_line()
p <- p + geom_point(size = 2)
p <- p + scale_color_manual(values = c("#C80404", "black"))
p <- p + scale_y_continuous(name = "",
			    limits = c(0, 100),
			    breaks = c(0, 25, 50, 75, 100),
			    labels = c("0%", "25%", "50%", "75%", "100%"))
p <- p + scale_x_continuous(name = "")
p <- p + theme_minimal()
p <- p + labs(title = '% US Bachelor Degrees Awarded by Gender',
	      caption = "hello")
p <- p + theme(legend.position = "none")
p
ggplotly(p, tooltip = c('x', 'y', 'gender'))
api_create(
	x = last_plot(),
	filename = "us-bach-deg-by-gender",
	fileopt = c("overwrite"),
	sharing = c("public")
)
