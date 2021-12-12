file <- './inst/extdata/pew_fortune_500_female_ceos.csv'
df <- readr::read_csv(file = file, skip = 1, col_names = F)
df.1 <-
	df |>
	rename(year = X1, pct_female_ceos = X2) |>
	mutate(pct_female_ceos = gsub("%", "", pct_female_ceos) |> as.numeric()) |>
	filter(year %in% seq(1995, 2020, by = 5))
library(ggplot2)
p <- ggplot(df.1, aes(x = year,
		      y = pct_female_ceos,
		      #color = pct_female_ceos
		      # text = paste0("Pct. Female CEOs: ",
		      # 	      pct_female_ceos,
		      # 	      "%")
		      )
	    )
p <- p + geom_line(color = '#C80404')
p <- p + geom_point(color = '#C80404', size = 2)
p <- p + scale_y_continuous(name = "",
			    limits = c(0, 100),
			    breaks = seq(0, 100, by = 25),
			    labels = c("0%", "25%", "50%", "75%", "100%"))
p <- p + theme_minimal()
p <- p + labs(title = "Percent Female CEOs in Fortune 500")
p <- p + scale_x_continuous(name = "")
p
# plotly ----
ggplotly(p, tooltip = c("y", "x", 'text'))
# push plot -----
api_create(
	x = last_plot(),
	filename = "pct-female-ceos",
	fileopt = c("new"),
	sharing = c("public")
)
