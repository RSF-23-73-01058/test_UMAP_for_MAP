library(tidyverse)
library(umap)

###Files to read and write should inserted instead of ".../output/pics/UMAP_scatterPlot_n-{n}.png", etc.

##Get the data
data_group <- c("Africa", "Eurasia", "northAmerica", "southAmerica", "Antarctica", "Australia")
data <- read_tsv("...\\output\\1_geopoints_adjusted.tsv") |>
				mutate(continent = if_else(continent == "North America", "northAmerica", continent)) |>
				mutate(continent = if_else(continent == "South America", "southAmerica", continent))
data <- data |> mutate(continent = factor(data |> pull(continent), levels = c("Africa", "Eurasia", "northAmerica", "southAmerica", "Antarctica", "Australia", "Continentless")))
data_continent <- data |> filter(continent != "Continentless")
##Prepare palette
earth_pal <- c("#B79A59", "#8B3834", "#9AB79A", "#324028", "#053161", "#51394E", "#8AA9C6")

##Center the X, Y, Z
data_continent <- data_continent |> mutate(x_centered = scale(data_continent |> pull(x)),
										   y_centered = scale(data_continent |> pull(y)),
										   z_centered = scale(data_continent |> pull(z)))

##Prepare rownumbers to subset the distance matrix latter
data_continent <- data_continent |> mutate(row_number = row_number())
for (i in seq(1 : length(data_group))) {
	#Row numbers by continents:
	row_number_i <- str_glue("row_number_{data_group[i]}")
	assign(row_number_i, data_continent |> filter(continent == data_group[i]) |> pull(row_number))
	#Row numbers without speciffic continent:
	row_number_i_not <- str_glue("row_number_{data_group[i]}_not")
	assign(row_number_i_not, data_continent |> filter(continent != data_group[i]) |> pull(row_number))
}

##Prepare the distance matrix
continent_distance <- dist(data_continent |> select(x_centered, y_centered, z_centered), method = "euclidean") |> as.matrix()

##Set seed
set.seed(1235) 

##Reduce dimensionality using UMAP
n_i <- seq(from = 5, to = 205, by = 10)
umap_time <- data.frame(time = rep(NA, length(n_i)), n = rep(NA, length(n_i)))
for (i in seq(1:length(n_i))) {
n <- n_i[i]
#UMAP the data and measure time
umap_start <- Sys.time()
continents_umap <- umap( data_continent |> select(x_centered, y_centered, z_centered), preserve.seed = TRUE, n_neighbors = n )
umap_end <- Sys.time()
umap_elapsed <- difftime(umap_end, umap_start, units = "secs")
umap_time[i,1] <- umap_elapsed
umap_time[i,2] <- n
#Get the UMAP coordinates 
data_continent$umap1_centered <- scale(continents_umap$layout[,1])
data_continent$umap2_centered <- scale(continents_umap$layout[,2])
#Plot the results
scatter_plot <- ggplot(data_continent, aes(x=umap1_centered, y=umap2_centered, color=continent)) +
	geom_point(size = .2) +
	scale_colour_manual(values = earth_pal, name = "Continent", labels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia")) +
	xlab("UMAP component 1") +
	ylab("UMAP component 2") +
	xlim(-5, 5) +
	ylim(-5, 5) +
	coord_fixed() +
	theme_classic()
scatter_plot
ggsave(str_glue(".../output/pics/UMAP_scatterPlot_n-{n}.png"), scatter_plot, device = "png",
			width = 10, height = 10, units = "in", dpi = 600)
#Get the distances in UMAP coordinates
continent_distance_umap <- dist(data_continent |> select(umap1_centered, umap2_centered), method = "euclidean") |> as.matrix()

##Assess the dimReduction's quality as the distance ratio
#_Overall
overall_ratio <- continent_distance / continent_distance_umap
overall_ratio[upper.tri(overall_ratio, diag = TRUE)] <- NA
overall_ratio_vector <- as.vector(overall_ratio[lower.tri(overall_ratio, diag = FALSE)])
#Plot the results
ggplot(mapping = aes(x = overall_ratio_vector, after_stat(scaled))) +
	geom_density() +
	scale_x_continuous(transform = "log10", limits=c(0.02, 2000)) +
	theme_classic()

#_Inside the continent
for (i_in in seq(1 : length(data_group))) {
	#Distance ratio by continents:
	i_ratio_inside <- str_glue("{data_group[i_in]}_ratio_inside")
	assign(i_ratio_inside, na.exclude(overall_ratio[get(str_glue("row_number_{data_group[i_in]}")), get(str_glue("row_number_{data_group[i_in]}"))] |> as.vector()))
}
#SEE: https://stackoverflow.com/questions/36727360/r-put-variables-from-globalenv-that-meet-certain-criteria-in-list
total_ratio_inside <- mget(ls(pattern = "[^i]_ratio_inside")) |> unlist() |> unname()
write(total_ratio_inside, str_glue(".../output/distance_ratio/total_ratio_inside_n-{n}.tsv"), ncolumns = 1)
#Save the results
# ggplot(mapping = aes(x = total_ratio_inside, after_stat(scaled))) +
# 	geom_density() +
# 	scale_x_continuous(transform = "log10", limits=c(0.02, 2000)) +
# 	theme_classic()

#_Between continents
for (i_out in seq(1 : length(data_group))) {
	#Distance ratio by continents:
	i_ratio_inside_not <- str_glue("{data_group[i_out]}_ratio_inside_not")
	assign(i_ratio_inside_not, na.exclude(overall_ratio[get(str_glue("row_number_{data_group[i_out]}")), get(str_glue("row_number_{data_group[i_out]}_not"))] |> as.vector()))
}
total_ratio_inside_not <- mget(ls(pattern = "[^i]_ratio_inside_not")) |> unlist() |> unname()
#Save the results
write(total_ratio_inside_not, str_glue(".../output/distance_ratio/total_ratio_inside_not_n-{n}.tsv"), ncolumns = 1)

##Clear
to_clear <- mget(ls(pattern = "_ratio_inside")) |> names()
rm(list = to_clear)
rm( i, i_in, i_out, n, to_clear, scatter_plot, continents_umap, continent_distance_umap, overall_ratio, overall_ratio_vector, umap_start, umap_end )
}
write_tsv(umap_time, ".../output/umap_time.tsv")