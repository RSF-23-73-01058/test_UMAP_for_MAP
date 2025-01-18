library(tidyverse)
library(patchwork)
library(umap)

###Files to read and write should inserted instead of "...\\output\\data_nz.tab", etc.

##Set seed
set.seed(1235)

##Get and process the input data for UMAP
query_nz <- read_tsv("...\\output\\data_nz.tab") |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id")
query_greenland <- read_tsv("...\\output\\data_greenland.tab") |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id")
data <- read_tsv("...\\output\\1_geopoints_adjusted.tsv")
data_continent <- data |> filter(continent != "Continentless")
data_ocean <- data |> filter(continent == "Continentless") |> slice_sample(n = round(nrow(data_continent) / 3))
data_all <- bind_rows(data_continent, data_ocean)
data_all <- data_all |> mutate(continent = factor(data_all |> pull(continent), levels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia", "Continentless"))) |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id")
data_continent <- data_continent |> mutate(continent = factor(data_continent |> pull(continent), levels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia"))) |>
				  					`row.names<-`(NULL) |>
				  					column_to_rownames(var = "id")
#Center X, Y, Z
data_all <- data_all |> mutate(x_scaled = scale(data_all |> pull(x)),
										   y_scaled = scale(data_all |> pull(y)),
										   z_scaled = scale(data_all |> pull(z)))
data_continent <- data_continent |> mutate(x_scaled = scale(data_continent |> pull(x)),
										   y_scaled = scale(data_continent |> pull(y)),
										   z_scaled = scale(data_continent |> pull(z)))
#Get the mean X, Y, Z to center the query data
continent_mean_x <- data_continent |> pull(x) |> mean()
continent_mean_y <- data_continent |> pull(y) |> mean()
continent_mean_z <- data_continent |> pull(z) |> mean()
all_mean_x <- data_all |> pull(x) |> mean()
all_mean_y <- data_all |> pull(y) |> mean()
all_mean_z <- data_all |> pull(z) |> mean()

##Get and process the query
query_nz <- read_tsv("...\\output\\data_nz.tab") |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id") |>
					mutate(x_scaled_continent = x - continent_mean_x,
						   y_scaled_continent = y - continent_mean_y,
						   z_scaled_continent = z - continent_mean_z,
						   x_scaled_all = x - all_mean_x,
						   y_scaled_all = y - all_mean_y,
						   z_scaled_all = z - all_mean_z)
query_greenland <- read_tsv("...\\output\\data_greenland.tab") |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id") |>
					mutate(x_scaled_continent = x - continent_mean_x,
						   y_scaled_continent = y - continent_mean_y,
						   z_scaled_continent = z - continent_mean_z,
						   x_scaled_all = x - all_mean_x,
						   y_scaled_all = y - all_mean_y,
						   z_scaled_all = z - all_mean_z)

query_madagascar <- read_tsv("...\\output\\data_madagascar.tab") |>
					`row.names<-`(NULL) |>
					column_to_rownames(var = "id") |>
					mutate(x_scaled_continent = x - continent_mean_x,
						   y_scaled_continent = y - continent_mean_y,
						   z_scaled_continent = z - continent_mean_z,
						   x_scaled_all = x - all_mean_x,
						   y_scaled_all = y - all_mean_y,
						   z_scaled_all = z - all_mean_z)

##Prepare palette
earth_pal <- c("#B79A59", "#8B3834", "#9AB79A", "#324028", "#053161", "#51394E", "#8AA9C6")

##Prepare distance matrices
#distance_continent <- dist(data_continent |> select(x, y, z), method = "euclidean") |> as.matrix()
#distance_all <- dist(data_all |> select(x, y, z), method = "euclidean") |> as.matrix()

##Reduce
umap_continent <- umap( data_continent |> select(x_scaled, y_scaled, z_scaled), preserve.seed = TRUE, n_neighbors = 75 )
umap_all <- umap( data_all |> select(x_scaled, y_scaled, z_scaled), preserve.seed = TRUE, n_neighbors = 25 )

##Analyze further without the UMAP-centering
#Combine without scaling
data_continent$umap1 <- umap_continent$layout[,1]
data_continent$umap2 <- umap_continent$layout[,2]
data_all$umap1 <- umap_all$layout[,1]
data_all$umap2 <- umap_all$layout[,2]
#Combine after scaling
data_continent$umap1_scaled <- scale(umap_continent$layout[,1])
data_continent$umap2_scaled <- scale(umap_continent$layout[,2])
data_all$umap1_scaled <- scale(umap_all$layout[,1])
data_all$umap2_scaled <- scale(umap_all$layout[,2])

#Get the mean X, Y, Z to center the query data
continent_mean_umap1 <- umap_continent$layout[,1] |> mean()
continent_mean_umap2 <- umap_continent$layout[,2] |> mean()
all_mean_umap1 <- umap_all$layout[,1] |> mean()
all_mean_umap2 <- umap_all$layout[,1] |> mean()

#Visualize
continent_plot <- ggplot(data_continent) +
	geom_point(aes(x=umap1, y=umap2, color=continent), alpha = .15, size = .7) +
	scale_colour_manual(values = earth_pal, name = "Continent", labels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia")) +
	xlab("UMAP component 1") +
	ylab("UMAP component 2") +
	coord_fixed() +
	theme_classic()
continent_plot
all_plot <- ggplot() +
	geom_point(data = data_all |> filter(continent != "Continentless"), aes(x=umap1, y=umap2, color=continent), alpha = .15, size = .7) +
	scale_colour_manual(values = earth_pal, name = "Continent", labels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia")) +
	xlab("UMAP component 1") +
	ylab("UMAP component 2") +
	coord_fixed() +
	theme_classic()
all_plot

##Predict
#NZ
umap_nz_continent <- predict(umap_continent, query_nz |> select(x_scaled_continent, y_scaled_continent, z_scaled_continent))
umap_nz_all <- predict(umap_all, query_nz |> select(x_scaled_all, y_scaled_all, z_scaled_all))
query_nz$umap1_continent <- umap_nz_continent[,1]
query_nz$umap2_continent <- umap_nz_continent[,2]
query_nz$umap1_all <- umap_nz_all[,1] 
query_nz$umap2_all <- umap_nz_all[,2]
query_nz <- query_nz |> mutate(Query = "New Zealand")
#Greenland
umap_greenland_continent <- predict(umap_continent, query_greenland |> select(x_scaled_continent, y_scaled_continent, z_scaled_continent))
umap_greenland_all <- predict(umap_all, query_greenland |> select(x_scaled_all, y_scaled_all, z_scaled_all))
query_greenland$umap1_continent <- umap_greenland_continent[,1]
query_greenland$umap2_continent <- umap_greenland_continent[,2]
query_greenland$umap1_all <- umap_greenland_all[,1]
query_greenland$umap2_all <- umap_greenland_all[,2]
query_greenland <- query_greenland |> mutate(Query = "Greenland")
#Madagascar
umap_madagascar_continent <- predict(umap_continent, query_madagascar |> select(x_scaled_continent, y_scaled_continent, z_scaled_continent))
umap_madagascar_all <- predict(umap_all, query_madagascar |> select(x_scaled_all, y_scaled_all, z_scaled_all))
query_madagascar$umap1_continent <- umap_madagascar_continent[,1]
query_madagascar$umap2_continent <- umap_madagascar_continent[,2]
query_madagascar$umap1_all <- umap_madagascar_all[,1]
query_madagascar$umap2_all <- umap_madagascar_all[,2]
query_madagascar <- query_madagascar |> mutate(Query = "Madagascar")

##Visualize
#Continents
query_continent_plot <- continent_plot +
						geom_point(data = query_nz, aes(x=umap1_continent, y=umap2_continent, shape = Query), size = 2.5, color = "gray10", fill = "#fcd596", stroke = .25) +
						geom_point(data = query_greenland, aes(x=umap1_continent, y=umap2_continent, shape = Query), size = 2.5, color = "gray10", fill = "#f05100", stroke = .25) +
						geom_point(data = query_madagascar, aes(x=umap1_continent, y=umap2_continent, shape = Query), size = 2.5, color = "gray90", fill = "#1b3e2e", stroke = .25) +
						scale_shape_manual(values = c("circle filled", "circle filled", "circle filled")) +
						theme(legend.position = "none")
query_continent_plot
#All
query_all_plot <- all_plot +
						geom_point(data = query_nz, aes(x=umap1_all, y=umap2_all, shape = Query), size = 2.5, color = "gray10", fill = "#fcd596", stroke = .25) +
						geom_point(data = query_greenland, aes(x=umap1_all, y=umap2_all, shape = Query), size = 2.5, color = "gray10", fill = "#f05100", stroke = .25) +
						geom_point(data = query_madagascar, aes(x=umap1_all, y=umap2_all, shape = Query), size = 2.5, color = "gray90", fill = "#1b3e2e", stroke = .25) +
						scale_shape_manual(values = c("circle filled", "circle filled", "circle filled")) +
						theme(legend.position = "top") +
						guides(color = "none")
query_all_plot

##Save
plot <- (query_continent_plot | query_all_plot) +
	 plot_layout(guides = "collect") &
	 guides(color = "none", shape = guide_legend(override.aes = list(size=5))) &
	 theme(legend.title = element_blank(), legend.position = "top", plot.margin = margin(t = 1, r = 1, b = 1, l = 1))
plot
ggsave(".../pics/Figure_9.tiff", plot = plot, width = 7, height = 3.5, units = "in", dpi = 600, compression = "lzw", scale = 1.4)