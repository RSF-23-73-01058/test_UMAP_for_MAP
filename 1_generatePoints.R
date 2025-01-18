library(tidyverse)
library(rworldmap)

###Files to read and write should inserted instead of "...\\output\\1_geopoints.tsv"

##Some variables
#Generative parameters
radius <- 64
number_evenlyD <- 100000 # = 2N + 1
phib <- (1 + sqrt(5))/2
#To store the results
data <- tibble(id=seq(1:number_evenlyD), x=rep(NA, number_evenlyD), y=rep(NA, number_evenlyD), z=rep(NA, number_evenlyD),
							longitude=rep(NA, number_evenlyD), latitude=rep(NA, number_evenlyD) )
#Palette
earth_pal <- c("#B79A59", "#8B3834", "#9AB79A", "#324028", "#053161", "#51394E", "#8AA9C6")


##Generate points using Fibonacci spiral
row_n <- 1
for (i in seq(from=-number_evenlyD/2, to=number_evenlyD/2)) {
	#Generate
	latitude_rad <- asin( 2 * i / number_evenlyD )
	latitude <- latitude_rad * 180 / pi
	longitude <- (i %% phib) * 360 / phib
	if (longitude < -180) { longitude <- 360 + longitude }
	if (longitude >  180) { longitude <- longitude - 360 }
	#Convert to XYZ
	x = ( cos(latitude_rad) * cospi(longitude/180) ) * radius
	y = ( cos(latitude_rad) * sinpi(longitude/180) ) * radius
	z = sin(latitude_rad) * radius
	#Gather
	data[row_n,2] <- x
	data[row_n,3] <- y
	data[row_n,4] <- z
	data[row_n,5] <- longitude
	data[row_n,6] <- latitude

	row_n <- row_n + 1
}

##Assess the results
ggplot(data |> select(x), aes(x=x)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data |> select(y), aes(x=y)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data |> select(z), aes(x=z)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data |> select(longitude), aes(x=longitude)) +
	geom_histogram() +
	xlim(-180, 180)
ggplot(data |> select(latitude), aes(x=latitude)) +
	geom_histogram() +
	xlim(-90, 90)
ggplot(data, aes(x=longitude, y=latitude)) +
	geom_point()

##Select random sample
set.seed(11111)
data_random <- data |> slice_sample(n = 10000)
##Assess the results
ggplot(data_random |> select(x), aes(x=x)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data_random |> select(y), aes(x=y)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data_random |> select(z), aes(x=z)) +
	geom_histogram() +
	xlim(-radius, radius)
ggplot(data_random |> select(longitude), aes(x=longitude)) +
	geom_histogram() +
	xlim(-180, 180)
ggplot(data_random |> select(latitude), aes(x=latitude)) +
	geom_histogram() +
	xlim(-90, 90)
ggplot(data_random, aes(x=longitude, y=latitude)) +
	geom_point()

##Get the continents
world_map <- getMap(resolution='less islands')
continent <- SpatialPoints(as.matrix(data_random |> select(longitude, latitude)), proj4string=CRS(proj4string(world_map))) |> over(world_map) |> pull(continent)
data_random$continent <- as.character(continent)
data_random <- data_random |> mutate(continent = replace_na(continent, "Continentless"))
data_random <- data_random |> mutate(continent = factor(data_random |> pull(continent), levels = c("Africa", "Eurasia", "North America", "South America", "Antarctica", "Australia", "Continentless")))
#Continents, no water or smth
data_solid <- data_random |> filter(continent != "Continentless")
##Assess the results
plot_random <- ggplot(data_random, aes(x=longitude, y=latitude, color=continent)) +
	geom_point() +
	scale_colour_manual(values = earth_pal) +
	theme_classic()
ggplot(data_solid, aes(x=longitude, y=latitude, color=continent)) +
	geom_point() +
	scale_colour_manual(values = earth_pal) +
	theme_classic()

##Save the results
write_tsv(data_random, "...\\output\\1_geopoints.tsv")

#Check
plot_random + theme_void()