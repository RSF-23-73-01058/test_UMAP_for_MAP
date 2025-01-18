library(tidyverse)
library(ggridges)

###Paths to read and write should inserted instead of ".../output/..", ".../pics/Figure_4.tiff", etc

##Path
path_excluding <- ".../output/distance_ratio/"
path_including <- ".../output/distance_ratio_covered_fraq3/"


##Import the data, SEE for example: https://forum.posit.co/t/how-to-import-multiple-csv-files/119449/2
#Excluding
list_of_files_excluding <- list.files(path = path_excluding,
                            recursive = TRUE,
                            pattern = "\\.tsv$",
                            full.names = TRUE)
#Including
list_of_files_including <- list.files(path = path_including,
                            recursive = TRUE,
                            pattern = "\\.tsv$",
                            full.names = TRUE)

data_raw_excluding <- read_tsv(list_of_files_excluding, id = "file_name", col_names = FALSE)
data_raw_including <- read_tsv(list_of_files_including, id = "file_name", col_names = FALSE)

##Categorize records
#Excluding
data_excluding <- data_raw_excluding |> mutate(file_name = str_replace(file_name, path_excluding, "")) |>
					rename(value = X1) |>
					mutate(type = if_else(str_detect(file_name, "inside_not_"), "outer", "inner"),
						   number = str_match(file_name, "[:digit:]{1,3}") ) |>
					mutate(number = as.integer(number)) |>
					mutate(number_fct = fct_reorder(as.factor(number), number)) |> 
					arrange(number) |>
					mutate(group = "excluding")
#Including
data_including <- data_raw_including |> mutate(file_name = str_replace(file_name, path_including, "")) |>
					rename(value = X1) |>
					mutate(type = if_else(str_detect(file_name, "inside_not_"), "outer", "inner"),
						   number = str_match(file_name, "[:digit:]{1,3}") ) |>
					mutate(number = as.integer(number)) |>
					mutate(number_fct = fct_reorder(as.factor(number), number)) |> 
					arrange(number) |>
					mutate(group = "including")
#all
data <- bind_rows(data_excluding, data_including)

##Mean value
mean_value_excluding <- mean(data_excluding |> pull(value))
mean_value_including <- mean(data_including |> pull(value))
mean_value <- mean(data |> pull(value))

##Plot the results
#Excluding
excluding <- ggplot(data_excluding, aes(x = value, y = number_fct, fill = type)) +
				geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
				scale_fill_manual(values = c("gray60", "gray85")) +
				ylab("Number of neighbors") +
				scale_x_log10(name = "Distance ratio", breaks = c(.1, .5, 1, 5, 10), limits = c(.1, 10)) +
				theme_ridges() +
				geom_vline(xintercept = mean_value_excluding, linetype = "dashed") +
				theme(legend.position = "none")
excluding
ggsave(".../pics/Figure_4.tiff", plot = excluding, width = 7, height = 5, units = "in", dpi = 600, compression = "lzw", scale = 1)
#Including
including <- ggplot(data_including, aes(x = value, y = number_fct, fill = type)) +
				geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
				scale_fill_manual(values = c("gray60", "gray85")) +
				ylab("Number of neighbors") +
				scale_x_log10(name = "Distance ratio", breaks = c(.1, .5, 1, 5, 10), limits = c(.1, 10)) +
				theme_ridges() +
				geom_vline(xintercept = mean_value_including, linetype = "dashed") +
				theme(legend.position = "none")
including
ggsave(".../pics/Figure_5.tiff", plot = including, width = 7, height = 5, units = "in", dpi = 600, compression = "lzw", scale = 1)
#All
all <- ggplot(data, aes(x = value, y = number_fct, fill = group)) +
				geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
				scale_fill_manual(values = c("gray60", "gray85")) +
				ylab("Number of neighbors") +
				scale_x_log10(name = "Distance ratio", breaks = c(.1, .5, 1, 5, 10), limits = c(.1, 10)) +
				theme_ridges() +
				geom_vline(xintercept = mean_value, linetype = "dashed") +
				theme(legend.position = "none")
all
ggsave(".../pics/Figure_6.tiff", plot = all, width = 7, height = 5, units = "in", dpi = 600, compression = "lzw", scale = 1)