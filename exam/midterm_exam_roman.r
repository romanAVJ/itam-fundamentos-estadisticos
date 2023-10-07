################################################################################
# roman alberto velez jimenez   cu: 165462
# 7 oct 2023
# midterm exam
################################################################################
# libraries
library(tidyverse)
library(nullabor)

SEED  <- 8
#### hypothesis test  ####
# read data 
df_shoes  <- read_csv("datos/zapatos-1.csv")

# look data #
df_shoes |> glimpse() 

#### part 1: visual hypothesis test using lineup test
set.seed(SEED)
lineup(
    method = null_permute("material"),
    true = df_shoes,
    n = 20
) |> 
mutate(
    material = factor(material)
) |> 
ggplot(aes(material, desgaste, color = material, group = )) +
    geom_boxplot() +
    facet_wrap(~.sample, nrow = 4) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# decrypt
decrypt("sD0f gCdC En JP2EdEPn 8F")

# answer: we have a significance of 1/20: 0.05

#### part 2: create permutation test between means of materials
# H0: there is no difference between the means of the materials
# H1: there is a difference between the means of the materials, bothways

# permutation test
df_material_perm  <- lineup(
    method = null_permute("material"),
    true = df_shoes,
    n = 1000
    ) |> 
    mutate( material = str_glue("material_{material}")) |>
    group_by(material, .sample) |>
    summarise(mean_wear = mean(desgaste)) |>
    pivot_wider(
        names_from = material,
        values_from = mean_wear
    ) |> 
    mutate(diff_means = material_1 - material_2)

# get original diff means
stat_diff_means  <- df_shoes |> 
    mutate(material = str_glue("material_{material}")) |>
    group_by(material) |>
    summarise(mean_wear = mean(desgaste)) |>
    pivot_wider(
        names_from = material,
        values_from = mean_wear
    ) |> 
    mutate(diff_means = material_1 - material_2) |>
    pull(diff_means)

# get empiricial p-value
ecdf_diff_means  <- ecdf(df_material_perm$diff_means)
stat_diff  <- ecdf_diff_means(stat_diff_means)
p_value  <- 2 * min(stat_diff, 1 - stat_diff)
cat("p-value: ", p_value, "\n")

# plot diff means in histogram and add density plot add original diff means
df_material_perm |> 
    ggplot(aes(diff_means)) +
    geom_rect(aes(xmin = -Inf, xmax = stat_diff_means, ymin = 0, ymax = Inf), fill = "pink", alpha = 0.01) +
    geom_vline(xintercept = stat_diff_means, color = "darkred", size = 1.5) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black", alpha = 0.5) +
    geom_density(color = "blue") +
    annotate("text", x = -2, y = 0.4, label = str_glue("pval: {p_value}"), color = "darkred")
    theme_minimal()

#### part 3: new info added
df_shoes2  <- read_csv("datos/zapatos-2.csv")
df_shoes2 |> glimpse()

# do permutation test controlling by kid
# df_material_perm2  <- 
df_shoes2 |> 
    group_split(niño) |> 
    map_df(~ lineup(null_permute("material"), .x, n = 100)) |> 
    arrange(niño, .sample, material)


