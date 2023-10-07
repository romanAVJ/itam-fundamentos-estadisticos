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
set.seed(SEED)
df_material_perm  <- lineup(
    method = null_permute("material"),
    true = df_shoes,
    n = 1000
    ) |> 
    mutate( material = str_glue("material_{material}")) |>
    group_by(material, .sample) |>
    summarise(mean_wear = mean(desgaste)) |>
    arrange(.sample, material) |>
    group_by(.sample) |>
    summarise(diff_mean_wear = diff(mean_wear)) 

# get original diff means
stat_diff_means  <- df_shoes |> 
    mutate(material = str_glue("material_{material}")) |>
    group_by(material) |>
    summarise(mean_wear = mean(desgaste)) |>
    arrange(material) |>
    pull(mean_wear) |> 
    diff()

# get empiricial p-value
ecdf_diff_means  <- ecdf(df_material_perm$diff_mean_wear)
stat_diff  <- ecdf_diff_means(stat_diff_means)
p_value  <- 2 * min(stat_diff, 1 - stat_diff)
cat("p-value: ", p_value, "\n")

# plot diff means in histogram and add density plot add original diff means
df_material_perm |> 
    ggplot(aes(diff_mean_wear)) +
    geom_rect(aes(xmax = Inf, xmin = stat_diff_means, ymin = 0, ymax = Inf), fill = "pink", alpha = 0.01) +
    geom_vline(xintercept = stat_diff_means, color = "darkred", size = 1.5) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black", alpha = 0.5) +
    geom_density(color = "blue") +
    annotate("text", x = -2, y = 0.4, label = str_glue("pval: {p_value}"), color = "darkred") +
    theme_minimal() +
    ggtitle("Prueba de permutación") +
    labs(x = "Diferencia de medias", y = "Densidad")

#### part 3: new info added
df_shoes2  <- read_csv("datos/zapatos-2.csv")
df_shoes2 |> glimpse()

# do permutation test controlling by kid
set.seed(SEED)
df_material_perm2 <- df_shoes2 |> 
    group_split(niño) |> 
    map_df(~ lineup(null_permute("material"), true = .x, n = 1000)) |> 
    mutate(
        material = str_glue("material_{material}")
        ) |> 
    group_by(material, .sample) |> 
    summarise(mean_wear = mean(desgaste)) |> 
    group_by(.sample) |>
    arrange(.sample, material) |> 
    summarise(diff_mean_wear = diff(mean_wear)) 

# get original diff means
stat_diff_means  <- df_shoes2 |> 
    mutate(material = str_glue("material_{material}")) |>
    group_by(material) |>
    summarise(mean_wear = mean(desgaste)) |>
    arrange(material) |>
    pull(mean_wear) |> 
    diff()

# get empiricial p-value
ecdf_diff_means  <- ecdf(df_material_perm2$diff_mean_wear)
stat_diff  <- ecdf_diff_means(stat_diff_means)
p_value  <- 2 * min(stat_diff, 1 - stat_diff)
cat("p-value: ", p_value, "\n")

# plot diff means in histogram and add density plot add original diff means
df_material_perm2 |> 
    ggplot(aes(diff_mean_wear)) +
    geom_rect(aes(xmax = Inf, xmin = stat_diff_means, ymin = 0, ymax = Inf), fill = "pink", alpha = 0.01) +
    geom_vline(xintercept = stat_diff_means, color = "darkred", size = 1.5) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black", alpha = 0.5) +
    geom_density(color = "blue") +
    annotate("text", x = 0.3, y = 2, label = str_glue("pval: {round(p_value, 4)}"), color = "darkred") +
    theme_minimal() +
    ggtitle("Prueba de permutación", "Control por niño") +
    labs(x = "Diferencia en medias", y = "Densidad")

#### bootstrap ####
# get survey data
# preprocesamiento de tablas de datos
df_computos <- read_delim("datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
    delim = "|", escape_double = FALSE, trim_ws = TRUE, quote = "\'",
    skip = 5) |> 
    rename(ID = CLAVE_MRCP) |> 
    mutate(ESTRATO = str_c(str_pad(ID_ENTIDAD, 2, pad = "0"), 
                            str_pad(ID_DISTRITO_FEDERAL, 2, pad = "0")),
            LISTA_NOMINAL = LISTA_NOMINAL_MRCP, 
            TOTAL = TOTAL_OPINIONES)

df_muestra <- read_delim("https://ine.mx/wp-content/uploads/2021/08/Conteos-ConsPop21-Lista-MuestraCalculo.txt", delim = "|", skip = 1) |> 
  mutate(
    ID_ESTADO = str_pad(ID_ESTADO, 2, pad = "0"),
    SECCION = str_pad(SECCION, 4, pad = "0"),
    ID_CASILLA = str_pad(ID_CASILLA, 2, pad = "0"),
    ID = str_c(ID_ESTADO, SECCION, TIPO_CASILLA, ID_CASILLA)
    ) |> 
  group_by(ESTRATO) |> 
  mutate(n = n()) |> 
  ungroup()

# get estimate of combined ratio of votes
df_combined_ratio |> 

















