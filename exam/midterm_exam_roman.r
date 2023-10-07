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

# get estimate of combined ratio of votes #
# step 0: get total votes by polling booth
df_muestra_by_polling_booth  <- df_muestra |> 
    group_by(ID_CASILLA, ESTRATO) |>
    summarise(across(c(SI, NO, NULOS, TOTAL), sum), num_obs = n())  |> 
    arrange(ESTRATO, ID_CASILLA) |> 
    group_by(ESTRATO) |>
    mutate(num_polling_booths = n()) |> 
    ungroup()
df_muestra_by_polling_booth

# step 1: get Nh / nh
df_polling_booths_total  <- df_computos |> count(ESTRATO) |> rename(Nh = n)
df_polling_booths_selected  <- df_muestra_by_polling_booth |> group_by(ESTRATO) |> summarise(nh = sum(num_obs))
df_polling_booths  <- df_polling_booths_total |> 
    inner_join(df_polling_booths_selected, by = "ESTRATO") |> 
    mutate(Nh_nh = Nh / nh) |> 
    select(ESTRATO, Nh_nh)

df_polling_booths

# step 2: get total votes expanded, i.e. sum(Nh_nh * sum_{ID_CASILLA}(TOTAL))
df_total_votes  <- df_muestra_by_polling_booth |> 
    inner_join(df_polling_booths, by = "ESTRATO") |> 
    mutate(weighted_total_votes = TOTAL * Nh_nh)

df_total_votes

total_expanded_votes  <- sum(df_total_votes$weighted_total_votes)

# step 3: get total votes expanded by option
df_total_votes_by_option  <- df_muestra_by_polling_booth |> 
    pivot_longer(c("SI", "NO", "NULOS"), names_to = "OPCION", values_to = "VOTOS") |> 
    inner_join(df_polling_booths, by = "ESTRATO") |>
    mutate(weighted_total_votes = VOTOS * Nh_nh)

df_total_votes_by_option

# step 4: get estimate of combined ratio of votes
table_combined_ratios  <- df_total_votes_by_option |> 
    group_by(OPCION) |> 
    summarise(combined_ratio = sum(weighted_total_votes) / total_expanded_votes)
# print table in html with percentage format
knitr::kable(
    table_combined_ratios, digits = 4, 
    format.args = list(big.mark = ",", decimal.mark = ".", format = "f"), 
    caption = "Tabla de razones combinadas de votos en la muestra"
    )

# create a bootstrap function to get the combined ratio of votes #
get_combined_ratio  <- function(df, df_polling_booths){
    # step 1: get total votes expanded
    total_expanded_votes  <- df |> 
        inner_join(df_polling_booths, by = "ESTRATO") |> 
        mutate(weighted_total_votes = TOTAL * Nh_nh) |> 
        pull(weighted_total_votes) |>
        sum()

    # step 2: get combined ratios expanded by option
    df_total_votes_by_option  <- df |> 
        pivot_longer(c("SI", "NO", "NULOS"), names_to = "OPCION", values_to = "VOTOS") |> 
        inner_join(df_polling_booths, by = "ESTRATO") |>
        mutate(weighted_total_votes = VOTOS * Nh_nh) |> 
        group_by(OPCION) |>
        summarise(combined_ratio = sum(weighted_total_votes) / total_expanded_votes)

    return(df_total_votes_by_option)
}
# look if function works
df_total_votes_by_optionv2  <- get_combined_ratio(df_muestra_by_polling_booth, df_polling_booths) # is ok!

# create bootstrap function
svy_boot  <- function(df_sample, df_polling){
    # get sample of polling booths BY ESTRATO
    df_sample_by_estrato  <- df_sample |> 
        group_split(ESTRATO) |> 
        map_df(~ slice_sample(.x, n = num_polling_booths - 1, replace = TRUE))
        # slice_sample(n = num_polling_booths - 1, replace = TRUE) |> # rao & wu 
        # ungroup()
    
    # get combined ratio of votes
    df_combined_ratio  <- get_combined_ratio(df_sample_by_estrato, df_polling)
    return(df_combined_ratio)
}

# generate bootstrap samples
set.seed(SEED)
N_BOOT  <- 1000
df_bootstrap_combined_ratio  <- map_df(1:N_BOOT, ~ svy_boot(df_muestra_by_polling_booth, df_polling_booths) |> 
    rename(BOOT = .x))

df_muestra_by_polling_booth|> 
        group_by(ESTRATO) |> 
        mutate(num_polling_booths = n())













