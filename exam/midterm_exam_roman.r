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
# i did not see any difference between the materials!!
# i would not reject the null hypothesis that there is no difference between the materials

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
# step 1: get Nh / nh
df_polling_booths_total  <- df_computos |> count(ESTRATO) |> rename(Nh = n)
df_polling_booths_selected  <- df_muestra |> count(ESTRATO) |> rename(nh = n)
df_polling_booths  <- df_polling_booths_total |> 
    inner_join(df_polling_booths_selected, by = "ESTRATO") |> 
    mutate(Nh_nh = Nh / nh) |> 
    select(ESTRATO, Nh_nh)

df_polling_booths

# step 2: get total votes expanded, i.e. sum(Nh_nh * sum_{ID}(TOTAL))
df_total_votes  <- df_muestra |> 
    inner_join(df_polling_booths, by = "ESTRATO") |> 
    mutate(weighted_total_votes = TOTAL * Nh_nh)

df_total_votes

total_expanded_votes  <- sum(df_total_votes$weighted_total_votes)

# step 3: get total votes expanded by option
df_total_votes_by_option  <- df_muestra |> 
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
get_combined_ratio  <- function(df_population, df_sample){
    # setp 0: get Nh / nh
    df_polling_booths_total  <- df_population |> count(ESTRATO) |> rename(Nh = n)
    df_polling_booths_selected  <- df_sample |> count(ESTRATO) |> rename(nh = n)
    df_polling_booths  <- df_polling_booths_total |> 
        inner_join(df_polling_booths_selected, by = "ESTRATO") |> 
        mutate(Nh_nh = Nh / nh) |> 
        select(ESTRATO, Nh_nh)

    # step 1: get total votes expanded
    total_expanded_votes  <- df_sample |> 
        inner_join(df_polling_booths, by = "ESTRATO") |> 
        mutate(weighted_total_votes = TOTAL * Nh_nh) |> 
        pull(weighted_total_votes) |>
        sum()

    # step 2: get combined ratios expanded by option
    df_total_votes_by_option  <- df_sample |> 
        pivot_longer(c("SI", "NO", "NULOS"), names_to = "OPCION", values_to = "VOTOS") |> 
        inner_join(df_polling_booths, by = "ESTRATO") |>
        mutate(weighted_total_votes = VOTOS * Nh_nh) |> 
        group_by(OPCION) |>
        summarise(combined_ratio = sum(weighted_total_votes) / total_expanded_votes)

    return(df_total_votes_by_option)
}
# look if function works
df_total_votes_by_optionv2  <- get_combined_ratio(df_computos, df_muestra) # is ok!
df_total_votes_by_optionv2

# create bootstrap function
svy_boot  <- function(df_population, df_sample){
    # get sample of polling booths BY ESTRATO
    df_sample_by_estrato  <- df_sample |> 
        group_split(ESTRATO) |> 
        map_df(~ slice_sample(
            .x, 
            n = first(.x$n), 
            replace = TRUE)
        )

    # get combined ratio of votes
    df_combined_ratio  <- get_combined_ratio(df_population, df_sample_by_estrato)
    return(df_combined_ratio)
}

# generate bootstrap samples
set.seed(SEED)
N_BOOT  <- 1000
df_bootstrap_combined_ratio  <- map_dfr(1:N_BOOT, ~ svy_boot(df_computos, df_muestra))
df_bootstrap_combined_ratio
# get normal confidence interval at 95%
ALPHA  <- 0.05
Z_ALPHA  <- qnorm(1 - ALPHA / 2)

# get confidence interval
table_se_combined_ratio  <- df_bootstrap_combined_ratio |> 
    group_by(OPCION) |> 
    summarise(
        mean = mean(combined_ratio), 
        sd = sd(combined_ratio), 
        lower = mean - Z_ALPHA * sd,
        upper = mean + Z_ALPHA * sd
    ) |> 
    mutate(longitud = upper - lower)

table_se_combined_ratio |> 
    knitr::kable(
        digits = 4, 
        format.args = list(big.mark = ",", decimal.mark = ".", format = "f"), 
        caption = "Intervalos de confianza al 95% para las razones combinadas de votos"
        )


# get observed ratios
table_observed_ratios  <- df_computos |> 
    rename(SI = OPINION_SI, NO = OPINION_NO) |>
    pivot_longer(c("SI", "NO", "NULOS"), names_to = "OPCION", values_to = "VOTOS") |> 
    group_by(OPCION) |>
    summarise(VOTOS = sum(VOTOS)) |> 
    ungroup() |>
    mutate(
        PERCENT_VOTOS = VOTOS / sum(VOTOS),
    )
table_observed_ratios

# get relative bais
table_se_combined_ratio |> 
    inner_join(table_observed_ratios, by = "OPCION") |> 
    mutate(
        relative_bais = (mean - PERCENT_VOTOS) / sd
    ) |> 
    knitr::kable(
        digits = 4, 
        format.args = list(big.mark = ",", decimal.mark = ".", format = "f"), 
        caption = "Tabla de sesgo relativo de las razones combinadas de votos"
        )

# plot confidence intervals. use error bars with lower and upper. use percent_votos as point estimate
table_se_combined_ratio |> 
    inner_join(table_observed_ratios, by = "OPCION") |> 
    ggplot(aes(x = OPCION, y = PERCENT_VOTOS, ymin = lower, ymax = upper)) +
    geom_errorbar(width = 0.2) +
    geom_point(color = "darkred") +
    theme_minimal() +
    ggtitle("Intervalos de confianza al 95% para las razones combinadas de votos") +
    labs(x = "Opción", y = "Porcentaje de votos") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    theme(legend.position = "none")

#### Calibracion ####
# from df_muestra get 50 samples of size M = length(df_muestra)
# and apply svy_boot to each sample
set.seed(SEED)
N_BOOT  <- 1000
N_CI  <- 50
M  <- 1745 # same as the sample size of df_muestra

applyboot  <- function(df_sample, df_population, num_boot=1000){
    # get combined ratio of votes
    df_combined_ratio  <- map_dfr(1:num_boot, ~ svy_boot(df_population, df_sample))
    return(df_combined_ratio)
}

df_bootstrap_combined_ratio_calibration  <- map_df(1:N_CI, ~ {
    # apply svy_boot to each sample of size M per polling booth
    df_sample_by_poll  <- df_computos |>
        rename(SI = OPINION_SI, NO = OPINION_NO) |> 
        slice_sample(n = M, replace = TRUE) |>
        # get num of polling booths by ESTRATO
        group_by(ESTRATO) |> 
        mutate(n = n()) |> 
        ungroup() |>
        # apply svy_boot to each polling booth
        applyboot(df_computos, num_boot=N_BOOT) |> 
        group_by(OPCION) |> 
        summarise(
            mean = mean(combined_ratio), 
            sd = sd(combined_ratio), 
            lower = mean - Z_ALPHA * sd, 
            upper = mean + Z_ALPHA * sd
        )
    return(df_sample_by_poll)},
    .id = 'rep'
)

df_bootstrap_combined_ratio_calibration

# get the number of times the observed ratio is in the confidence interval
table_calibration  <- df_bootstrap_combined_ratio_calibration |> 
    inner_join(table_observed_ratios, by = "OPCION") |> 
    mutate(
        in_ci = ifelse(lower <= PERCENT_VOTOS & PERCENT_VOTOS <= upper, 1, 0)
    ) |> 
    group_by(OPCION) |> 
    summarise(
        total = n(),
        in_ci = sum(in_ci),
        not_in_ci = total - sum(in_ci),
    ) |> 
    mutate(
        percent_in_ci = in_ci / total,
        percent_not_in_ci = not_in_ci / total
    )
table_calibration

# plot calibration with error bars. add obsered ratio as point estimate. color not_in_ci in red
table_aux  <- df_bootstrap_combined_ratio_calibration |> 
    inner_join(table_observed_ratios, by = "OPCION") |> 
    mutate(
        in_ci = ifelse(lower <= PERCENT_VOTOS & PERCENT_VOTOS <= upper, "yes", "no"),
        rep = factor(as.integer(rep), ordered = TRUE)
    ) 
table_aux
# graph for each OPCION group
split(table_aux, table_aux$OPCION) |>
    map(~ {
        ggplot(.x, aes(x = rep, ymin = lower, ymax = upper, color = in_ci)) +
            geom_errorbar(width = 0.2) +
            geom_hline(aes(yintercept = PERCENT_VOTOS), color = "gray30", linetype = 2) +
            theme_minimal() +
            ggtitle(str_glue("Calibración de los intervalos de confianza al 95% para {first(.x$OPCION)}")) +
            labs(x = "Opción", y = "Porcentaje de votos") +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            scale_color_manual(values = c("yes" = "darkgreen", "no" = "darkred")) +
            theme(legend.position = "none")
    }) 

# I observe that the intervals are not well calibrated. The observed ratio is not in the confidence interval
# This could be because the sample is baised by Null votes which are not observed (people that didn't went to vote). I would try to do a stratified sampling by null votes
# observe distributions of Opcion (violin plot + boxplot)
df_bootstrap_combined_ratio_calibration |> 
    ggplot(aes(x = OPCION, y = mean)) +
    geom_violin(aes(fill = OPCION), alpha = 0.5) +
    geom_boxplot(alpha = 0.5) +
    theme_minimal() +
    ggtitle("Distribución de las razones combinadas de votos") +
    labs(x = "Opción", y = "Porcentaje de votos") +
    coord_flip() +
    theme(legend.position = "none")

# do a qq normal plot for each option
df_bootstrap_combined_ratio_calibration |> 
    group_split(OPCION) |> 
    map(~ {
        .x |> 
            ggplot(aes(sample = mean)) +
            geom_qq() + geom_qq_line() +
            theme_minimal() +
            ggtitle(str_glue("QQ-plot para {first(.x$OPCION)}")) +
            labs(x = "Cuantiles teóricos", y = "Cuantiles empíricos") +
            theme(legend.position = "none")
    })



# actually they do not look very baised

#### exploratory analysis for null votes ####
# from the df_muestra, do an eda for null votes
df_muestra |> glimpse()

#### explore by percentage by poll booth 
df_muestra |> 
    group_by(ESTRATO, ID) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.5) +
    geom_density(color = "blue") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Histograma de porcentaje de votos nulos", "Por casilla") +
    labs(x = "Porcentaje de votos nulos", y = "Densidad")


# is ok
df_muestra |> 
    group_by(ESTRATO, ID) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    geom_boxplot(fill = "steelblue", color = "black", alpha = 0.5) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Caja y brazos de porcentaje de votos nulos", "Por casilla") +
    labs(x = "Porcentaje de votos nulos", y = "") 

# i liked more this viz
df_muestra |> 
    group_by(ESTRATO, ID) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    stat_ecdf() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Distribución acumulada", "Por casilla") +
    labs(x = "Porcentaje de votos nulos", y = "") 

#### explore by strata
df_muestra |> 
    group_by(ESTRATO) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |>
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.5) +
    geom_density(color = "blue") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Histograma de porcentaje de votos nulos", "Por estrato") +
    labs(x = "Porcentaje de votos nulos", y = "Densidad")

# is ok
df_muestra |> 
    group_by(ESTRATO) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    geom_boxplot(fill = "steelblue", color = "black", alpha = 0.5) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Caja y brazos de porcentaje de votos nulos", "Por estrato") +
    labs(x = "Porcentaje de votos nulos", y = "")

# i liked more this viz
df_muestra |> 
    group_by(ESTRATO) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    ungroup() |>
    ggplot(aes(x = PERCENT_NULL)) +
    stat_ecdf() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Distribución acumulada", "Por estrato") +
    labs(x = "Porcentaje de votos nulos", y = "")

#### explore by type of polling booth
table_null_by_type_poll_booth  <- df_muestra |> 
    group_by(TIPO_CASILLA) |> 
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    )
knitr::kable(
    table_null_by_type_poll_booth, digits = 4, 
    format.args = list(big.mark = ",", decimal.mark = ".", format = "f"), 
    caption = "Tabla de porcentaje de votos nulos por tipo de casilla"
    )

#### by state
df_muestra |> 
    group_by(ENTIDAD) |> 
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |> 
    arrange(desc(ENTIDAD)) |>
    # col plot
    ggplot(aes(ENTIDAD, PERCENT_NULL)) +
    geom_col(fill = "steelblue", color = "black", alpha = 0.5) +
    geom_text(aes(label = scales::percent(PERCENT_NULL)), vjust = -0.5) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    ggtitle("Porcentaje de votos nulos por estado") +
    labs(x = "Estado", y = "Porcentaje de votos nulos") +
    coord_flip()

#### relationship between null votes and total votes (scatter + loess)
df_muestra |> 
    group_by(ESTRATO, ID) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL),
        TOTAL = sum(TOTAL)
    ) |> 
    ungroup() |> 
    ggplot(aes(x = TOTAL, y = PERCENT_NULL)) +
    geom_point(color = "#46b478", alpha = 0.5) +
    geom_smooth(method = "loess", color = "darkred", se = FALSE) +
    scale_x_continuous(labels = scales::comma, trans = "log2") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), trans = "sqrt") +
    theme_minimal() +
    ggtitle("Porcentaje de votos nulos vs total de votos", "Por casilla") +
    labs(x = "Total de votos", y = "Porcentaje de votos nulos")

# there is something weird with the tendency of the loess.
# looks like the simpson paradox. there are "groups" of casillas with downward tendency
# but the overall tendency is upward (see the loess)

# My shot is that there is more tendency to observe less null votes in casillas with more total votes
# by the law of large numbers
# Also by the fact that there is an ubserverd variable that is the number of voters which didnt went
# to vote and could reflect the real tendency of null votes


#### relationship between null votes and time (by minutes)
df_muestra |> 
    mutate(
        TIMESTAMP = str_c(ANIO, MES, DIA, HORA, MINUTOS, sep = "-") |> 
            lubridate::ymd_hm()
    ) |> 
    group_by(TIMESTAMP) |>
    summarise(
        PERCENT_NULL = sum(NULOS) / sum(TOTAL)
    ) |>
    ggplot(aes(x = TIMESTAMP, y = PERCENT_NULL)) +
    geom_point(color = "steelblue", alpha = 0.5) +
    geom_smooth(method = "loess", color = "darkred", se = FALSE) +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    ggtitle("Porcentaje de votos nulos vs tiempo", "Por casilla") +
    labs(x = "Tiempo", y = "Porcentaje de votos nulos")

# there doesnt seem to be a relationship between null votes and time but some outliers    

# notes about outliers #
# There are two big outlier groups: the ones which are zero and the ones bigger than 25% (to say a number)
# It could be that the ones which are zero are casillas with very few voters and the ones bigger than 25%
# are atypical casillas with a lot of null votes

# the second thing is that there are some states: cdmx, edo mex and veracruz who have an atypical behavior
# with respect to rest of the states

# about handling outliers #
# I would first analyze which states and poll booths have 0 null votes ore more than 25% null votes.
# For the first group, I would try to unbais by inputating the null votes with a sample of poll booths with percent of
# null votes greater than 0
# For the second group, I would try to do a separete analysis for those states and poll booths (the ones greater than 25%)

# Indepently of the method, it is true that this outliers of the "conteo rapido" bais the results. Maybe it will be appropiate
# to use a different statistical perspective, like bayesian inference, to handle this outliers with prior information.
# For example, we could use the results of the last election to inputate the null votes of the "conteo rapido" of this election
# and update the prior with the results of the "conteo rapido" of this election