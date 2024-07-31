# Determinar:
# Tensão de Escoamento
# Tensão máxima
# Tensão de Ruptura
# Alongamento
# Massa linear
# Diagrama tensão x deformação

library(ggplot2)

data_path <- "data.csv"

# Importação do arquivo, do clipboard para um dataframe do R
get_from_clipboard <- FALSE
if (get_from_clipboard) {
  df <- read.table(file = "clipboard", sep = "\t", header = TRUE)
}
write_into_csv <- FALSE
# Dos dados salvos em df, salvá-los em csv, para facilitar importação
if (write_into_csv) {
write.csv(df, file = data_path, row.names=FALSE) # nolint
}

# Ask user if he wants to read the data. This  prevents reading accidentaly
get_from_csv <- TRUE
if (get_from_csv) {
  df <- read.csv(data_path)
  cat("Data from csv file read.\n")
}

# Constantes e Valores anotados
nominal_diameter <- 12.5 #mm
mass <- 545.8 #g
l_total <- 600 #mm
l_exp <- 500 #mm
mass_esp <- 0.00785 #g/mm3 #nolint

# Cálculo da Massa Linear
linear_mass <- mass / l_total

effective_diameter_function <- function(M, gamma_esp, L) { #nolint
  d <- 2 * sqrt(M / (pi * gamma_esp * L))
  return(d)
}

effective_diameter <- effective_diameter_function(
  M = mass,
  gamma_esp = mass_esp,
  L = l_total
)

print(effective_diameter)

# Obter tensão
(pi * nominal_diameter ^ 2) / 4
area_section <- mass / (mass_esp * l_total)
df["tensao_MPa"] <- df["forca_N"] / area_section

# Desenhar gráfico

max_value <- c(max(df$tensao_MPa), max(df$deformacao_mm))
plot_breaks_x <- seq(from = 0, to = max_value[2], by = .1)
plot_breaks_y <- seq(from = 0, to = max_value[1], by = (max_value[1]) / 10)

plot <- ggplot(df, aes(x = deformacao_mm, y = tensao_MPa))

for (y in plot_breaks_y) {
  plot <- plot + geom_hline(yintercept = y, color = "grey")
}

# Pegar dados presentes apenas no regime elástico
i <- 1
n_rows <- nrow(df)
elastic_df <- data.frame()

# Aqui o código analisa se o próximo valor de tensão é superior ao anterior.
# Se não, assume-se que não há mais regime elástico.
while (i < n_rows && df$tensao_MPa[i + 1] > df$tensao_MPa[i]) {
  elastic_df <- rbind(elastic_df, df[i, ])
  i <- i + 1
}

# A tensão de escoamento é a tensão máxima no regime elástico
tensao_esc <- max(elastic_df$tensao_MPa)
tensao_esc

# Calcular coeficiente de elasticidade
elastic_lm <- lm(tensao_MPa ~ deformacao_mm, data = elastic_df)
elastic_coef <- elastic_lm$coefficients[2]
y_cross_value <- elastic_lm$coefficients[1]
elastic_coef

plot <- plot +
  theme_classic() +
  geom_point(size = .1, col = "#000000", aes(color = "Data points")) +
  scale_x_continuous(limits = c(0, max_value[2]),
                     breaks = plot_breaks_x) +
  scale_y_continuous(limits = c(0, max_value[1]),
                     breaks = plot_breaks_y) +
  geom_abline(slope = elastic_coef,
              intercept = y_cross_value,
              color = "red") + # Adiciona a reta de elasticidade, em vermelho
  labs(x = "deformação (mm)",
       y = "Tensão (MPa)")

print(plot)


get_zoom_plot <- function(x0, x1, dframe) {
  n_rows <- nrow(dframe)
  zoom_df <- data.frame()

  for (n in seq(n_rows)) {
    if (x1 >= dframe$deformacao_mm[n] && dframe$deformacao_mm[n] >= x0) { #nolint
      zoom_df <- rbind(zoom_df, dframe[n, ])
    }
  }

  zoom_plot <- ggplot() +
    geom_point(data = zoom_df,
               aes(x = zoom_df$deformacao_mm, y = zoom_df$tensao_MPa)) +
    theme_classic()

  return(zoom_plot)
}

# Observar o limite de elasticidade, escolhendo pontos específicos
zoom_plot_elastic <- get_zoom_plot(.15, .2, df)


# análise no limite de ruptura
zoom_plot_rup <- get_zoom_plot(2.8, 3.1, df)


tensao_max <- max(df[df$deformacao_mm != max(df$deformacao_mm), ]$tensao_MPa)
tensao_rup <- tail(df, n = 1)$tensao_MPa

# Cáculo do Alongamento
calc_alongamento <- function(L0, Lf) { #nolint
  return((100 * (Lf - L0) / L0))
}

alongamento <- calc_alongamento(L0 = 120, Lf = 138.1)


print_results <- function() {
  cat(rep("-", 20), "\n")
  cat("Tensão de Escoamento (MPa):", tensao_max, "\n")
  cat("Tensão Máxima (MPa):", tensao_max, "\n")
  cat("Tensão de Ruptura (MPa):", tensao_rup, "\n")
  cat("Alongamento (%):", alongamento, "\n")
  cat("Massa Linear (g/mm):", linear_mass, "\n")
  cat("Módulo de Elasticidade")
  cat(rep("-", 20), "\n")
  ggsave(file = "Main_Plot.png", plot = plot)
  ggsave(file = "Zoom_Plot.png", plot = zoom_plot_elastic)
}
print_results()
