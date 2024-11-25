#calculo del area de dos funciones en un intervalo

# Cargar las librerías necesarias
library(ggplot2)

# Definir el intervalo y las funciones
x_vals <- seq(0, pi/4, length.out = 100)
sin_vals <- sin(x_vals)
cos_vals <- cos(x_vals)

# Crear un dataframe para ggplot
data <- data.frame(
  x = x_vals,
  sin = sin_vals,
  cos = cos_vals
)

# Crear un dataframe para sombrear el área
area_data <- data.frame(
  x = c(x_vals, rev(x_vals)),
  y = c(sin_vals, rev(cos_vals))
)

# Gráfico
ggplot() +
  # Área sombreada
  geom_polygon(data = area_data, aes(x = x, y = y), fill = "lightblue", alpha = 0.6) +
  # Curva sin(x)
  geom_line(data = data, aes(x = x, y = sin), color = "red", size = 1.2, linetype = "dashed") +
  # Curva cos(x)
  geom_line(data = data, aes(x = x, y = cos), color = "blue", size = 1.2) +
  # Estilo y etiquetas
  labs(
    title = "Área entre las curvas sin(x) y cos(x)",
    subtitle = "Intervalo: [0, π/4]",
    x = "x",
    y = "y",
    fill = "Leyenda"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  # Agregar etiquetas de leyenda
  annotate("text", x = pi/8, y = 0.8, label = "Área sombreada", color = "darkblue", size = 4) +
  annotate("text", x = 0.6, y = 0.3, label = "y = sin(x)", color = "red", size = 4) +
  annotate("text", x = 0.6, y = 0.95, label = "y = cos(x)", color = "blue", size = 4)
