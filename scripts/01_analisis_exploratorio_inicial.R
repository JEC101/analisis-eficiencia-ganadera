# ============================================================================
# Análisis Exploratorio - Sistemas Ganaderos
# Autor: [Tu Nombre]
# Fecha: 2025-01-XX
# ============================================================================

# Cargar librerías
library(tidyverse)

# Crear datos de prueba
set.seed(123)

datos_prueba <- tibble(
  establecimiento = paste0("Campo_", 1:30),
  carga_animal = rnorm(30, mean = 1.2, sd = 0.3),
  ganancia_diaria = rnorm(30, mean = 0.8, sd = 0.15),
  eficiencia_conversion = rnorm(30, mean = 7.5, sd = 1.2),
  sistema = sample(c("Intensivo", "Semi-intensivo", "Extensivo"), 30, replace = TRUE)
)

# Ver datos
print(datos_prueba)
summary(datos_prueba)

# Estadísticas por grupo
datos_prueba %>%
  group_by(sistema) %>%
  summarise(
    n = n(),
    carga_promedio = mean(carga_animal),
    ganancia_promedio = mean(ganancia_diaria),
    sd_ganancia = sd(ganancia_diaria)
  )

# Visualización
ggplot(datos_prueba, aes(x = sistema, y = ganancia_diaria, fill = sistema)) +
  geom_boxplot() +
  labs(
    title = "Ganancia Diaria de Peso por Sistema Productivo",
    x = "Sistema",
    y = "Ganancia Diaria (kg/día)",
    caption = "Datos simulados - Proyecto preparatorio Maestría UBA"
  ) +
  theme_minimal()

# Guardar gráfico
ggsave("outputs/boxplot_ganancia_sistemas.png", width = 8, height = 6)

# Regresión simple
modelo <- lm(ganancia_diaria ~ carga_animal + sistema, data = datos_prueba)
summary(modelo)
