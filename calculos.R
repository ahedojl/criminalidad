library(ggplot2)
library(readODS)
library(tidyverse)
library(kableExtra)
library(knitr)
library(DT)
library(rsconnect)
library(plotly)
library(patchwork)
library(bslib)
library(bsicons)
library(plotly)
library(viridis)
library(RColorBrewer)

# Tasa

tasa <- read_ods("~/Documentos/RStudio/Ertzaina/Datos/tasa.ods") %>%
  mutate(round(across(3:46, ~ .x), 2))

tasa_th <- tasa %>%
  slice(1:3, 24:26)

tasa_th_25 <- tasa_th %>%
  filter(Fecha == 2025)

tasa_th_24 <- tasa_th %>%
  filter(Fecha == 2024)

tasa_capitales <- tasa %>%
  slice(5, 6, 9, 28, 29, 32)

tasa_capitales_25 <- tasa_capitales %>%
  filter(Fecha == 2025)

tasa_capitales_24 <- tasa_capitales %>%
  filter(Fecha == 2024)

tasa_mun <- tasa %>%
  slice(5:23, 28:46)

tasa_mun_25 <- tasa_mun %>%
  filter(Fecha == 2025)

tasa_mun_24 <- tasa_mun %>%
  filter(Fecha == 2024)

#--------------------TOTAL-------------------------------

tasa_th_total <- tasa_th %>%
  select(1, 2, 46) %>%
  rename(Total = `TOTAL INFRACCIONES PENALES`)

tasa_th_total_25 <- tasa_th_total %>%
  filter(Fecha == 2025)

tasa_th_total_24 <- tasa_th_total %>%
  filter(Fecha == 2024)

tasa_capitales_total <- tasa_capitales %>%
  select(1, 2, 46) %>%
  rename(Total = `TOTAL INFRACCIONES PENALES`)

tasa_capitales_total_25 <- tasa_capitales_total %>%
  filter(Fecha == 2025)

tasa_capitales_total_24 <- tasa_capitales_total %>%
  filter(Fecha == 2024)

tasa_mun_total <- tasa_mun %>%
  select(1, 2, 46) %>%
  rename(Total = `TOTAL INFRACCIONES PENALES`)

tasa_mun_total_25 <- tasa_mun_total %>%
  filter(Fecha == 2025)

tasa_th_total_24 <- tasa_mun_total %>%
  filter(Fecha == 2024)

# Variacion

variacion <- read_ods("~/Documentos/RStudio/Ertzaina/Datos/variacion.ods") %>%
  mutate(round(across(2:45, ~ .x), 2))

variacion_th <- variacion %>%
  slice(1:3)

variacion_capitales <- variacion %>%
  slice(5, 6, 9)

variacion_mun <- variacion %>%
  slice(5:23)

variacion_th_total <- variacion_th %>%
  select(1, 45) %>%
  rename

variacion_capitales_total <- variacion_capitales %>%
  select(1, 45) %>%
  rename(Total = `TOTAL INFRACCIONES PENALES`)

variacion_mun_total <- variacion_mun %>%
  select(1, 45) %>%
  rename(Total = `TOTAL INFRACCIONES PENALES`)

ggplot(data = variacion_th_total) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - Total
  ),
  y = Total,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad Total",
       title = "Criminalidad Total - Variación 24-25"
  ) +
  # scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = Total,
                 label = paste0(Total,"%")
  ),
  size = 5,
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(#angle = 45,
    color = "black", 
    size = 15,
    vjust = 0.5,
    hjust = 0.5,
    face = "bold"
  ),
  axis.title = element_blank()
  )

th_total <- ggplot(data = tasa25_total) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - TOTAL
  ),
  y = TOTAL,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad Total",
       title = "Criminalidad Total - Tasa"
  ) +
  #scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = TOTAL,
                 label = TOTAL
  ),
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(#angle = 45,
    color = "black", 
    size = 10,
    vjust = 0.5,
    hjust = 0.5,
    face = "bold"
  ),
  axis.title = element_blank()
  ) +
  facet_wrap(~Fecha)

th_total

th_capitales <- ggplot(data = tasa25_total) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - TOTAL
  ),
  y = TOTAL,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad Total",
       title = "Criminalidad Total - Tasa"
  ) +
  #scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = TOTAL,
                 label = TOTAL
  ),
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(angle = 45,
    color = "black", 
    size = 10,
    vjust = 1,
    hjust = 1,
    face = "bold"
  ),
  axis.title = element_blank()
  ) +
  facet_wrap(~Fecha)

th_capitales

th_municipios_vertical <- ggplot(data = tasa25_total) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     TOTAL
  ),
  y = TOTAL,
  fill = TOTAL,
  #color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad Total",
       title = "Criminalidad Total 2025 - Tasa"
  ) +
  coord_flip() +
  scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = TOTAL,
                 label = TOTAL
  ),
  size = 10,
  vjust = 0.5,
  hjust = 1
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "darkblue",
                                  size = 30,
                                  face = "bold"
  ),
  axis.text.x = element_blank(),
  axis.text.y = element_text(#angle = 45,
    color = "black", 
    size = 20,
    vjust = 0.5,
    hjust = 1,
    face = "bold"),
  axis.title = element_blank()
  )

th_municipios_vertical

th_municipios

#------------------------Seguridad02-------------------
tasa <- read_ods("~/Documentos/RStudio/Ertzaina/Datos/tasa.ods") %>%
  mutate(round(across(3:46, ~ .x), 2))

tasa_th <- tasa %>%
  slice(1:3, 24:26)

tasa_th_25 <- tasa_th %>%
  filter(Fecha == 2025)

tasa_th_24 <- tasa_th %>%
  filter(Fecha == 2024)

tasa_capitales <- tasa %>%
  slice(5, 6, 9, 28, 29, 32)

tasa_capitales_25 <- tasa_capitales %>%
  filter(Fecha == 2025)

tasa_capitales_24 <- tasa_capitales %>%
  filter(Fecha == 2024)

tasa_mun <- tasa %>%
  slice(5:23, 28:46)

tasa_mun_25 <- tasa_mun %>%
  filter(Fecha == 2025)

tasa_mun_24 <- tasa_mun %>%
  filter(Fecha == 2024)

#--------------------`Salud pública: Tráfico de drogas (UE)`-------------------------------

tasa_th_seguridad02 <- tasa_th %>%
  select(1, 2, 32)

tasa_th_seguridad02_25 <- tasa_th_seguridad02 %>%
  filter(Fecha == 2025)

tasa_th_seguridad02_24 <- tasa_th_seguridad02 %>%
  filter(Fecha == 2024)

tasa_capitales_seguridad02 <- tasa_capitales %>%
  select(1, 2, 32)

tasa_capitales_seguridad02_25 <- tasa_capitales_seguridad02 %>%
  filter(Fecha == 2025)

tasa_capitales_seguridad02_24 <- tasa_capitales_seguridad02 %>%
  filter(Fecha == 2024)

tasa_mun_seguridad02 <- tasa_mun %>%
  select(1, 2, 32)

tasa_mun_seguridad02_25 <- tasa_mun_seguridad02 %>%
  filter(Fecha == 2025)

tasa_mun_seguridad02_24 <- tasa_mun_seguridad02 %>%
  filter(Fecha == 2024)

# Variacion

variacion <- read_ods("~/Documentos/RStudio/Ertzaina/Datos/variacion.ods") %>%
  mutate(round(across(2:45, ~ .x), 2))

variacion_th <- variacion %>%
  slice(1:3)

variacion_capitales <- variacion %>%
  slice(5, 6, 9)

variacion_mun <- variacion %>%
  slice(5:23)

variacion_th_seguridad02 <- variacion_th %>%
  select(1, 31)

variacion_capitales_seguridad02 <- variacion_capitales %>%
  select(1, 31)

variacion_mun_seguridad02 <- variacion_mun %>%
  select(1, 31)

# Gráficos
# TH

graf_tasa_th_seguridad02_24 <- ggplot(data = tasa_th_seguridad02_24) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`"
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)`\n2024"
  ) +
  # scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label = `Salud pública: Tráfico de drogas (UE)`
  ),
  size = 5,
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(#angle = 45,
    color = "black", 
    size = 15,
    vjust = 0.5,
    hjust = 0.5,
    face = "bold"
  ),
  axis.title = element_blank()
  )

graf_tasa_th_seguridad02_25 <- ggplot(data = tasa_th_seguridad02_25) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`"
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)`\n2025"
  ) +
  # scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label = `Salud pública: Tráfico de drogas (UE)`
  ),
  size = 5,
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(#angle = 45,
    color = "black", 
    size = 15,
    vjust = 0.5,
    hjust = 0.5,
    face = "bold"
  ),
  axis.title = element_blank()
  )

# Variación
graf_variacion_th_seguridad02 <- ggplot(data = variacion_th_seguridad02) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     - `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar,
  color = "black"
  ),
  show.legend = FALSE
  ) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`"
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)` - Variación 24-25"
  ) +
  # scale_fill_gradient(low = "yellow", high = "darkred") +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label = paste0(`Salud pública: Tráfico de drogas (UE)`,"%")
  ),
  size = 5,
  vjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 14,
                                  face = "bold"
  ),
  axis.text.x = element_text(#angle = 45,
    color = "black", 
    size = 15,
    vjust = 0.5,
    hjust = 0.5,
    face = "bold"
  ),
  axis.title = element_blank()
  )

# capitales

graf_tasa_capitales_seguridad02_24 <- ggplot(data = tasa_capitales_seguridad02_24, 
                                             mapping = aes(x = reorder(Lugar, -`Salud pública: Tráfico de drogas (UE)`), y = `Salud pública: Tráfico de drogas (UE)`, fill = Lugar)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Vitoria-Gasteiz" = "purple", 
    "Donostia / San Sebastián" = "orange", 
    "Bilbao" = "darkgreen"
  )) +
  geom_label(aes(label = `Salud pública: Tráfico de drogas (UE)`), 
             fill = "white",
             color = "black",
             size = 5, 
             vjust = 0.5,
             show.legend = FALSE
  ) +
  labs(x = "Lugar", y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
    axis.text.x = element_text(color = "black", size = 15, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    legend.position = "none"
  )

graf_tasa_capitales_seguridad02_25 <- ggplot(data = tasa_capitales_seguridad02_25, 
                                             mapping = aes(x = reorder(Lugar, -`Salud pública: Tráfico de drogas (UE)`), y = `Salud pública: Tráfico de drogas (UE)`, fill = Lugar)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Vitoria-Gasteiz" = "purple", 
    "Donostia / San Sebastián" = "orange", 
    "Bilbao" = "darkgreen"
  )) +
  geom_label(aes(label = `Salud pública: Tráfico de drogas (UE)`), 
             fill = "white",
             color = "black",
             size = 5, 
             vjust = 0.5,
             show.legend = FALSE
  ) +
  labs(x = "Lugar", y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
    axis.text.x = element_text(color = "black", size = 15, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    legend.position = "none"
  )

# Variación
graf_variacion_capitales_seguridad02 <- ggplot(data = variacion_capitales_seguridad02, 
                                               mapping = aes(x = reorder(Lugar, -`Salud pública: Tráfico de drogas (UE)`), y = `Salud pública: Tráfico de drogas (UE)`, fill = Lugar)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c(
    "Vitoria-Gasteiz" = "purple", 
    "Donostia / San Sebastián" = "orange", 
    "Bilbao" = "darkgreen"
  )) +
  geom_label(aes(label = paste0(`Salud pública: Tráfico de drogas (UE)`,"%")), 
             fill = "white",
             color = "black",
             size = 5, 
             vjust = 0.5,
             show.legend = FALSE
  ) +
  labs(x = "Lugar", y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
    axis.text.x = element_text(color = "black", size = 15, vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    legend.position = "none"
  ) 

# Municipios
mun <- unique(c(tasa_mun_seguridad02_24$Lugar, tasa_mun_seguridad02_25$Lugar, variacion_mun_seguridad02 ))

getPalette = colorRampPalette(brewer.pal(12, "Set3"))
mun_col = getPalette(length(mun))

names(mun_col) <- mun

graf_tasa_mun_seguridad02_24 <-  ggplot(data = tasa_mun_seguridad02_24) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar
  ),
  color = "black",
  show.legend = FALSE
  ) +
  scale_fill_manual(values = mun_col) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`",
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)` 2025 - Tasa"
  ) +
  coord_flip() +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label =  `Salud pública: Tráfico de drogas (UE)`
  ),
  size = 5,
  vjust = 0.5,
  hjust = 1
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "darkblue",
                                  size = 30,
                                  face = "bold"
  ),
  axis.text.x = element_blank(),
  axis.text.y = element_text(#angle = 45,
    color = "black", 
    size = 10,
    vjust = 0.5,
    hjust = 1,
    #face = "bold"
  ),
  axis.title = element_blank()
  )

graf_tasa_mun_seguridad02_25 <-  ggplot(data = tasa_mun_seguridad02_25) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar
  ),
  color = "black",
  show.legend = FALSE
  ) +
  scale_fill_manual(values = mun_col) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`",
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)` 2025 - Tasa"
  ) +
  coord_flip() +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label =  `Salud pública: Tráfico de drogas (UE)`
  ),
  size = 5,
  vjust = 0.5,
  hjust = 1
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "darkblue",
                                  size = 30,
                                  face = "bold"
  ),
  axis.text.x = element_blank(),
  axis.text.y = element_text(#angle = 45,
    color = "black", 
    size = 10,
    vjust = 0.5,
    hjust = 1,
    #face = "bold"
  ),
  axis.title = element_blank()
  )

graf_variacion_mun_seguridad02 <-  ggplot(data = variacion_mun_seguridad02) +
  geom_col(mapping = aes(x = reorder(Lugar,
                                     `Salud pública: Tráfico de drogas (UE)`
  ),
  y = `Salud pública: Tráfico de drogas (UE)`,
  fill = Lugar
  ),
  color = "black",
  show.legend = FALSE
  ) +
  scale_fill_manual(values = mun_col) +
  labs(x = "Lugar",
       y = "Criminalidad `Salud pública: Tráfico de drogas (UE)`",
       #title = "Criminalidad `Salud pública: Tráfico de drogas (UE)` 2025 - Tasa"
  ) +
  coord_flip() +
  geom_label(aes(x = Lugar,
                 y = `Salud pública: Tráfico de drogas (UE)`,
                 label =  paste0(`Salud pública: Tráfico de drogas (UE)`,"%")
  ),
  size = 5,
  vjust = 0.5,
  hjust = 0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "darkblue",
                                  size = 30,
                                  face = "bold"
  ),
  axis.text.x = element_blank(),
  axis.text.y = element_text(#angle = 45,
    color = "black", 
    size = 10,
    vjust = 0.5,
    hjust = 1,
    face = "bold"
  ),
  axis.title = element_blank()
  )
graf_tasa_th_seguridad02_24
graf_tasa_th_seguridad02_25
