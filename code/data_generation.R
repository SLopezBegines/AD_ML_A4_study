

#Dataset creation
set.seed(123)

# Número de pacientes únicos
n_pacientes <- 10000

# Pacientes con 2-5 visitas
visitas_paciente <- sample(2:10, n_pacientes, replace = TRUE)
total_registros <- sum(visitas_paciente)

# Crear vector de IDs
id <- rep(1:n_pacientes, times = visitas_paciente)

# Tiempo desde el diagnóstico (en meses)
tiempo_evol <- unlist(lapply(visitas_paciente, function(v) sort(sample(1:(v*6), v))))

# Estado clínico con progresión (leve -> moderado -> grave)
estado_clinico <- unlist(lapply(visitas_paciente, function(v) {
  estados <- c("Leve", "Moderado", "Grave")
  sample(estados[1:min(3,v)], v, replace = TRUE)
}))

# Variables fisiológicas (algunas con cambios)
edad_base <- rnorm(n_pacientes, mean = 72, sd = 10)
edad <- rep(edad_base, times = visitas_paciente) + tiempo_evol / 12

presion_sistolica <- rnorm(total_registros, mean = 130, sd = 15)
colesterol <- rgamma(total_registros, shape = 2, scale = 50)
glucosa <- rlnorm(total_registros, meanlog = 4.5, sdlog = 0.4)

# Marcadores metabólicos y clínicos
lactato <- rgamma(total_registros, shape = 1.5, scale = 1.2)
cetonas <- rlnorm(total_registros, meanlog = 0.3, sdlog = 0.2)
creatinina <- rnorm(total_registros, mean = 0.9, sd = 0.2)
gpt <- rlnorm(total_registros, meanlog = 4, sdlog = 0.3)
pcr <- rgamma(total_registros, shape = 1, scale = 5)

# Genética (una sola vez por paciente)
# APOE ajustado según prevalencia
apoe_probs <- c("E2/E3"=0.05, "E3/E3"=0.50, "E3/E4"=0.30, "E4/E4"=0.10, "E2/E4"=0.05)
apoe <- rep(sample(names(apoe_probs), n_pacientes, replace = TRUE, prob = apoe_probs), times = visitas_paciente)

# Niveles de Aβ42: distribución inversamente relacionada con enfermedad
abeta42 <- rgamma(total_registros, shape = 2, scale = 180)  # media aprox ~360 pg/mL
abeta42[sample(1:total_registros, 10)] <- runif(10, 100, 200)  # outliers bajos (más grave)
abeta42[sample(1:total_registros, round(total_registros * 0.07))] <- NA  # 7% NA

# Niveles de Tau total: más altos en AD
tau <- rgamma(total_registros, shape = 2, scale = 250)  # media aprox ~500 pg/mL
tau[sample(1:total_registros, 10)] <- runif(10, 800, 1000)  # outliers altos
tau[sample(1:total_registros, round(total_registros * 0.06))] <- NA  # 6% NA


# SNPs ficticios
snp_rs123 <- rep(sample(c("AA", "AG", "GG"), n_pacientes, replace = TRUE), times = visitas_paciente)
snp_rs456 <- rep(sample(c("CC", "CT", "TT"), n_pacientes, replace = TRUE), times = visitas_paciente)

# Demográficos y estilo de vida
sexo <- rep(sample(c("Hombre", "Mujer"), n_pacientes, replace = TRUE), times = visitas_paciente)
fuma <- sample(c("Sí", "No"), total_registros, replace = TRUE)
actividad_fisica <- sample(c("Baja", "Media", "Alta"), total_registros, replace = TRUE)

# Comentarios médicos
# Frases positivas
frases_positivas <- c(
  "El paciente muestra mejoría en las funciones cognitivas.",
  "Buena adherencia al tratamiento.",
  "No se detectan nuevos síntomas.",
  "Estable desde la última visita.",
  "Actividad física regular reportada.",
  "No hay progresión de la enfermedad."
)

# Frases neutras
frases_neutras <- c(
  "Se realiza nueva analítica de control.",
  "El paciente acudió acompañado.",
  "Continúa con el mismo tratamiento.",
  "Se programan nuevas pruebas de imagen.",
  "Paciente en seguimiento rutinario.",
  "Se revisan resultados de laboratorio."
)

# Frases negativas
frases_negativas <- c(
  "Progresión de los síntomas cognitivos.",
  "Aumento en los niveles de glucosa.",
  "Deterioro en pruebas neuropsicológicas.",
  "El paciente refiere fatiga y confusión.",
  "Empeoramiento del estado funcional.",
  "Signos de inflamación persistente."
)

# Función para combinar frases
generar_comentario <- function() {
  n_pos <- sample(0:1, 1)
  n_neu <- sample(0:1, 1)
  n_neg <- sample(0:2, 1)
  frases <- c(
    sample(frases_positivas, n_pos),
    sample(frases_neutras, n_neu),
    sample(frases_negativas, n_neg)
  )
  paste(frases, collapse = " ")
}

# Generar comentarios mixtos
comentarios <- replicate(total_registros, generar_comentario())


# Introducir valores ausentes
colesterol[sample(1:total_registros, size = round(total_registros * 0.08))] <- NA
comentarios[sample(1:total_registros, size = 30)] <- NA
pcr[sample(1:total_registros, size = 20)] <- NA

# Introducir outliers
glucosa[sample(1:total_registros, 5)] <- rnorm(5, mean = 300, sd = 20)
creatinina[sample(1:total_registros, 5)] <- 5

# DataFrame final
df <- data.frame(
  id_paciente = id,
  visita = ave(id, id, FUN = seq_along),
  tiempo_evol,
  edad,
  sexo,
  apoe,
  snp_rs123,
  snp_rs456,
  presion_sistolica,
  colesterol,
  glucosa,
  lactato,
  cetonas,
  creatinina,
  gpt,
  pcr,
  fuma,
  actividad_fisica,
  comentarios,
  estado_clinico
)

# Ver algunas filas
head(df)
