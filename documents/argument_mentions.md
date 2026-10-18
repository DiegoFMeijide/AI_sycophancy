# Menciones de argumentos

`06_argument_mentions.R` aplica las reglas bilingües guardadas en `mentions_core.R` y `mentions_additional.R` exclusivamente al campo de explicación `e`. No utiliza la decisión elegida ni los tratamientos para clasificar el texto.

La muestra son las 1.800 respuestas oficiales sin búsqueda ni mitigación, con 450 por cada combinación de escala y nombre. La tabla informa presencia temática, incluso cuando el modelo critica o relativiza el argumento. Las categorías se superponen y los porcentajes no suman 100 %.

Las doce categorías y sus definiciones se exportan en la tabla `menciones_argumentos_sin_busqueda_17102026_categorias.csv`. Las reglas fueron fijadas durante el análisis exploratorio de estas explicaciones; no constituyen una medición humana validada ni un diccionario preregistrado. Pueden producir falsos positivos o negativos. La categoría sobre límites de la autodeterminación es un subconjunto de las menciones de voluntad isleña.

Se preservan las reglas originales, sus conteos de referencia y las clasificaciones individuales para que un tercero pueda inspeccionar la sensibilidad a otras definiciones sin hacer nuevas llamadas al modelo.
