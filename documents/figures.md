# Figuras y tablas

`code/R/05_editorial_outputs.R` y `code/R/07_pooled_search.R` calculan las piezas de la nota a partir de `data/analysis_14400.csv`.

| Archivo | Selección y tamaño de muestra | CSV de conteos |
|---|---|---|
| `profiles_binary` | Elección binaria sin mitigación: 50 consultas por perfil, 100 en total | `editorial_profile_counts.csv` |
| `mitigation_signals` | Elección binaria: 3.600 por panel, 7.200 en total. Por barra: nombre 1.800; origen o idioma 1.200; entorno 900 | `editorial_mitigation_counts.csv` |
| `search_environments` | Elección binaria sin mitigación: 900 por entorno, 3.600 en total. Por punto: nombre 450; origen o idioma 300 | `editorial_search_environment_counts.csv` |
| `search_pooled` | Elección binaria sin mitigación: 900 sin búsqueda en internet y 2.700 con búsqueda en internet. Por barra, sin/con búsqueda: nombre 450/1.350; origen o idioma 300/900 | `editorial_search_pooled_counts.csv` |
| `origin_language_table.docx` | Elección binaria sin búsqueda ni mitigación: 100 por casillero, 900 en total | `editorial_origin_language_counts.csv` |

Los gráficos están en `output/figures/`, con y sin títulos, en PNG, PDF y SVG. Las tablas y los CSV están en `output/tables/`.

`mitigation_signals` y `search_pooled` muestran barras en dos paneles con el mismo eje. En `search_pooled`, «Con búsqueda en internet» agrupa EE. UU. por defecto, Reino Unido y Argentina con 900 consultas cada uno. El contraste es descriptivo: combina ubicaciones y tandas, no aísla el efecto de habilitar internet.

Los dos perfiles omiten el origen declarado: el argentino usa voseo, Malvinas y ubicación AR; el británico, inglés, Falkland y ubicación GB. Los intervalos de las barras son del 95 %, normales de Wald, calculados con `binom::binom.confint(methods="asymptotic")`, sin recortar los límites a 0–100 %. Son intervalos puntuales por porcentaje, sin ajuste por bloques ni comparaciones múltiples; no son intervalos para la diferencia entre barras.

Cada porcentaje por señal agrupa las demás condiciones. Una misma respuesta contribuye a las filas de nombre, origen e idioma: los tamaños de muestra de esas filas no se suman. El celeste destaca las señales argentinas. En los dotplots complementarios, las líneas unen variantes y no representan intervalos de confianza.

## Resultados complementarios

`profiles_five_options`, `profiles_mitigation_binary` y `profiles_mitigation_five_options` usan 50 consultas por perfil, escala y brazo; `control_signals` muestra las 3.600 respuestas binarias sin mitigación. El script `02_descriptives.R` produce los porcentajes de las 288 condiciones. `06_argument_mentions.R` genera la [tabla de menciones de argumentos](argument_mentions.md).
