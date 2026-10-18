# Sycophancy en LLMs: un experimento sobre las Islas Malvinas

Datos y código de **Diego Fernández Meijide** para reproducir los gráficos y tablas de la nota. El ejercicio reúne 14.400 respuestas de GPT-6 Luna, variando idioma, nombre de las islas, origen declarado, búsqueda y mensaje de mitigación.

## Reproducir

Desde la carpeta del repositorio, con R instalado:

```sh
Rscript install_packages.R
Rscript --vanilla run_all.R
```

El análisis usa los datos incluidos; no requiere una clave de API. Las versiones de R y los paquetes están en [software_versions.csv](documents/software_versions.csv).

## Gráficos y tabla de la nota

| Pieza | Archivo |
|---|---|
| Perfil argentino frente al británico | [profiles_binary.png](output/figures/con_titulos/png/profiles_binary.png) |
| Señales con y sin mitigación: barras con IC del 95 % | [mitigation_signals.png](output/figures/con_titulos/png/mitigation_signals.png) |
| Señales según el entorno de búsqueda | [search_environments.png](output/figures/con_titulos/png/search_environments.png) |
| Señales sin y con búsqueda en internet: barras con IC del 95 % | [search_pooled.png](output/figures/con_titulos/png/search_pooled.png) |
| Idioma y origen declarado | [origin_language_table.docx](output/tables/origin_language_table.docx) |

Los gráficos están en `output/figures/`, con y sin títulos, en PNG, PDF y SVG. Los conteos, porcentajes y resultados complementarios están en `output/tables/`.

## Datos y método

- [Datos](data/analysis_14400.csv) y [diccionario de variables](documents/data_dictionary.md).
- [Prompts](data/prompts_144.csv) y [diseño del ejercicio](documents/protocol.md).
- [Filtros y tamaños de muestra de cada figura](documents/figures.md).

Referencia: Fernández Meijide, Diego (2026). *Sycophancy en LLMs: un experimento sobre las Islas Malvinas*. Datos y código de replicación.
