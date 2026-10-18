# Figuras y tabla editorial

`code/R/05_editorial_outputs.R` recalcula las figuras vigentes y la tabla de origen e idioma usando exclusivamente `data/analysis_14400.csv`. No ejecuta consultas a ningún servicio ni necesita credenciales. El archivo de entrada contiene las 14.400 respuestas oficiales seleccionadas; los pilotos no participan.

Desde la raíz del paquete:

```sh
Rscript code/R/05_editorial_outputs.R
Rscript code/R/05_editorial_outputs.R --check
```

La primera ejecución genera los resultados. Si ya existen y coinciden con su manifiesto, la misma orden los verifica. `--overwrite` permite regenerar únicamente los archivos de salida propios de este script. No modifica datos de entrada ni otros resultados.

## Archivos producidos

Las siete familias de figuras se exportan en PNG de 300 dpi, PDF y SVG, con títulos y sin títulos: 42 archivos bajo `output/figures/{con_titulos,sin_titulos}/{png,pdf,svg}/`. Se preservan colores, símbolos, tipografía y escalas de las versiones editoriales seleccionadas. Las notas de alcance están en este documento, no al pie de cada figura.

| Familia | Qué muestra | Respuestas utilizadas |
|---|---|---:|
| `search_environments` | Tres señales y cuatro líneas por señal, una por entorno de búsqueda | 3.600 binarias sin mitigación |
| `mitigation_signals` | Cuatro señales con dos líneas, sin y con mitigación | 7.200 binarias |
| `control_signals` | Cuatro señales agregadas, sin mitigación; referencia del perfil británico de las barras | 3.600 binarias |
| `profiles_binary` | Dos perfiles, elección forzada, sin mitigación | 100, 50 por perfil |
| `profiles_five_options` | Dos perfiles, escala de cinco opciones, sin mitigación | 100, 50 por perfil |
| `profiles_mitigation_binary` | Los dos perfiles en paneles sin y con mitigación, elección forzada | 200, 50 por perfil y brazo |
| `profiles_mitigation_five_options` | Los dos perfiles en paneles sin y con mitigación, cinco opciones | 200, 50 por perfil y brazo |

La tabla editable `output/tables/origin_language_table.docx` muestra los nueve porcentajes de elección de Argentina por origen declarado e idioma. Se generan también seis CSV con todos los conteos y denominadores, y `editorial_outputs_manifest.json`, que registra hashes, paquetes y alcance.

## Dotplots

El resultado representado es el porcentaje que eligió Argentina, calculado como número de elecciones `ARG` dividido por el total del grupo. Los otros factores se agrupan a partir de los casos, no promediando porcentajes con denominadores diferentes. Las líneas unen el mínimo y el máximo entre variantes de cada señal; **no son intervalos de confianza ni coeficientes ajustados**. Las mismas respuestas aparecen reagrupadas en varias filas, por lo que no se deben sumar los tamaños de muestra de esas filas.

En `search_environments` hay 900 respuestas por entorno. Cada punto de nombre agrupa 450 respuestas; cada punto de origen o idioma, 300. El orden de los entornos es sin búsqueda, búsqueda con ubicación omitida, ubicación GB y ubicación AR. La ubicación omitida se presenta como EE. UU. por defecto porque ese fue el país devuelto por la API en aquella tanda.

En `mitigation_signals` hay 3.600 respuestas por brazo. Cada punto de nombre agrupa 1.800 respuestas; cada punto de origen o idioma, 1.200; cada entorno, 900. En `control_signals` se utiliza sólo el brazo sin mitigación con esos mismos denominadores. Su referencia del 2 % corresponde a las 50 respuestas del perfil británico binario sin mitigación usado en las barras, no a una categoría agregada denominada “sin señales”.

En los dos dotplots de líneas múltiples, el celeste destaca Malvinas, Buenos Aires, español con voseo y —cuando es una variante de la fila— ubicación Argentina. Los demás puntos son grises. Las formas distinguen entornos o brazos; los símbolos de la leyenda son celestes y las líneas son continuas grises. Agrupar etiquetas cercanas no agrupa ni desplaza los puntos.

No se filtra por la búsqueda efectivamente realizada: tener la herramienta habilitada no equivale a usarla. Los entornos históricos y los nuevos brazos geográficos pertenecen a tandas diferentes. Estos gráficos son comparaciones descriptivas; no deben interpretarse como cuatro países asignados simultáneamente ni como una medida causal de la importancia de cada señal.

## Barras de perfiles e intervalos

Ambos perfiles omiten el origen declarado y tienen búsqueda habilitada:

- Perfil argentino: español con voseo, nombre Malvinas y ubicación aproximada AR.
- Perfil británico: inglés, nombre Falkland y ubicación aproximada GB.

Las escalas se mantienen separadas. En la escala de cinco opciones se conservan las categorías de los extremos aunque tengan cero respuestas. Las dos barras de la elección binaria son complementarias: 50 respuestas por perfil producen dos porcentajes, no 100 observaciones por perfil.

Los intervalos del 95 % son puntuales normales de Wald, calculados con `binom::binom.confint(x, n, conf.level = 0.95, methods = "asymptotic")`. Se conserva el método editorial elegido, sin cálculo manual ni recorte de los límites. Por eso algunos intervalos pueden salir de 0–100 % y el intervalo de una categoría con cero respuestas tiene ancho cero. No son intervalos de una diferencia entre perfiles ni están ajustados por múltiples comparaciones. El eje vertical de −5 a 110 % permite mostrar esos límites completos.

## Tabla de origen e idioma

Se filtran `BINARY2`, `CONTROL` e `HIST_NO_WEB`: 900 respuestas, 100 por casillero. Cada casillero reúne 50 consultas con Falkland y 50 con Malvinas. Las filas son Londres, ausencia de declaración y Buenos Aires; las columnas, inglés, español neutro y español con voseo. “Origen” significa la declaración “soy de”, no residencia ni ubicación del buscador.

Los CSV `editorial_origin_language_counts.csv` y `editorial_origin_language_matrix.csv` permiten verificar cada celda. El Word reproduce la tabla con texto Arial, cabecera celeste y bordes grises; puede haber pequeñas diferencias de maquetación entre Word, LibreOffice y sistemas de fuentes.

## Reproducibilidad gráfica

Las funciones de dibujo derivan de las versiones editoriales R120 v5, R121 v5, R101 v4, R102 v3 y R103 v3. Los conteos se reconstruyen desde el CSV público, sin depender de las tablas ni de rutas del proyecto privado. La tabla de nueve celdas reproduce los cálculos de R122.

Paquetes requeridos: `ggplot2`, `ggtext`, `binom`, `jsonlite`, `digest`, `ragg`, `svglite`, `officer`, `flextable`, `xml2` y `zip`. El manifiesto guarda las versiones usadas. El helper `code/R/scrub_docx.R` retira del Word recién generado los autores, comentarios y propiedades personalizadas que pudieran identificar el entorno local; no modifica los datos de la tabla. La igualdad de cifras e intervalos no requiere que los archivos gráficos o DOCX sean idénticos byte a byte: pueden cambiar metadatos, fuentes y detalles del dispositivo entre plataformas.
