# Resultados históricos de referencia

Estos 22 CSV son copias byte a byte de los resultados auditados del 17 de octubre de 2026, anteriores a la preparación de este repositorio. Sus hashes se encuentran en `SHA256SUMS`. No contienen rutas personales, credenciales ni registros operativos privados.

Además se conservan tres CSV de la tabla editorial de menciones de argumentos (`menciones_argumentos_sin_busqueda_17102026_*`): conteos, matriz y categorías. El script editorial 06 comprueba esos productos por separado. El manifiesto incluye las 25 referencias; el verificador 04 compara los 22 CSV del análisis integral.

Los scripts públicos `02_analyze_14400.R` y `03_multinomial.R` vuelven a calcular las tablas desde `data/analysis_14400.csv`; no usan estos CSV como entradas del ajuste. `04_verify_results.R` los usa exclusivamente como referencias para comprobar la reproducción. Las salidas se generan en `output/tables/` con los mismos nombres.

## Alcance reproducido

- 14.400 respuestas válidas oficiales, 288 condiciones de 50 casos, 7.200 por escala; sin pilotos.
- Distribuciones completas, contextos, márgenes de factores, cronología y bloques.
- Logit binario principal; multinomial nominal principal de las tres categorías observadas de la escala ordinal; modelos lineales de probabilidad como sensibilidad. Las cinco opciones ofrecidas siguen presentes en los descriptivos.
- En conjunto: 343 coeficientes, 648 contrastes de efectos marginales, 296 cambios de brecha con mitigación y 64 probabilidades estandarizadas, incluyendo sensibilidades.
- La especificación, selección de modelos y familias de contrastes fueron postcampo, no preregistradas. Los contrastes contemporáneos AR–GB no deben confundirse con las comparaciones entre olas históricas.

## Criterio de reproducción

`04_verify_results.R` comprueba primero los hashes de estas referencias. Exige igualdad de nombres de columnas, filas, etiquetas, valores faltantes y conteos enteros. Para valores descriptivos no enteros admite una diferencia de hasta `1e-10 × (1 + abs(referencia))`. Para resultados ajustados numéricamente admite `1e-6 × (1 + abs(referencia))`, por diferencias entre implementaciones de BLAS, versiones de R y sistemas operativos. También informa si cada CSV es byte a byte idéntico y su máxima diferencia absoluta. No se comparan hashes de figuras, fuentes, sesiones o documentos renderizados entre sistemas.

Los modos `--check` de 02 y 03 recalculan y comparan con las salidas ya generadas (tolerancia `1e-7 × (1 + abs(referencia))`), sin escribir archivos. El modo normal se detiene si ya existe cualquiera de sus productos: nunca sobrescribe silenciosamente.

## Adaptación pública

La matemática procede de los scripts auditados R94 y R98 del proyecto original. Se conservaron fórmula, niveles de referencia, algoritmos, estandarización uniforme, orientación 50/50, ajuste CR1 por bloques reales, grados de libertad 49 y corrección Holm por las mismas familias. Se reemplazaron dependencias del inventario privado y de reservas operativas por el manifiesto público, los controles de estructura y los datos seleccionados. El paquete reproduce el análisis a partir de esa selección validada; no pretende reauditar archivos operativos que no se publican.

Los JSON generados son nuevas envolturas públicas con metadatos actuales y sólo rutas relativas, no copias de los registros privados. Su metadata aclara que LOGIT y MULTINOMIAL son principales: la primera versión histórica de R94 precedía a esa decisión postdiagnóstico y etiquetaba al modelo lineal como principal.
