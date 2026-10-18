# Comprobaciones de la publicación

Fecha: 18 de octubre de 2026. Entorno probado: R 4.5.2, macOS ARM64; versiones de paquetes en `software_versions.csv`.

## Prueba de reproducción

El paquete se copió a una carpeta temporal separada, sin resultados precalculados y sin la variable de clave de OpenAI. Desde allí se ejecutó `Rscript --vanilla run_all.R` usando sólo los archivos públicos. El programa reconstruyó los resultados y terminó sin errores.

- 14.400 respuestas oficiales únicas; 288 condiciones con 50 respuestas; 7.200 por escala. Cero observaciones de pilotos.
- 144 prompts, 576 cuerpos de solicitud, cuatro mapas y 14.400 asignaciones enlazados y verificados; enums, parámetros y tratamientos comprobados.
- Las 22 tablas estadísticas reconstruidas coincidieron byte a byte con las referencias históricas: diferencia numérica máxima cero en esta instalación.
- Verificación de 343 coeficientes, 648 efectos marginales, 296 contrastes de cambio de brecha y 64 probabilidades modeladas. Estas cantidades suman análisis principales y sensibilidades; no son tests independientes.
- Los conteos e intervalos de las figuras y las nueve celdas de la tabla de idioma y origen coincidieron con los originales. Los 14 SVG de siete familias, con y sin títulos, coincidieron con las figuras vigentes en esta instalación.
- Los 48 conteos de menciones de argumentos, correspondientes a 12 categorías por dos nombres y dos escalas, coincidieron con las tablas de referencia.
- Se inspeccionaron las variantes PNG y las dos tablas Word renderizadas, incluyendo la versión posterior a la limpieza de metadatos.

La comparación pública usa tolerancias numéricas documentadas en `expected/README.md`; no exige identidad binaria de documentos o imágenes entre distintos sistemas. No se probó todavía otra plataforma.

## Revisión de privacidad

Se publicó una selección explícita de campos y archivos, con historial Git nuevo. Se revisaron textos, CSV, JSON, metadatos web descomprimidos, PDFs y los XML internos de los Word para detectar credenciales, identificadores de cuenta/proyecto, rutas personales, enlaces con autenticación y autores añadidos automáticamente por el software.

Los Word se regeneran con un paso de limpieza de autores, comentarios y propiedades personalizadas. Las URLs de metadatos web que no superaron la política de publicación se reemplazaron por `null`, conservando el registro y dejando constancia de las cantidades en `data/provenance.json`. Esto no cambia los prompts, explicaciones, decisiones, denominadores ni estimaciones.

No se incluyen el historial de conversaciones, archivos de autorización o facturación, el árbol bruto de respuestas del proveedor, las credenciales, ni las versiones anteriores del proyecto. La revisión reduce riesgos de publicación; no convierte los textos generados por el modelo en hechos verificados ni sustituye una revisión del uso futuro que se haga de ellos.
