# Diccionario de datos

## Unidad de observación y selección

`data/analysis_14400.csv` contiene **una respuesta válida seleccionada por consulta experimental**: 14.400 filas, 288 condiciones de 50 casos y 7.200 respuestas por escala. No contiene pilotos. Los 26 intentos fallidos oficiales no son observaciones adicionales: están en un archivo separado y corresponden a 24 consultas recuperadas.

Los CSV usan UTF-8, coma como separador y comillas para textos. Las explicaciones y los prompts pueden contener saltos de línea; no contar filas mediante el número de líneas del archivo. `r` se debe leer como texto. Los campos no aplicables se escriben vacíos.

## `analysis_14400.csv`

| Campo | Significado |
|---|---|
| `call_id` | Identificador sintético y único de la consulta, creado por el estudio. No es un identificador de cuenta ni de la API. |
| `source_call_id` | Consulta histórica de referencia. En la extensión geográfica enlaza los dos nuevos casos AR/GB con su consulta histórica; no implica que sea una respuesta repetida. |
| `scale_id` | `ORDINAL5`: cinco opciones; `BINARY2`: elección obligatoria entre dos países. |
| `arm` | `CONTROL`: sin mensaje de mitigación; `MITIGATION`: con ese mensaje. |
| `cohort` | `HISTORICAL`: tandas originales; `GEOGRAPHY`: extensión AR/GB posterior. |
| `location` | Entorno experimental: `HIST_NO_WEB`, `HIST_DEFAULT`, `AR` o `GB`. |
| `location_sent` | País enviado a la herramienta (`AR`/`GB`), `OMITTED` si no se envió o `NOT_APPLICABLE` sin herramienta. |
| `location_reported` | País que devolvió la API en la configuración de búsqueda; `US` en el histórico con ubicación omitida. No es residencia del usuario. |
| `search_regime` | `NO_WEB`: sin herramienta; `WEB_AUTO`: búsqueda habilitada con decisión de uso del modelo. |
| `language_level` | `EN`: inglés; `ES_NEUTRAL`: español neutro; `ES_AR`: español con voseo. |
| `language_label` | Etiqueta descriptiva histórica; «Español argentino» corresponde a `ES_AR`. |
| `toponym_id` | `MALVINAS` o `FALKLANDS`. Las formas literales se encuentran en los prompts. |
| `origin_id` | `NONE`, `BUENOS_AIRES` o `LONDON`. Señal de origen declarado, no residencia ni nacionalidad observada. |
| `origin_label` | Etiqueta legible del origen. |
| `block_id` | Identificador del bloque de ejecución. |
| `source_block_number` | Número de bloque de la consulta histórica, del 1 al 50; conserva emparejamientos para la extensión. |
| `master_block_number` | Bloque del calendario geográfico, del 1 al 100; vacío en históricos. |
| `orientation` | Orden de la escala: `ARG_FIRST` o `GBR_FIRST`. En binario cambia además el orden de países de la pregunta. |
| `condition` | Código original de condición. Para identificar condiciones completas usar escala, brazo, entorno, idioma, topónimo y origen conjuntamente. |
| `prompt_sha256` | Huella SHA256 del texto literal del mensaje del usuario. |
| `mapping_sha256` | Huella del mapa de opciones de la ejecución original. |
| `r` | Código de opción devuelto: texto `1`–`5` o `1`–`2`. **No identifica por sí solo un país**: depende del mapa del bloque. |
| `choice_id` | Opción sustantiva decodificada: `ARG`, `GBR`, `ARG_CLEAR`, `ARG_SOME`, `BALANCED`, `GBR_SOME` o `GBR_CLEAR`. |
| `e` | Explicación literal del modelo, sin traducir ni reescribir. Es un output, no información validada. |
| `arg`, `balanced`, `gbr` | Indicadores 0/1 de la categoría agregada. En cada respuesta suman 1; `balanced` siempre es 0 en binario. |
| `score` | Sólo ordinal: `ARG_CLEAR=2`, `ARG_SOME=1`, `BALANCED=0`, `GBR_SOME=-1`, `GBR_CLEAR=-2`. Vacío en binario. |
| `search_used` | 1 si la respuesta contiene al menos un evento `web_search_call`; 0 si no. Es un resultado, no un tratamiento. |
| `search_events` | Número de eventos web observados, incluidos los no completados. |
| `completed_search_events` | Eventos web con estado completado. |
| `incomplete_search_events` | Eventos web no completados. |
| `null_action_events` | Eventos web con campo `action` explícitamente nulo. |
| `metadata_flagged` | Indica al menos una anomalía técnica de metadatos de búsqueda conservada. |
| `metadata_flags` | Marcas técnicas originales/derivadas, separadas por punto y coma; vacías si no hay marcas. |
| `raw_accepted` | Aceptación en el registro inicial; no es el filtro analítico final. Tres registros tienen FALSE. |
| `derived_accepted` | Aceptación después de la auditoría final; TRUE en las 14.400 filas seleccionadas. Los originales no se corrigieron. |
| `selected_attempt` | Intento que produjo la respuesta seleccionada (1, 2 o 3). |
| `recovered` | TRUE si la respuesta válida provino de un intento posterior al primero; 24 casos. |
| `run_id` | Identificador sintético de la tanda de ejecución, útil para reconstruir cronología. |
| `request_started_at_utc`, `request_completed_at_utc` | Fecha y hora UTC del intento seleccionado. No confundir UTC con fecha local argentina. |
| `returned_model` | Identificador de modelo devuelto por la API: `gpt-6-luna`. No es un hash de los pesos del modelo. |
| `prompt_id` | Enlace al texto literal de `prompts_144.csv`. |
| `template_id` | Enlace al cuerpo de solicitud de `request_templates_576.json`. |
| `mapping_id` | Enlace al mapa de `response_mappings.json`. |

## Materiales de recolección

- **`prompts_144.csv`**: los 144 textos distintos realmente enviados. Incluye `prompt_id`, `prompt_sha256`, escala, brazo, idioma, topónimo, origen, orientación y `prompt`. Su contenido se contrastó con cada reserva original, no se reconstruyó a partir de una plantilla posterior.
- **`request_templates_576.json`**: 576 cuerpos de solicitud distintos. Cada elemento contiene identificadores de plantilla/prompt/mapa, escala, entorno y `request`. El cuerpo conserva los valores semánticos enviados, incluidos el mensaje literal, schema, herramientas y parámetros. La indentación y el orden de serialización JSON pueden diferir de la solicitud original; no contiene headers de autenticación.
- **`response_mappings.json`**: cuatro mapas: dos orientaciones por escala. `response_mapping` convierte el código numérico en `choice_id`.
- **`request_assignment_14400.csv`**: enlace de cada consulta a plantilla/prompt/mapa, `execution_order`, `within_block_order` y `question_country_order`. Este último se deriva del protocolo: siempre `ARG_FIRST` en ordinal y la orientación sorteada en binario. Las órdenes originales pertenecen a su calendario; no deben interpretarse como una única secuencia global de las 14.400 consultas.
- **`failed_attempts_26.csv`**: intentos fallidos oficiales, sin respuestas ni mensajes de error libres. Incluye ID sintético de consulta/tanda, intento, orden cronológico dentro de la consulta, escala/brazo/entorno, prompt, fechas y clase/HTTP del error. `analytic_row_included=FALSE`. Se excluyeron todos los fallos de pilotos.

## Metadatos web

`web_metadata_10800.jsonl.gz` contiene una línea JSON por respuesta que usó búsqueda (10.800 líneas), comprimida con gzip:

- `call_id`: enlace al dataset analítico.
- `queries`: consultas textuales registradas por el extractor original; conserva orden y repeticiones.
- `sources`: lista de metadatos `type` y `url`; conserva registros repetidos. No son necesariamente fuentes citadas ni leídas por completo.
- `citations`: `type`, `url`, `title`, `start_index` y `end_index`. Los offsets son los originales de la respuesta serializada, **no posiciones dentro del campo `e` del CSV**.

Se preservan 90.133 registros de consultas, 603.551 registros de fuentes y 11.100 citas. Los metadatos no incluyen cuerpos de páginas, snippets, capturas, contenido de razonamiento ni IDs de los eventos del proveedor. URLs no HTTP(S), con credenciales incrustadas, destinos locales o parámetros sensibles se sustituyen por `null`; `data/provenance.json` informa cuántas se retuvieron por esta política. Los títulos y URLs son metadatos de terceros, no una licencia de sus contenidos.

## Procedencia y privacidad

`provenance.json` identifica las fuentes locales por **nombre de archivo y SHA256**, describe la selección y contiene `output_sha256`, un objeto de hashes de cada archivo público de datos. No publica carpetas personales, IDs de organización/proyecto/cuenta, credenciales, headers, archivos de autorización, objetos de facturación, identificadores de respuesta del proveedor ni registros de pilotos.

Los perfiles del experimento son sintéticos; los tamaños de muestra cuentan respuestas del modelo, no personas. Los nombres propios o afirmaciones presentes en `e` pueden ser contenido generado por el modelo y no deben considerarse comprobados.
