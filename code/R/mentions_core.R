# Reglas locales bilingües: presencia temática, no respaldo ni verdad factual.
# Sólo reciben la explicación e. No usan tratamientos ni decisión del modelo.

core_codebook <- data.frame(
  code = c("ES_TITLE", "AR_PRE1833", "DISPLACEMENT_1833", "UK_ADMIN",
           "ISLANDERS_WISHES", "AR_INTEGRITY", "AR_PROXIMITY", "UN_NEGOTIATION"),
  label = c("Herencia de los derechos españoles", "Presencia argentina previa a 1833",
            "Toma británica y desplazamiento de 1833", "Administración británica prolongada",
            "Voluntad de los isleños y autodeterminación", "Integridad territorial argentina",
            "Proximidad geográfica con Argentina", "ONU y negociación de la soberanía"),
  definition = c(
    "Mención explícita de España/derechos españoles en el argumento de título, sucesión o continuidad histórica; no inferirlo de argumentos históricos genéricos.",
    "Mención explícita de presencia, administración o autoridad argentina anterior a 1833; la expulsión de autoridades no basta para inferirla, ni la presencia española o británica previa.",
    "Toma, recuperación u ocupación británica cuestionada, expulsión, remoción o desplazamiento de autoridades argentinas, o interrupción de su autoridad asociada al episodio de 1833. Incluye referencias indirectas explícitas a hechos/eventos/circunstancias de 1833 dentro del argumento histórico; la fecha aislada en administración desde 1833 no basta. Excluye ocupación de 1982 e interrupción británica de 1774.",
    "Administración, gobierno, control o ejercicio efectivo/prolongado británico de las islas, aunque se cuestione que resuelva la soberanía.",
    "Voluntad, preferencias, deseos, voto o autodeterminación/libre determinación de los habitantes, incluso si se niega su carácter decisivo.",
    "Mención explícita de integridad territorial; no inferir de título territorial, territorio, sucesión territorial o proximidad.",
    "Mención explícita de proximidad/cercanía o argumento geográfico referido al reclamo territorial; no inferir de integridad ni de aproximadamente equivalente.",
    "Mención de la ONU/Naciones Unidas y su tratamiento de la disputa, resoluciones, descolonización o negociación, o llamado explícito a negociar la soberanía. Ausencia de fallo judicial sin ONU/negociación no basta."
  ), stringsAsFactors = FALSE
)

normalize_core_text <- function(e) {
  x <- tolower(enc2utf8(as.character(e)))
  x[is.na(x)] <- ""
  x <- chartr("áéíóúñü", "aeiounu", x)
  x <- gsub("[’‘`´]", "'", x, perl = TRUE)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

classify_core <- function(e) {
  x <- normalize_core_text(e)
  original <- as.character(e)
  original[is.na(original)] <- ""
  hit <- function(p) grepl(p, x, perl = TRUE)
  # Country-specific clauses help disambiguate 'its/su prior administration'.
  ar_prior <- vapply(x, function(s) {
    chunks <- regmatches(s, gregexpr("\\bargentina(?:'s)?\\b(?:(?!\\b(?:reino unido|united kingdom|the uk|britain|british|britanica|britanico)\\b)[^.])*", s, perl=TRUE))[[1]]
    direct <- grepl("(?:presencia|administracion|autoridad|control|soberania) (?:de autoridades |propia |efectiva )?argentin[ao]s?[^.;]{0,40}(?:\\bprevi|\\banterior|\\bantes|hasta 1833|182[09])|argentin[ae] (?:administration|presence|authority|control)[^.;]{0,35}(?:before|prior|earlier|pre-1833)|earlier spanish/argentine presence", s, perl=TRUE)
    owned <- any(grepl("(?:\\bsu |\\bits |\\bits own )(?:(?:effective|efectiva|breve|prior|earlier|previa|propia|brief) )?(?:administracion|presencia|autoridad|administration|presence|authority)[^.;]{0,45}(?:\\bprevi|\\banterior|\\bantes|\\bbefore|\\bprior|\\bearlier|hasta 1833|182[09])|(?:\\bits |\\bsu )(?:prior|earlier|previa|anterior|brief|breve|1820s) (?:administracion|presencia|autoridad|administration|presence|authority)|(?:exercise|establishment) of authority before|(?:ejercicio de autoridad|actos de gobierno|control efectivo) (?:antes|anteriores)|una autoridad efectiva antes|una administracion propia hasta 1833|(?:administracion|presencia) (?:previa|y administracion antes)|presencia y administracion antes", chunks, perl=TRUE))
    direct || owned
  }, logical(1))
  # Do not attach the 1982 occupation exception to a nearby 'since 1833'.
  xd <- gsub("(?:ocupacion|occupation)[^.;]{0,30}1982|1982 (?:argentine )?occupation", "", x, perl=TRUE)
  dhit <- function(p) grepl(p, xd, perl=TRUE)
  displacement <- dhit("\\b(?:expuls\\w*|expel\\w*|remov\\w*|remoci\\w*)\\b[^.;]{0,100}(?:argentin|1833|autoridad|authorit|official)|(?:argentin|1833|autoridad|authorit|official)[^.;]{0,100}\\b(?:expuls\\w*|expel\\w*|remov\\w*|remoci\\w*)\\b") |
    dhit("\\b(?:displac\\w*|desplaz\\w*)\\b[^.;]{0,35}(?:1833|authorit|official|autoridad|funcionari|argentin)|(?:autoridad|authorit|official)[^.;]{0,35}\\b(?:displac\\w*|desplaz\\w*)\\b|1833[- ](?:displacement|desplazamiento)|unlawful displacement|wrongful british displacement") |
    dhit("(?:toma|ocupacion|takeover|seizure|reassert\\w*|return|reimplantacion|restablec\\w*|reocupacion|retorno|took control)[^.;]{0,55}1833|1833[^.;]{0,55}(?:toma|ocupacion|takeover|seizure|reassert\\w*|return|reimplantacion|restablec\\w*|reocupacion|retorno|took control)") |
    dhit("(?:toma|takeover|seizure|reimplantacion|reocupacion) (?:del control |de control )?(?:britanic|british)|brit(?:ish|ain's) (?:takeover|seizure|reoccupation)|\\b(?:ruptura|interrupcion|interrump\\w*|interrupted)[^.;]{0,70}1833|1833[^.;]{0,70}\\b(?:ruptura|interrupcion|interrump\\w*|interrupted)|\\b(?:ruptura|interrupcion|interrump\\w*|interrupted)[^.;]{0,45}(?:autoridad argentina|administracion argentina|soberania argentina|argentine authority|argentine sovereignty)") |
    dhit("(?:protesta|objecion)[^.;]{0,35}ocupacion britanica|toma de control de ese ano|toma de control por la fuerza|origen de esa ocupacion|origen de su control en 1833|restablecio (?:su presencia|el control britanico)|retomo el control|legitimidad de aquella toma") |
    dhit("(?:hechos|episodio|circunstancias|acontecimientos) de 1833|1833 events|events of 1833|circumstances of (?:the )?(?:britain's )?1833|circumstances of britain's return|(?:cambio de control|change of control)[^.;]{0,20}1833")
  uk_admin <- hit("(?:administracion|administration|control|gobierno|governance|rule|continuidad)[^.;]{0,30}(?:britanic|british)|(?:britanic\\w*|british|britain's|uk's|united kingdom's)[^.;]{0,60}(?:administracion|administration|control|gobierno|governance|rule)|ocupacion britanica (?:desde|efectiva|posterior|prolongada)") |
    hit("(?:reino unido|united kingdom|\\buk\\b|britain)[^.;]{0,220}(?:administra|administer|governed|administration|control efectivo|control continu|control prolong|effective control|long-standing control|sustained control|ejercicio prolongado|ejercicio efectivo|long-standing governance|presencia efectiva|gobierno prolongado|posesion efectiva)")
  wishes <- hit("autodeterminacion|self[- ]determination|libre determinacion|\\bvoluntad\\b|\\bwish(?:es)?\\b|\\bpreferenc\\w*|\\bdeseo\\w*|\\breferend\\w*|\\bwill of\\b|\\bchosen\\b[^.;]{0,50}(?:british|sovereign)|(?:inhabitants|islanders|residents|population|habitantes|islenos|poblacion)[^.;]{0,80}(?:wish|vote|vot|dese|prefer|apoyo|support|favor|remain|determine)|(?:apoyo|respaldo|support|choice|eleccion)[^.;]{0,35}(?:habitantes|islenos|poblacion|inhabitants|islanders|residents|population)")
  data.frame(
    ES_TITLE = hit("\\bespana\\b|\\bespanol\\w*|\\bspain\\b|\\bspanish\\b"),
    AR_PRE1833 = ar_prior,
    DISPLACEMENT_1833 = displacement,
    UK_ADMIN = uk_admin,
    ISLANDERS_WISHES = wishes,
    AR_INTEGRITY = hit("integridad territorial|territorial[- ]integrity"),
    AR_PROXIMITY = hit("\\bproximidad\\b|\\bcercania\\b|\\bproximity\\b|\\bgeografic\\w*|\\bgeographic\\w*|\\bgeography\\b"),
    UN_NEGOTIATION = hit("\\bonu\\b|naciones unidas|united nations|\\bnegocia\\w*|\\bnegotiat\\w*") | grepl("\\bUN\\b|\\bU\\.N\\.", original, perl=TRUE),
    check.names = FALSE
  )
}
