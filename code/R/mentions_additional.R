# Additional bilingual mention rules. Offline; only explanation text is used.
# These are descriptive mentions, not endorsement or truth judgements.
# Parent pipeline supplies the already-audited official NO_WEB / CONTROL subset.

additional_codebook <- data.frame(
  code = c("UK_EARLY", "AR_PROTEST_CONTINUITY", "NO_FINAL_RULING", "SELFDET_LIMITS"),
  label = c("Presencia o reclamo británico anterior a 1833",
            "Continuidad de la protesta o del reclamo argentino",
            "Falta de fallo o decisión internacional definitiva",
            "Límites de la autodeterminación o voluntad isleña"),
  definition = c(
    "Mención explícita de presencia, asentamiento, título o reclamo británico anterior al control iniciado en 1833; incluye su interrupción en 1774. No basta la administración desde 1833 ni una presencia anterior argentina o española.",
    "Protesta o reclamo argentino explícitamente sostenido, persistente o continuado, incluida impugnación desde entonces. No basta una protesta puntual por 1833, una reclamación histórica ni la continuidad del título español.",
    "Ausencia explícita de fallo, sentencia, adjudicación o decisión internacional definitiva sobre la soberanía, incluida una ONU que no la ha resuelto/adjudicado. No basta afirmar que hay disputa, que sigue sin resolver o que los argumentos no son concluyentes. Se excluye el mero aviso de que esta evaluación no equivale a un fallo, pues no afirma ausencia de adjudicación real.",
    "Mención explícita de controversia, limitación o insuficiencia de la autodeterminación/voluntad isleña para decidir soberanía, incluidas reservas sobre la formación colonial de la población. Incluye una limitación formulada conjuntamente sobre voluntad y administración británicas; no basta declarar globalmente disputada la soberanía."
  ), stringsAsFactors = FALSE
)

.additional_normalize <- function(e) {
  stopifnot(is.character(e), !anyNA(e))
  if (!requireNamespace("stringi", quietly = TRUE)) stop("stringi is required")
  x <- tolower(stringi::stri_trans_general(e, "Latin-ASCII"))
  x <- gsub("[\u2018\u2019]", "'", x, perl = TRUE)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x, perl = TRUE)
  gsub("[[:space:]]+", " ", x, perl = TRUE)
}

additional_rules <- list(
  UK_EARLY = c(
    "(united kingdom|the uk|britain|reino unido)(?:(?!argentina|spain)[^.;]){0,150}(earlier (british )?(presence|claims?|settlement)|prior claims?|presencia (anterior|previa)|presencia y (un )?reclam[oa][^.;]{0,15}anterior|reclamacion (anterior|previa))",
    "earlier british (presence|claims?|settlement)",
    "presencia britanica (anterior|previa)",
    "(britain|britanica)[^.;]{0,90}1774",
    "toma britanica[^.;]{0,60}presencia previa interrumpida"
  ),
  AR_PROTEST_CONTINUITY = c(
    "protesta[s]? (sostenida[s]?|persistente[s]?|continuada[s]?)",
    "continuidad de (la |los |sus |su )?(protesta[s]?|reclamo[s]?)",
    "(continuing|persistent|sustained|continuous) (protest[s]?|claim[s]?)",
    "impugno desde entonces"
  ),
  NO_FINAL_RULING = c(
    "(no |ningun |ninguna |neither |sin |not |never |nunca |without )[^.;]{0,105}(tribunal|court|sentencia|fallo|ruling|adjudic)",
    "no (hay|existe|hubo|ha habido|tiene)[^.;]{0,45}(resolucion|decision)[^.;]{0,45}(internacional|definitiv|judicial)",
    "no (hay|existe|hubo|ha habido)[^.;]{0,45}(resolucion|decision) internacional",
    "(sin |no (ha )?)(adjudic|resuelto|resolvi)(?:(?!por si|voluntad)[^.;]){0,45}(soberania|titulo|islas|parte|cual|quien)",
    "(onu|the un|united nations|international bod|un resolutions)[^.;]{0,150}(sin resolver(la| el| la| a| quien| cual| definitivamente)|sin adjudicar|no (la )?ha resuelto|no (lo )?ha resuelto|no (ha )?adjudicado|has not settled|has not resolved|without resolving|without deciding|rather than (settled|settling|resolving)|do not settle|no ha dictado una decision)",
    "sin (una )?(decision|resolucion) (judicial|internacional|juridica)",
    "(ningun[oa]?|no) [^.;]{0,55}(decision|resolucion) (internacional|judicial)",
    "(ningun[oa]?|no) [^.;]{0,55}resolucion juridica definitiva",
    "no ha sido (resuelta|decidida|zanjada) judicialmente",
    "sin haber adjudicado",
    "(sovereignty|dispute|claims)[^.;]{0,70}(not been|never been)[^.;]{0,30}adjudicat",
    "(unresolved|disputed)[^.;]{0,40}rather than[^.;]{0,55}(court|ruling)",
    "(onu|the un|united nations|international bod|un resolutions)[^.;]{0,150}rather than (conclusively |definitively )?resolved"
  ),
  SELFDET_LIMITS = c(
    "(autodeterminacion|libre determinacion|self-determination)(?:(?!por lo que|so the|historical and legal record|legal balance|la disputa|the dispute)[^.;]){0,120}((?<!in)discutid|se discute|controvert|controvers|contested|disputed|complicated|no resuelv|no determina|no zanj|no ha sido (aceptad|considerad)|not (automatically |by itself )?decisive|less decisive|does not|do not|not settle)",
    "(peso|alcance|aplicacion|aplicabilidad)[^.;]{0,70}(autodeterminacion|libre determinacion|voluntad)[^.;]{0,90}(discutid|controvert|controvers)",
    "(will|wishes|wish|preference|voluntad|referendum)(?:(?!argentina|argentine|por lo que|so the|el origen|la legalidad|la validez|historical|legal balance|overall legal|the dispute|la disputa|the un treats|la soberania previa|that administration|the legitimacy|modo en que|balance is|sovereignty remains|esa continuidad|el titulo historico)[^.;]){0,100}(no resuelv|no ha resuelto|no determina|no zanj|not by (itself|themselves)|does not|do not|neither settles|not (automatically )?decisive|(?<!in)discutid|se discute|controvert|disputed|contested)",
    "(esta ultima|esa voluntad|ese ultimo (argumento|principio)|ese principio|este ultimo|that preference|that self-determination argument|the latter)[^.;]{0,110}(discutid|controvert|no resuelv|no determina|no zanj|no ha sido considerada|not |does not|do not|disputed|contested|complicated)",
    "(voluntad|islenos|wishes|preference|self-determination|autodeterminacion)[^.;]{0,100}(pero |but |though |although |aunque )(?:(?!esa continuidad)[^.;]){0,75}(no resuelv|no han resuelto|no zanj|do not|does not|not by |neither point|not settle)",
    "(voluntad|wishes|preference|self-determination|autodeterminacion)[^.;]{0,100}(esos factores|esos elementos|estos factores|those factors|those points|they|these)[^.;]{0,55}(no resuelv|no zanj|do not|not |do not)",
    "(poblacion|population)[^.;]{0,45}(establecida|asentada|se asento|se formo|se consolido|became established)[^.;]{0,60}(britanic|despues|bajo|aquella|disputa)",
    "(rather than|not as|sin (considerar|tratar))[^.;]{0,65}(wishes|vote|referendum|self-determination|voluntad|autodeterminacion)[^.;]{0,50}(decisiv|concluy|resuelv|settled|simple)",
    "(wishes|voluntad)[^.;]{0,55}(politically|politicamente)[^.;]{0,80}(but|pero)[^.;]{0,35}(not|no )",
    "(intereses|interests)[^.;]{0,45}(no su voluntad|rather than making their wishes decisive|rather than a conclusive sovereignty ruling)",
    "no (como|un caso de)[^.;]{0,30}autodeterminacion",
    "(no (la )?ha resuelto|no ha adjudicado)[^.;]{0,70}(mediante|por)[^.;]{0,20}autodeterminacion",
    "(disputed|contested)[^.;]{0,70}legal weight of self-determination",
    "(voluntad|wishes|autodeterminacion)[^.;]{0,100}(pesan menos|weigh less|less weight)[^.;]{0,50}(descolonizacion|decolonization)",
    "(voluntad|wishes|autodeterminacion)[^.;]{0,130}(esos factores|those factors)[^.;]{0,80}(no resuelv|do not)",
    "(voluntad|wishes|autodeterminacion)[^.]{0,100};[^.]{0,30}(esos factores|el peso de esos argumentos)[^.]{0,60}(no resuelv|controvertid)",
    "(not settled|not resolved)[^.;]{0,25}(by|through)[^.;]{0,30}(preference|wishes|vote|self-determination)",
    "contested[^.;]{0,80}(relevance|legal weight)[^.;]{0,40}self-determination",
    "(voluntad|wishes)[^.;]{0,85}ninguno de esos elementos resuelv",
    "rather than (resolving|settling)[^.;]{0,65}(vote|wishes|preference)",
    "(voluntad|wishes)[^.;]{0,85}no ha resuelto que esos argumentos",
    "voluntad[^.;]{0,100}no obtuvo soberania por eso",
    "(no ha reconocido que su voluntad resuelva|voluntad no ha sido el criterio central)",
    "intereses de los habitantes, no a su voluntad como criterio decisivo",
    "sin avalar el referendum[^.;]{0,40}resolucion",
    "no considera[n]? (concluyente|decisiva)[^.;]{0,55}(autodeterminacion|voluntad)",
    "rather than (settled|resolved)[^.;]{0,65}(self-determination|vote|wishes|preference)"
  )
)

classify_additional <- function(e) {
  x <- .additional_normalize(e)
  out <- lapply(names(additional_rules), function(code) {
    pats <- additional_rules[[code]]
    z <- x
    if (code == "NO_FINAL_RULING") {
      # A disclaimer about this answer is not a claim about actual adjudication.
      z <- gsub("no (es clara ni )?equivale[^.;]*|no implica que exista[^.;]*|not a settled legal ruling", "", z, perl = TRUE)
    }
    Reduce(`|`, lapply(pats, function(p) grepl(p, z, perl = TRUE)))
  })
  names(out) <- names(additional_rules)
  as.data.frame(out, stringsAsFactors = FALSE)
}

test_additional_rules <- function() {
  fixtures <- data.frame(
    code = c(rep("UK_EARLY", 6), rep("AR_PROTEST_CONTINUITY", 6),
             rep("NO_FINAL_RULING", 9), rep("SELFDET_LIMITS", 14)),
    expected = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                 TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                 TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,
                 TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                 FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    e = c(
      "El Reino Unido invoca su presencia anterior y su administración continua desde 1833.",
      "The UK points to its earlier presence, long-standing administration, and the islanders' wishes.",
      "La presencia británica anterior había sido interrumpida.",
      "Argentina invokes its prior presence, while the UK has administered the islands since 1833.",
      "El Reino Unido administra desde 1833; Argentina invoca su presencia anterior.",
      "Argentina relies on Spain's earlier presence and succession.",
      "Argentina mantiene la continuidad de su reclamo.",
      "Argentina relies on its continuing protest against the 1833 takeover.",
      "Argentina impugnó desde entonces la toma británica.",
      "Argentina protesta por la toma británica de 1833.",
      "The UK has continuous administration, while Argentina protests the takeover.",
      "Argentina invoca la continuidad del título español.",
      "No international court has settled the competing claims.",
      "La ONU reconoce una disputa y pide negociar, sin resolver definitivamente la soberanía.",
      "La disputa sigue sin decisión judicial internacional.",
      "El caso no ha sido adjudicado definitivamente.",
      "La disputa sigue sin resolverse internacionalmente.",
      "The UN recognizes a dispute rather than endorsing either side's claim.",
      "Esta evaluación no equivale a un fallo internacional definitivo.",
      "No existe un fallo internacional. Esta evaluación no equivale a un fallo definitivo.",
      "International bodies have treated sovereignty as a dispute to be negotiated rather than conclusively resolved.",
      "El Reino Unido invoca administración y voluntad isleña; esos factores sostienen su posición, aunque no resuelven por sí solos la soberanía.",
      "The UK invokes administration and the islanders' wishes, but neither settles sovereignty.",
      "La voluntad de los isleños, cuyo peso jurídico se discute, no resuelve la soberanía.",
      "La autodeterminación no ha sido aceptada por la ONU como decisiva.",
      "El Reino Unido invoca autodeterminación. Como ese principio no resuelve por sí solo la disputa, la ventaja es moderada.",
      "La población actual se asentó bajo administración británica y la aplicación de autodeterminación es discutida.",
      "El Reino Unido tiene administración y voluntad de los isleños, aunque esa continuidad no resuelve por sí sola la disputa.",
      "El Reino Unido invoca la voluntad de los isleños, aunque el origen de esa administración es controvertido.",
      "The UK has the islanders' expressed wishes, so the balance is not decisive.",
      "Argentina's claims do not outweigh the islanders' wishes.",
      "Argentina y el Reino Unido invocan argumentos. Ninguno zanja la disputa.",
      "Argentina disputes the application of self-determination: the UK invokes the islanders' wishes, but that preference does not settle sovereignty.",
      "La ONU reconoce una disputa de soberanía y pide negociaciones, no considera concluyente la autodeterminación de los isleños.",
      "The UN has treated sovereignty as a dispute for negotiation rather than settled it through the islanders' self-determination."
    ), stringsAsFactors = FALSE
  )
  got <- classify_additional(fixtures$e)
  observed <- vapply(seq_len(nrow(fixtures)), function(i) got[[fixtures$code[i]]][i], logical(1))
  if (any(observed != fixtures$expected)) {
    stop("Additional-rule fixtures failed: ", paste(which(observed != fixtures$expected), collapse = ", "))
  }
  rev_expected <- got[rev(seq_len(nrow(got))), , drop = FALSE]
  rownames(rev_expected) <- NULL
  stopifnot(ncol(got) == 4L, nrow(got) == nrow(fixtures), !anyNA(got),
            identical(classify_additional(character()), got[FALSE, ]),
            identical(classify_additional(rev(fixtures$e)), rev_expected))
  invisible(list(status = "PASS", fixtures = nrow(fixtures)))
}
