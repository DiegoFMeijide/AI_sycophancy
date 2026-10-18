# Remove machine/user metadata that officer adds during print.rdocx().
# Only for newly generated publication outputs; never applied to source documents.
scrub_public_docx <- function(path) {
  stopifnot(file.exists(path), requireNamespace("xml2", quietly = TRUE), requireNamespace("zip", quietly = TRUE))
  path <- normalizePath(path)
  stage <- tempfile("public_docx_"); dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = stage)
  readx <- function(p) xml2::read_xml(p)
  corepath <- file.path(stage, "docProps/core.xml")
  core <- readx(corepath)
  authors <- xml2::xml_find_all(core, "//*[local-name()='creator' or local-name()='lastModifiedBy']")
  xml2::xml_text(authors) <- ""
  xml2::write_xml(core, corepath)
  paths <- list.files(stage, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  for (p in paths[grepl("[.]rels$|\\[Content_Types\\][.]xml$", paths)]) {
    x <- readx(p)
    bad <- xml2::xml_find_all(x, "//*[@Target or @PartName]")
    refs <- paste(xml2::xml_attr(bad, "Target"), xml2::xml_attr(bad, "PartName"))
    xml2::xml_remove(bad[grepl("comments[^/ ]*[.]xml|docProps/custom[.]xml", refs)])
    xml2::write_xml(x, p)
  }
  for (p in paths[grepl("[.]xml$", paths)]) {
    x <- readx(p)
    xml2::xml_remove(xml2::xml_find_all(x, "//*[local-name()='commentRangeStart' or local-name()='commentRangeEnd' or local-name()='commentReference']"))
    attrs <- xml2::xml_find_all(x, "//@*[starts-with(local-name(), 'rsid')]")
    xml2::xml_remove(attrs)
    xml2::write_xml(x, p)
  }
  removed <- paths[grepl("/word/comments[^/]*[.]xml$|/word/_rels/comments[^/]*[.]xml[.]rels$|/docProps/custom[.]xml$", paths)]
  if (length(removed)) unlink(removed)
  rebuilt <- tempfile(fileext = ".docx")
  on.exit(unlink(rebuilt), add = TRUE)
  zip::zipr(rebuilt, files = list.files(stage, recursive = TRUE, all.files = TRUE), root = stage, mode = "mirror")
  stopifnot("word/document.xml" %in% zip::zip_list(rebuilt)$filename)
  stopifnot(file.copy(rebuilt, path, overwrite = TRUE))
  invisible(path)
}
