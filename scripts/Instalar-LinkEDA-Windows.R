# Instalación interna usada por los instaladores de Windows.
local({
  if (.Platform$OS.type != "windows") {
    stop("Este instalador es para Windows.", call. = FALSE)
  }
  file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_argument)) {
    source_file <- sub("^--file=", "", tail(file_argument, 1L))
  } else {
    source_files <- lapply(sys.frames(), function(frame) frame$ofile)
    source_files <- Filter(function(path) is.character(path) && length(path) == 1L &&
                             !is.na(path) && nzchar(path), source_files)
    if (!length(source_files)) {
      stop("Abra este archivo desde R con source(file.choose()).", call. = FALSE)
    }
    source_file <- tail(source_files, 1L)[[1L]]
  }
  bundle <- dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE))
  archives <- list.files(bundle, pattern = "^LinkEDA_[0-9.]+_Windows[.]tar[.]gz$",
                         full.names = TRUE)
  if (length(archives) != 1L) {
    stop("Se esperaba un único archivo LinkEDA_*_Windows.tar.gz junto al instalador.",
         call. = FALSE)
  }
  metadata <- file.path(bundle, "LinkEDA-DESCRIPTION")
  if (!file.exists(metadata)) {
    stop("Falta LinkEDA-DESCRIPTION en la carpeta descargada.", call. = FALSE)
  }
  description <- read.dcf(metadata, fields = c("Package", "Version", "Depends", "Imports"))
  if (!identical(unname(description[1L, "Package"]), "LinkEDA")) {
    stop("Los metadatos del paquete no corresponden a LinkEDA.", call. = FALSE)
  }
  if (getRversion() < "4.1.0") {
    stop("LinkEDA necesita R 4.1.0 o posterior.", call. = FALSE)
  }
  dependencies <- paste(description[1L, c("Depends", "Imports")], collapse = ",")
  dependencies <- trimws(unlist(strsplit(dependencies, ",", fixed = TRUE)))
  dependencies <- sub("\\s*\\(.*$", "", dependencies)
  dependencies <- setdiff(unique(dependencies[nzchar(dependencies)]), "R")
  cran <- "https://cloud.r-project.org"
  available <- utils::available.packages(repos = cran, type = "binary")
  transitive <- tools::package_dependencies(
    dependencies, db = available,
    which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE
  )
  required <- unique(c(dependencies, unlist(transitive, use.names = FALSE)))
  required <- required[!is.na(required) & nzchar(required)]
  missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
  unavailable <- setdiff(missing, rownames(available))
  if (length(unavailable)) {
    stop("CRAN no ofrece binarios compatibles con esta versión de R para: ",
         paste(unavailable, collapse = ", "), call. = FALSE)
  }

  personal <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(personal)) {
    personal <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library",
                          paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = "."))
  }
  personal <- path.expand(personal)
  if (!dir.exists(personal) && !dir.create(personal, recursive = TRUE)) {
    stop("No se pudo crear la biblioteca personal de R: ", personal, call. = FALSE)
  }
  .libPaths(unique(c(personal, .libPaths())))

  if (length(missing)) {
    message("Instalando paquetes necesarios desde CRAN: ", paste(missing, collapse = ", "))
    utils::install.packages(missing, lib = personal, repos = cran,
                            dependencies = FALSE, type = "binary")
  }
  missing <- required[!vapply(required, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing)) {
    stop("No se pudieron instalar estos paquetes para R ",
         paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = "."),
         ": ", paste(missing, collapse = ", "), call. = FALSE)
  }

  message("Instalando LinkEDA ", description[1L, "Version"], "...")
  utils::install.packages(archives, repos = NULL, type = "source", lib = personal)
  if (!requireNamespace("LinkEDA", quietly = TRUE)) {
    stop("R no pudo cargar LinkEDA después de instalarlo.", call. = FALSE)
  }
  message("LinkEDA instalado correctamente.")
})
