#include "dataset_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <ostream>
#include <set>
#include <sstream>

namespace rlispstat {
namespace core {

namespace {

std::string TrimCopy(const std::string &text)
{
    std::size_t start = 0;
    while (start < text.size() && std::isspace(static_cast<unsigned char>(text[start]))) {
        ++start;
    }
    std::size_t end = text.size();
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1]))) {
        --end;
    }
    return text.substr(start, end - start);
}

std::string LowerCopy(const std::string &text)
{
    std::string out = text;
    std::transform(out.begin(), out.end(), out.begin(),
                   [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return out;
}

std::string JoinValues(const std::vector<std::string> &values, const std::string &separator)
{
    std::ostringstream out;
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i) {
            out << separator;
        }
        out << values[i];
    }
    return out.str();
}

std::string LimitInlineCellText(const std::string &text, std::size_t maxChars)
{
    if (text.size() <= maxChars) {
        return text;
    }
    if (maxChars <= 3) {
        return text.substr(0, maxChars);
    }
    return text.substr(0, maxChars - 3) + "...";
}

double QuantileValue(std::vector<double> values, double probability)
{
    values.erase(std::remove_if(values.begin(), values.end(), [](double value) {
        return !std::isfinite(value);
    }), values.end());
    if (values.empty()) {
        return NAN;
    }
    std::sort(values.begin(), values.end());
    if (values.size() == 1) {
        return values.front();
    }
    probability = std::max(0.0, std::min(1.0, probability));
    double position = probability * static_cast<double>(values.size() - 1);
    std::size_t lower = static_cast<std::size_t>(std::floor(position));
    std::size_t upper = std::min(values.size() - 1, lower + 1);
    double fraction = position - static_cast<double>(lower);
    return values[lower] + (values[upper] - values[lower]) * fraction;
}

} // namespace

bool DataCellIsMissing(const std::string &value)
{
    return value.empty() || value == "NA" || value == "NaN";
}

bool ParseDataCellDouble(const std::string &value, double &out)
{
    if (DataCellIsMissing(value)) {
        return false;
    }
    char *endptr = nullptr;
    out = std::strtod(value.c_str(), &endptr);
    while (endptr && *endptr && std::isspace(static_cast<unsigned char>(*endptr))) {
        ++endptr;
    }
    return endptr && endptr != value.c_str() && *endptr == '\0' && std::isfinite(out);
}

double ParseOptionalDataCellDouble(const std::string &value)
{
    double parsed = NAN;
    return ParseDataCellDouble(value, parsed) ? parsed : NAN;
}

int ParseOptionalDataCellInt(const std::string &value)
{
    double parsed = NAN;
    if (!ParseDataCellDouble(value, parsed)) {
        return 0;
    }
    return static_cast<int>(std::llround(parsed));
}

std::vector<int> ParseRowIdList(const std::string &text)
{
    std::vector<int> rows;
    std::string current;
    std::istringstream input(text);
    while (std::getline(input, current, ',')) {
        if (current.empty()) {
            continue;
        }
        int row = std::atoi(current.c_str());
        if (row > 0) {
            rows.push_back(row);
        }
    }
    return rows;
}

std::string CsvEscape(const std::string &value)
{
    bool quote = value.find_first_of(",\"\n\r") != std::string::npos;
    std::string out;
    for (char ch : value) {
        if (ch == '"') {
            out += "\"\"";
        } else {
            out += ch;
        }
    }
    return quote ? "\"" + out + "\"" : out;
}

bool WriteDataFrameCSV(std::ostream &out,
                       const DataFrameModel &df,
                       const std::vector<int> &includeRows)
{
    out << "..rlispstat_row_id";
    for (std::size_t c = 0; c < df.columns.size(); ++c) {
        out << ",";
        out << CsvEscape(df.columns[c].name);
    }
    out << "\n";
    std::set<int> include(includeRows.begin(), includeRows.end());
    for (int row = 0; row < df.rows; ++row) {
        if (!include.empty() && include.find(row + 1) == include.end()) {
            continue;
        }
        out << (row + 1);
        for (std::size_t c = 0; c < df.columns.size(); ++c) {
            out << ",";
            const DataColumn &col = df.columns[c];
            std::string value = row < static_cast<int>(col.values.size()) ? col.values[(std::size_t)row] : "NA";
            out << CsvEscape(value);
        }
        out << "\n";
    }
    return true;
}

DataFrameModel SubsetDataFrame(const DataFrameModel &source,
                               const std::vector<int> &originalRowIds,
                               const std::string &newGroup)
{
    std::vector<std::size_t> indices;
    std::set<int> seen;
    for (int row : originalRowIds) {
        if (row <= 0 || row > source.rows || !seen.insert(row).second) continue;
        indices.push_back(static_cast<std::size_t>(row - 1));
    }

    auto subsetStrings = [&](const std::vector<std::string> &values) {
        std::vector<std::string> result;
        result.reserve(indices.size());
        for (std::size_t index : indices) {
            result.push_back(index < values.size() ? values[index] : std::string());
        }
        return result;
    };
    auto subsetBools = [&](const std::vector<bool> &values) {
        std::vector<bool> result;
        result.reserve(indices.size());
        for (std::size_t index : indices) {
            result.push_back(index < values.size() ? values[index] : false);
        }
        return result;
    };
    auto subsetSparse = [&](const std::map<std::size_t, std::string> &values) {
        std::map<std::size_t, std::string> result;
        for (std::size_t target = 0; target < indices.size(); ++target) {
            auto found = values.find(indices[target]);
            if (found != values.end()) result[target] = found->second;
        }
        return result;
    };

    DataFrameModel subset = source;
    subset.group = newGroup;
    subset.rows = static_cast<int>(indices.size());
    if (source.datasetType != "multiple_imputation") {
        subset.sourceDatasetId = source.group;
    }
    for (DataColumn &column : subset.columns) {
        column.values = subsetStrings(column.values);
        if (!column.displayValues.empty()) {
            column.displayValues = subsetStrings(column.displayValues);
        }
        if (!column.imputedMissing.empty()) {
            column.imputedMissing = subsetBools(column.imputedMissing);
        }
        if (!column.imputationOriginalValues.empty()) {
            column.imputationOriginalValues = subsetStrings(column.imputationOriginalValues);
        }
        for (std::vector<std::string> &version : column.imputationValues) {
            version = subsetStrings(version);
        }
        column.imputationOriginalSparse = subsetSparse(column.imputationOriginalSparse);
        for (std::map<std::size_t, std::string> &version : column.imputationValuesSparse) {
            version = subsetSparse(version);
        }
    }
    return subset;
}

std::string NativeImportRScript()
{
    return R"RLSIMPORT(
args <- commandArgs(TRUE)
if (length(args) < 3L) stop("Expected path, dataset name, and output path.", call. = FALSE)
path <- args[[1L]]
dataset_name <- args[[2L]]
output <- args[[3L]]

fail <- function(...) stop(sprintf(...), call. = FALSE)
clean_names <- function(names) {
  names <- as.character(names)
  names[is.na(names) | !nzchar(names)] <- "V"
  names <- gsub("[\r\n\t|]+", " ", names)
  names <- trimws(names)
  names[!nzchar(names)] <- "V"
  make.unique(names)
}
check_path <- function(path) {
  if (!file.exists(path)) fail("Cannot import data because file does not exist: %s", path)
  if (file.access(path, 4) != 0L) fail("Cannot import data because file is not readable: %s", path)
  normalizePath(path, mustWork = TRUE)
}
read_import <- function(path) {
  ext <- tolower(tools::file_ext(path))
  source <- NULL
  data <- switch(
    ext,
    csv = {
      source <- "CSV file"
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    },
    tsv = ,
    txt = {
      source <- "Delimited text file"
      utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
    },
    sav = ,
    zsav = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SPSS file because package 'haven' is not installed.")
      source <- "SPSS file"
      haven::read_sav(path)
    },
    dta = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import Stata file because package 'haven' is not installed.")
      source <- "Stata file"
      haven::read_dta(path)
    },
    sas7bdat = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SAS file because package 'haven' is not installed.")
      source <- "SAS file"
      haven::read_sas(path)
    },
    xpt = {
      if (!requireNamespace("haven", quietly = TRUE)) fail("Cannot import SAS transport file because package 'haven' is not installed.")
      source <- "SAS transport file"
      haven::read_xpt(path)
    },
    xls = ,
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) fail("Cannot import Excel file because package 'readxl' is not installed.")
      sheets <- readxl::excel_sheets(path)
      if (!length(sheets)) fail("Cannot import Excel file because no readable sheets were found.")
      source <- paste0("Excel file: ", sheets[[1L]])
      readxl::read_excel(path, sheet = sheets[[1L]], .name_repair = "unique")
    },
    rds = {
      source <- "RDS file"
      readRDS(path)
    },
    rda = ,
    rdata = {
      env <- new.env(parent = emptyenv())
      loaded <- load(path, envir = env)
      data_names <- loaded[vapply(loaded, function(object_name) is.data.frame(get(object_name, envir = env)), logical(1L))]
      if (!length(data_names)) fail("The selected R data file did not contain a data frame.")
      sizes <- vapply(data_names, function(object_name) {
        data <- get(object_name, envir = env)
        nrow(data) * max(1L, ncol(data))
      }, numeric(1L))
      selected <- data_names[[which.max(sizes)]]
      source <- paste0("R data file: ", selected)
      get(selected, envir = env)
    },
    fail("Unsupported data file. Use .csv, .txt, .tsv, .sav, .zsav, .dta, .sas7bdat, .xpt, .xlsx, .xls, .rds, .rda, or .RData.")
  )
  list(data = data, source = source)
}
variable_type <- function(x) {
  if (is.numeric(x)) "numeric"
  else if (is.ordered(x)) "ordered"
  else if (is.factor(x)) "factor"
  else if (is.character(x)) "character"
  else if (is.logical(x)) "logical"
  else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) "datetime"
  else "other"
}
variable_payload <- function(data) {
  numeric_names <- names(data)[vapply(data, is.numeric, logical(1L))]
  lines <- c("VARS", as.character(length(numeric_names)))
  for (name in numeric_names) {
    values <- as.double(data[[name]])
    value_lines <- ifelse(is.na(values) | !is.finite(values), "NA", sprintf("%.17g", values))
    lines <- c(lines, name, as.character(length(values)), value_lines)
  }
  lines <- c(lines, "VARMETA", as.character(length(names(data))))
  for (name in names(data)) {
    lines <- c(lines, name, variable_type(data[[name]]))
  }
  lines
}
dataframe_payload <- function(data, max_cell_chars = 120L) {
  clean_values <- function(values) {
    values[is.na(values)] <- "NA"
    values <- substr(values, 1L, max_cell_chars)
    gsub("[\r\n\t|]", " ", values)
  }
  raw_values <- function(x) {
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      raw <- unclass(x)
      attributes(raw) <- NULL
      return(as.character(raw))
    }
    as.character(x)
  }
  labelled_values <- function(x) {
    labels <- attr(x, "labels", exact = TRUE)
    raw <- if (!is.null(labels)) {
      tmp <- unclass(x)
      attributes(tmp) <- NULL
      tmp
    } else {
      x
    }
    out <- as.character(raw)
    if (!is.null(labels) && length(labels)) {
      label_values <- unclass(labels)
      attributes(label_values) <- NULL
      label_names <- names(labels)
      if (is.null(label_names)) label_names <- rep("", length(labels))
      if (length(label_names) < length(labels)) {
        label_names <- c(label_names, rep("", length(labels) - length(label_names)))
      }
      for (i in seq_along(label_values)) {
        label <- label_names[[i]]
        if (!length(label) || is.na(label) || !nzchar(label)) label <- as.character(label_values[[i]])
        out[!is.na(raw) & raw == label_values[[i]]] <- label
      }
    }
    out
  }
  lines <- c("DATAFRAME", as.character(nrow(data)), as.character(ncol(data)))
  display <- list()
  for (name in names(data)) {
    values <- clean_values(raw_values(data[[name]]))
    shown <- clean_values(labelled_values(data[[name]]))
    if (!identical(values, shown)) display[[name]] <- shown
    lines <- c(lines, name, variable_type(data[[name]]), values)
  }
  if (length(display)) {
    lines <- c(lines, "DATADISPLAY", as.character(length(display)))
    for (name in names(display)) {
      lines <- c(lines, name, display[[name]])
    }
  }
  lines
}

tryCatch({
  path <- check_path(path)
  imported <- read_import(path)
  if (!is.data.frame(imported$data)) fail("The selected file was read successfully, but it did not contain a data frame.")
  data <- as.data.frame(imported$data, stringsAsFactors = FALSE)
  if (!nrow(data) || !ncol(data)) fail("The selected file was read successfully, but it contained an empty data frame.")
  names(data) <- clean_names(names(data))
  payload <- c("REGISTER_DATASET", dataset_name, variable_payload(data), dataframe_payload(data))
  writeLines(payload, output, useBytes = TRUE)
}, error = function(e) {
  message(conditionMessage(e))
  quit(status = 1L)
})
)RLSIMPORT";
}

std::string NativeMiceImputationRScript()
{
    return R"RLSMICE(
args <- commandArgs(TRUE)
if (length(args) < 9L) stop("Expected input, output, dataset name, impute variables, predictors, methods, m, maxit, and seed.", call. = FALSE)
scalar_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return("")
  as.character(x[[1L]])
}
has_text <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(x)
}
input <- args[[1L]]
output <- args[[2L]]
dataset_name <- args[[3L]]
impute_text <- args[[4L]]
predictor_text <- args[[5L]]
method_text <- args[[6L]]
m <- as.integer(args[[7L]])
maxit <- as.integer(args[[8L]])
seed_text <- scalar_text(args[[9L]])
seed <- if (has_text(seed_text)) as.integer(seed_text) else NULL

missing_msg <- 'Multiple imputation requires the mice package.\nInstall it with install.packages("mice") and try again.'
if (!requireNamespace("mice", quietly = TRUE)) stop(missing_msg, call. = FALSE)

fail <- function(...) stop(sprintf(...), call. = FALSE)
stage <- "initializing"
clean_values <- function(values, max_cell_chars = 120L) {
  values <- as.character(values)
  values[is.na(values)] <- "NA"
  values <- substr(values, 1L, max_cell_chars)
  gsub("[\r\n\t|]", " ", values)
}
split_names <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (length(grep("\t", text, fixed = TRUE))) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
split_fields <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (length(grep("\t", text, fixed = TRUE))) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
parse_methods <- function(text) {
  fields <- split_fields(text)
  out <- character()
  for (field in fields) {
    pair <- strsplit(field, "=", fixed = TRUE)[[1L]]
    left <- if (length(pair) >= 1L) trimws(pair[[1L]]) else ""
    right <- if (length(pair) >= 2L) trimws(pair[[2L]]) else ""
    if (length(pair) != 2L || !has_text(left) || !has_text(right)) {
      fail("Malformed imputation method specification.")
    }
    out[[left]] <- right
  }
  out
}
is_missing_text <- function(x) is.na(x) | x %in% c("", "NA", "NaN")
variable_type <- function(x) {
  if (is.numeric(x)) "numeric"
  else if (is.ordered(x)) "ordered"
  else if (is.factor(x)) "factor"
  else if (is.character(x)) "character"
  else if (is.logical(x)) "logical"
  else "other"
}
read_payload <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  i <- 1L
  next_line <- function() {
    if (i > length(lines)) fail("Malformed imputation payload.")
    value <- lines[[i]]
    i <<- i + 1L
    value
  }
  if (!identical(next_line(), "DATASET")) fail("Malformed imputation payload.")
  source <- next_line()
  rows <- as.integer(next_line())
  cols <- as.integer(next_line())
  data <- list()
  types <- character()
  for (ci in seq_len(cols)) {
    name <- next_line()
    type <- next_line()
    values <- vapply(seq_len(rows), function(ri) next_line(), character(1L))
    miss <- is_missing_text(values)
    if (identical(type, "numeric")) {
      x <- suppressWarnings(as.numeric(values))
      x[miss] <- NA_real_
    } else if (identical(type, "logical")) {
      text <- tolower(values)
      x <- rep(NA, length(values))
      x[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
      x[text %in% c("false", "f", "0", "no", "n")] <- FALSE
    } else if (identical(type, "factor") || identical(type, "ordered")) {
      x <- values
      x[miss] <- NA_character_
      x <- factor(x, ordered = identical(type, "ordered"))
    } else {
      x <- values
      x[miss] <- NA_character_
    }
    data[[name]] <- x
    types[[name]] <- type
  }
  list(source = source, data = as.data.frame(data, check.names = FALSE), types = types)
}
is_id_like <- function(x, name) {
  lname <- tolower(scalar_text(name))
  if (length(grep("(^id$|_id$|^id_|identifier|subject|case|etiqueta|label)", lname))) return(TRUE)
  observed <- x[!is.na(x)]
  if (!length(observed)) return(FALSE)
  if (is.numeric(observed) || is.integer(observed)) {
    sorted <- sort(unique(observed))
    return(length(sorted) == length(observed) &&
           all(abs(sorted - round(sorted)) < .Machine$double.eps^0.5) &&
           all(diff(sorted) == 1))
  }
  length(unique(observed)) == length(observed) && length(observed) >= 0.9 * length(x)
}
default_method <- function(data, types, variable) {
  x <- data[[variable]]
  type <- types[[variable]]
  observed <- x[!is.na(x)]
  if (!length(observed)) return(NA_character_)
  if (identical(type, "numeric")) return("pmm")
  if (identical(type, "logical")) return("logreg")
  if (identical(type, "character")) {
    return(if (nlevels(factor(observed)) <= 2L) "logreg" else "polyreg")
  }
  if (identical(type, "ordered")) {
    n <- nlevels(droplevels(x))
    if (n <= 2L) "logreg" else "polr"
  } else if (identical(type, "factor")) {
    n <- nlevels(droplevels(x))
    if (n <= 2L) "logreg" else if (is.ordered(x)) "polr" else "polyreg"
  } else {
    NA_character_
  }
}
variable_payload <- function(data, types) {
  numeric_names <- names(data)[vapply(names(data), function(nm) identical(types[[nm]], "numeric"), logical(1L))]
  lines <- c("VARS", as.character(length(numeric_names)))
  for (name in numeric_names) {
    values <- suppressWarnings(as.double(data[[name]]))
    value_lines <- ifelse(is.na(values) | !is.finite(values), "NA", sprintf("%.17g", values))
    lines <- c(lines, name, as.character(length(values)), value_lines)
  }
  lines <- c(lines, "VARMETA", as.character(length(names(data))))
  for (name in names(data)) lines <- c(lines, name, types[[name]])
  lines
}
dataframe_payload <- function(current, original, completed, mask, types, impute_id, source_id) {
  lines <- c("DATAFRAME", as.character(nrow(current)), as.character(ncol(current)))
  for (name in names(current)) {
    lines <- c(lines, name, types[[name]], clean_values(current[[name]]))
  }
  imputed_names <- names(current)[vapply(names(current), function(name) {
    name %in% names(mask) && any(mask[[name]])
  }, logical(1L))]
  lines <- c(lines, "IMPUTATION_SPARSE", "multiple_imputation", impute_id, source_id,
             as.character(length(completed)), "1", "version", as.character(length(imputed_names)))
  for (name in imputed_names) {
    missing_rows <- which(mask[[name]])
    lines <- c(lines, name, as.character(length(missing_rows)), as.character(missing_rows),
               clean_values(original[[name]][missing_rows]))
    for (version in seq_along(completed)) {
      lines <- c(lines, clean_values(completed[[version]][[name]][missing_rows]))
    }
  }
  lines
}

tryCatch({
  stage <- "reading native data payload"
  payload <- read_payload(input)
  data <- payload$data
  types <- payload$types
  stage <- "parsing imputation selections"
  impute <- split_names(impute_text)
  predictors <- split_names(predictor_text)
  method_overrides <- parse_methods(method_text)
  supported <- names(data)[types %in% c("numeric", "factor", "ordered", "character", "logical")]
  stage <- "choosing variables"
  if (!length(impute)) {
    impute <- supported[vapply(supported, function(nm) any(is.na(data[[nm]])) && !all(is.na(data[[nm]])) && !is_id_like(data[[nm]], nm), logical(1L))]
  }
  if (!length(predictors)) {
    predictors <- supported[vapply(supported, function(nm) !all(is.na(data[[nm]])) && !is_id_like(data[[nm]], nm), logical(1L))]
  }
  predictors <- predictors[vapply(predictors, function(nm) {
    x <- data[[nm]]
    observed <- x[!is.na(x)]
    length(unique(observed)) > 1L && !is_id_like(x, nm)
  }, logical(1L))]
  unknown <- setdiff(c(impute, predictors), names(data))
  if (length(unknown)) fail("Variable '%s' was not found in the dataset.", unknown[[1L]])
  if (!length(impute)) fail("No variables with missing values were selected for imputation.")
  if (!length(predictors)) fail("No valid predictor variables were selected.")
  stage <- "validating imputation variables"
  for (nm in impute) {
    if (!any(is.na(data[[nm]]))) fail("Variable '%s' has no missing values to impute.", nm)
    if (all(is.na(data[[nm]]))) fail("Variable '%s' has all values missing and cannot be imputed safely.", nm)
  }
  stage <- "building mice method vector"
  methods <- rep("", ncol(data))
  names(methods) <- names(data)
  unknown_method_vars <- setdiff(names(method_overrides), impute)
  if (length(unknown_method_vars)) fail("Method was supplied for non-imputed variable '%s'.", unknown_method_vars[[1L]])
  for (nm in impute) {
    methods[[nm]] <- if (nm %in% names(method_overrides)) method_overrides[[nm]] else default_method(data, types, nm)
  }
  if (anyNA(methods[impute])) fail("Variable '%s' has an unsupported type for imputation.", impute[is.na(methods[impute])][[1L]])
  stage <- "building predictor matrix"
  predictor_matrix <- matrix(0, nrow = ncol(data), ncol = ncol(data), dimnames = list(names(data), names(data)))
  predictor_matrix[impute, predictors] <- 1
  diag(predictor_matrix) <- 0
  stage <- "preparing data for mice"
  data_for_mice <- data
  for (nm in names(data_for_mice)) {
    if (identical(types[[nm]], "character")) data_for_mice[[nm]] <- factor(data_for_mice[[nm]])
    if (identical(types[[nm]], "logical")) data_for_mice[[nm]] <- factor(data_for_mice[[nm]], levels = c(FALSE, TRUE))
  }
  stage <- "running mice"
  mice_args <- list(data = data_for_mice, m = m, maxit = maxit, method = methods,
                    predictorMatrix = predictor_matrix, printFlag = FALSE)
  if (!is.null(seed)) mice_args$seed <- seed
  mids <- do.call(mice::mice, mice_args)
  stage <- "collecting completed datasets"
  completed <- lapply(seq_len(m), function(i) as.data.frame(mice::complete(mids, i), stringsAsFactors = FALSE)[names(data)])
  mask <- as.data.frame(lapply(data, function(x) rep(FALSE, length(x))), stringsAsFactors = FALSE)
  for (nm in impute) mask[[nm]] <- is.na(data[[nm]])
  current <- completed[[1L]]
  stage <- "writing imputed dataset payload"
  out <- c("REGISTER_DATASET", dataset_name, variable_payload(current, types),
           dataframe_payload(current, data, completed, mask, types,
                             paste0("native_", dataset_name), payload$source))
  writeLines(out, output, useBytes = TRUE)
}, error = function(e) {
  tb <- paste(utils::capture.output(traceback(2)), collapse = "\n")
  msg <- conditionMessage(e)
  if (!nzchar(msg)) msg <- "Unknown imputation error."
  message(sprintf("Multiple imputation failed while %s: %s", stage, msg))
  if (nzchar(tb)) message(tb)
  quit(status = 1L)
})
)RLSMICE";
}

std::string NativePooledAnalysisRScript()
{
    return R"RLSANALYSIS(
args <- commandArgs(TRUE)
if (length(args) < 9L) stop("Expected input, output, mode, id, variables, group, response, terms, and scope.", call. = FALSE)
input <- args[[1L]]
output <- args[[2L]]
mode <- args[[3L]]
analysis_id <- args[[4L]]
variables_text <- args[[5L]]
group_text <- args[[6L]]
response_text <- args[[7L]]
terms_text <- args[[8L]]
scope_text <- args[[9L]]
model_spec_text <- if (length(args) >= 10L) args[[10L]] else ""

suppressPackageStartupMessages(library(LinkEDA))
`%||%` <- function(x, y) if (is.null(x)) y else x
scalar_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return("")
  as.character(x[[1L]])
}
has_text <- function(x) {
  length(x) == 1L && !is.na(x) && nzchar(x)
}
split_names <- function(text) {
  text <- trimws(scalar_text(text))
  if (!has_text(text)) return(character())
  sep <- if (grepl("\t", text, fixed = TRUE)) "\t" else ","
  out <- trimws(strsplit(text, sep, fixed = TRUE)[[1L]])
  out[nzchar(out)]
}
decode_field <- function(text) {
  text <- scalar_text(text)
  if (!nzchar(text)) return("")
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  out <- character()
  i <- 1L
  while (i <= length(chars)) {
    if (identical(chars[[i]], "%") && i + 2L <= length(chars) &&
        grepl("^[0-9A-Fa-f]{2}$", paste0(chars[[i + 1L]], chars[[i + 2L]]))) {
      out <- c(out, rawToChar(as.raw(strtoi(paste0(chars[[i + 1L]], chars[[i + 2L]]), 16L))))
      i <- i + 3L
    } else {
      out <- c(out, chars[[i]])
      i <- i + 1L
    }
  }
  paste0(out, collapse = "")
}
parse_model_specs <- function(text) {
  text <- scalar_text(text)
  if (!nzchar(text)) return(list())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  specs <- list()
  for (line in lines[nzchar(lines)]) {
    fields <- strsplit(line, "|", fixed = TRUE)[[1L]]
    if (length(fields) < 3L) next
    terms <- character()
    terms_text <- fields[[3L]]
    if (nzchar(terms_text)) {
      terms <- vapply(strsplit(terms_text, "\t", fixed = TRUE)[[1L]], decode_field, character(1L))
      terms <- terms[nzchar(terms)]
    }
    specs[[length(specs) + 1L]] <- list(
      label = decode_field(fields[[1L]]),
      response = decode_field(fields[[2L]]),
      terms = terms
    )
  }
  specs
}
is_missing_text <- function(x) is.na(x) | x %in% c("", "NA", "NaN")
convert_values <- function(values, type) {
  values <- as.character(values)
  miss <- is_missing_text(values)
  if (identical(type, "numeric")) {
    x <- suppressWarnings(as.numeric(values))
    x[miss] <- NA_real_
    x
  } else if (identical(type, "logical")) {
    text <- tolower(values)
    x <- rep(NA, length(values))
    x[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
    x[text %in% c("false", "f", "0", "no", "n")] <- FALSE
    x
  } else {
    x <- values
    x[miss] <- NA_character_
    x
  }
}
finalize_types <- function(data, original, completed, types) {
  for (name in names(data)) {
    type <- types[[name]]
    if (identical(type, "factor")) {
      all_values <- c(as.character(data[[name]]), as.character(original[[name]]),
                      unlist(lapply(completed, function(one) as.character(one[[name]])), use.names = FALSE))
      levels <- unique(all_values[!is.na(all_values)])
      data[[name]] <- factor(as.character(data[[name]]), levels = levels)
      original[[name]] <- factor(as.character(original[[name]]), levels = levels)
      completed <- lapply(completed, function(one) {
        one[[name]] <- factor(as.character(one[[name]]), levels = levels)
        one
      })
    } else if (identical(type, "character")) {
      data[[name]] <- as.character(data[[name]])
      original[[name]] <- as.character(original[[name]])
      completed <- lapply(completed, function(one) {
        one[[name]] <- as.character(one[[name]])
        one
      })
    }
  }
  list(data = data, original = original, completed = completed)
}
read_payload <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  i <- 1L
  next_line <- function() {
    if (i > length(lines)) stop("Malformed native analysis payload.", call. = FALSE)
    value <- lines[[i]]
    i <<- i + 1L
    value
  }
  if (!identical(next_line(), "DATASET")) stop("Malformed native analysis payload.", call. = FALSE)
  group <- next_line()
  rows <- as.integer(next_line())
  cols <- as.integer(next_line())
  data <- list()
  types <- character()
  for (ci in seq_len(cols)) {
    name <- next_line()
    type <- next_line()
    values <- vapply(seq_len(rows), function(ri) next_line(), character(1L))
    data[[name]] <- convert_values(values, type)
    types[[name]] <- type
  }
  data <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  original <- data
  completed <- list(data)
  mask <- as.data.frame(lapply(data, function(x) rep(FALSE, length(x))), stringsAsFactors = FALSE)
  dataset_type <- "data_frame"
  imputation_id <- ""
  source_id <- ""
  imputation_count <- 0L
  active_version <- 1L
  display_mode <- "version"
  if (i <= length(lines) && identical(lines[[i]], "IMPUTATION_SPARSE")) {
    i <- i + 1L
    dataset_type <- next_line()
    imputation_id <- next_line()
    source_id <- next_line()
    imputation_count <- as.integer(next_line())
    active_version <- as.integer(next_line())
    display_mode <- next_line()
    mi_cols <- as.integer(next_line())
    completed <- replicate(max(1L, imputation_count), data, simplify = FALSE)
    for (ci in seq_len(mi_cols)) {
      name <- next_line()
      missing_count <- as.integer(next_line())
      missing_rows <- as.integer(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)))
      original_values <- convert_values(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)), types[[name]])
      original[[name]][missing_rows] <- original_values
      mask[[name]][missing_rows] <- TRUE
      for (version in seq_len(imputation_count)) {
        values <- convert_values(vapply(seq_len(missing_count), function(ri) next_line(), character(1L)), types[[name]])
        completed[[version]][[name]][missing_rows] <- values
      }
    }
  }
  typed <- finalize_types(data, original, completed, types)
  data <- typed$data
  original <- typed$original
  completed <- typed$completed
  metadata <- LinkEDA:::.rls_variable_metadata(data)
  for (name in names(types)) {
    row <- metadata$variable_name == name
    metadata$current_analysis_type[row] <- types[[name]]
    metadata$type[row] <- types[[name]]
  }
  list(
    dataset_id = group,
    name = group,
    dataset_name = group,
    group = group,
    original_data = original,
    data_frame = data,
    data = data,
    n_rows = nrow(data),
    n_columns = ncol(data),
    original_row_ids = seq_len(nrow(data)),
    selection_state = integer(0),
    row_color_state = character(0),
    metadata = metadata,
    variable_metadata = metadata,
    dataset_type = dataset_type,
    imputation_id = imputation_id,
    source_dataset_id = source_id,
    completed_datasets = completed,
    missing_cell_mask = mask,
    active_imputation_version = active_version,
    imputation_display_mode = display_mode,
    imputation_count = imputation_count,
    source = "Native R/mice analysis bridge",
    row_colors = character(0),
    modified = FALSE
  )
}
register_record <- function(record) {
  LinkEDA:::.rls_set_dataset_record(record)
  state <- get(".rls_state", envir = asNamespace("LinkEDA"))
  state$active_dataset <- record$group
  invisible(record)
}

tryCatch({
  record <- register_record(read_payload(input))
  variables <- split_names(variables_text)
  terms <- split_names(terms_text)
  group_var <- scalar_text(group_text)
  response <- scalar_text(response_text)
  scope <- scalar_text(scope_text)
  if (!nzchar(scope)) scope <- "all"

  if (identical(mode, "table1")) {
    handle <- LinkEDA::ls_new_table1(
      record$group,
      variables = if (length(variables)) variables else NULL,
      group = if (nzchar(group_var)) group_var else NULL,
      name = if (nzchar(analysis_id)) analysis_id else NULL
    )
    table_record <- LinkEDA:::.rls_table1_record(handle)
    writeLines(LinkEDA:::.rls_table1_native_payload(table_record), output, useBytes = TRUE)
  } else if (identical(mode, "glm")) {
    if (!length(terms)) stop("Add at least one independent variable before opening the pooled MI General Linear Model table.", call. = FALSE)
    model <- LinkEDA::ls_new_glm(record$group)
    if (nzchar(response)) model <- LinkEDA::ls_glm_set_dependent(model, response)
    for (term in terms) model <- LinkEDA::ls_glm_add_predictor(model, term)
    model <- LinkEDA::ls_glm_fit(model)
    model_record <- LinkEDA:::.rls_glm_model_record(model)
    writeLines(LinkEDA:::.rls_glm_pooled_native_payload(model_record), output, useBytes = TRUE)
  } else if (identical(mode, "gglm")) {
    if (!nzchar(response)) stop("Choose a response variable before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    if (!length(terms)) stop("Add at least one predictor before opening the pooled MI Generalized Linear Model table.", call. = FALSE)
    family <- "gaussian"
    link <- ""
    if (nzchar(model_spec_text)) {
      fields <- strsplit(model_spec_text, "|", fixed = TRUE)[[1L]]
      if (length(fields) >= 1L && nzchar(fields[[1L]])) family <- decode_field(fields[[1L]])
      if (length(fields) >= 2L && nzchar(fields[[2L]])) link <- decode_field(fields[[2L]])
    }
    model <- LinkEDA::ls_new_generalized_linear_model(
      record$group,
      response = response,
      terms = terms,
      family = family,
      link = if (nzchar(link)) link else NULL,
      scope = scope,
      name = if (nzchar(analysis_id)) analysis_id else NULL,
      native = FALSE
    )
    model_record <- LinkEDA:::.rls_generalized_glm_record(model)
    writeLines(LinkEDA:::.rls_generalized_glm_pooled_native_payload(model_record), output, useBytes = TRUE)
  } else if (identical(mode, "regcmp")) {
    model_specs <- parse_model_specs(model_spec_text)
    if (length(model_specs)) {
      model_responses <- vapply(model_specs, function(spec) spec$response, character(1L))
      shared_response <- if (nzchar(response) && all(model_responses == response)) response else NULL
      models <- lapply(model_specs, function(spec) {
        if (is.null(shared_response)) {
          list(response = spec$response, terms = spec$terms)
        } else {
          spec$terms
        }
      })
      labels <- vapply(seq_along(model_specs), function(i) {
        label <- model_specs[[i]]$label
        if (nzchar(label)) label else sprintf("Model %d", i)
      }, character(1L))
      names(models) <- labels
      response_arg <- shared_response
    } else {
      if (!nzchar(response)) stop("Choose a response variable before opening pooled MI model comparison.", call. = FALSE)
      models <- list(terms)
      names(models) <- if (length(terms)) "Model 1" else "Null"
      response_arg <- response
    }
    comparison <- LinkEDA::ls_new_regression_comparison(
      record$group,
      response = response_arg,
      models = models,
      scope = scope,
      name = if (nzchar(analysis_id)) analysis_id else NULL,
      native = FALSE
    )
    comparison_record <- LinkEDA:::.rls_regcmp_record(comparison)
    writeLines(LinkEDA:::.rls_regcmp_pooled_native_payload(comparison_record), output, useBytes = TRUE)
  } else {
    stop(sprintf("Unknown native R/mice analysis mode: %s", mode), call. = FALSE)
  }
}, error = function(e) {
  message(conditionMessage(e))
  quit(status = 1L)
})
)RLSANALYSIS";
}

std::string NormalizeVariableType(const std::string &type)
{
    std::string lowered = LowerCopy(TrimCopy(type));
    if (lowered == "numeric" || lowered == "number" ||
        lowered == "numerico" || lowered == "numérico") {
        return "numeric";
    }
    if (lowered == "factor") {
        return "factor";
    }
    if (lowered == "ordered" || lowered == "ordered_factor" ||
        lowered == "ordered factor" || lowered == "factor ordenado") {
        return "ordered";
    }
    if (lowered == "character" || lowered == "text" ||
        lowered == "texto" || lowered == "string") {
        return "character";
    }
    if (lowered == "logical" || lowered == "boolean" || lowered == "bool") {
        return "logical";
    }
    return lowered;
}

bool VariableTypeIsSupported(const std::string &type)
{
    std::string normalized = NormalizeVariableType(type);
    return normalized == "numeric" || normalized == "factor" ||
        normalized == "ordered" || normalized == "character" ||
        normalized == "logical";
}

bool VariableTypeIsFactorLike(const std::string &type)
{
    std::string normalized = NormalizeVariableType(type);
    return normalized == "factor" || normalized == "ordered" ||
        normalized == "character" || normalized == "logical";
}

std::string VariableTypeDisplayName(const std::string &type)
{
    std::string normalized = NormalizeVariableType(type);
    if (normalized == "numeric") return "Numeric";
    if (normalized == "factor") return "Factor";
    if (normalized == "ordered") return "Ordered factor";
    if (normalized == "character") return "Text";
    if (normalized == "logical") return "Logical";
    return normalized.empty() ? "unknown" : normalized;
}

std::string VariableRoleDisplayName(const std::string &role)
{
    if (role == "Y" || role == "dependent") return "Dependent";
    if (role == "Predictor" || role == "independent" || role == "predictor") return "Independent";
    return "None";
}

std::string VariableTypeEditingStatus(const std::string &variable)
{
    return "Editing type for `" + variable + "`.";
}

std::string VariableTypeChangedStatus(const std::string &variable,
                                      const std::string &type)
{
    return variable + " is now treated as " + VariableTypeDisplayName(type) + ".";
}

std::string VariableDecimalsUnavailableStatus(const std::string &variable,
                                              const std::string &type)
{
    return "Decimals are available only for numeric variables; `" + variable +
        "` is " + VariableTypeDisplayName(type) + ".";
}

std::string VariableDecimalsInvalidStatus(bool bounded)
{
    return bounded
        ? "Decimals must be an integer from 0 to 12, or Automatic."
        : "Decimals must be an integer or blank for automatic.";
}

std::string VariableDecimalsChangedStatus(const std::string &variable,
                                          int decimals)
{
    return decimals < 0
        ? "Decimals for `" + variable + "` set to automatic."
        : "Decimals for `" + variable + "` set to " + std::to_string(decimals) + ".";
}

std::string VariableDescriptionChangedStatus(const std::string &variable)
{
    return "Updated description for `" + variable + "`.";
}

std::string VariableRenamedStatus(const std::string &oldName,
                                  const std::string &newName)
{
    return "Renamed `" + oldName + "` to `" + newName + "`.";
}

std::string VariableNotFoundStatus(const std::string &variable)
{
    return "Variable `" + variable + "` was not found.";
}

std::string VariableNotFoundInDatasetStatus(const std::string &variable,
                                            const std::string &group)
{
    return "Variable `" + variable + "` was not found in dataset `" + group + "`.";
}

std::string VariableUnavailableStatus(const std::string &variable)
{
    return "Variable `" + variable + "` is not available.";
}

std::string DerivedDataColumnAddedStatus(const std::string &column,
                                         const std::string &group)
{
    return "Added `" + column + "` to dataset `" + group + "`.";
}

std::string VariableRoleChangedStatus(const std::string &variable,
                                      const std::string &roleAction)
{
    if (roleAction == "dependent") {
        return "Set `" + variable + "` as response variable.";
    }
    if (roleAction == "predictor" || roleAction == "independent") {
        return "Added `" + variable + "` as predictor.";
    }
    if (roleAction == "remove_predictor") {
        return "Removed predictor role from `" + variable + "`.";
    }
    if (roleAction == "none" || roleAction == "clear") {
        return "Cleared model role for `" + variable + "`.";
    }
    return "Updated role for `" + variable + "`.";
}

std::string VariableViewModelRoleSummary(const std::string &dependent,
                                         std::size_t predictorCount)
{
    return "Model roles: Y=" + (dependent.empty() ? std::string("(none)") : dependent) +
        " | predictors=" + std::to_string(predictorCount);
}

bool DataColumnAllowsNumeric(const DataColumn &col, std::string *message)
{
    double parsed = NAN;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            continue;
        }
        if (!ParseDataCellDouble(value, parsed)) {
            if (message) {
                *message = "Variable `" + col.name +
                    "` cannot be treated as numeric because some values cannot be converted.";
            }
            return false;
        }
    }
    return true;
}

bool DataColumnHasMissing(const DataColumn &col)
{
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            return true;
        }
    }
    return false;
}

bool DataColumnAllMissing(const DataColumn &col)
{
    if (col.values.empty()) {
        return true;
    }
    for (const std::string &value : col.values) {
        if (!DataCellIsMissing(value)) {
            return false;
        }
    }
    return true;
}

bool DataColumnSupportedForMice(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    return type == "numeric" || type == "factor" || type == "ordered" ||
        type == "character" || type == "logical";
}

bool DataColumnLooksLikeId(const DataColumn &col)
{
    std::string name = LowerCopy(col.name);
    if (name == "id" || name.find("_id") != std::string::npos ||
        name.find("identifier") != std::string::npos ||
        name.find("subject") != std::string::npos ||
        name.find("case") != std::string::npos ||
        name.find("etiqueta") != std::string::npos ||
        name.find("label") != std::string::npos) {
        return true;
    }

    std::vector<std::string> observed;
    observed.reserve(col.values.size());
    for (const std::string &value : col.values) {
        if (!DataCellIsMissing(value)) {
            observed.push_back(value);
        }
    }
    if (observed.empty()) {
        return false;
    }
    std::set<std::string> uniqueValues(observed.begin(), observed.end());
    return uniqueValues.size() == observed.size() &&
        observed.size() >= static_cast<std::size_t>(
            std::ceil(0.9 * static_cast<double>(std::max<std::size_t>(1, col.values.size()))));
}

bool DataColumnLooksBinaryNumeric(const DataColumn &col)
{
    std::set<std::string> levels;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) continue;
        double numeric = NAN;
        if (!ParseDataCellDouble(value, numeric)) return false;
        if (std::fabs(numeric) > 1.0e-9 && std::fabs(numeric - 1.0) > 1.0e-9) return false;
        levels.insert(std::fabs(numeric - 1.0) <= 1.0e-9 ? "1" : "0");
    }
    return levels.size() == 2;
}

bool DataColumnLooksGroupingCandidate(const DataColumn &col, int rows)
{
    if (DataColumnAllMissing(col) || DataColumnLooksLikeId(col)) return false;
    int levels = DataColumnObservedLevelCount(col);
    if (levels < 2) return false;
    if (NormalizeVariableType(col.type) == "numeric") {
        return levels <= std::max(20, static_cast<int>(std::ceil(static_cast<double>(std::max(1, rows)) / 4.0)));
    }
    return true;
}

int DataColumnMissingCount(const DataColumn &col)
{
    int count = 0;
    for (const std::string &value : col.values) {
        if (DataCellIsMissing(value)) {
            ++count;
        }
    }
    return count;
}

int DataColumnObservedLevelCount(const DataColumn &col)
{
    std::set<std::string> levels;
    for (const std::string &value : col.values) {
        if (!DataCellIsMissing(value)) {
            levels.insert(value);
        }
    }
    return static_cast<int>(levels.size());
}

std::string DefaultMiceMethod(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    if (type == "numeric") return "pmm";
    if (type == "logical") return "logreg";
    if (type == "ordered") {
        return DataColumnObservedLevelCount(col) <= 2 ? "logreg" : "polr";
    }
    if (type == "factor" || type == "character") {
        return DataColumnObservedLevelCount(col) <= 2 ? "logreg" : "polyreg";
    }
    return "";
}

std::vector<std::string> MiceMethodOptions(const DataColumn &col)
{
    std::string type = NormalizeVariableType(col.type);
    if (type == "numeric") return {"pmm", "norm", "cart"};
    if (type == "logical") return {"logreg"};
    if (type == "ordered") {
        if (DataColumnObservedLevelCount(col) <= 2) return {"logreg"};
        return {"polr", "cart"};
    }
    if (type == "factor" || type == "character") {
        if (DataColumnObservedLevelCount(col) <= 2) return {"logreg"};
        return {"polyreg", "cart"};
    }
    return {};
}

std::string MiceVariableStatusText(const DataColumn &col, int rowCount)
{
    std::string status = col.type.empty() ? "unknown" : col.type;
    int missing = DataColumnMissingCount(col);
    status += ", " + std::to_string(missing) + " missing";
    if (missing >= std::max(1, rowCount)) status += ", all missing";
    if (DataColumnLooksLikeId(col)) status += ", id-like";
    if (!DataColumnSupportedForMice(col)) {
        status += ", " + BuildMissingDataImputationDialogState().unsupportedMethodTitle;
    }
    return status;
}

std::vector<std::string> DefaultImputeVariables(const DataFrameModel &df)
{
    std::vector<std::string> out;
    for (const DataColumn &col : df.columns) {
        if (DataColumnSupportedForMice(col) && DataColumnHasMissing(col) &&
            !DataColumnAllMissing(col) && !DataColumnLooksLikeId(col)) {
            out.push_back(col.name);
        }
    }
    return out;
}

std::vector<std::string> DefaultPredictorVariables(const DataFrameModel &df)
{
    std::vector<std::string> out;
    for (const DataColumn &col : df.columns) {
        if (DataColumnSupportedForMice(col) && !DataColumnAllMissing(col) &&
            !DataColumnLooksLikeId(col)) {
            out.push_back(col.name);
        }
    }
    return out;
}

std::string MiceDatasetSummaryText(const DataFrameModel &df,
                                   int missingColumnCount,
                                   int imputeColumnCount)
{
    std::ostringstream summary;
    summary << df.group << ": " << df.rows << " rows, "
            << df.columns.size() << " variables, "
            << missingColumnCount << " variables with missing values, "
            << imputeColumnCount << " selected for imputation.";
    return summary.str();
}

std::string MiceSelectionSummaryText(std::size_t imputeCount,
                                     std::size_t predictorCount)
{
    return std::to_string(imputeCount) + " variables to impute, " +
        std::to_string(predictorCount) + " predictors selected.";
}

bool SetDataColumnType(DataColumn &col, const std::string &type, std::string *message)
{
    std::string normalized = NormalizeVariableType(type);
    if (!VariableTypeIsSupported(normalized)) {
        if (message) {
            *message = "Variable type must be numeric, factor, ordered factor, text, or logical.";
        }
        return false;
    }
    if (normalized == "numeric" && !DataColumnAllowsNumeric(col, message)) {
        return false;
    }
    col.type = normalized;
    return true;
}

bool SetDataColumnDescription(DataColumn &col, const std::string &description)
{
    col.description = description;
    return true;
}

bool SetDataFrameCellValue(DataFrameModel &df,
                           const std::string &variable,
                           std::size_t row,
                           const std::string &value,
                           std::string *message)
{
    DataColumn *column = FindDataColumnInDataFrame(df, variable);
    if (!column) {
        if (message) *message = "Variable `" + variable + "` was not found.";
        return false;
    }
    if (row >= static_cast<std::size_t>(std::max(0, df.rows))) {
        if (message) *message = "The edited row is outside the dataset.";
        return false;
    }
    std::string stored = TrimCopy(value);
    const bool missing = stored.empty() || stored == "NA" || stored == "NaN";
    const std::string type = NormalizeVariableType(column->type);
    if (!missing && type == "numeric") {
        double parsed = NAN;
        if (!ParseDataCellDouble(stored, parsed)) {
            if (message) *message = "`" + value + "` is not a valid numeric value.";
            return false;
        }
        std::ostringstream normalized;
        normalized << std::setprecision(17) << parsed;
        stored = normalized.str();
    } else if (!missing && type == "logical") {
        const std::string lowered = LowerCopy(stored);
        if (lowered == "true" || lowered == "t" || lowered == "yes" || lowered == "y" || lowered == "1") {
            stored = "TRUE";
        } else if (lowered == "false" || lowered == "f" || lowered == "no" || lowered == "n" || lowered == "0") {
            stored = "FALSE";
        } else {
            if (message) *message = "Logical values must be TRUE, FALSE, 1, 0, yes, or no.";
            return false;
        }
    }
    if (missing) stored = "NA";
    if (column->values.size() < static_cast<std::size_t>(df.rows)) {
        column->values.resize(static_cast<std::size_t>(df.rows), "NA");
    }
    column->values[row] = stored;
    if (!column->displayValues.empty()) {
        if (column->displayValues.size() < static_cast<std::size_t>(df.rows)) {
            column->displayValues.resize(static_cast<std::size_t>(df.rows), "NA");
        }
        column->displayValues[row] = stored;
    }
    if (!missing && VariableTypeIsFactorLike(type) &&
        std::find(column->definedLevels.begin(), column->definedLevels.end(), stored) == column->definedLevels.end()) {
        column->definedLevels.push_back(stored);
    }
    if (message) {
        *message = "Updated row " + std::to_string(row + 1) + ", variable `" + variable + "`.";
    }
    return true;
}

bool SetDataColumnDecimals(DataColumn &col, int decimals, std::string *message)
{
    if (decimals < -1 || decimals > 12) {
        if (message) {
            *message = "Decimals must be between 0 and 12, or blank for automatic.";
        }
        return false;
    }
    col.decimals = decimals;
    return true;
}

bool IsValidVariableName(const std::string &name, std::string *message)
{
    if (name.empty()) {
        if (message) *message = "Variable name cannot be empty.";
        return false;
    }
    if (name.find('|') != std::string::npos || name.find('\t') != std::string::npos ||
        name.find('\n') != std::string::npos || name.find('\r') != std::string::npos) {
        if (message) *message = "Variable names cannot contain tabs, line breaks, or `|`.";
        return false;
    }
    return true;
}

std::string SafeDataColumnSuffix(const std::string &text)
{
    std::string out;
    for (unsigned char ch : text) {
        if (std::isalnum(ch)) {
            out.push_back(static_cast<char>(std::tolower(ch)));
        } else if (ch == '_' || ch == '-' || std::isspace(ch)) {
            if (out.empty() || out.back() != '_') {
                out.push_back('_');
            }
        }
    }
    while (!out.empty() && out.back() == '_') {
        out.pop_back();
    }
    if (out.empty()) {
        out = "plot";
    }
    if (out.size() > 32) {
        out.resize(32);
    }
    while (!out.empty() && out.back() == '_') {
        out.pop_back();
    }
    return out.empty() ? "plot" : out;
}

std::string SafeDatasetName(const std::string &text)
{
    std::string cleaned;
    bool previousSpace = false;
    for (char ch : text) {
        unsigned char c = static_cast<unsigned char>(ch);
        bool keep = std::isalnum(c) || ch == '_' || ch == '.' || ch == '-';
        if (keep) {
            cleaned.push_back(ch);
            previousSpace = false;
        } else if (!previousSpace) {
            cleaned.push_back(' ');
            previousSpace = true;
        }
    }
    while (!cleaned.empty() && cleaned.front() == ' ') {
        cleaned.erase(cleaned.begin());
    }
    while (!cleaned.empty() && cleaned.back() == ' ') {
        cleaned.pop_back();
    }
    return cleaned.empty() ? "dataset" : cleaned;
}

std::string SafeDatasetNameForPath(const std::string &path)
{
    std::string base = path;
    std::size_t slash = base.find_last_of("/\\");
    if (slash != std::string::npos) {
        base = base.substr(slash + 1);
    }
    std::size_t dot = base.find_last_of('.');
    if (dot != std::string::npos && dot > 0) {
        base = base.substr(0, dot);
    }
    return SafeDatasetName(base);
}

std::string UniqueDatasetName(const std::string &baseName,
                              const std::vector<std::string> &existingNames)
{
    std::string base = SafeDatasetName(baseName);
    std::set<std::string> existing(existingNames.begin(), existingNames.end());
    std::string candidate = base;
    int index = 2;
    while (existing.find(candidate) != existing.end()) {
        candidate = base + " " + std::to_string(index++);
    }
    return candidate;
}

std::string UniqueDataColumnName(const DataFrameModel &df, const std::string &base)
{
    std::set<std::string> existing;
    for (const DataColumn &col : df.columns) {
        existing.insert(col.name);
    }
    std::string candidate = base.empty() ? "column" : base;
    int suffix = 2;
    while (existing.find(candidate) != existing.end()) {
        candidate = (base.empty() ? "column" : base) + "_" + std::to_string(suffix++);
    }
    return candidate;
}

bool DerivedDataColumnKindIsSupported(const std::string &kind)
{
    return kind == "selection" || kind == "color";
}

bool AddDerivedDataColumn(DataFrameModel &df,
                          const std::string &source,
                          const std::string &kind,
                          const std::set<int> &selectedRows,
                          const std::map<int, std::string> &rowColors,
                          std::string *createdName,
                          std::string *message)
{
    if (!DerivedDataColumnKindIsSupported(kind)) {
        if (message) {
            *message = "Derived column kind must be selection or color.";
        }
        return false;
    }
    std::string safeSource = source.empty() ? "dataset" : source;
    std::string base = (kind == "selection" ? "selected_from_" : "point_color_from_") +
        SafeDataColumnSuffix(safeSource);

    DataColumn col;
    col.name = UniqueDataColumnName(df, base);
    col.displayName = col.name;
    col.type = "factor";
    col.decimals = -1;
    col.description = kind == "selection"
        ? "Selection state exported from LinkEDA plot `" + safeSource + "`."
        : "Point color state exported from LinkEDA plot `" + safeSource + "`.";
    col.values.reserve(static_cast<std::size_t>(std::max(0, df.rows)));

    for (int row = 1; row <= df.rows; ++row) {
        if (kind == "selection") {
            bool selected = selectedRows.find(row) != selectedRows.end();
            col.values.push_back(selected ? "selected" : "not_selected");
        } else {
            std::string color = "default";
            auto hit = rowColors.find(row);
            if (hit != rowColors.end() && !hit->second.empty()) {
                color = hit->second;
            }
            col.values.push_back(color);
        }
    }

    if (createdName) {
        *createdName = col.name;
    }
    df.columns.push_back(col);
    return true;
}

std::string VariableInformationText(const DataFrameModel &df,
                                    const std::string &variable)
{
    const DataColumn *col = FindDataColumnInDataFrame(df, variable);
    if (!col) {
        return "Variable `" + variable + "` was not found in dataset `" + df.group + "`.";
    }

    int missing = 0;
    std::set<std::string> uniqueValues;
    for (const std::string &value : col->values) {
        if (DataCellIsMissing(value)) {
            ++missing;
        } else if (uniqueValues.size() < 12) {
            uniqueValues.insert(value);
        }
    }

    std::ostringstream out;
    out << "Variable: " << col->name
        << "\nDisplay name: " << (col->displayName.empty() ? col->name : col->displayName)
        << "\nDataset: " << df.group
        << "\nAnalysis type: " << col->type
        << "\nDescription: " << (col->description.empty() ? "(none)" : col->description)
        << "\nDisplayed decimals: " << (col->decimals < 0 ? "automatic" : std::to_string(col->decimals))
        << "\nRows: " << col->values.size()
        << "\nMissing values: " << missing
        << "\nDistinct preview: ";
    if (uniqueValues.empty()) {
        out << "(none)";
    } else {
        std::size_t i = 0;
        for (const std::string &value : uniqueValues) {
            if (i++) {
                out << ", ";
            }
            out << value;
        }
    }
    return out.str();
}

bool RenameDataFrameColumn(DataFrameModel &df,
                           const std::string &oldName,
                           const std::string &newName,
                           std::string *message)
{
    if (!IsValidVariableName(newName, message)) {
        return false;
    }
    if (oldName == newName) {
        if (message) *message = "Variable name unchanged.";
        return true;
    }
    if (FindDataColumnInDataFrame(df, newName)) {
        if (message) *message = "Variable `" + newName + "` already exists.";
        return false;
    }
    DataColumn *col = FindDataColumnInDataFrame(df, oldName);
    if (!col) {
        if (message) *message = "Variable `" + oldName + "` was not found.";
        return false;
    }
    col->name = newName;
    if (col->displayName == oldName || col->displayName.empty()) {
        col->displayName = newName;
    }
    return true;
}

std::vector<VariableViewRow> VariableViewRowsForDataFrame(const DataFrameModel &df)
{
    std::vector<VariableViewRow> rows;
    rows.reserve(df.columns.size());
    for (const DataColumn &col : df.columns) {
        rows.push_back({col.name, col.type, col.description, col.decimals});
    }
    return rows;
}

DataColumn *FindDataColumnInDataFrame(DataFrameModel &df, const std::string &name)
{
    for (DataColumn &col : df.columns) {
        if (col.name == name) {
            return &col;
        }
    }
    return nullptr;
}

const DataColumn *FindDataColumnInDataFrame(const DataFrameModel &df, const std::string &name)
{
    for (const DataColumn &col : df.columns) {
        if (col.name == name) {
            return &col;
        }
    }
    return nullptr;
}

bool DataFrameCellIsImputed(const DataFrameModel &df, const DataColumn &col, std::size_t row)
{
    return df.datasetType == "multiple_imputation" &&
        row < col.imputedMissing.size() &&
        col.imputedMissing[row];
}

int ImputationVersionCountForCell(const DataFrameModel &df, const DataColumn &col)
{
    int count = df.imputationCount;
    count = std::max(count, static_cast<int>(col.imputationValues.size()));
    count = std::max(count, static_cast<int>(col.imputationValuesSparse.size()));
    return count;
}

std::string OriginalImputationValueForCell(const DataColumn &col, std::size_t row)
{
    if (row < col.imputationOriginalValues.size()) {
        return col.imputationOriginalValues[row];
    }
    auto found = col.imputationOriginalSparse.find(row);
    if (found != col.imputationOriginalSparse.end()) {
        return found->second;
    }
    return "NA";
}

std::string DisplayValueForCell(const DataColumn &col, std::size_t row)
{
    if (row < col.displayValues.size()) {
        return col.displayValues[row];
    }
    if (row < col.values.size()) {
        if (col.decimals >= 0 && col.type == "numeric") {
            double parsed = NAN;
            if (ParseDataCellDouble(col.values[row], parsed)) {
                std::ostringstream out;
                out << std::fixed << std::setprecision(std::max(0, std::min(12, col.decimals))) << parsed;
                return out.str();
            }
        }
        return col.values[row];
    }
    return "";
}

std::vector<std::string> DisplayValuesForColumnsAtRow(
    const std::vector<const DataColumn *> &columns,
    std::size_t row,
    const std::string &missingValue)
{
    std::vector<std::string> values;
    for (const DataColumn *col : columns) {
        if (!col) {
            continue;
        }
        std::string value = DisplayValueForCell(*col, row);
        values.push_back(DataCellIsMissing(value) ? missingValue : value);
    }
    if (values.empty()) {
        values.push_back(missingValue);
    }
    return values;
}

std::map<int, std::string> RowLabelMapForColumn(const DataColumn &col)
{
    std::map<int, std::string> labels;
    const std::size_t n = std::max(col.values.size(), col.displayValues.size());
    for (std::size_t i = 0; i < n; ++i) {
        std::string value = DisplayValueForCell(col, i);
        if (!DataCellIsMissing(value)) {
            labels[static_cast<int>(i) + 1] = value;
        }
    }
    return labels;
}

std::string ImputationVersionValueForCell(const DataColumn &col,
                                          std::size_t row,
                                          std::size_t versionIndex)
{
    if (versionIndex < col.imputationValues.size() &&
        row < col.imputationValues[versionIndex].size()) {
        return col.imputationValues[versionIndex][row];
    }
    if (versionIndex < col.imputationValuesSparse.size()) {
        auto found = col.imputationValuesSparse[versionIndex].find(row);
        if (found != col.imputationValuesSparse[versionIndex].end()) {
            return found->second;
        }
    }
    return DisplayValueForCell(col, row);
}

std::string DisplayValueForDataFrameCell(const DataFrameModel &df,
                                         const DataColumn &col,
                                         std::size_t row)
{
    if (DataFrameCellIsImputed(df, col, row)) {
        if (df.imputationDisplayMode == "all") {
            static const std::size_t kMaxInlineImputationValues = 8;
            static const std::size_t kMaxInlineImputationChars = 180;
            std::size_t count = static_cast<std::size_t>(std::max(0, ImputationVersionCountForCell(df, col)));
            if (count == 0) {
                return "";
            }
            std::size_t shown = std::min(count, kMaxInlineImputationValues);
            std::vector<std::string> values;
            values.reserve(shown);
            for (std::size_t i = 0; i < shown; ++i) {
                values.push_back(ImputationVersionValueForCell(col, row, i));
            }
            std::string text = JoinValues(values, " | ");
            if (count > shown) {
                text += " | ... (" + std::to_string(count) + " imputations)";
            }
            return LimitInlineCellText(text, kMaxInlineImputationChars);
        }
        if (df.imputationDisplayMode == "original") {
            return OriginalImputationValueForCell(col, row);
        }
        int count = ImputationVersionCountForCell(df, col);
        int index = std::max(1, std::min(df.activeImputationVersion, count)) - 1;
        if (index >= 0) {
            return ImputationVersionValueForCell(col, row, static_cast<std::size_t>(index));
        }
    }
    return DisplayValueForCell(col, row);
}

void ApplyImputationDisplayModeToStoredValues(DataFrameModel &df)
{
    if (df.datasetType != "multiple_imputation" || df.imputationCount <= 0) {
        return;
    }
    int activeIndex = std::max(1, std::min(df.activeImputationVersion, df.imputationCount)) - 1;
    for (DataColumn &col : df.columns) {
        for (std::size_t row = 0; row < col.values.size(); ++row) {
            if (!DataFrameCellIsImputed(df, col, row)) {
                continue;
            }
            if (df.imputationDisplayMode == "original") {
                col.values[row] = OriginalImputationValueForCell(col, row);
            } else {
                col.values[row] = ImputationVersionValueForCell(col, row, static_cast<std::size_t>(std::max(0, activeIndex)));
            }
        }
    }
}

double NumericValueForDataFrameCellVersion(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row,
                                           int versionIndex)
{
    std::string text;
    if (DataFrameCellIsImputed(df, col, row)) {
        text = ImputationVersionValueForCell(col, row, static_cast<std::size_t>(std::max(0, versionIndex)));
    } else if (row < col.values.size()) {
        text = col.values[row];
    } else {
        text = DisplayValueForCell(col, row);
    }
    double value = NAN;
    return ParseDataCellDouble(text, value) ? value : NAN;
}

std::vector<int> CompleteRowsForDataColumns(const DataFrameModel &df,
                                            const std::vector<const DataColumn *> &columns,
                                            int versionIndex)
{
    std::vector<int> rows;
    if (columns.empty()) {
        return rows;
    }
    for (int row = 0; row < df.rows; ++row) {
        bool complete = true;
        for (const DataColumn *col : columns) {
            if (!col || !std::isfinite(NumericValueForDataFrameCellVersion(df, *col, static_cast<std::size_t>(row), versionIndex))) {
                complete = false;
                break;
            }
        }
        if (complete) {
            rows.push_back(row + 1);
        }
    }
    return rows;
}

std::vector<int> CompleteRowsForNumericVectors(const std::vector<std::vector<double>> &columns)
{
    std::vector<int> rows;
    if (columns.empty()) {
        return rows;
    }
    std::size_t n = static_cast<std::size_t>(-1);
    for (const std::vector<double> &column : columns) {
        n = std::min(n, column.size());
    }
    if (n == static_cast<std::size_t>(-1)) {
        return rows;
    }
    for (std::size_t i = 0; i < n; ++i) {
        bool complete = true;
        for (const std::vector<double> &column : columns) {
            if (!std::isfinite(column[i])) {
                complete = false;
                break;
            }
        }
        if (complete) {
            rows.push_back(static_cast<int>(i) + 1);
        }
    }
    return rows;
}

bool ScatterImputationUncertaintyModeIsValid(const std::string &mode)
{
    return mode == "central80" || mode == "iqr" || mode == "sd" || mode == "se";
}

std::string NormalizedScatterImputationUncertaintyMode(const std::string &mode)
{
    return ScatterImputationUncertaintyModeIsValid(mode) ? mode : "central80";
}

std::string ScatterImputationUncertaintyDisplayName(const std::string &mode)
{
    std::string normalized = NormalizedScatterImputationUncertaintyMode(mode);
    if (normalized == "iqr") return "IQR";
    if (normalized == "sd") return "Mean +/- 1 SD";
    if (normalized == "se") return "Mean +/- 1 SE";
    return "Central 80% interval";
}

void NumericRangeInclude(NumericImputationRange &range, double value)
{
    if (!std::isfinite(value)) {
        return;
    }
    if (!range.any) {
        range.min = value;
        range.max = value;
        range.any = true;
    } else {
        range.min = std::min(range.min, value);
        range.max = std::max(range.max, value);
    }
}

NumericImputationRange NumericImputationRangeForCell(const DataFrameModel &df,
                                                     const DataColumn &col,
                                                     std::size_t row,
                                                     const std::string &uncertaintyMode)
{
    NumericImputationRange range;
    bool markedImputed = DataFrameCellIsImputed(df, col, row);
    int count = ImputationVersionCountForCell(df, col);
    std::vector<double> values;
    values.reserve(static_cast<std::size_t>(std::max(0, count)));
    for (int version = 0; version < count; ++version) {
        double value = NAN;
        if (ParseDataCellDouble(ImputationVersionValueForCell(col, row, static_cast<std::size_t>(version)), value)) {
            values.push_back(value);
        }
    }
    values.erase(std::remove_if(values.begin(), values.end(), [](double value) {
        return !std::isfinite(value);
    }), values.end());
    if (values.empty()) {
        return range;
    }

    double observedMin = values.front();
    double observedMax = values.front();
    for (double value : values) {
        observedMin = std::min(observedMin, value);
        observedMax = std::max(observedMax, value);
    }
    if (!markedImputed && std::fabs(observedMax - observedMin) <= 1.0e-12) {
        return range;
    }

    double sum = 0.0;
    for (double value : values) {
        sum += value;
    }
    double mean = sum / static_cast<double>(values.size());
    double halfWidth = 0.0;
    std::string mode = NormalizedScatterImputationUncertaintyMode(uncertaintyMode);
    if (mode == "iqr") {
        halfWidth = (QuantileValue(values, 0.75) - QuantileValue(values, 0.25)) / 2.0;
    } else if (mode == "sd" || mode == "se") {
        double sumSquares = 0.0;
        for (double value : values) {
            double delta = value - mean;
            sumSquares += delta * delta;
        }
        double sd = values.size() > 1
            ? std::sqrt(sumSquares / static_cast<double>(values.size() - 1))
            : 0.0;
        halfWidth = mode == "se" ? sd / std::sqrt(static_cast<double>(values.size())) : sd;
    } else {
        halfWidth = (QuantileValue(values, 0.90) - QuantileValue(values, 0.10)) / 2.0;
    }

    range.center = mean;
    range.min = mean - std::max(0.0, halfWidth);
    range.max = mean + std::max(0.0, halfWidth);
    if (!std::isfinite(range.min) || !std::isfinite(range.max) || !std::isfinite(range.center)) {
        return NumericImputationRange();
    }
    if (range.min > range.max) {
        std::swap(range.min, range.max);
    }
    range.any = true;
    return range;
}

bool DataFrameShowsAllImputations(const DataFrameModel &df)
{
    return df.datasetType == "multiple_imputation" &&
        df.imputationCount > 0 &&
        df.imputationDisplayMode == "all";
}

std::string DataFrameStatusText(const DataFrameModel &df,
                                std::size_t selectedCount)
{
    std::string status = df.group + ": " + std::to_string(df.rows) + " rows, " +
        std::to_string(df.columns.size()) + " variables, " +
        std::to_string(selectedCount) + " selected";
    if (df.datasetType == "multiple_imputation") {
        if (df.imputationDisplayMode == "all") {
            status += " | Showing compact all-imputation preview across " +
                std::to_string(df.imputationCount) + " imputations";
        } else if (df.imputationDisplayMode == "original") {
            status += " | Showing original incomplete data";
        } else {
            status += " | Showing imputation: " + std::to_string(df.activeImputationVersion) +
                " of " + std::to_string(df.imputationCount);
        }
    }
    return status;
}

std::string DataFrameImputationTooltipText(const DataFrameModel &df,
                                           const DataColumn &col,
                                           std::size_t row)
{
    if (df.datasetType != "multiple_imputation" ||
        !DataFrameCellIsImputed(df, col, row)) {
        return "";
    }
    if (df.imputationDisplayMode == "all") {
        return "Originally missing. Showing a compact preview across " +
            std::to_string(df.imputationCount) + " imputations.";
    }
    if (df.imputationDisplayMode == "original") {
        return "Originally missing. Showing original incomplete data.";
    }
    return "Originally missing. Imputed value in imputation " +
        std::to_string(df.activeImputationVersion) + ".";
}

std::string DataFrameWindowTitle(const std::string &group)
{
    return "Data Sheet - " + group;
}

std::string DataSheetChooseDataColumnStatus()
{
    return "Choose a data column, not the row-number column.";
}

std::string VariableInformationDialogTitle()
{
    return "Variable Information";
}

std::string NoActiveDatasetTitle()
{
    return "No Active Dataset";
}

std::string ActiveDatasetTitle()
{
    return "Active Dataset";
}

std::string ActiveDatasetUnavailableStatus()
{
    return "The active dataset is no longer available.";
}

std::string ActiveDatasetSummaryText(const std::string &group,
                                     int rows,
                                     std::size_t variableCount,
                                     std::size_t selectedRows)
{
    std::ostringstream msg;
    msg << "Active dataset: " << group << "\n"
        << "Rows: " << rows << "\n"
        << "Variables: " << variableCount << "\n"
        << "Selected rows: " << selectedRows;
    return msg.str();
}

std::string ImportOrRegisterDatasetStatus()
{
    return "Import or register a dataset first.";
}

std::string NoActiveDatasetImportOrRegisterStatus()
{
    return "No active dataset. Import or register a dataset first.";
}

std::string NoActiveDatasetStatus()
{
    return "No active dataset is available.";
}

std::string NoRegisteredDatasetGroupStatus()
{
    return "No registered dataset/group is available.";
}

std::string DatasetNotAvailableStatus()
{
    return "The dataset is not available.";
}

std::string DatasetColumnNotFoundStatus(const std::string &column,
                                        const std::string &group)
{
    return "Column `" + column + "` was not found in dataset `" + group + "`.";
}

std::string DatasetVariableNameUnchangedStatus()
{
    return "Variable name unchanged.";
}

std::string DatasetPointLabelsResetStatus()
{
    return "Point labels reset.";
}

std::string DatasetPointLabelColumnSetStatus(const std::string &column)
{
    return "Point label column set to `" + column + "`.";
}

std::string DatasetTemporaryDataWriteFailedStatus()
{
    return "Could not write temporary GLM data.";
}

std::string VariableViewTitle()
{
    return "Variable View";
}

std::string VariableViewWindowTitle(const std::string &group)
{
    return VariableViewTitle() + " - " + group;
}

std::string VariableViewInstructionText()
{
    return "Click Name or Description to edit. Click Type, Decimals, or Role to choose an action.";
}

std::string VariableDescriptionDialogTitle(const std::string &variable)
{
    return "Description for " + variable;
}

std::string VariableDescriptionDialogInformationText()
{
    return "Edit the explanatory text used by Variable Information and the Variable View.";
}

std::string DatasetNotRegisteredStatus(const std::string &group)
{
    return "Dataset `" + group + "` is not registered.";
}

std::string DataSheetOpenTitle()
{
    return "Open Data Sheet";
}

std::string DataSheetBackendPayloadUnavailableStatus()
{
    return "No backend data payload is available for this dataset yet. Create a new plot from R so the data can be sent to the native workbench.";
}

std::string VariableTypeTitle()
{
    return "Variable Type";
}

std::string NoActiveVariableForTypeStatus()
{
    return "No active variable is available. Open a data sheet and right-click a column.";
}

std::string VariableDecimalsInfoText()
{
    return "Use automatic formatting or choose a fixed number of displayed decimals.";
}

std::string NoRowsSelectedStatus()
{
    return "No rows selected.";
}

std::string NoCompleteCasesStatus()
{
    return "No complete cases for the selected variables and missing-data mode.";
}

std::string DatasetForPlotNotAvailableStatus()
{
    return "The dataset for this plot is not available.";
}

std::string VariableForPlotNotAvailableStatus()
{
    return "The variable for this plot is not available.";
}

std::string SelectedVariableNotAvailableStatus()
{
    return "The selected variable is not available.";
}

std::string NoVariablesToSummarizeStatus()
{
    return "This plot does not have variables that can be summarized.";
}

std::string Table1HistogramRequiresNumericStatus()
{
    return "Histogram requires a numeric or ordinal variable.";
}

std::string Table1BarplotRequiresValueStatus()
{
    return "Bar chart requires at least one observed value.";
}

std::string Table1BoxplotRequiresNumericStatus()
{
    return "Boxplot requires a numeric or ordinal variable.";
}

std::string DatasetNotAvailableForRecomputeStatus()
{
    return "Dataset is not available for native recompute.";
}

std::string NativeBackendCannotInspectRStatus()
{
    return "The native backend cannot inspect R environments directly. Run ls_refresh_r_dataframes() from R, then use ls_set_active_dataset().";
}

std::string VariableDecimalsAutoButtonTitle()
{
    return "Automatic";
}

std::string ScopeAllDataTitle()
{
    return "All data";
}

std::string ScopeSelectedRowsTitle()
{
    return "Selected rows";
}

std::string ScopeUnselectedRowsTitle()
{
    return "Unselected rows";
}

std::string ScopeCompareSelectedAllTitle()
{
    return "Compare selected/all";
}

std::string ChooseLabelColumnStatusTitle()
{
    return "Label Column";
}

std::string AlertOKButtonTitle()
{
    return "OK";
}

std::string AlertApplyButtonTitle()
{
    return "Apply";
}

std::string AlertCancelButtonTitle()
{
    return "Cancel";
}

std::string RFileExtension()
{
    return "R";
}

std::string PDFFileExtension()
{
    return "PDF";
}

std::string PNGFileExtension()
{
    return "PNG";
}

ChooseLabelColumnDialogState BuildChooseLabelColumnDialogState()
{
    return ChooseLabelColumnDialogState{};
}

std::string DatasetDialogLabel(const std::string &group)
{
    return BuildChooseLabelColumnDialogState().datasetLabelPrefix + group;
}

std::string NoVariableOptionTitle()
{
    return BuildChooseLabelColumnDialogState().noneOptionTitle;
}

std::string ChooseLabelColumnDatasetLabel(const std::string &group)
{
    return DatasetDialogLabel(group);
}

MissingDataImputationDialogState BuildMissingDataImputationDialogState()
{
    return MissingDataImputationDialogState{};
}

std::string NativeImportDialogTitle()
{
    return "Import Data";
}

std::string NativeImportFailedTitle()
{
    return "Import Failed";
}

std::string NativeImportSupportedFormatsText()
{
    return "CSV, TSV/TXT, Excel (.xls/.xlsx), SPSS (.sav/.zsav), Stata (.dta), "
           "SAS (.sas7bdat/.xpt), and R (.rds/.rda/.RData) files.";
}

std::vector<std::string> NativeImportAllowedFileExtensions()
{
    return {
        "csv",
        "txt",
        "tsv",
        "sav",
        "zsav",
        "dta",
        "sas7bdat",
        "xpt",
        "xls",
        "xlsx",
        "rds",
        "rda",
        "RData"
    };
}

std::vector<NativeImportFileFilter> NativeImportFileFilters()
{
    const std::vector<std::string> all = NativeImportAllowedFileExtensions();
    return {
        {"all", "All supported data files", all},
        {"spss", "SPSS files (.sav, .zsav)", {"sav", "zsav"}},
        {"delimited", "CSV and text files (.csv, .tsv, .txt)", {"csv", "tsv", "txt"}},
        {"excel", "Excel files (.xlsx, .xls)", {"xlsx", "xls"}},
        {"stata", "Stata files (.dta)", {"dta"}},
        {"sas", "SAS files (.sas7bdat, .xpt)", {"sas7bdat", "xpt"}},
        {"r", "R data files (.rds, .rda, .RData)", {"rds", "rda", "RData"}}
    };
}

std::string NativeImportTemporaryScriptFailedStatus()
{
    return "Could not create temporary import script.";
}

std::string NativeImportRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for data import.";
}

std::string NativeImportRscriptFailedStatus()
{
    return "Rscript failed while importing the selected file.";
}

std::string NativeImportPayloadMissingStatus()
{
    return "The import reader did not return a dataset payload.";
}

std::string NativeImportDatasetLoadedStatus(const std::string &group)
{
    return "Imported `" + group + "`.";
}

std::string NativeImportDatasetLoadedStatus(const std::string &group,
                                            int rows,
                                            std::size_t variableCount)
{
    std::ostringstream out;
    out << "Imported `" << group << "` with " << rows
        << " rows and " << variableCount << " variables.";
    return out.str();
}

std::string NativeRDataPayloadTemporaryFileFailedStatus()
{
    return "Could not create temporary R data payload.";
}

std::string NativeMiceTemporaryScriptFailedStatus()
{
    return "Could not create temporary imputation script.";
}

std::string NativeMiceRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for multiple imputation.";
}

std::string NativeMiceRscriptFailedStatus()
{
    return "Rscript failed while running multiple imputation.";
}

std::string NativeMicePayloadMissingStatus()
{
    return "The imputation script did not return a dataset payload.";
}

std::string NativeMiceCreatedDatasetStatus(const std::string &outputGroup, int m)
{
    return "Created imputed dataset `" + outputGroup + "` with " +
        std::to_string(std::max(1, m)) + " imputations.";
}

std::string NativePooledAnalysisTemporaryScriptFailedStatus()
{
    return "Could not create temporary R analysis script.";
}

std::string NativePooledAnalysisRscriptLaunchFailedStatus()
{
    return "Could not run Rscript for the multiple-imputation analysis.";
}

std::string NativePooledAnalysisRscriptFailedStatus()
{
    return "Rscript failed while computing the multiple-imputation analysis.";
}

std::string NativePooledAnalysisPayloadMissingStatus()
{
    return "The R/mice analysis did not return a native table payload.";
}

std::string NativePooledAnalysisOpenedStatus()
{
    return "Opened pooled multiple-imputation analysis.";
}

bool DatasetRegistry::empty() const
{
    return dataFrames_.empty();
}

std::size_t DatasetRegistry::size() const
{
    return dataFrames_.size();
}

bool DatasetRegistry::contains(const std::string &group) const
{
    return dataFrames_.find(group) != dataFrames_.end();
}

void DatasetRegistry::clear()
{
    dataFrames_.clear();
    activeGroup_.clear();
}

void DatasetRegistry::registerDataset(const DataFrameModel &df)
{
    if (df.group.empty()) {
        return;
    }
    dataFrames_[df.group] = df;
    if (activeGroup_.empty()) {
        activeGroup_ = df.group;
    }
}

bool DatasetRegistry::erase(const std::string &group)
{
    auto erased = dataFrames_.erase(group);
    if (!erased) {
        return false;
    }
    if (activeGroup_ == group) {
        activeGroup_ = dataFrames_.empty() ? "" : dataFrames_.begin()->first;
    }
    return true;
}

DataFrameModel *DatasetRegistry::find(const std::string &group)
{
    auto it = dataFrames_.find(group);
    return it == dataFrames_.end() ? nullptr : &it->second;
}

const DataFrameModel *DatasetRegistry::find(const std::string &group) const
{
    auto it = dataFrames_.find(group);
    return it == dataFrames_.end() ? nullptr : &it->second;
}

const std::map<std::string, DataFrameModel> &DatasetRegistry::datasets() const
{
    return dataFrames_;
}

bool DatasetRegistry::setActiveDataset(const std::string &group)
{
    if (!contains(group)) {
        return false;
    }
    activeGroup_ = group;
    return true;
}

void DatasetRegistry::rememberActiveDatasetGroup(const std::string &group)
{
    activeGroup_ = group;
}

std::string DatasetRegistry::activeDatasetGroup(const std::string &fallbackGroup) const
{
    if (!activeGroup_.empty() && contains(activeGroup_)) {
        return activeGroup_;
    }
    if (!fallbackGroup.empty() && contains(fallbackGroup)) {
        return fallbackGroup;
    }
    if (!dataFrames_.empty()) {
        return dataFrames_.begin()->first;
    }
    return "";
}

const DataFrameModel *DatasetRegistry::activeDataset(const std::string &fallbackGroup) const
{
    std::string group = activeDatasetGroup(fallbackGroup);
    return group.empty() ? nullptr : find(group);
}

std::vector<std::string> DatasetRegistry::datasetGroups() const
{
    std::vector<std::string> groups;
    groups.reserve(dataFrames_.size());
    for (const auto &entry : dataFrames_) {
        groups.push_back(entry.first);
    }
    return groups;
}

std::string DatasetRegistry::uniqueDatasetName(const std::string &baseName) const
{
    return UniqueDatasetName(baseName, datasetGroups());
}

bool DatasetRegistry::isMultipleImputation(const std::string &group) const
{
    const DataFrameModel *df = find(group);
    return df && df->datasetType == "multiple_imputation";
}

size_t DatasetRegistry::rowCount(const std::string &group) const
{
    const DataFrameModel *df = find(group);
    return df ? (size_t)std::max(0, df->rows) : 0;
}

std::string ActiveDatasetWindowTitle()
{
    return "Active Dataset";
}

std::string RefreshRDataFramesWindowTitle()
{
    return "Refresh R Data Frames";
}

std::string HashColumnHeader()
{
    return "#";
}

} // namespace core
} // namespace rlispstat
