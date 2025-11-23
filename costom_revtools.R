library(revtools)

read_bibliography_internal2 <- function (filename, return_df = TRUE) 
{
  if (grepl(".csv$", filename)) {
    result <- revtools_csv(filename)
    if (!return_df) {
      result <- as.bibliography(result)
    }
  }
  else {
    z <- tryCatch({
      scan(filename, sep = "\t", what = "character", quote = "", 
           quiet = TRUE, blank.lines.skip = FALSE)
    }, warning = function(w) {
      stop("file import failed: data type not recognized by read_bibliography", 
           call. = FALSE)
    }, error = function(e) {
      stop("file import failed: data type not recognized by read_bibliography", 
           call. = FALSE)
    })
    #Encoding(z) <- "latin1"
    z <- gsub("<[[:alnum:]]{2}>", "", z, useBytes = TRUE)
    nrows <- min(c(200, length(z)))
    zsub <- z[seq_len(nrows)]
    n_brackets <- length(grep("\\{", zsub))
    n_dashes <- length(grep(" - ", zsub))
    if (n_brackets > n_dashes) {
      result <- revtools:::read_bib(z)
    }
    else {
      if (grepl(".ciw$", filename)) {
        tag_type <- "wos"
      }
      else {
        tag_type <- "ris"
      }
      if (grepl(".nbib$", filename)) {
        pub_delimiter <- "space"
      } else {
        pub_delimiter <- revtools:::detect_delimiter(zsub)}
      z_dframe <- revtools:::prep_ris(z, pub_delimiter)
      if (any(z_dframe$ris == "PMID")) {
        result <- revtools:::read_medline(z_dframe)
      }
      else {
        result <- revtools:::read_ris(z_dframe, tag_type)
      }
    }
    if (return_df) {
      result <- as.data.frame(result)
    }
  }
  return(result)
}

### Making another modified function. 
read_bibliography2 <- function (filename, return_df = TRUE) 
{
  invisible(Sys.setlocale("LC_ALL", "C"))
  on.exit(invisible(Sys.setlocale("LC_ALL", "")))
  if (missing(filename)) {
    stop("filename is missing with no default")
  }
  file_check <- unlist(lapply(filename, file.exists))
  if (any(!file_check)) {
    stop("file not found")
  }
  if (length(filename) > 1) {
    result_list <- lapply(filename, function(a, df) {
      read_bibliography_internal2(a, df)
    }, df = return_df)
    names(result_list) <- filename
    if (return_df) {
      result <- merge_columns(result_list)
      result$filename <- unlist(lapply(seq_len(length(result_list)), 
                                       function(a, data) {
                                         rep(names(data)[a], nrow(data[[a]]))
                                       }, data = result_list))
      if (any(colnames(result) == "label")) {
        result$label <- make.unique(result$label)
      }
      return(result)
    }
    else {
      result <- do.call(c, result_list)
      return(result)
    }
  }
  else {
    return(read_bibliography_internal2(filename, return_df))
  }
}