ggplot2_namespace <- asNamespace("ggplot2")
ggplot2_exports <- getNamespaceExports("ggplot2")
ggplot2_exports <- setdiff(ggplot2_exports, grep("^scale|^theme|^x|^y|^element", ggplot2_exports, value = TRUE, invert = TRUE))
theme_args <- names(formals("theme", envir = ggplot2_namespace))
element_funs <- grep("^element_", ggplot2_exports, value = TRUE)
element_args <- setNames(lapply(element_funs, function(x) names(formals(x, envir = ggplot2_namespace))), element_funs)
element_calls <- unlist(lapply(element_funs, function(x) paste0("theme(?? = ", x, "(", element_args[[x]], " = ??)")))
easy_namespace <- asNamespace("ggeasy")
easy_exports <- getNamespaceExports("ggeasy")
easy_funs <- ls.str(easy_funs, envir = easy_namespace)
easy_args <- setNames(lapply(easy_funs, function(x) names(formals(x, envir = asNamespace("ggeasy")))), easy_funs)
easy_calls <- unlist(lapply(easy_funs, function(x) paste0(x, "(", easy_args[[x]], " = ??)")))
synonyms <- list(
  angle = c("angle", "rotate", "turn", "direction", "tilt", "aim", "direct", "slant", "degrees", "skew"),
  color = c("color", "colour", "col", "tint", "shade", "hue", "tone", "coloring", "colouring", "fill"),
  size = c("size", "dimension", "dimesions", "measure", "length", "height", "big", "bigger", "small", "smaller", "magnitude", "large", "larger", "increase", "decrease", "width", "span", "medium"),
  move = c("move", "put", "place", "set", "arrange", "shift", "locate", "location"),
  text = c("text", "labels", "words", "names", "font", "letters"),
  remove = c("remove", "drop", "no", "delete", "reject", "lose", "dump", "clear", "abandon"),
  legend = c("legend", "guide")
)

find_call <- function(text) {
  text_split <- strsplit(text, split = " ", fixed = TRUE)[[1]]
  direct_matches <- unname(unlist(c(sapply(text_split, match_text))))
  synonym_extracts <- unname(unlist(c(sapply(text_split,
                            function(x) {
                              names(Filter(function(z) length(z) > 0, sapply(synonyms, function(y) {
                                grep(x, y, value = TRUE)
                              })))
                            }))))
  synonym_matches <- sapply(synonym_extracts, match_text)

  res <- unique(
    c(
      unlist(direct_matches),
      unlist(synonym_matches)
    )
  )


  dists <- stringdist::stringdist(text, gsub("_", "", res))
  ordered_res <- res[order(dists)]
  easy_res <- grep("^easy_", ordered_res, value = TRUE)
  theme_res <- grep("^theme", ordered_res, value = TRUE)
  hard_res <- grep("^easy_|^theme", ordered_res, value = TRUE, invert = TRUE)

  return(list(ggeasy = easy_res, ggplot2 = c(theme_res, hard_res)))
}

match_text <- function(text) {

  export_found <- grep(text, ggplot2_exports, value = TRUE, fixed = TRUE)
  theme_args_found <- grep(text, theme_args, value = TRUE, fixed = TRUE)
  element_calls_found <- grep(text, element_calls, value = TRUE, fixed = TRUE)
  easy_export_found <- grep(text, easy_exports, value = TRUE, fixed = TRUE)
  easy_calls_found <- grep(text, easy_calls, value = TRUE, fixed = TRUE)

  c(
    unique(
      c(
        if (length(easy_export_found)) paste0(easy_export_found, "(??)"),
        if (length(easy_calls_found)) easy_calls_found
      )
    ),
    unique(
      c(
        if (length(export_found)) paste0(export_found, "(??)"),
        if (length(theme_args_found)) paste0("theme(", theme_args_found, " = ??)"),
        if (length(element_calls_found)) element_calls_found
      )
    )
  )
}
