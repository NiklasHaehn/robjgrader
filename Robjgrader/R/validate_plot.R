# Validator for ggplot objects.
#
# Groups (for reference comparison):
#   aesthetics  all global and layer-level aesthetic mappings
#   geoms       geom types across all layers
#   facets      faceting type and variables
#   stats       stat transformations per layer
#   theme       theme class (structural check only)
#   data        dataset used in the plot
#
# Individual checks:
#   aes_x, aes_y, aes_color / aes_colour, aes_fill, aes_size,
#   aes_shape, aes_alpha, aes_group, aes_linetype
#     character -- expected aesthetic expression, e.g. aes_x = "wt"
#   geom
#     character vector -- geom type(s) that must be present
#   facet_var
#     character vector -- faceting variable(s) that must be present

.plot_default_groups <- c("aesthetics", "geoms", "facets")
.plot_all_groups     <- c("aesthetics", "geoms", "facets", "stats", "theme", "data")

.validate_plot <- function(obj, reference, checks, exclude = NULL, name) {
  resolved <- .resolve_groups(checks, exclude, .plot_default_groups, .plot_all_groups)
  results  <- list()
  cls      <- class(obj)[1L]

  # -- Group checks (require reference) ----------------------------------------
  if (length(resolved$groups) > 0L) {
    if (is.null(reference))
      stop("'reference' is required for group-level ggplot checks.")
    if (!inherits(reference, "ggplot"))
      stop("'reference' must be a ggplot object.")

    for (grp in resolved$groups) {
      cmp <- .compare_plot_group(obj, reference, grp)
      results[[grp]] <- .make_check(grp, cmp$pass, cmp$expected, cmp$observed, cmp$msg)
    }
  }

  # -- Individual checks --------------------------------------------------------
  chk      <- resolved$checks
  aes_keys <- grep("^aes_", names(chk), value = TRUE)

  for (key in aes_keys) {
    aes_name <- sub("^aes_", "", key)
    expected <- chk[[key]]
    observed <- .get_aes_label(obj, aes_name)
    pass     <- !is.null(observed) && observed == expected

    results[[key]] <- .make_check(
      key, pass, expected,
      if (is.null(observed)) "<not mapped>" else observed,
      if (pass)
        sprintf("%s: '%s' correct", key, expected)
      else if (is.null(observed))
        sprintf("%s: '%s' not found -- aesthetic not mapped", key, expected)
      else
        sprintf("%s: expected '%s', found '%s'", key, expected, observed)
    )
  }

  if (!is.null(chk$geom)) {
    expected_geoms <- vapply(chk$geom, .normalize_geom, character(1L))
    observed_geoms <- vapply(obj$layers, function(l) class(l$geom)[1L], character(1L))
    missing_geoms  <- setdiff(expected_geoms, observed_geoms)
    pass           <- length(missing_geoms) == 0L
    missing_short  <- chk$geom[expected_geoms %in% missing_geoms]

    results[["geom"]] <- .make_check(
      "geom", pass,
      paste(chk$geom, collapse = ", "),
      paste(observed_geoms, collapse = ", "),
      if (pass) sprintf("required geom(s) present: %s", paste(chk$geom, collapse = ", "))
      else      sprintf("missing geom(s): %s", paste(missing_short, collapse = ", "))
    )
  }

  if (!is.null(chk$facet_var)) {
    facet_vars   <- .get_facet_vars(obj)
    missing_vars <- setdiff(chk$facet_var, facet_vars)
    pass         <- length(missing_vars) == 0L

    results[["facet_var"]] <- .make_check(
      "facet_var", pass, chk$facet_var, facet_vars,
      if (pass) sprintf("facet variable(s) present: %s", paste(chk$facet_var, collapse = ", "))
      else      sprintf("missing facet variable(s): %s", paste(missing_vars, collapse = ", "))
    )
  }

  if (!is.null(chk$facet_type)) {
    facet_class  <- class(obj$facet)[1L]
    expected_cls <- switch(tolower(chk$facet_type),
      "wrap" = "FacetWrap",
      "grid" = "FacetGrid",
      "null" = "FacetNull",
      chk$facet_type
    )
    pass <- facet_class == expected_cls
    results[["facet_type"]] <- .make_check(
      "facet_type", pass, chk$facet_type, facet_class,
      if (pass) sprintf("facet type correct: '%s'", chk$facet_type)
      else      sprintf("facet type: expected '%s' (%s), found '%s'",
                        chk$facet_type, expected_cls, facet_class)
    )
  }

  if (!is.null(chk$stat)) {
    expected_stats <- vapply(chk$stat, .normalize_stat, character(1L))
    observed_stats <- vapply(obj$layers, function(l) class(l$stat)[1L], character(1L))
    missing_stats  <- setdiff(expected_stats, observed_stats)
    pass           <- length(missing_stats) == 0L
    missing_short  <- chk$stat[expected_stats %in% missing_stats]

    results[["stat"]] <- .make_check(
      "stat", pass,
      paste(chk$stat, collapse = ", "),
      paste(observed_stats, collapse = ", "),
      if (pass) sprintf("required stat(s) present: %s", paste(chk$stat, collapse = ", "))
      else      sprintf("missing stat(s): %s", paste(missing_short, collapse = ", "))
    )
  }

  .make_result(name, "ggplot", cls, results)
}


# -- Group comparison helpers ---------------------------------------------------

.compare_plot_group <- function(obj, ref, group) {
  switch(group,
    aesthetics = {
      cmp <- .compare_ggplot_mappings(obj, ref)
      list(pass = cmp$pass, expected = NULL, observed = NULL,
           msg = cmp$msg %||% "aesthetic mappings match reference")
    },
    geoms = {
      cmp <- .compare_ggplot_geoms(obj, ref)
      list(pass = cmp$pass, expected = NULL, observed = NULL,
           msg = cmp$msg %||% "geom layer types match reference")
    },
    facets = {
      obj_class <- class(obj$facet)[1L]
      ref_class <- class(ref$facet)[1L]
      if (obj_class != ref_class) {
        list(pass = FALSE, expected = ref_class, observed = obj_class,
             msg = sprintf("facet type differs: expected %s, found %s",
                           ref_class, obj_class))
      } else {
        obj_vars <- sort(.get_facet_vars(obj))
        ref_vars <- sort(.get_facet_vars(ref))
        pass     <- identical(obj_vars, ref_vars)
        list(pass = pass, expected = ref_vars, observed = obj_vars,
             msg = if (pass) "facets match reference"
                   else sprintf("facet variables differ: expected {%s}, found {%s}",
                                 paste(ref_vars, collapse = ", "),
                                 paste(obj_vars, collapse = ", ")))
      }
    },
    stats = {
      obj_stats <- sort(vapply(obj$layers, function(l) class(l$stat)[1L], character(1L)))
      ref_stats <- sort(vapply(ref$layers, function(l) class(l$stat)[1L], character(1L)))
      pass      <- identical(obj_stats, ref_stats)
      list(pass = pass, expected = ref_stats, observed = obj_stats,
           msg = if (pass) "stat transformations match reference"
                 else sprintf("stats differ: expected {%s}, found {%s}",
                               paste(ref_stats, collapse = ", "),
                               paste(obj_stats, collapse = ", ")))
    },
    theme = {
      obj_class <- class(obj$theme)[1L]
      ref_class <- class(ref$theme)[1L]
      pass      <- identical(obj_class, ref_class)
      list(pass = pass, expected = ref_class, observed = obj_class,
           msg = if (pass) "theme class matches reference"
                 else sprintf("theme class differs: expected %s, found %s",
                               ref_class, obj_class))
    },
    data = {
      pass <- isTRUE(all.equal(obj$data, ref$data, check.attributes = FALSE))
      list(pass = pass, expected = NULL, observed = NULL,
           msg = if (pass) "plot data matches reference"
                 else      "plot data does not match reference")
    },
    list(pass = NA, expected = NULL, observed = NULL,
         msg = sprintf("group '%s' not implemented", group))
  )
}


# -- Aesthetic extraction helpers -----------------------------------------------

.get_aes_label <- function(plot, aes_name) {
  aes_name <- gsub("^color$", "colour", aes_name)
  if (aes_name %in% names(plot$mapping))
    return(rlang::as_label(plot$mapping[[aes_name]]))
  for (layer in plot$layers)
    if (aes_name %in% names(layer$mapping))
      return(rlang::as_label(layer$mapping[[aes_name]]))
  NULL
}

.normalize_geom <- function(x) {
  if (grepl("^Geom", x)) return(x)
  paste0("Geom", toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

.normalize_stat <- function(x) {
  if (grepl("^Stat", x)) return(x)
  paste0("Stat", toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

.get_facet_vars <- function(plot) {
  facet_class <- class(plot$facet)[1L]
  if (facet_class == "FacetNull") return(character(0L))
  if (facet_class == "FacetWrap") return(names(plot$facet$params$facets))
  if (facet_class == "FacetGrid")
    return(c(names(plot$facet$params$rows), names(plot$facet$params$cols)))
  character(0L)
}

.collect_all_aes <- function(plot) {
  mapping <- list()
  for (layer in plot$layers)
    for (nm in names(layer$mapping))
      if (nzchar(nm) && is.null(mapping[[nm]]))
        mapping[[nm]] <- rlang::as_label(layer$mapping[[nm]])
  for (nm in names(plot$mapping))
    if (nzchar(nm))
      mapping[[nm]] <- rlang::as_label(plot$mapping[[nm]])
  mapping <- mapping[sort(names(mapping))]
  setNames(as.character(unlist(mapping)), names(mapping))
}

.compare_ggplot_mappings <- function(obj, ref) {
  obj_map <- .collect_all_aes(obj)
  ref_map <- .collect_all_aes(ref)
  pass    <- identical(obj_map, ref_map)
  list(pass = pass,
       msg  = if (pass) NULL else sprintf(
         "aesthetic mappings differ -- expected {%s}, found {%s}",
         paste(names(ref_map), ref_map, sep = "=", collapse = ", "),
         paste(names(obj_map), obj_map, sep = "=", collapse = ", ")
       ))
}

.compare_ggplot_geoms <- function(obj, ref) {
  geom_types <- function(p) sort(vapply(p$layers, function(l) class(l$geom)[1L], character(1L)))
  obj_geoms  <- geom_types(obj)
  ref_geoms  <- geom_types(ref)
  pass       <- identical(obj_geoms, ref_geoms)
  list(pass = pass,
       msg  = if (pass) NULL else sprintf(
         "geom layers differ -- expected {%s}, found {%s}",
         paste(ref_geoms, collapse = ", "),
         paste(obj_geoms, collapse = ", ")
       ))
}
