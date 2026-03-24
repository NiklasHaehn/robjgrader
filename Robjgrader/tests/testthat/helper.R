# Shared helper: build a minimal robjgrader_records object from a list of
# (object, name) pairs without needing the live task callback.
make_records <- function(...) {
  items <- list(...)
  recs  <- vector("list", length(items))
  for (i in seq_along(items)) {
    it  <- items[[i]]
    obj <- it$obj
    nm  <- it$name
    tp  <- .classify_object(obj, list(df=TRUE, ggplot=TRUE, model=TRUE, table=TRUE))
    recs[[i]] <- list(
      event_id    = i,
      event_type  = if (!is.null(nm)) "assignment" else "visible_return",
      object_name = nm,
      object_type = tp,
      object_class = class(obj)[1L],
      expr_text   = "test_expr",
      timestamp   = Sys.time(),
      object      = obj
    )
  }
  structure(recs, class = "robjgrader_records")
}
