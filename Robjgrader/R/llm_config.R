#' Configure the LLM endpoint used by validate_text()
#'
#' Sets package-level \code{options()} that supply default values for the
#' \code{model}, \code{base_url}, and \code{api_key} parameters of
#' \code{validate_text()}.  Call this once at the top of an autograder script
#' to avoid repeating credentials on every \code{validate_text()} invocation.
#'
#' @details
#' The following options are set (all prefixed \code{robjgrader.llm.*}):
#' \describe{
#'   \item{\code{robjgrader.llm.model}}{Model identifier string.}
#'   \item{\code{robjgrader.llm.base_url}}{OpenAI-compatible API base URL.}
#'   \item{\code{robjgrader.llm.api_key}}{Bearer token for the API.}
#' }
#'
#' When \code{provider} is given, sensible defaults for \code{model},
#' \code{base_url}, and the API-key environment variable are applied
#' automatically.  Any explicitly supplied arguments override the
#' provider defaults.
#'
#' Supported providers:
#' \describe{
#'   \item{\code{"groq"}}{Groq (\code{https://api.groq.com/openai/v1});
#'     key from \env{GROQ_API_KEY}.  Default model:
#'     \code{"llama-3.3-70b-versatile"}.}
#'   \item{\code{"openai"}}{OpenAI (\code{https://api.openai.com/v1});
#'     key from \env{OPENAI_API_KEY}.  Default model: \code{"gpt-4o-mini"}.}
#'   \item{\code{"ollama"}}{Local Ollama instance
#'     (\code{http://localhost:11434/v1}); no real key required.  Set
#'     \code{model} to the name of your locally running model.}
#' }
#'
#' For providers not listed (e.g. a self-hosted vLLM server, LiteLLM, or an
#' Anthropic proxy), pass \code{base_url} and \code{api_key} directly.
#'
#' @param model    Character. Model identifier.
#' @param base_url Character. Base URL of the OpenAI-compatible endpoint.
#' @param api_key  Character. API bearer token.
#' @param provider Character. Shorthand; one of \code{"groq"},
#'   \code{"openai"}, \code{"ollama"}.
#'
#' @return Invisibly, a named list of the options that were set.
#' @seealso \code{\link{validate_text}}
#' @export
robjgrader_set_llm <- function(
  model    = NULL,
  base_url = NULL,
  api_key  = NULL,
  provider = NULL
) {
  if (!is.null(provider)) {
    provider <- match.arg(provider, c("groq", "openai", "ollama"))
    defs     <- switch(provider,
      groq   = list(
        model    = "llama-3.3-70b-versatile",
        base_url = "https://api.groq.com/openai/v1",
        api_key  = Sys.getenv("GROQ_API_KEY")
      ),
      openai = list(
        model    = "gpt-4o-mini",
        base_url = "https://api.openai.com/v1",
        api_key  = Sys.getenv("OPENAI_API_KEY")
      ),
      ollama = list(
        model    = "llama3",
        base_url = "http://localhost:11434/v1",
        api_key  = "ollama"
      )
    )
    if (is.null(model))    model    <- defs$model
    if (is.null(base_url)) base_url <- defs$base_url
    if (is.null(api_key))  api_key  <- defs$api_key
  }

  opts <- list()
  if (!is.null(model))    opts[["robjgrader.llm.model"]]    <- model
  if (!is.null(base_url)) opts[["robjgrader.llm.base_url"]] <- base_url
  if (!is.null(api_key))  opts[["robjgrader.llm.api_key"]]  <- api_key

  if (length(opts) > 0L) do.call(options, opts)
  invisible(opts)
}


#' Add custom GOF statistic labels for table validation
#'
#' Extends the built-in set of goodness-of-fit row labels recognised by the
#' \code{"gof"} group check in \code{validate()}.  The default list covers
#' common \pkg{modelsummary} / \pkg{broom} conventions; use this function to
#' register custom or course-specific GOF rows (e.g.
#' \code{"Pseudo-R2 (McFadden)"}, \code{"Mean VIF"}).
#'
#' @details
#' Labels are stored in \code{getOption("robjgrader.gof_labels")}.
#' \code{.is_gof_label()} consults this option in addition to the built-in
#' hard-coded list, so any labels registered here are recognised globally for
#' the remainder of the R session.
#'
#' To reset to only the built-in list, call
#' \code{options(robjgrader.gof_labels = NULL)}.
#'
#' @param labels Character vector of additional GOF row labels to register.
#' @return Invisibly, the updated full set of custom labels (not including
#'   the built-in list).
#' @seealso \code{\link{validate}}
#' @export
robjgrader_add_gof_labels <- function(labels) {
  current <- getOption("robjgrader.gof_labels", default = character(0L))
  updated <- unique(c(current, as.character(labels)))
  options(robjgrader.gof_labels = updated)
  invisible(updated)
}
