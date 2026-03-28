get_lang <- function() {
    lang <- Sys.getenv("LANGUAGE", "")
    if (nzchar(lang)) {
        # "bg:en" ==> "bg", "pt_BR" ==> "pt_BR"
        lang <- strsplit(lang, "[:]", fixed = FALSE)[[1L]][1L]
        return(sub("[.@].*$", "", lang))
    }

    loc <- Sys.getlocale("LC_MESSAGES")
    if (!is.na(loc) && nzchar(loc)) {
        # "bg_BG.UTF-8" ==> "bg"
        return(sub("[_.@].*$", "", loc))
    }

    "en"
}

pick_user_trans <- function(x) {
    if (is.null(x)) return(NULL)
    # user supplied a single string → use as-is
    if (length(x) == 1L) return(as.character(x))
    # multiple supplied → try language match
    lang <- get_lang()                 # e.g. "bg", "en", "pt_BR"
    nms <- names(x)
    # Named and matching current language → use that
    if (!is.null(nms) && lang %in% nms) {
        return(as.character(x[[lang]]))
    }
    # Fallback → first element
    as.character(x[[1L]])
}
