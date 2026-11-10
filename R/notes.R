# notes.R — notes and verdicts that readers write on a served map, kept in a file beside it.
# Moved from ramb's R/map.R on 2026-11-10.

#' Notes pinned on a served map
#'
#' A map served with `rr_serve(notes = )` takes notes: in its Notes group the reader presses "Add a note", clicks
#' the map and writes, and the server writes the note to the file, a GeoJSON FeatureCollection with a point for each
#' note. A note keeps its `id`, its `time` (UTC), its `author` (`"reader"` from the page), its `text`, the map's `zoom`
#' and the page's `address` (its query and position: the view and layers as the reader saw them), and `under`, the
#' features under it: each "layer: title", from the layers whose popup has a `title`. A reply joins a note's thread
#' (`reply_to`, the first note's `id`) at its place. The page shows a pin for each thread, red while its last word is
#' the reader's and blue once answered, with the thread and a box to reply in on a click, and asks for the notes again
#' when it regains the focus and every 30 s.
#'
#' `rr_notes()` reads the notes, `rr_notes_add()` adds a note or a reply from R: a dialogue about places on a
#' map, between a reader in the page and someone at R (or an assistant reading the file). Both may run while the map is
#' served, in this R session or another: each write replaces the file whole, by a rename. The file is plain GeoJSON, a
#' note a line, and opens in any GIS. Notes are written only by R in a UTF-8 locale, where letters outside ASCII are
#' kept as they are.
#'
#' @param path The notes' file (`.geojson`).
#' @param text The note's text.
#' @param lon,lat Where it is pinned. A reply's place is its thread's.
#' @param author Who wrote it.
#' @param reply_to The `id` of the note it answers; a reply to a reply joins the same thread.
#' @return `rr_notes()`: a data frame, a row for each note in the order written: `id`, `thread` (the `id` of its
#'   thread's first note), `reply_to`, `time` (POSIXct, UTC), `author`, `text`, `lon`, `lat`, `zoom`, `under` (the
#'   features under it, separated by "; ") and `address`; no rows when there is no file. `rr_notes_add()`: the
#'   new note's `id`, invisibly.
#' @seealso [rr_serve()]
#' @export
#' @examples
#' f <- tempfile(fileext = ".geojson")
#' rr_notes_add(f, "Is the bank's edge here?", lon = -14.6, lat = 64.0, author = "reader")
#' rr_notes_add(f, "Yes, at the 200 m contour.", reply_to = 1, author = "analyst")
#' rr_notes(f)[, c("id", "thread", "author", "text")]
#' # a map served with them: rr_serve(m, notes = f)
rr_notes <- function(path) {
  path <- .rr_notes_path(path, folder = FALSE)
  fs <- .rr_notes_read(path)
  p <- lapply(fs, `[[`, "properties")
  chr <- function(k) vapply(p, function(x) if (is.null(x[[k]])) NA_character_ else as.character(x[[k]]), "")
  num <- function(k) vapply(p, function(x) if (is.null(x[[k]])) NA_real_ else as.numeric(x[[k]]), 0)
  xy <- function(i) vapply(fs, function(f) as.numeric(.rr_or(f$geometry$coordinates[[i]], NA)), 0)
  id <- as.integer(num("id"))
  reply_to <- as.integer(num("reply_to"))
  thread <- id
  thread[!is.na(reply_to)] <- reply_to[!is.na(reply_to)]
  data.frame(
    id = id,
    thread = thread,
    reply_to = reply_to,
    time = as.POSIXct(chr("time"), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    author = chr("author"),
    text = chr("text"),
    lon = xy(1),
    lat = xy(2),
    zoom = num("zoom"),
    under = vapply(p, function(x) if (length(x$under)) paste(unlist(x$under), collapse = "; ") else NA_character_, ""),
    address = chr("address"),
    stringsAsFactors = FALSE
  )
}

#' @rdname rr_notes
#' @export
rr_notes_add <- function(path, text, lon = NULL, lat = NULL, author = "analyst", reply_to = NULL) {
  path <- .rr_notes_path(path)
  .rr_notes_utf8()
  if (!is.character(author) || length(author) != 1 || is.na(author) || !nzchar(trimws(author))) {
    stop("`author` is a name.", call. = FALSE)
  }
  why <- .rr_notes_check(list(text = text, lon = lon, lat = lat, reply_to = reply_to), .rr_notes_read(path))
  if (!is.null(why)) {
    stop(why, call. = FALSE)
  }
  invisible(.rr_notes_add(path, list(text = text, lon = lon, lat = lat, reply_to = reply_to), trimws(author)))
}

# Notes are written only by an R in a UTF-8 locale: in another (C, as Rscript may start), R's strings and jsonlite write
# a letter outside ASCII as "<c3><b3>", which would stay in the file
.rr_notes_utf8 <- function() {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop(
      "Notes are written by R in a UTF-8 locale (this one is ", Sys.getlocale("LC_CTYPE"), "): start R with ",
      "LANG=en_US.UTF-8, or Sys.setlocale(\"LC_ALL\", \"en_US.UTF-8\").",
      call. = FALSE
    )
  }
}

# The notes' file: one path, a .geojson or .json file whose folder exists (when it is to be written)
.rr_notes_path <- function(path, folder = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !grepl("[.](geo)?json$", path, ignore.case = TRUE)) {
    stop("The notes' file is one path to a .geojson file.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (folder && !dir.exists(dirname(path))) {
    stop(sprintf("The notes' folder %s does not exist.", dirname(path)), call. = FALSE)
  }
  path
}

# The notes as features, none when there is no file; a file that is not a FeatureCollection of numbered notes stops
# whatever would read or write it, so it is never overwritten
.rr_notes_read <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  x <- tryCatch(
    jsonlite::fromJSON(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  ok <- is.list(x) && identical(x$type, "FeatureCollection") && is.list(x$features) &&
    all(vapply(x$features, function(f) is.numeric(f$properties$id) && length(f$properties$id) == 1, logical(1)))
  if (!ok) {
    stop(sprintf("%s is not a notes file (a GeoJSON FeatureCollection, each feature with an id): left as it is.", path),
         call. = FALSE)
  }
  structure(x$features, last_id = if (is.numeric(x$last_id)) x$last_id)
}

# The highest id given, deleted notes' included: a number is never given to two notes
.rr_notes_last_id <- function(features) {
  max(c(0, attr(features, "last_id"), vapply(features, function(f) as.numeric(f$properties$id), 0)))
}

# Each note on a line of its own, and the last id given (a foreign member, which GeoJSON readers pass over), written to
# a file beside it and renamed over it
.rr_notes_write <- function(path, features, last_id) {
  one <- function(f) as.character(jsonlite::toJSON(f, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  body <- if (length(features)) paste0("\n", paste(vapply(features, one, ""), collapse = ",\n"), "\n") else ""
  tmp <- paste0(path, ".tmp")
  writeBin(charToRaw(enc2utf8(paste0('{"type": "FeatureCollection", "last_id": ', last_id, ', "features": [', body,
                                     "]}\n"))), tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop(sprintf("The notes could not be written to %s.", path), call. = FALSE)
  }
  invisible(path)
}

# A note's fields checked against the notes there: NULL, or why it is refused
.rr_notes_check <- function(n, features) {
  one_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!is.character(n$text) || length(n$text) != 1 || is.na(n$text) || !nzchar(trimws(n$text))) {
    return("A note has a `text`.")
  }
  if (nchar(n$text) > 4000) {
    return("A note's text is at most 4,000 characters.")
  }
  if (!is.null(n$reply_to)) {
    ids <- vapply(features, function(f) as.numeric(f$properties$id), 0)
    if (!one_num(n$reply_to) || !n$reply_to %in% ids) {
      return("`reply_to` is the id of a note there.")
    }
  } else if (!one_num(n$lon) || !one_num(n$lat) || abs(n$lon) > 180 || abs(n$lat) > 90) {
    return("A note has a place, `lon` and `lat` in degrees (a reply takes its thread's).")
  }
  if (!is.null(n$zoom) && !(one_num(n$zoom) && n$zoom >= 0 && n$zoom <= 24)) {
    return("A note's `zoom` is a number from 0 to 24.")
  }
  if (!is.null(n$under) && !(is.list(n$under) || is.character(n$under))) {
    return("A note's `under` is a list of texts.")
  }
  if (!is.null(n$under) && (length(n$under) > 50 ||
                              !all(vapply(n$under, function(u) is.character(u) && length(u) == 1, logical(1))))) {
    return("A note's `under` is at most 50 texts.")
  }
  if (!is.null(n$address) && !(is.character(n$address) && length(n$address) == 1 && nchar(n$address) <= 4000)) {
    return("A note's `address` is a text of at most 4,000 characters.")
  }
  NULL
}

# A note added, checked before: a reply at its thread's first note, which it names; its id, one more than the last
.rr_notes_add <- function(path, n, author) {
  fs <- .rr_notes_read(path)
  ids <- vapply(fs, function(f) as.numeric(f$properties$id), 0)
  reply_to <- NULL
  if (!is.null(n$reply_to)) {
    root <- fs[[match(n$reply_to, ids)]]
    if (!is.null(root$properties$reply_to)) {
      root <- fs[[match(root$properties$reply_to, ids)]]
    }
    reply_to <- as.integer(root$properties$id)
    n$lon <- root$geometry$coordinates[[1]]
    n$lat <- root$geometry$coordinates[[2]]
  }
  id <- as.integer(.rr_notes_last_id(fs) + 1)
  fs[[length(fs) + 1]] <- list(
    type = "Feature",
    geometry = list(type = "Point", coordinates = list(round(n$lon, 6), round(n$lat, 6))),
    properties = list(
      id = id,
      reply_to = reply_to,
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      author = author,
      text = trimws(n$text),
      zoom = if (!is.null(n$zoom)) round(n$zoom, 2),
      under = as.list(vapply(n$under, function(u) substr(u, 1, 300), "")),
      address = n$address
    )
  )
  .rr_notes_write(path, fs, id)
  id
}

# A note deleted, and with the first note of a thread its replies
.rr_notes_delete <- function(path, id) {
  fs <- .rr_notes_read(path)
  ids <- vapply(fs, function(f) as.numeric(f$properties$id), 0)
  to <- vapply(fs, function(f) as.numeric(.rr_or(f$properties$reply_to, NA)), 0)
  if (!id %in% ids) {
    return(FALSE)
  }
  .rr_notes_write(path, fs[!(ids == id | to %in% id)], .rr_notes_last_id(fs))
  TRUE
}

# The routes of the notes: GET /notes, POST /notes (a note or a reply), DELETE /notes/<id>, each answered with the notes
# (a new note's id in `added`). Only for a page opened from this computer (its Host, against DNS rebinding); a write
# only from the page itself (its header, which a page elsewhere may not send without the server's leave, and its Origin)
.rr_map_notes_reply <- function(req, notes, hosts, reply, json) {
  refused <- .rr_map_local_only(req, hosts, reply, "the notes", "a note")
  if (!is.null(refused)) {
    return(refused)
  }
  method <- req$REQUEST_METHOD
  answer <- function(added = NULL) {
    force(added) # a note added before the notes are read
    fc <- list(type = "FeatureCollection", features = .rr_notes_read(notes))
    fc$added <- added
    reply(200L, json(fc), "application/geo+json; charset=utf-8", list(`Cache-Control` = "no-store"))
  }
  path <- req$PATH_INFO
  tryCatch(
    {
      if (path == "/notes" && method == "GET") {
        return(answer())
      }
      if (path == "/notes" && method == "POST") {
        if (suppressWarnings(as.numeric(.rr_or(req$CONTENT_LENGTH, "0"))) > 65536) {
          return(reply(413L, "a note is at most 64 kB"))
        }
        b <- req$rook.input$read()
        if (length(b) > 65536) {
          return(reply(413L, "a note is at most 64 kB"))
        }
        b <- rawToChar(b)
        Encoding(b) <- "UTF-8"
        n <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.list(n) || is.null(names(n))) {
          return(reply(400L, "a note is a JSON object"))
        }
        if (is.list(n$under)) {
          n$under <- lapply(n$under, function(u) if (is.character(u)) u else NA)
        }
        why <- .rr_notes_check(n, .rr_notes_read(notes))
        if (!is.null(why)) {
          return(reply(400L, why))
        }
        return(answer(.rr_notes_add(notes, n, "reader")))
      }
      d <- regmatches(path, regexec("^/notes/([0-9]+)$", path))[[1]]
      if (length(d) == 2 && method == "DELETE") {
        if (!.rr_notes_delete(notes, as.numeric(d[2]))) {
          return(reply(404L, "no such note"))
        }
        return(answer())
      }
      reply(405L, "GET /notes, POST /notes or DELETE /notes/<id>")
    },
    error = function(e) reply(500L, sub("\n.*", "", conditionMessage(e)))
  )
}

# A request for what the server keeps (the notes, the verdicts), checked: only from a page opened from this computer
# (its Host, against DNS rebinding), and a write only from the page itself (its header, which a page elsewhere may not
# send without the server's leave, and its Origin). NULL, or the refusal
.rr_map_local_only <- function(req, hosts, reply, what, one) {
  host <- tolower(sub(":[0-9]+$", "", .rr_or(req$HTTP_HOST, "")))
  if (!host %in% hosts) {
    return(reply(403L, sprintf("%s are kept for a page opened from this computer", what)))
  }
  if (req$REQUEST_METHOD != "GET" && (!identical(req$HTTP_X_RB_NOTES, "1") ||
                                        (!is.null(req$HTTP_ORIGIN) &&
                                           !identical(req$HTTP_ORIGIN, paste0("http://", req$HTTP_HOST))))) {
    return(reply(403L, sprintf("%s comes from the map's page", one)))
  }
  NULL
}

#' Verdicts given on a served map
#'
#' A map served with `rr_serve(verdicts = )` takes the reader's verdict on the features of a layer whose popup has
#' a `verdict` ([rr_layer()]): its questions in a feature's popup, a button for each answer, a box for the name an
#' answer points at (`other`, with the names in view offered), and the feature's note where the verdict asks for one.
#' Each answer is a row added to a `.csv` file, never a row changed: when, the layer, the feature (its value of the
#' verdict's `field`), the question, the answer, the other feature's name, the note, the feature's title, its place
#' (the feature's own for a point, else where it was clicked) and the zoom. The note is a question of its own, "note",
#' its text the answer. A later answer to a question replaces the earlier, and an empty one withdraws it. The page draws
#' a mark on each feature judged: the first question's answer its fill, the second's its ring. A review written down
#' where it is made, and read back as data: what a map shows can be checked by hand, and the checks counted. A file of
#' 2026.11.09, before the questions, reads as the question "verdict" with no other names.
#'
#' @param path The verdicts' file (`.csv`).
#' @param all `FALSE`, each feature's current answer to each question (the last, none where it was withdrawn; a note
#'   where it has a text); `TRUE`, every answer given, in the order given.
#' @return A data frame: `time` (UTC, as text), `layer`, `id`, `question`, `value`, `other`, `text`, `title`, `lon`,
#'   `lat`, `zoom`; no rows when there is no file.
#' @export
#' @examples
#' # a map whose harbours take a verdict each:
#' # popup = list(click = "harbours-fill", title = "SI_HARB",
#' #              verdict = list(field = "SI_HARB", text = TRUE,
#' #                             options = list(list(value = "right", label = "right"),
#' #                                            list(value = "wrong", label = "wrong"))))
#' # rr_serve(m, verdicts = "harbour_verdicts.csv")
#' f <- tempfile(fileext = ".csv")
#' rr_verdicts(f) # none yet
rr_verdicts <- function(path, all = FALSE) {
  v <- .rr_verdicts_read(.rr_verdicts_path(path, folder = FALSE))
  if (isTRUE(all) || !nrow(v)) {
    return(v)
  }
  last <- !duplicated(paste(v$layer, v$id, v$question, sep = "\r"), fromLast = TRUE)
  given <- ifelse(v$question == "note", nzchar(v$text), nzchar(v$value))
  v <- v[last & given, , drop = FALSE]
  rownames(v) <- NULL
  v
}

.rr_verdict_cols <- c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom")

.rr_verdict_cols_v1 <- c("time", "layer", "id", "value", "text", "title", "lon", "lat", "zoom") # 2026.11.09

# The verdicts' file: one path, a .csv file whose folder exists (when it is to be written)
.rr_verdicts_path <- function(path, folder = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !grepl("[.]csv$", path, ignore.case = TRUE)) {
    stop("The verdicts' file is one path to a .csv file.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (folder && !dir.exists(dirname(path))) {
    stop(sprintf("The verdicts' folder %s does not exist.", dirname(path)), call. = FALSE)
  }
  path
}

# The verdicts as rows, none when there is no file; a file without the verdicts' columns stops whatever would read or
# write it, so it is never overwritten
.rr_verdicts_read <- function(path) {
  empty <- as.data.frame(stats::setNames(replicate(length(.rr_verdict_cols), character(), simplify = FALSE),
                                         .rr_verdict_cols), stringsAsFactors = FALSE)
  num <- function(v) {
    for (k in c("lon", "lat", "zoom")) v[[k]] <- suppressWarnings(as.numeric(v[[k]]))
    v
  }
  if (!file.exists(path)) {
    return(num(empty))
  }
  v <- tryCatch(utils::read.csv(path, colClasses = "character", na.strings = character(), encoding = "UTF-8",
                                check.names = FALSE),
                error = function(e) NULL)
  if (is.data.frame(v) && identical(names(v), .rr_verdict_cols_v1)) { # one question, before the questions
    v$question <- rep("verdict", nrow(v))
    v$other <- rep("", nrow(v))
    v <- v[, .rr_verdict_cols]
  }
  if (!is.data.frame(v) || !identical(names(v), .rr_verdict_cols)) {
    stop(sprintf("%s is not a verdicts file (a .csv with the columns %s): left as it is.", path,
                 paste(.rr_verdict_cols, collapse = ", ")), call. = FALSE)
  }
  num(v)
}

# A verdict's fields checked against the layers that take one: NULL, or why it is refused. An answer to a question is
# one of its options, with the other feature's name where the option names one and none where it does not, or empty
# to withdraw it; a note ("note") has a text, empty to clear it
.rr_verdicts_check <- function(n, specs) {
  one <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  one_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!one(n$layer) || is.null(specs[[n$layer]])) {
    return(sprintf("`layer` is one of the layers that take a verdict: %s.", paste(names(specs), collapse = ", ")))
  }
  if (!(one(n$id) || one_num(n$id)) || !nzchar(as.character(n$id)) || nchar(as.character(n$id)) > 300) {
    return("A verdict's `id` names its feature: a text of 1-300 characters.")
  }
  qs <- specs[[n$layer]]$questions
  ids <- vapply(qs, `[[`, "", "id")
  question <- .rr_or(n$question, if (length(qs) == 1) ids[1])
  if (!one(question) || !question %in% c(ids, if (specs[[n$layer]]$text) "note")) {
    return(sprintf("A verdict's `question` is one of %s.", paste(c(ids, if (specs[[n$layer]]$text) "note"), collapse = ", ")))
  }
  if (!is.null(n$text) && !(one(n$text) && nchar(n$text) <= 1000)) {
    return("A verdict's `text` is at most 1,000 characters.")
  }
  if (question == "note") {
    if (!is.null(n$value) && !identical(n$value, "")) {
      return("A note has a `text`, no `value`.")
    }
  } else {
    o <- qs[[match(question, ids)]]$options
    values <- vapply(o, `[[`, "", "value")
    if (!one(n$value) || !(n$value %in% c(values, ""))) {
      return(sprintf("An answer to \"%s\" is one of %s, or empty to withdraw it.", question, paste(values, collapse = ", ")))
    }
    asks <- nzchar(n$value) && isTRUE(o[[match(n$value, values)]]$other)
    if (asks && !(one(n$other) && nzchar(trimws(n$other)) && nchar(n$other) <= 200)) {
      return(sprintf("The answer \"%s\" names another feature: `other`, a name of 1-200 characters.", n$value))
    }
    if (!asks && !is.null(n$other) && !identical(n$other, "")) {
      return(sprintf("The answer \"%s\" names no other feature: no `other`.", n$value))
    }
  }
  if (!is.null(n$title) && !(one(n$title) && nchar(n$title) <= 300)) {
    return("A verdict's `title` is at most 300 characters.")
  }
  if (!one_num(n$lon) || !one_num(n$lat) || abs(n$lon) > 180 || abs(n$lat) > 90) {
    return("A verdict has its feature's place, `lon` and `lat` in degrees.")
  }
  if (!is.null(n$zoom) && !(one_num(n$zoom) && n$zoom >= 0 && n$zoom <= 24)) {
    return("A verdict's `zoom` is a number from 0 to 24.")
  }
  NULL
}

# A verdict added, checked before: the file written beside itself and renamed over it
.rr_verdicts_add <- function(path, n) {
  v <- .rr_verdicts_read(path)
  row <- data.frame(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), layer = n$layer, id = as.character(n$id),
    question = n$question, value = .rr_or(n$value, ""), other = trimws(.rr_or(n$other, "")),
    text = trimws(.rr_or(n$text, "")), title = .rr_or(n$title, ""),
    lon = round(n$lon, 6), lat = round(n$lat, 6), zoom = if (is.null(n$zoom)) NA_real_ else round(n$zoom, 2),
    stringsAsFactors = FALSE
  )
  v <- rbind(v, row)
  tmp <- paste0(path, ".tmp")
  con <- file(tmp, open = "w", encoding = "UTF-8")
  utils::write.csv(v, con, row.names = FALSE, na = "")
  close(con)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop(sprintf("The verdicts could not be written to %s.", path), call. = FALSE)
  }
  invisible(path)
}

# The layers of a map that take a verdict, by id: their verdicts' specs. Layers that keep their answers under one id
# (`store`, another layer's, else their own) share them, so they must ask the same questions with the same answers
.rr_map_verdict_layers <- function(map) {
  specs <- lapply(map$layers, function(l) l$config$popup$verdict)
  names(specs) <- vapply(map$layers, function(l) l$config$id, "")
  specs <- Filter(Negate(is.null), specs)
  shape <- function(v) paste(vapply(v$questions, function(q) paste0(q$id, ":", paste(vapply(q$options, `[[`, "", "value"), collapse = ",")), ""),
                             collapse = ";")
  store <- vapply(names(specs), function(id) .rr_or(specs[[id]]$store, id), "")
  for (k in unique(store)) {
    same <- names(specs)[store == k]
    if (length(unique(vapply(specs[same], shape, ""))) > 1) {
      stop(sprintf("The layers %s keep their verdicts as \"%s\" but ask different questions: give them the same.",
                   paste(same, collapse = ", "), k), call. = FALSE)
    }
  }
  specs
}

# The routes of the verdicts: GET /verdicts, POST /verdicts (given or withdrawn), each answered with the current ones
.rr_map_verdicts_reply <- function(req, verdicts, specs, hosts, reply, json) {
  refused <- .rr_map_local_only(req, hosts, reply, "the verdicts", "a verdict")
  if (!is.null(refused)) {
    return(refused)
  }
  answer <- function() {
    v <- rr_verdicts(verdicts)
    rows <- lapply(seq_len(nrow(v)), function(i) {
      as.list(v[i, c("layer", "id", "question", "value", "other", "text", "lon", "lat", "time")])
    })
    reply(200L, json(list(verdicts = rows)), "application/json; charset=utf-8", list(`Cache-Control` = "no-store"))
  }
  method <- req$REQUEST_METHOD
  tryCatch(
    {
      if (req$PATH_INFO != "/verdicts") {
        return(reply(404L, "no such route"))
      }
      if (method == "GET") {
        return(answer())
      }
      if (method == "POST") {
        if (suppressWarnings(as.numeric(.rr_or(req$CONTENT_LENGTH, "0"))) > 16384) {
          return(reply(413L, "a verdict is at most 16 kB"))
        }
        b <- req$rook.input$read()
        if (length(b) > 16384) {
          return(reply(413L, "a verdict is at most 16 kB"))
        }
        b <- rawToChar(b)
        Encoding(b) <- "UTF-8"
        n <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.list(n) || is.null(names(n))) {
          return(reply(400L, "a verdict is a JSON object"))
        }
        why <- .rr_verdicts_check(n, specs)
        if (!is.null(why)) {
          return(reply(400L, why))
        }
        if (is.null(n$question)) {
          n$question <- specs[[n$layer]]$questions[[1]]$id # a page of one question may leave it out
        }
        .rr_verdicts_read(verdicts) # a file broken by hand stops here, left as it is
        n$layer <- .rr_or(specs[[n$layer]]$store, n$layer) # kept under the answers it shares
        .rr_verdicts_add(verdicts, n)
        return(answer())
      }
      reply(405L, "GET /verdicts or POST /verdicts")
    },
    error = function(e) reply(500L, sub("\n.*", "", conditionMessage(e)))
  )
}

