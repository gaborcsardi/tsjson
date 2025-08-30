#' @export

delete_selections <- function(json) {
  select <- get_selected_nodes(json)

  if (length(select) == 0) {
    return(json)
  }

  # if deleting from an array, then we need to look at the array and
  # remove some commas, probably
  pares <- json$parent[select]
  ptypes <- json$type[pares]
  trimmed_arrays <- unique(pares[ptypes == "array"])
  trimmed_pairs <- pares[ptypes == "pair"]
  select <- c(select, trimmed_pairs)
  trimmed_objects <- unique(json$parent[trimmed_pairs])

  deleted <- select
  while (TRUE) {
    deleted2 <- unique(c(deleted, unlist(json$children[deleted])))
    if (length(deleted2) == length(deleted)) {
      break
    }
    deleted <- deleted2
  }

  # TODO: when should we leave trailing whitespace? E.g. when removing
  # a trailing comma?
  trim_commas <- function(id, open, close) {
    # remove deleted children and see what is left
    allchld <- json$children[[id]]
    chld <- setdiff(allchld, deleted)
    nc <- length(chld)
    # if nothing left, then nothing to do
    if (nc == 2) {
      return()
    }
    ctypes <- json$type[chld]
    todel <- rep(FALSE, length(chld))
    # this is hard to write in a vectorized form, it is easier iteratively
    # leading commas
    for (i in seq_along(todel)[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # trailing commas
    for (i in rev(seq_along(todel))[-1]) {
      if (ctypes[i] == ',') {
        todel[i] <- TRUE
      } else {
        break
      }
    }
    # duplicate commas
    todel[ctypes[-nc] == ',' & ctypes[-1] == ','] <- TRUE
    chdel <- chld[which(todel)]
    deleted <<- c(deleted, chdel)

    # if the last element is deleted, take the trailing whitespace of
    # its last token and add it to the last token of the last kept element
    chdeld <- intersect(allchld, deleted)
    chkept <- setdiff(allchld, chdeld)
    last_deld <- chdeld[length(chdeld)]
    last_kept <- chkept[length(chkept) - 1L]
    if (last_deld > last_kept) {
      while (is.na(json$code[last_deld])) {
        last_deld <- tail(json$children[[last_deld]], 1)
      }
      while (is.na(json$code[last_kept])) {
        last_kept <- tail(json$children[[last_kept]], 1)
      }
      json$tws[last_kept] <<- json$tws[last_deld]
    }
  }

  # trim commas from arrays
  trimmed_arrays <- setdiff(trimmed_arrays, deleted)
  for (arr in trimmed_arrays) {
    trim_commas(arr, "[", "]")
  }

  # trim pairs and commas from objects
  trimmed_objects <- setdiff(trimmed_objects, deleted)
  for (obj in trimmed_objects) {
    trim_commas(obj, "{", "}")
  }

  json2 <- json[-deleted, ]

  # update text
  parts <- c(rbind(json2$code, json2$tws))
  text <- unlist(lapply(na_omit(parts), charToRaw))

  # TODO: update coordinates without reparsing
  new <- load_json(text = text)
  attr(new, "file") <- attr(json, "file")

  new
}
