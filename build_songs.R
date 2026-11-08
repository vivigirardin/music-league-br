# Builds songs.js for the GitHub page from the Music League exports.
# Put each league's export .zip (or its extracted folder) in data/, named with
# the number of its month, e.g. data/1.zip (janeiro), data/2.zip (fevereiro).
# The page shows the month name in Portuguese.
# Usage (from this folder): Rscript build_songs.R [data-folder]

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
data_path <- if (length(args) > 0) args[1] else "data"
# A .js file (not .json) so the page also works when opened from disk (file://)
out_file <- "songs.js"

exports <- list.files(data_path, full.names = TRUE)
exports <- exports[dir.exists(exports) | grepl("\\.zip$", tolower(exports))]
if (length(exports) == 0) stop("No .zip files or folders found in ", normalizePath(data_path))

meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho",
           "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
month_of <- function(path) {
  m <- as.integer(regmatches(basename(path), regexpr("\\d+", basename(path))))
  if (length(m) == 0 || m < 1 || m > 12) stop("No month number (1-12) in the name of ", path)
  m
}
months <- vapply(exports, month_of, integer(1), USE.NAMES = FALSE)
if (anyDuplicated(months)) stop("Two exports have the same month: ", meses[months[duplicated(months)]])
exports <- exports[order(months)]
months <- sort(months)

# Unzip each export to a temp folder and return the folder holding submissions.csv
export_folder <- function(x) {
  if (!dir.exists(x)) {
    dest <- tempfile("league")
    unzip(x, exdir = dest)
    x <- dest
  }
  csv <- list.files(x, pattern = "^submissions\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(csv) == 0) stop("No submissions.csv in ", x)
  dirname(csv[1])
}

read_league <- function(path) {
  folder <- export_folder(path)
  subs <- read.csv(file.path(folder, "submissions.csv"), check.names = FALSE, encoding = "UTF-8")
  rounds <- read.csv(file.path(folder, "rounds.csv"), check.names = FALSE, encoding = "UTF-8")
  merged <- merge(subs, rounds[, c("ID", "Name")], by.x = "Round ID", by.y = "ID", all.x = TRUE)
  list(
    rounds = setNames(as.list(rounds[["Description"]]), rounds[["Name"]]),
    subs = data.frame(
      uri = merged[["Spotify URI"]],
      title = merged[["Title"]],
      artist = merged[["Artist(s)"]],
      album = merged[["Album"]],
      round = merged[["Name"]],
      date = substr(merged[["Created"]], 1, 10),
      stringsAsFactors = FALSE
    )
  )
}

leagues <- lapply(exports, read_league)

# Leagues are referred to by their position (1 = earliest month); the page looks up the title
league_titles <- meses[months]
round_info <- setNames(lapply(leagues, `[[`, "rounds"), seq_along(leagues))
all_subs <- do.call(rbind, lapply(seq_along(leagues), function(i)
  cbind(leagues[[i]]$subs, season = i)))

# Same song on a different release (remaster, live album...) gets a different URI,
# so group on a normalised "title + first artist" key instead.
normalise <- function(x) {
  x <- tolower(x)
  x <- sub("\\s+[-(\\[].*(remaster|version|edit|live|mono|stereo|mix|feat).*$", "", x)
  trimws(gsub("[^[:alnum:] ]", "", x))
}
first_artist <- sapply(strsplit(all_subs$artist, ","), `[`, 1)
all_subs$key <- paste(normalise(all_subs$title), normalise(first_artist), sep = "|")
all_subs <- all_subs[order(all_subs$season, all_subs$date), ]

songs <- lapply(split(all_subs, all_subs$key), function(d) {
  list(
    title = d$title[1],
    artist = d$artist[1],
    album = d$album[1],
    spotify = sub("spotify:track:", "", d$uri[1]),
    posts = lapply(seq_len(nrow(d)), function(i) list(season = d$season[i], round = d$round[i], date = d$date[i]))
  )
})
songs <- unname(songs[order(tolower(sapply(songs, `[[`, "artist")))])

json <- toJSON(
  list(updated = format(Sys.Date()), leagues = as.list(league_titles),
       rounds = round_info, songs = songs),
  auto_unbox = TRUE
)
writeLines(paste0("window.SONG_DATA = ", json, ";"), out_file, useBytes = TRUE)
cat("Wrote", length(songs), "songs from", nrow(all_subs), "submissions to", out_file, "\n")
