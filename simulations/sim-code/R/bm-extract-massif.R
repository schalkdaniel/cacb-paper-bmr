#' Extract data from valgrinds massif file
#'
#' @param ms_path [character(1L)] Path to the massif file
#' @return data.frame with columns `snapshot`, `time`, `mem_heap_B`, `unit`, and `uses_compboost`
extractMassifData = function (ms_path)
{
  ms_lines = readLines(con = file(ms_path))
  idx_snapshots = grep(pattern = "snapshot", x = ms_lines)
  idx_compboost = grep(pattern = "compboost", x = ms_lines)

  # Check if snapshot contains a call to compboost:
  uses_compboost = logical(length(idx_snapshots))
  for (i in seq_along(idx_snapshots)) {
    lower = idx_snapshots[i]
    if (i < length(idx_snapshots)) {
      upper = idx_snapshots[i + 1]
    } else {
      upper = length(ms_lines)
    }
    uses_compboost[i] = any ((idx_compboost > lower) & (idx_compboost < upper))
  }

  ms_extract = data.frame(
    snapshot = unlist(lapply(strsplit(ms_lines[idx_snapshots], split = "="), function (x) as.numeric(as.character(x[2])))),
    time = unlist(lapply(strsplit(ms_lines[idx_snapshots + 2], split = "="), function (x) as.numeric(as.character(x[2])))),
    mem_heap_B = unlist(lapply(strsplit(ms_lines[idx_snapshots + 3], split = "="), function (x) as.numeric(as.character(x[2])))) / 1024^2,
    unit = "MB",
    uses_compboost = uses_compboost
  )
  lapply(strsplit(ms_lines[idx_snapshots + 2], split = "="), function (x) as.numeric(as.character(x[2])))
  as.integer(strsplit(x = ms_lines[idx_snapshots + 2], split = "=")[[1]][2])

  return (ms_extract)
}
