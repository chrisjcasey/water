.onLoad <- function(libname, pkgname) {
  pkg_env = parent.env(environment())
  cache_path = "c:/Code/assets/Volatile/data.RData"

  message("📦 [water] .onLoad triggered")

  refresh_needed = !file.exists(cache_path)

  # Check if cache is stale (i.e., not from today)
  if (!refresh_needed) {
    cache_date = as.Date(file.info(cache_path)$mtime)
    today = Sys.Date()
    if (cache_date < today) {
      message("🔁 Cache file is from a previous day — refreshing.")
      refresh_needed = TRUE
    }
  }

  if (refresh_needed) {
    tryCatch({
      uid = Sys.getenv("WSNZDBUSER")
      pwd = Sys.getenv("WSNZDBPASS")

      if (!nzchar(uid) || !nzchar(pwd)) {
        message("⚠️ Skipping CacheData(): missing WSNZDBUSER or WSNZDBPASS")
      } else {
        CacheData()
      }
    }, error = function(e) {
      warning("❌ CacheData() failed: ", conditionMessage(e))
    })
  }

  if (file.exists(cache_path)) {
    tmpenv = new.env()
    load(cache_path, envir = tmpenv)
    for (obj in ls(tmpenv)) {
      assign(obj, get(obj, envir = tmpenv), envir = pkg_env)
    }
    message("✅ Cached data loaded into package namespace.")
  } else {
    message("⚠️ No data.RData loaded — working without cache.")
  }
}
