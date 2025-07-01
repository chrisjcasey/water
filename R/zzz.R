.onLoad <- function(libname, pkgname) {
  pkg_env <- parent.env(environment())
  cache_path <- file.path(tempdir(), "data.RData")

  message("📦 [water] .onLoad triggered")

  if (!file.exists(cache_path)) {
    message("🔁 No cache file found — attempting to run CacheData()")

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
    tmpenv <- new.env()
    load(cache_path, envir = tmpenv)
    for (obj in ls(tmpenv)) {
      assign(obj, get(obj, envir = tmpenv), envir = pkg_env)
    }
    message("✅ Cached data loaded into package namespace.")
  } else {
    message("⚠️ No data.RData loaded — working without cache.")
  }
}
