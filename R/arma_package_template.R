arma_package_template <- function(name="anRpackage",
                                           environment=.GlobalEnv,
                                           path=".", force=FALSE,
                                           code_files=character()) {

  env <- parent.frame(1)

  haveKitten <- requireNamespace("pkgKitten", quietly=TRUE)
  skelFunUsed <- ifelse(haveKitten, pkgKitten::kitten, package.skeleton)
  skelFunName <- ifelse(haveKitten, "kitten", "package.skeleton")
  message("\nCalling ", skelFunName, " to create basic package.")

  ## first let the traditional version (or the kitten alternate) do its business
  call <- match.call()
  call[[1]] <- skelFunUsed
  tryCatch(eval(call, envir=env),
           error = function(e) {
             cat(paste(e, "\n")) # print error
             stop(paste("error while calling `", skelFunName, "`", sep=""))
           })

  message("\nAdding RcppArmadillo settings")

  ## now pick things up
  root <- file.path(path, name)

  ## Add Rcpp to the DESCRIPTION
  DESCRIPTION <- file.path(root, "DESCRIPTION")
  if (file.exists(DESCRIPTION)) {
    x <- cbind(read.dcf(DESCRIPTION),
               "Imports" = sprintf("Rcpp (>= %s)", packageDescription("Rcpp")[["Version"]]),
               "LinkingTo" = "Rcpp, RcppArmadillo")
    write.dcf(x, file=DESCRIPTION)
    message(" >> added Imports: Rcpp")
    message(" >> added LinkingTo: Rcpp, RcppArmadillo")
  }

  ## add a useDynLib to NAMESPACE,
  NAMESPACE <- file.path( root, "NAMESPACE")
  lines <- readLines( NAMESPACE )
  if (! grepl("useDynLib", lines)) {
    lines <- c(sprintf("useDynLib(%s, .registration=TRUE)", name),
               "importFrom(Rcpp, evalCpp)",        ## ensures Rcpp instantiation
               lines)
    writeLines(lines, con = NAMESPACE)
    message( " >> added useDynLib and importFrom directives to NAMESPACE")
  }

  ## lay things out in the src directory
  src <- file.path(root, "src")
  if (!file.exists(src)) {
    dir.create(src)
  }
  man <- file.path(root, "man")
  if (!file.exists(man)) {
    dir.create(man)
  }
  skeleton <- system.file("skeleton", package="skeleton")

  for (file_name in c("Makevars.in", "Makevars.win", "configure", "configure.ac", "cleanup",
                      ".travis.yml", "codecov.yml", ".Rbuildignore", "rcpparma_hello_world.cpp")) {
    dest <- file.path(src, file_name)
    if (!file.exists(dest)) {
      file.copy(file.path(skeleton, file_name), dest)
      message(sprintf(" >> added %s file with Rcpp settings", file_name))
    }
  }

  for (file_name in c("configure", "configure.ac", "cleanup",
                      ".travis.yml", "codecov.yml", ".Rbuildignore")) {
    dest <- file.path(root, file_name)
    if (!file.exists(dest)) {
      file.copy(file.path(skeleton, file_name), dest)
      message(sprintf(" >> added %s file with Rcpp settings", file_name))
    }
  }


  inst <- file.path(root, "inst")
  if (!file.exists(inst)) {
    dir.create(inst)
    dir.create(file.path(inst, "include"))
  }
  dest <- file.path(file.path(inst, "include"), "template.h")
  if (!file.exists(dest)) {
    file.copy(file.path(skeleton, "template.h"), dest)
    message(sprintf(" >> added %s file with Rcpp settings", "template.h"))
  }
  file.rename(file.path(inst, "include", "template.h"), file.path(inst, "include", sprintf("%s.h", name)))

  header <- readLines(file.path(root, "configure"))
  header <- gsub("@PKG@", name, header, fixed = TRUE)
  writeLines(header, file.path(root, "configure"))

  header <- readLines(file.path(root, "configure.ac"))
  header <- gsub("@PKG@", name, header, fixed = TRUE)
  writeLines(header, file.path(root, "configure.ac"))

  header <- readLines(file.path(src, "rcpparma_hello_world.cpp"))
  header <- gsub("@PKG@", name, header, fixed = TRUE)
  writeLines(header, file.path(src, "rcpparma_hello_world.cpp"))



  invisible(NULL)
}
