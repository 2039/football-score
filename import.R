makeNamespace <- function(name, version = NULL, lib = NULL) {
    #' what is documentation
    #' copypasted from https://github.com/r-lib/devtools/issues/545
    impenv <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
    attr(impenv, "name") <- paste("imports", name, sep = ":")
    env <- new.env(parent = impenv, hash = TRUE)
    name <- as.character(as.name(name))
    version <- as.character(version)
    info <- new.env(hash = TRUE, parent = baseenv())
    assign(".__NAMESPACE__.", info, envir = env)
    assign("spec", c(name = name, version = version), envir = info)
    setNamespaceInfo(env, "exports", new.env(hash = TRUE, parent = baseenv()))
    dimpenv <- new.env(parent = baseenv(), hash = TRUE)
    attr(dimpenv, "name") <- paste("lazydata", name, sep = ":")
    setNamespaceInfo(env, "lazydata", dimpenv)
    setNamespaceInfo(env, "imports", list(base = TRUE))
    setNamespaceInfo(env, "path", normalizePath(file.path(lib,
        name), "/", TRUE))
    setNamespaceInfo(env, "dynlibs", NULL)
    setNamespaceInfo(env, "S3methods", matrix(NA_character_,
        0L, 3L))
    assign(".__S3MethodsTable__.", new.env(hash = TRUE, parent = baseenv()),
        envir = env)
    .Internal(registerNamespace(name, env))
    env
}

import <- function(imports=NULL, from, namespace) {
    #' @description
    #'    imports variables from the source file into a namespace
    #'    variables defined in the `exports` variable in the source file
    #'    are exported (accessible by ::),
    #'    the rest are internal (accessible by :::)
    #'
    #'    exported variables that are named in the `imports` parameter
    #'    are also exposed to the global environment
    #'
    #' @param imports: the globally imported variables
    #'
    #' @param from: the path to the source file
    #'
    #' @param namespace: the name of the namespace
    #'     will be the path of the source file without the suffix
    #'     (should be just the name, but that will be on the
    #'      never-to-be-fixed-list)

    if(missing(namespace))
        namespace <- strsplit(from, "\\.")[[1]][1]

    ns <- makeNamespace(namespace)
    source(from, local=ns)
    base::namespaceExport(ns, ns$exports)

    for (name in intersect(imports, ns$exports))
        assign(name, ns[[name]], envir=globalenv())
}
