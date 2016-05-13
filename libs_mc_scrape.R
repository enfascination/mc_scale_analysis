### from https://github.com/hadley/dplyr/blob/master/R/utils.r
"%||%" <- function(x, y) if(is.null(x)) y else x
in_travis <- function() identical(Sys.getenv("TRAVIS"), "true")
### from https://github.com/hadley/dplyr/blob/cc2cb7b53b23d66e7dfe811777d079cee26d5197/R/dbi-s3.r
db_disconnector <- function(con, name, quiet = FALSE) {
    reg.finalizer(environment(), function(...) {
                      if (!quiet) {
                              message("Auto-disconnecting ", name, " connection ",
                                              "(", paste(con@Id, collapse = ", "), ")")
                          }
                          dbDisconnect(con)
                        })
  environment()
}
src_postgres_mod <- function (dbname = NULL, host = NULL, port = NULL, user = NULL, 
              password = NULL, con=NULL, ...) 
{
      if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
                stop("RPostgreSQL package required to connect to postgres db", 
                                 call. = FALSE)
    }
    user <- user %||% if (in_travis()) 
              "postgres"
        else ""
        if (is.null(con)) {
            con <- dbConnect(RPostgreSQL::PostgreSQL(), host = host %||% 
                                     "", dbname = dbname %||% "", user = user, password = password %||% 
                                             "", port = port %||% "", ...)
        }
            info <- dbGetInfo(con)
                src_sql("postgres", con, info = info, disco = db_disconnector(con, 
                                                                                      "postgres"))
}
 
