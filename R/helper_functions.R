# ==============================================================================
# is.error
# ==============================================================================
#' Check if an expression returns an error
#'
#' Does a given expression return an error?
#'
#' Useful for tests where you want to make sure your function throws an error.
#' This function was imported from the berryFunctions package.
#'
#' @return TRUE/FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{stop}}, \code{\link{try}}, \code{\link{inherits}}
#' @export
#' @family helper functions
#' @examples
#' is.error(  log(3)              )
#' is.error(  log("a")            )
#' is.error(  log(3),   tell=TRUE )
#' is.error(  log("a"), tell=TRUE )
#' stopifnot( is.error( log("a")  )  ) # or shorter:
#' is.error(  log("a"), force=TRUE)
#' # is.error(  log(3),   force=TRUE)
#' stopifnot(is.error(  is.error(log(3), force=TRUE)  ))
#'
#' @param expr Expression to be tested for returning an error
#' @param tell Logical: Should the error message be printed via \code{\link{message}}? DEFAULT: FALSE
#' @param force Logical: Should an error be returned if the expression is not an error? DEFAULT: FALSE

is.error <-
    function(expr,
             tell = FALSE,
             force = FALSE) {

    expr_name <- deparse(substitute(expr))
    test <- try(expr, silent = TRUE)
    iserror <- inherits(test, "try-error")
    if(tell) if(iserror) message("Note in is.error: ", test)
    if(force) if(!iserror) stop(expr_name, " is not returning an error.", call.=FALSE)
    # output:
    iserror

    }

# ==============================================================================


# ==============================================================================
# key_valid
# ==============================================================================

#' Check if a provided SSH key is valid
#'
#' checks if a provided SSH key is a valid for authentication on the SFTP server.
#'
#' This helper function checks if a provided private SSH key is a valid for
#' authentication on the SFTP server by trying to read out the "user" folder
#' file list.
#'
#' @family helper functions
#'
#' @param ssh_key Path to private SSH key for authentication (.ppk file).
#' @param url a string giving the URL
#' @param username a string giving the user name for authentication (equals the
#'   folder to be read out on the FTP server).
#' @return TRUE if authentication successful; FALSE if not.

key_valid <-
    function (ssh_key, url, username) {

        # ERRORS
        #============================================
        #
        # ssh_key is provided
        if (missing(ssh_key)) stop ("Parameter 'ssh_key' is not supplied.")
        # ssh_key is character
        if (!is.character(ssh_key)) stop ("Parameter 'ssh_key' must be a valid path name. Please supply a character string.")
        # ssh_key ends with .ppk
        if (!endsWith(ssh_key, ".ppk")) stop ("Parameter 'ssh_key' must be a valid private SSH key file. Please refer to a file with extension '.ppk'.")
        #
        # url is provided
        if (missing(url)) stop ("Parameter 'url' is not supplied.")
        # url is character
        if (!is.character(url)) stop ("Parameter 'url' must be a valid path name. Please supply a character string.")
        #
        # username is provided
        if (missing(username)) stop ("Parameter 'username' is not supplied.")
        # username is character
        if (!is.character(username)) stop ("Parameter 'username' must be a valid path name. Please supply a character string.")
        #
        # FUNCTION
        #===========================================
        !is.error(RCurl::getURL(url = url,
                               username = username,
                               keypasswd = "",
                               dirlistonly = TRUE,
                               verbose = TRUE,
                               ssh.private.keyfile = ssh_key))
    }

# ==============================================================================




