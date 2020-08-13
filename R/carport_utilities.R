# ==============================================================================
# cp_pull_loadcells
# ==============================================================================

#' Download carport load cell data
#'
#' copies carport load cell data files as is from UFZ SFTP server to local directory.
#'
#' This function downloads all (or only new) available load cell data files from
#' the Revolution Pi running at the UBZ carport green roof. To run the function,
#' you need a valid private SSH key to authenticate yourself on the FTP server.
#' The data files are saved in a local directory. Note, that this function does
#' not import any data into your R environment, please use other function in the
#' \emph{carport utility functions} group to import the data into your R
#' environment.
#'
#' @export
#'
#' @family carport utility functions
#'
#' @param save_to Path to the folder where the data should be saved (also used
#'   to check existing vs. new data on the server).
#' @param ssh_key Path to private SSH key for authentication (.ppk file).
#' @return Data files in CSV format in the specified folder and a summary message.

cp_pull_loadcells <-
    function (save_to, ssh_key) {

        # DEFINITIONS AND ERRORS
        #============================================

        # general definitions
        url = "sftp://files.ufz.de/ubz-carport-gruendach/carport_messungen/"
        username = "ubz-carport-gruendach"

        # save_to is provided
        if (missing(save_to)) {
            stop ("Parameter 'save_to' is not supplied.")
        }
        # save_to is character
        if (!is.character(save_to)) {
            stop ("Parameter 'save_to' must be a valid path name. Please supply a character string.")
        }
        # clean save_to
        if (str_starts(save_to, "/")) {
            save_to = str_replace(save_to, "/", "")
        }
        if (!str_ends(save_to, "/")) {
            save_to = paste0(save_to, "/", "")
        }

        # ssh_key is provided
        if (missing(ssh_key)) {
            stop ("Parameter 'ssh_key' is not supplied.")
        }
        # ssh_key is character
        if (!is.character(ssh_key)) {
            stop ("Parameter 'ssh_key' must be a valid path name. Please supply a character string.")
        }
        # clean ssh_key
        if (str_starts(ssh_key, "/")) {
            ssh_key = str_replace(ssh_key, "/", "")
        }
        # ssh_key ends with .ppk
        if (!endsWith(ssh_key, ".ppk")) {
            stop ("Parameter 'ssh_key' must be a valid private SSH key file. Please refer to a file with extension '.ppk'.")
        }
        # ssh_key file exists
        if (!file.exists(ssh_key)) {
            stop ("Parameter 'ssh_key' does not refer to an existing file.")
        }
        # ssh_key is valid
        if (!ubzutils:::key_valid(url = url,
                                  username = username,
                                  ssh_key = ssh_key)) {
            stop ("Provided 'ssh_key' is not valid for authentication on the remote server.")
        }

        # create save_to folder
        dir.create(file.path(save_to))
        #
        # FUNCTION
        #=============================================
        #
        #
        # define which data files to download
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        # get file names from local directory
        filenames_local =
            list.files(save_to) %>%
            # filter for hourly data files
            str_subset(pattern = "h.csv")
        #
        message("* ", length(filenames_local), " existing file(s) detected in local folder.")
        #
        # get file names from remote directory
        filenames_remote =
            # authenticate with SSH private key on UFZ FTP server
            # and read out available file names
            getURL(url = url,
                   username = username,
                   keypasswd = "",
                   dirlistonly = TRUE,
                   verbose = TRUE,
                   ssh.private.keyfile = ssh_key) %>%
            # format list into dataframe
            strsplit(., '\n') %>%
            unlist()  %>%
            # filter for hourly data files
            str_subset(pattern = "h.csv")
        #
        # subset list of files to download
        filenames_download =
            setdiff(filenames_remote, filenames_local)
        #
        message("* ", length(filenames_download), " new file(s) detected on remote server.", "\n")
        #
        # download files
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        if (length(filenames_download) != 0) {

            # extract header information from the first file in the list
            header =
                getURL(url = paste0(url, filenames_download[[1]]),
                       username = username,
                       keypasswd = "",
                       dirlistonly = TRUE,
                       verbose = TRUE,
                       ssh.private.keyfile = ssh_key) %>%
                strsplit(., '\n') %>%
                unlist() %>%
                read_delim(delim = ";", n_max = 0) %>%
                names() %>%
                str_trim()

            filenames_download %>%
                # iterate over all file names and download the data
                # plyr:llply() is similar to lapply() but enables progress bars
                plyr::llply(function (x) {
                    getURL(url = paste0(url, x),
                           username = username,
                           keypasswd = "",
                           dirlistonly = TRUE,
                           verbose = TRUE,
                           ssh.private.keyfile = ssh_key) %>%
                        strsplit(., '\n') %>%
                        unlist() %>%
                        tibble() %>%
                        # TODO remove line below
                        dplyr::rename(content = ".") %>%
                        separate(content,
                                 into = c(header),
                                 sep = ";") %>%
                        write_csv(path = paste0(save_to, x),
                                  col_names = FALSE)
                },
                .progress = "text"
                )

        }
        #
        # print summary
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        summary_status =
            paste0("Summary: ",
                   length(filenames_local), " existing, ",
                   length(filenames_download), " new, ",
                   length(list.files(save_path, pattern = "h.csv")), " total data files in local folder.")
        #
        message("* ", summary_status)
        #
        # return first argument invisibly (for pipes)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        invisible(save_to)
        #
        #=============================================
        # END
    }

# ==============================================================================


# ==============================================================================
# cp_tidy_loadcells
# ==============================================================================

#' Cleans local load cell data files
#'
#' cleans load cell data files available in local folder.
#'
#' This function loads carport load cell data files available in a specified
#' local folder, cleans the data, converts them into tidy format and saves an
#' uncompressed .rds file containing all observations. Note, that this function
#' does not import any data into your R environment, please use other function
#' in the \emph{carport utility functions} group to import the data into your R
#' environment.
#'
#' @export
#'
#' @family carport utility functions
#'
#' @param load_from Path to the local folder where the data is stored.
#' @return .RData file in specified folder containing all data in tidy format.

cp_tidy_loadcells <-
    function (load_from) {

        # DEFINITIONS AND ERRORS
        #============================================

        # general definitions
        export_name = "carport_loadcell.rds"

        # load_from is provided
        if (missing(load_from)) {
            stop ("Parameter 'load_from' is not supplied.")
        }
        # load_from is character
        if (!is.character(load_from)) {
            stop ("Parameter 'load_from' must be a valid path name. Please supply a character string.")
        }
        # clean load_from
        if (str_starts(load_from, "/")) {
            load_from = str_replace(load_from, "/", "")
        }
        if (!str_ends(load_from, "/")) {
            load_from = paste0(load_from, "/", "")
        }
        # load_from is existing folder
        if (!dir.exists(load_from)) {
            stop ("Folder 'load_from' does not exist.")
        }

        # FUNCTION
        #=============================================
        #
        message("* ", "Cleaning data files...", "\n")

        list.files(path = load_from,
                   pattern = 'h.csv',
                   full.names = TRUE) %>%
            plyr::llply(function(x) {
                read_csv(x,
                         col_types = cols("DATETIME" = col_character(),
                                          "GROSS WEITH" = col_double(),
                                          "NET WEIGHT" = col_double(),
                                          "CELL 1" = col_double(),
                                          "CELL 2" = col_double(),
                                          "CELL 3" = col_double(),
                                          "CELL 4" = col_double(),
                                          "CELL 5" = col_double(),
                                          "CELL 6" = col_double()))
            },
            .progress = "text"
            ) %>%
            bind_rows() %>%
            dplyr::rename("datetime" = "DATETIME",
                          "tot_weight" = "GROSS WEITH",
                          "net_weight" = "NET WEIGHT",
                          "cell_1" = "CELL 1",
                          "cell_2" = "CELL 2",
                          "cell_3" = "CELL 3",
                          "cell_4" = "CELL 4",
                          "cell_5" = "CELL 5",
                          "cell_6" = "CELL 6") %>%
            # add leading zeros to datetime
            separate(datetime,
                     into = c("ymd", "HMS"),
                     sep = " ") %>%
            separate(ymd,
                     into = c("y", "m", "d"),
                     sep = "-") %>%
            separate(HMS,
                     into = c("H", "M", "S"),
                     sep = ":") %>%
            mutate(m = str_pad(m, 2, pad = "0"),
                   d = str_pad(d, 2, pad = "0"),
                   H = str_pad(H, 2, pad = "0"),
                   M = str_pad(M, 2, pad = "0"),
                   S = str_pad(S, 6, pad = "0")) %>%
            unite(ymd,
                  y, m, d,
                  sep = "-",
                  remove = TRUE) %>%
            unite(HMS,
                  H, M, S,
                  sep = ":",
                  remove = TRUE) %>%
            unite(datetime,
                  ymd, HMS,
                  sep = " ",
                  remove = TRUE) %>%
            # parse the correct column types
            mutate(datetime = as.POSIXct(datetime)) %>%
            # remove duplicates
            distinct() %>%
            # adjust time stamp from RevPi clock
            mutate(datetime = as_datetime(ifelse(datetime < "2020-08-05 11:09:00",
                                                 datetime + (as_datetime("2020-08-05 11:09:00") - as_datetime("2020-06-04 08:13:00")),
                                                 datetime))) %>%
            write_rds(path = paste0(load_from, export_name))

        message("* ", "Done.", "\n")


        # return first argument invisibly (for pipes)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #
        invisible(load_from)
        #
        #=============================================
        # END
    }

# ==============================================================================
