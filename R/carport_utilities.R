#' Download carport load cell data
#'
#' copies carport load cell data files from UFZ SFTP server to local directory.
#'
#' This function downloads all (or only new) available load cell data files from
#' the Revolution Pi running at the UBZ carport green roof. To run the function,
#' you need a valid private SSH key to authenticate yourself on the FTP server.
#' The data files are saved in a local directory. Note, that this function does
#' export any data into your R environment, please use other function in the
#' \emph{carport utility functions} group to import the data into your R
#' environment.
#'
#' @export
#'
#' @family carport utility functions
#' @seealso \code{\link{test}} for test, \code{\link{test}} for test.
#'
#' @param save_to Path to the folder where the data should be saved (also used
#'   to check existing vs. new data on the server).
#' @param ssh_key Path to private SSH key for authentication (.ppk file).
#' @param only_new Logical. TRUE downloads only new data from the server
#'   (default), FALSE downloads all available data from the server.
#' @return Data files in CSV format in the specified folder and a summary message.

cplc_pull <-
    function (save_to, ssh_key, only_new = TRUE) {

    # ERRORS
    #============================================
    #
    # save_to is provided
    if (missing(save_to)) stop ("Parameter 'save_to' is not supplied.")
    # save_to is character
    if (!is.character(save_to)) stop ("Parameter 'save_to' must be a valid path name. Please supply a character string.")
    #
    # ssh_key is provided
    if (missing(ssh_key)) stop ("Parameter 'ssh_key' is not supplied.")
    # ssh_key is character
    if (!is.character(ssh_key)) stop ("Parameter 'ssh_key' must be a valid path name. Please supply a character string.")
    # ssh_key ends with .ppk
    if (!endsWith(ssh_key, ".ppk")) stop ("Parameter 'ssh_key' must be a valid private SSH key file. Please refer to a file with extension '.ppk'.")
    #
    # only_new is provided
    if (missing(only_new)) stop ("Parameter 'only_new' is not supplied.")
    # only_new is logical
    if (!is.logical(only_new)) stop ("Parameter 'only_new' must be logical.")
    #
    # FUNCTION
    #=============================================
    #
    # get absolute paths
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    # concatenate absolute path for saving directory
    save_path =
        # if path is absolute
        if (stringr::str_detect(save_to, ":/")) {
            save_to
            # if path is relative
        } else {
            paste0(getwd(), save_to)
        }
    #
    # test if path is valid exists
    # TODO
    #
    # create directory # TODO write own message here if directory already exists
    dir.create(file.path(save_path))
    #
    # concatenate absolute path for ssh key file
    ssh_path =
        # if path is absolute
        if (stringr::str_detect(ssh_key, ":/")) {
            save_to
            # if path is relative
        } else {
            paste0(getwd(), ssh_key)
        }
    #
    # test if file exists
    if (!file.exists(ssh_path)) stop ("Parameter 'ssh_key' does not refer to an existing file.")
    # test if ssh_key is valid
    # TODO
    #
    # define which data files to download
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    message("* Checking local folder...")
    #
    # get file names from local directory
    filenames_local =
        list.files(save_path) %>%
        # filter for hourly data files
        stringr::str_subset(pattern = "h.csv")
    #
    message("*     ", length(filenames_local), " existing file(s) detected in local folder.")
    #
    # process local filenames to match remote structure
    # currently, file names on remote have no leading zeros for month and day
    filenames_local_for_remote =
        filenames_local %>%
        stringr::str_replace_all(pattern = "_00_", replacement = "_0_") %>%
        stringr::str_replace_all(pattern = "_01_", replacement = "_1_") %>%
        stringr::str_replace_all(pattern = "_02_", replacement = "_2_") %>%
        stringr::str_replace_all(pattern = "_03_", replacement = "_3_") %>%
        stringr::str_replace_all(pattern = "_04_", replacement = "_4_") %>%
        stringr::str_replace_all(pattern = "_05_", replacement = "_5_") %>%
        stringr::str_replace_all(pattern = "_06_", replacement = "_6_") %>%
        stringr::str_replace_all(pattern = "_07_", replacement = "_7_") %>%
        stringr::str_replace_all(pattern = "_08_", replacement = "_8_") %>%
        stringr::str_replace_all(pattern = "_09_", replacement = "_9_")
    #
    message("* Checking remote server...") #TODO add error message here if authentication fails
    #
    # get file names from remote directory
    filenames_remote =
        # authenticate with SSH private key on UFZ FTP server
        # and read out available file names
        RCurl::getURL(url = "sftp://files.ufz.de/ubz-carport-gruendach/carport_messungen/",
               username = "ubz-carport-gruendach",
               keypasswd = "",
               dirlistonly = TRUE,
               verbose = TRUE,
               ssh.private.keyfile = ssh_path) %>%
        # format list into dataframe
        strsplit(., '\n') %>%
        unlist()  %>%
        # filter for hourly data files
        stringr::str_subset(pattern = "h.csv")
    #
    # subset list of files to download
    filenames_download =
        # if all data should be downloaded
        if (only_new == FALSE) {
            filenames_remote
            # if only new data should be downloaded
        } else {
            setdiff(filenames_remote, filenames_local_for_remote)
        }
    #
    message("*     ", length(filenames_download), " new file(s) detected on remote server for downloading.", "\n")
    #
    # download files
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    message("* Downloading file(s) from remote server...")
    #
    data_new_raw =
        filenames_download %>%
        # iterate over all file names and download the data
        # plyr:plyr::llply() is similar to lapply() but enables progress bars
        plyr::llply(function (x) {
            RCurl::getURL(url = paste0("sftp://files.ufz.de/ubz-carport-gruendach/carport_messungen/", x),
                   username = "ubz-carport-gruendach",
                   keypasswd = "",
                   dirlistonly = TRUE,
                   verbose = TRUE,
                   ssh.private.keyfile = ssh_path) %>%
                strsplit(., '\n') %>%
                unlist() %>%
                tibble::tibble() %>%
                dplyr::rename(content = ".") %>%
                dplyr::mutate(origin = x)
        },
        .progress = "text"
        )
    #
    message("*     ", length(data_new_raw), " file(s) successfully downloaded.", "\n")
    #TODO: add control that length(data_new_raw) == length(filenames_download)
    #TODO: add progress bar
    #
    #
    # clean data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    data_new_clean =
        data_new_raw %>%
        # iterate over all list entries (i.e. data files) and clean
        lapply(function (x)
            x %>%
                tidyr::separate(content,
                         into = c("datetime", "tot_weight", "net_weight", "cell_1", "cell_2", "cell_3", "cell_4", "cell_5", "cell_6"),
                         sep = ";") %>%
                # remove old header
                dplyr::filter(datetime != "DATETIME") %>%
                # add leading zeros to datetime
                tidyr::separate(datetime,
                         into = c("ymd", "HMS"),
                         sep = " ") %>%
                tidyr::separate(ymd,
                         into = c("y", "m", "d"),
                         sep = "-") %>%
                tidyr::separate(HMS,
                         into = c("H", "M", "S"),
                         sep = ":") %>%
                dplyr::mutate(m = stringr::str_pad(m, 2, pad = "0"),
                       d = stringr::str_pad(d, 2, pad = "0"),
                       H = stringr::str_pad(H, 2, pad = "0"),
                       M = stringr::str_pad(M, 2, pad = "0"),
                       S = stringr::str_pad(S, 6, pad = "0")) %>%
                tidyr::unite(ymd,
                      y, m, d,
                      sep = "-",
                      remove = TRUE) %>%
                tidyr::unite(HMS,
                      H, M, S,
                      sep = ":",
                      remove = TRUE) %>%
                tidyr::unite(datetime,
                      ymd, HMS,
                      sep = " ",
                      remove = TRUE)  %>%
                # add leading zeros to origin
                tidyr::separate(origin,
                         into = c("y", "m", "d", "H"),
                         sep = "_") %>%
                dplyr::mutate(m = stringr::str_pad(m, 2, pad = "0"),
                       d = stringr::str_pad(d, 2, pad = "0")) %>%
                tidyr::unite(origin,
                      y, m, d, H,
                      sep = "_",
                      remove = TRUE) %>%
                # parse the correct column types
                dplyr::mutate(datetime = as.POSIXct(datetime),
                       datetime = ymd_hms(datetime),
                       tot_weight = as.numeric(tot_weight),
                       net_weight  = as.numeric(net_weight ),
                       cell_1  = as.numeric(cell_1),
                       cell_2  = as.numeric(cell_2),
                       cell_3  = as.numeric(cell_3),
                       cell_4  = as.numeric(cell_4),
                       cell_5  = as.numeric(cell_5),
                       cell_6  = as.numeric(cell_6))
        )
    #
    # save files
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    # TODO delete duplicate files first if all data is downloaded
    # TODO use save_path instead of save_to
    #
    message("* Saving data files in local folder...")
    #
    data_new_clean %>%
        plyr::llply(function (x) {
            readr::write_csv(x %>%
                          select(-origin),
                      path =
                          # if path is absolute
                          if (stringr::str_detect(save_to, ":/")) {
                              paste0(save_to, dplyr::distinct(x, origin))
                              # if path is relative
                          } else {
                              paste0(getwd(), save_to, dplyr::distinct(x, origin))
                          } )},
            .progress = "text"
        )
    #
    message("*     ", length(data_new_clean), " file(s) successfully saved.", "\n")
    #
    # print exit status
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    exit_status =
        paste0("Summary: ",
               length(filenames_local), " existing, ",
               length(data_new_clean), " new, ",
               length(list.files(save_path, pattern = "h.csv")), " total data files in local folder.")
    #
    message("* ", exit_status)
    #
    # return first argument invisibly (for pipes)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    invisible(save_to)
    #
    #=============================================
    # END
}
################################################################################
