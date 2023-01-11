#' @title Get previously simulated data
#' @description Retrieves previously simulated data stored in csv file.
#'
#' @param file_name String argument. Name of file.
#'
#' @return Return a \code{tibble}.
#' @importFrom readr read_csv
#' @export
#' @examples
#' # extdata is the name of the folder the file in.
#' path = system.file("extdata", "pre_sim_data.csv", package = "Karadoller2020")
#' get_simulation(path)



get_csv = function(file_name) {
  path = system.file("extdata", file_name, package = "Karadoller2020")
  csv <- suppressMessages(read_csv(path))
  return(csv)

}
