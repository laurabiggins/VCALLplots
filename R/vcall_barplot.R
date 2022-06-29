library(magrittr)

ighv_order <- c("IGHV5-1","IGHV5-2","IGHV2-2","IGHV5-4", "IGHV2-3","IGHV5-6","IGHV2-4","IGHV5-9","IGHV2-5","IGHV5-12","IGHV2-6","IGHV5-9-1","IGHV2-9-1","IGHV2-6-8","IGHV5-15","IGHV5-16","IGHV5-17","IGHV2-9","IGHV7-1","IGHV14-1","IGHV4-1","IGHV3-1","IGHV11-1","IGHV14-2","IGHV4-2","IGHV11-2","IGHV14-3","IGHV16-1","IGHV9-1","IGHV12-1","IGHV9-2","IGHV9-3","IGHV7-3","IGHV14-4","IGHV3-3","IGHV7-4","IGHV3-4","IGHV3-5","IGHV3-6","IGHV9-4","IGHV5-21","IGHV3-8","IGHV13-2","IGHV12-3","IGHV6-3","IGHV6-4","IGHV6-5","IGHV6-6","IGHV6-7","IGHV8-2","IGHV1-2","IGHV10-1","IGHV1-4","IGHV1-5","IGHV10-3","IGHV1-7","IGHV15-2","IGHV1-9","IGHV1-11","IGHV1-12","IGHV1-13","IGHV1-14","IGHV1-15","IGHV1-16","IGHV1-18","IGHV1-19","IGHV1-20","IGHV1-21-1","IGHV1-21","IGHV1-22","IGHV1-23","IGHV1-25","IGHV1-26","IGHV1-28","IGHV1-31","IGHV1-32","IGHV1-34","IGHV1-36","IGHV1-37","IGHV1-39","IGHV1-42","IGHV1-43","IGHV1-46","IGHV1-47","IGHV8-4","IGHV1-48","IGHV1-49","IGHV8-5","IGHV1-50","IGHV1-52","IGHV1-53","IGHV8-6","IGHV1-54","IGHV1-55","IGHV1-56","IGHV8-7","IGHV8-8","IGHV1-58","IGHV1-59","IGHV1-61","IGHV1-62","IGHV1-62-2","IGHV1-62-3","IGHV8-9","IGHV1-63","IGHV1-64","IGHV8-11","IGHV1-66","IGHV1-67","IGHV1-69","IGHV8-12","IGHV1-70","IGHV1-72","IGHV8-13","IGHV1-74","IGHV1-75","IGHV1-76","IGHV1-77","IGHV1-78","IGHV1-79","IGHV1-80","IGHV1-81","IGHV1-82","IGHV1-83","IGHV1-84","IGHV1-85","IGHV1-86")

#' process_vcalls
#'
#' Counts occurrences of a V_CALLs in a tibble. Returns a tibble of raw counts and percentages
#'
#' @param vcall_tbl tibble of data
#' @param vcall_col name of the column that contains the v calls [default: "V_CALL"]
#'
#' @return tibble of raw counts and percentages
#' @export
#'
#' @examples
#' processed_vcalls <- process_vcalls(vcall_eg)
#'
process_vcalls <- function(vcall_tbl, vcall_col = "V_CALL"){
  VCALL <- raw_count <- NULL # to remove notes re.no visible binding for global variable

  ighvs_present <- ighv_order[ighv_order %in% unique(vcall_tbl[[vcall_col]])]

  vcall_tbl |>
    dplyr::select({{vcall_col}}) |>
    dplyr::rename(VCALL = {{vcall_col}}) |>
    dplyr::mutate(VCALL = forcats::as_factor(VCALL)) |>
    dplyr::count(VCALL, name="raw_count") |>
    dplyr::mutate(VCALL = forcats::fct_relevel(VCALL, ighvs_present)) |>
    dplyr::arrange(VCALL) |>
    dplyr::mutate(percentage = (raw_count/sum(raw_count))*100)
}

#' vcall_barplot
#'
#' @param vcall_data output from process_vcalls
#' @param x_val what to plot on x axis, "VCALL" [default]
#' @param y_val what to plot on y axis, "percentage" [default] or "raw_count"
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' processed_vcalls <- process_vcalls(vcall_eg)
#' vcall_barplot(processed_vcalls)
vcall_barplot <- function(vcall_data, x_val="VCALL", y_val="percentage"){
  vcall_data |>
    ggplot2::ggplot(ggplot2::aes(x=.data[[x_val]], y=.data[[y_val]])) +
      ggplot2::geom_col(col = "black", fill = "#7AB8A8", width = 0.7) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size =5, vjust = 0)) +
      ggplot2::xlab("")
}

# -------------------------------------------------------------------------

# There may be multiple replicates to show on one plot.
# This will need to be updated to deal with this.

# ds2 <- readr::read_tsv("D:/Harry_White/umtboth.txt")
#
# ds2_vcall <- process_vcalls(ds2)
# vcall_barplot(ds2_vcall)
# vcall_barplot(ds2_vcall, y_val="n")
#
# vcall_eg <- head(ds2, n= 2000)
# usethis::use_data(vcall_eg)



