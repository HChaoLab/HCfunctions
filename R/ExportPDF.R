# 提取单个 ggplot 对象的数据（主 data + layers）
.extract_from_ggplot <- function(plot) {
  res <- list()

  # 主数据
  if (!is.null(nrow(plot$data))) {
    res[["main"]] <- plot$data
  }

  # layers 数据
  layer_data <- lapply(seq_along(plot$layers), function(i) {
    lyr <- plot$layers[[i]]
    if (!is.null(nrow(lyr$data))) {
      return(lyr$data)
    } else {
      return(NULL)
    }
  })
  names(layer_data) <- paste0("layer", seq_along(plot$layers))

  res <- c(res, layer_data[!sapply(layer_data, is.null)])
  return(res)
}

# 适配 ggplot 和 patchwork
extract_plot_data <- function(p) {
  if (inherits(p, "ggplot")&!inherits(p, "patchwork")) {
    return(.extract_from_ggplot(p))
  }
  if (inherits(p, "patchwork")) {
    # 获取 patchwork 中的所有 ggplot
    # plots <- patchwork:::plots(plot)
    res <- NULL
    for(i in seq_along(p)){
      res_i <- .extract_from_ggplot(p[[i]])
      names(res_i) <- paste0("plot_", i,'_',names(res_i))
      res <- c(res,res_i)
    }

    return(res)
  } else {
    stop("Input must be a ggplot or patchwork object.")
  }
}
#' ExportPDF_TiffTolocal_withData
#'
#' @param p
#' @param filename
#' @param width_screen
#' @param Heigh_screen
#' @param pig_device
#' @param units
#' @param dpi
#' @param col_names
#' @param row_names
#' @param format_headers
#' @param use_zip64
#'
#' @return
#' @export
#'
#' @examples
ExportPDF_TiffTolocal_withData <- function (p, filename = "Expot", width_screen = 500, Heigh_screen = 500, pig_device='tiff',
                                            units = c("in", "cm", "mm"), dpi = 92, col_names = TRUE, row_names = FALSE,
                                            format_headers = TRUE, use_zip64 = FALSE)
{
  library(shiny)
  library(ggplot2)
  # if('patchwork'%in%class(p)){
  #   data <- list()
  #   for(i in 1:length(p)){
  #     data[[i]] <- p[[i]]$data
  #   }
  #   names(data) <- paste('plot',1:length(p),sep='_')
  #
  # }else{
  #   data <- p$data
  # }
  Heigh_screen = Heigh_screen * dpi/92
  width_screen = width_screen * dpi/92
  data <- extract_plot_data(p)
  runApp(list(ui = fluidPage(downloadButton("foo", label = "PDF Download"),
                             downloadButton("tiff", label = "TIFF Download"),
                             downloadButton("png", label = "PNG Download"),
                             downloadButton("excel", label = "Data Download")), server = function(input,
                                                                                                  output) {
                               suppressMessages(extrafont::loadfonts())
                               width = width_screen/dpi
                               height = Heigh_screen/dpi
                               output$foo = downloadHandler(filename = paste(filename,
                                                                             ".pdf", sep = ""), content = function(file) {
                                                                               ggsave(file, plot = p, device = cairo_pdf, width = width,
                                                                                      height = height, units = units[1])
                                                                             })
                               output$tiff = downloadHandler(filename = paste(filename,
                                                                              paste('.','tiff',sep=''), sep = ""), content = function(file) {
                                                                                ggsave(file, plot = p, device = 'tiff', width = width,
                                                                                       height = height, units = units[1], dpi = dpi)
                                                                              })
                               output$png = downloadHandler(filename = paste(filename,
                                                                             paste('.','png',sep=''), sep = ""), content = function(file) {
                                                                               ggsave(file, plot = p, device = 'png', width = width,
                                                                                      height = height, units = units[1], dpi = dpi)
                                                                             })
                               output$excel = downloadHandler(filename = paste(filename,
                                                                               ".xlsx", sep = ""), content = function(file) {
                                                                                 if (row_names & (!is.null(rownames(data)))) {
                                                                                   data = tibble::add_column(data, RowNames = rownames(data),
                                                                                                             .before = 1)
                                                                                   writexl::write_xlsx(data, path = file, col_names = col_names,
                                                                                                       format_headers = format_headers, use_zip64 = use_zip64)
                                                                                 } else {
                                                                                   writexl::write_xlsx(data, path = file, col_names = col_names,
                                                                                                       format_headers = format_headers, use_zip64 = use_zip64)
                                                                                 }})
                             }))
}
