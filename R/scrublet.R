#' run_scrublet_seurat
#'
#' @param seurat_obj
#' @param split.by
#' @param assay
#' @param env_name
#'
#' @return
#' @export
#'
#' @examples
run_scrublet_seurat <- function(
    seurat_obj,
    split.by = NULL,
    assay = "RNA",
    env_name = "r-reticulate"
) {
  # ------------------------------ #
  #   基础检查
  # ------------------------------ #
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  library(reticulate)

  if (!is.null(split.by) && !split.by %in% colnames(seurat_obj@meta.data)) {
    stop(paste0("split.by variable '", split.by, "' not found in meta.data"))
  }

  # 使用指定 conda 环境
  use_condaenv(env_name, required = TRUE)

  # 导入 Scrublet
  scr <- import("scrublet")

  # ------------------------------ #
  #   内部功能：运行 Scrublet
  # ------------------------------ #
  run_scrub <- function(mat) {
    scrub <- scr$Scrublet(t(mat))
    res <- scrub$scrub_doublets()

    list(
      score = as.numeric(res[[1]]),
      pred  = as.logical(res[[2]])
    )
  }

  # ------------------------------ #
  #   提取计数矩阵
  # ------------------------------ #
  counts_matrix <- GetAssayData(seurat_obj, assay = assay, slot = "counts")

  # ------------------------------ #
  #   有分组：分组运行
  # ------------------------------ #
  if (!is.null(split.by)) {
    groups <- seurat_obj@meta.data[[split.by]]
    unique_groups <- unique(groups)

    all_scores <- numeric(ncol(seurat_obj))
    all_preds  <- logical(ncol(seurat_obj))

    for (g in unique_groups) {
      message("Running scrublet for group: ", g)

      cell_ids <- names(groups)[groups == g]
      sub_mat <- counts_matrix[, cell_ids, drop = FALSE]

      res <- run_scrub(sub_mat)

      all_scores[cell_ids] <- res$score
      all_preds[cell_ids]  <- res$pred
    }

  } else {
    # ------------------------------ #
    #   无分组：整体运行
    # ------------------------------ #
    res <- run_scrub(counts_matrix)
    all_scores <- res$score
    all_preds  <- res$pred
  }

  # ------------------------------ #
  #   写入 meta.data
  # ------------------------------ #
  seurat_obj$doublet_score  <- all_scores
  seurat_obj$is_doublet     <- all_preds

  return(seurat_obj)
}
#' run_scrublet
#'
#' @param counts_matrix genes x cells
#' @param env_name
#'
#' @return
#' @export
#'
#' @examples
run_scrublet <- function(counts_matrix, env_name = "r-reticulate") {
  # 确保 reticulate 可用
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }
  library(reticulate)

  # 1. 指定要使用的 Python 环境（这里是 scDNS）
  use_condaenv(env_name, required = TRUE)

  # 2. 导入 Scrublet 模块
  scr <- import("scrublet")

  # 3. 初始化 Scrublet 对象
  scrub <- scr$Scrublet(t(counts_matrix))

  # 4. 运行检测
  res <- scrub$scrub_doublets()

  # 5. 提取结果
  doublet_scores <- as.numeric(res[[1]])
  predicted_doublets <- as.logical(res[[2]])

  # 6. 返回结果
  return(list(
    doublet_scores = doublet_scores,
    predicted_doublets = predicted_doublets
  ))
}
