#' reorderXtiticks
#' ggplot2排序X轴标签
#'
#' @param fun mean
#' @param decreasing FALSE
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' ggplot(pdata,aes(RareQcluster%>%as.character(),PTPRC))+geom_violin()+reorderXtiticks(fun = mean,decreasing = F)
reorderXtiticks <- function(fun = mean, decreasing=FALSE,...) {
  # 返回一个自定义类的对象，存储排序函数
  structure(list(fun = fun, decreasing = decreasing, extra_args = list(...)),
            class = "reorder_x_autofill")
}

# 2. 定义 ggplot_add 方法：这是魔法发生的地方
# 当你输入 plot + reorderXtiticks() 时，这个函数会被调用
ggplot_add.reorder_x_autofill <- function(object, plot, object_name) {

  # --- 第一步：获取数据和映射 ---
  # 注意：这要求 aes(x, y) 必须写在 ggplot() 主函数中，而不是 geom_ 中
  if (is.null(plot$data) || is.null(plot$mapping$x) || is.null(plot$mapping$y)) {
    stop("reorderXtiticks requires data and global aes(x, y) in the main ggplot() call.")
  }

  # 使用 eval_tidy 在数据上下文中提取 x 和 y 的实际向量
  x_vec <- rlang::eval_tidy(plot$mapping$x, plot$data)
  y_vec <- rlang::eval_tidy(plot$mapping$y, plot$data)

  # --- 第二步：计算排序 ---
  # 使用 reorder 逻辑计算新的因子水平顺序
  # 将 x 转换为因子（如果还不是），并根据 y 和 fun 排序
  if(object$decreasing){
    x_factor_ordered <- stats::reorder(factor(x_vec), -y_vec, FUN = object$fun)
  }else{
    x_factor_ordered <- stats::reorder(factor(x_vec), y_vec, FUN = object$fun)
  }

  new_limits <- levels(x_factor_ordered)

  # --- 第三步：创建 Scale 图层 ---
  # 创建一个设定了 limits 的 scale_x_discrete
  # limits 参数控制了坐标轴的顺序
  new_scale <- scale_x_discrete(limits = new_limits)

  # --- 第四步：将新 Scale 添加到原图表中 ---
  # 相当于执行了: plot + scale_x_discrete(...)
  ggplot2::ggplot_add(new_scale, plot, object_name)
}

