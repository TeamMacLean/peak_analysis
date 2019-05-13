# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


read_peaks <- function(fname, format = "xlsx", ...) {
  if (format == "xlsx"){
    raw <- readxl::read_excel(fname, ...)
  }
  else if (format == "csv"){
    raw <- readr::read_csv(fname, ...)
  }
  else{
    stop("unsupported file format", fname)
  }
  return(raw)
}

#' renames kathrins dataset to useful names and reshapres a little. Specific to one file.
rename_kathrin <-function(df){

    df %>% dplyr::rename(
      minutes = `Time (min)`,
      start_time = `flg22 added`,
      cell_number = `cell#`,
      tech_rep = TechRepID,
      bio_rep = BioRepID
      ) %>%
      tidyr::gather('YFP', 'CFP', key = "reporter", value = "intensity") %>%
      dplyr::mutate(has_started = minutes >= start_time) %>%
      dplyr::mutate(cell_number = as.character(cell_number),
                    tech_rep = as.character(tech_rep),
                    bio_rep = as.character(bio_rep)
                    )
}


#' adds smoothed intensity for each curve. Time before `has_started` counts as seperate curve
#' @export
smooth <- function(df, col_to_smooth = ratio, ...){
  col_to_smooth <- dplyr::enquo(col_to_smooth)
  df %>%
    dplyr::filter(! is.na(!! col_to_smooth )) %>%
    dplyr::mutate(smoothed = smooth.spline(!!col_to_smooth)$y )

}

#' @export
ratio_curve <- function(df, value = corrected_intensity, key = reporter,  test_reporter = YFP, base_reporter = CFP ){


  value <- dplyr::enquo(value)
  key <- dplyr::enquo(key)
  test_reporter <- dplyr::enquo(test_reporter)
  base_reporter <- dplyr::enquo(base_reporter)

  df %>%
    tidyr::spread(!!key, !!value) %>%
    dplyr::mutate(ratio = !!test_reporter / !!base_reporter)


}

get_proportions <- function(df){
  return( df %>% dplyr::group_by(cell_number, tech_rep, bio_rep, line) %>%
    tidyr::spread(reporter, intensity) %>%
    dplyr::mutate(total = YFP + CFP,
                  prop_yfp = YFP / total,
                  prop_cfp = CFP / total
                  )
  )

}

#' converts a grouped_dataframe to a list, each member of which is an original group
grouped_df_to_list <- function(gdf){
  indices <- attributes(gdf)$indices
  df <- dplyr::ungroup(gdf) %>%
    dplyr::mutate(idx = rownames(gdf) )
  groups <- lapply(indices, function(x,df){df[x + 1,]}, df = df )
  return(groups)

}
find_loess_points <- function(group_list,
  #grouped_df,
                              x_col = "minutes",
                              y_col = "intensity",
                              reporter_col = "reporter",
                              test_reporter_name = "YFP",
                              base_reporter_name = "CFP",
                              test_reporter_quantile = 0.75,
                              base_reporter_quantile = 0.25 ){

  #group_list <- grouped_df_to_list(grouped_df)

  result <- lapply(group_list, find_loess_points_ ,
                   x_col = x_col,
                   y_col = y_col,
                   reporter_col = reporter_col,
                   test_reporter_name = test_reporter_name,
                   base_reporter_name = base_reporter_name,
                   test_reporter_quantile = test_reporter_quantile,
                   base_reporter_quantile = base_reporter_quantile)
  return(result)
}
find_loess_points_ <- function(df, x_col = "minutes",
                               y_col = "intensity",
                               reporter_col = "reporter",
                               test_reporter_name = "YFP",
                               base_reporter_name = "CFP",
                               test_reporter_quantile = 0.75,
                               base_reporter_quantile = 0.25
                               ){

  mod <- lm(y ~ x, data = data.frame(x = df[[x_col]], y = df[[y_col]] ))

  if ( df[[reporter_col]][[1]] == base_reporter_name ){
    limit <- quantile( resid(mod), c(base_reporter_quantile) )
    peaks <- 1:length(df[[y_col]]) %in% quantmod::findPeaks(df[[y_col]])
    df$is_loess_point <- (resid(mod) > limit) & peaks
    return( df )
  }

  if ( df[[reporter_col]][[1]] == test_reporter_name ){
    limit <- quantile( resid(mod), c(test_reporter_quantile) )
    valleys <- 1:length(df[[y_col]]) %in% quantmod::findValleys(df[[y_col]])
    df$is_loess_point <- (resid(mod) > limit) & valleys
    return( df )
  }
}

get_corrected <- function(df,
                          x_col = "minutes",
                          y_col = "intensity"){

  points <- dplyr::filter(df, is_loess_point == TRUE )

  mod_d <- tibble::tibble(x = points[[x_col]], y = points[[y_col]])
  baseline_mod <- loess(y ~ x, data = mod_d )
  values <- predict( baseline_mod,  newdata = data.frame( x = df[[x_col]] ))
  values <- abs(values - max(values, na.rm = TRUE))
  df$corrected_intensity <- df[[y_col]] + values
  return(df)
}


correct_intensity <- function(grouped_df,
                              x_col = "minutes",
                              y_col = "intensity",
                              reporter_col = "reporter",
                              test_reporter_name = "YFP",
                              base_reporter_name = "CFP",
                              test_reporter_quantile = 0.75,
                              base_reporter_quantile = 0.25
                              ){

  #points_col_quo <- dplyr::enquo(points_column)
  #points <- dplyr::filter(df,  !! points_col_quo == TRUE )
  group_list <- grouped_df_to_list(grouped_df)
  loess_point_list <- lapply(group_list, find_loess_points_ ,
                             x_col = x_col,
                             y_col = y_col,
                             reporter_col = reporter_col,
                             test_reporter_name = test_reporter_name,
                             base_reporter_name = base_reporter_name,
                             test_reporter_quantile = test_reporter_quantile,
                             base_reporter_quantile = base_reporter_quantile)


  corrected_list <- lapply(loess_point_list, get_corrected, x_col = x_col, y_col = y_col)
  return(dplyr::bind_rows(corrected_list))


}

#' @export
find_peak_floor <- function(df){
  df %>% dplyr::group_by(reporter, cell_number, tech_rep, bio_rep) %>%
    dplyr::summarize(peak_floor = mean(smoothed) )
}

single_curve <- function(df){
  df %>% dplyr::filter(cell_number == "1", tech_rep == "1", bio_rep == "1", line == "WT")
}


#' df.  Applies smmoth spline to a column called corrected_intensity
smooth_corrected <- function(df, col_to_smooth = corrected_intensity){

  smooth_col_quo <- dplyr::enquo(col_to_smooth)
  dplyr::filter(df, ! is.na( !!smooth_col_quo) )  %>%
    dplyr::mutate(smoothed = smooth.spline(!! smooth_col_quo )$y )


}

find_peaks <- function(df){
  quantmod::findPeaks(df[['smoothed']])
}
