corrected_intensity_plot <- function(df){
df1 <- dplyr::filter(df, line == "WT", reporter == "YFP")
df2 <- dplyr::filter(df, line != "WT", reporter == "YFP")
df3 <- dplyr::filter(df, line == "WT", reporter == "CFP")
df4 <- dplyr::filter(df, line != "WT", reporter == "CFP")

p1 <- ggplot2::ggplot(df1) + ggplot2::aes(minutes, corrected_intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
p2 <- ggplot2::ggplot(df2) + ggplot2::aes(minutes, corrected_intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
p3 <- ggplot2::ggplot(df3) + ggplot2::aes(minutes, corrected_intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
p4 <- ggplot2::ggplot(df4) + ggplot2::aes(minutes, corrected_intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)

cowplot::plot_grid(p1, p2, p3, p4,  labels = c("WT-YFP", "oscal-YFP", "WT-CFP", "oscal-CFP"))


}


all_data_plot<- function(df){
  df1 <- dplyr::filter(df, line == "WT", reporter == "YFP")
  df2 <- dplyr::filter(df, line != "WT", reporter == "YFP")
  df3 <- dplyr::filter(df, line == "WT", reporter == "CFP")
  df4 <- dplyr::filter(df, line != "WT", reporter == "CFP")

  p1 <- ggplot2::ggplot(df1) + ggplot2::aes(minutes, intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
  p2 <- ggplot2::ggplot(df2) + ggplot2::aes(minutes, intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
  p3 <- ggplot2::ggplot(df3) + ggplot2::aes(minutes, intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)
  p4 <- ggplot2::ggplot(df4) + ggplot2::aes(minutes, intensity)  + ggplot2::geom_line(aes(colour = cell_number)) + ggplot2::facet_grid(bio_rep ~ tech_rep)

  cowplot::plot_grid(p1, p2, p3, p4,  labels = c("WT-YFP", "oscal-YFP", "WT-CFP", "oscal-CFP"))


}

ratio_plot <- function(df){

  df1 <- dplyr::filter(df, line == "WT")
  df2 <- dplyr::filter(df, line != "WT")

  p1 <- ggplot2::ggplot(df1) + aes(minutes, ratio) + ggplot2::geom_line(aes(colour = cell_number) ) + ggplot2::facet_grid(bio_rep ~ tech_rep)
  p2 <- ggplot2::ggplot(df2) + aes(minutes, ratio) + ggplot2::geom_line(aes(colour = cell_number) ) + ggplot2::facet_grid(bio_rep ~ tech_rep)

  cowplot::plot_grid(p1, p2,   labels = c("WT", "oscal"))
}


peak_count_plot <- function(df){

  ggplot2::ggplot(df) + ggplot2::aes(bio_rep, peak_count) + ggplot2::geom_jitter(aes(colour = tech_rep, shape = cell_number), position = ggplot2::position_dodge(width=0.5) ) + ggplot2::facet_wrap( ~ line )
}
