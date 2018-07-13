#' A plotting function for CPT data
#' 
#' This function takes sounding information from a typical ConeTec investigation
#' @param data This is a dataframe containing the sounding information
#' @param sounding A string to name the sounding for file purposes
#' @keywords CPT
#' @export
#' @examples
#' scpt_plot()

scpt_plot <- function(data, sounding = "sounding"){
  # Set a uniform limit on elevation
  elev_min <- round(min(data$elevation),0)
  elev_max <- round(max(data$elevation),0)
  
  # The SBTn will have to wait - that is a lot of work
  # Plot Ic for now
  p_ic <- ggplot(data) +
    geom_col(aes(x = elevation, y = Ic, fill = Ic)) +
    coord_flip() +
    ggtitle("Ic") +
    xlab("Elevation (mASL)") +
    ylim(0,5) +
    xlim(elev_min, elev_max)
  
  # Sleeve friction
  p_fs <- ggplot(data) +
    geom_point(aes(x = elevation, y = fs * 100), size = 0.5, colour = "dark green") +
    geom_path(aes(x = elevation, y = fs * 100), colour = "dark green") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("fs (kPa)") +
    ggtitle("Sleeve Friction (kPa)") +
    xlim(elev_min, elev_max)
  
  # Tip Resistance
  p_qt <- ggplot(data) +
    geom_point(aes(x = elevation, y = qt / 10), size = 0.5, colour = "dark red") +
    geom_path(aes(x = elevation, y = qt / 10), colour = "dark red") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("qt (MPa)") +
    ggtitle("Tip Resistance (kPa)") +
    xlim(elev_min, elev_max)
  
  # Pore Pressure
  p_pp <- ggplot(data) +
    geom_point(aes(x = elevation, y = pp / 10), size = 0.5, colour = "dark blue") +
    geom_path(aes(x = elevation, y = pp / 10), colour = "dark blue") +
    geom_path(aes(x = elevation, y = hydrostatic / 10), size = 1, colour = "dark blue", linetype = 'dashed') +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("Pore Pressure (kPa)") +
    ggtitle("Pore Pressure (kPa)") +
    xlim(elev_min, elev_max)
  
  # Shear Strength
  p_su <- ggplot(data) +
    geom_point(aes(x = elevation, y = su), size = 0.5, colour = "black") +
    geom_path(aes(x = elevation, y = su), colour = "black") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("su (kPa)") +
    ggtitle("Shear Strength (kPa)") +
    xlim(elev_min, elev_max)
  
  # Friction Ratio
  p_rf <- ggplot(data) +
    geom_point(aes(x = elevation, y = Rf), size = 0.5, colour = "black") +
    geom_path(aes(x = elevation, y = Rf), colour = "black") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("Rf (%)") +
    ggtitle("Friction Ratio") +
    xlim(elev_min, elev_max)
  
  # Drained Modulus
  p_mod <- ggplot(data) +
    geom_point(aes(x = elevation, y = schnaid_E/1000), size = 0.5, colour = "black") +
    geom_path(aes(x = elevation, y = schnaid_E/1000), colour = "black") +
    geom_point(aes(x = elevation, y = mayne_E/1000), size = 0.5, colour = "blue") +
    geom_path(aes(x = elevation, y = mayne_E/1000), colour = "blue") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("E' (MPa)") +
    ggtitle("Drained Modulus")
  
  # OCR
  p_ocr <- ggplot(data) +
    geom_point(aes(x = elevation, y = OCR_mayne), size = 0.5, colour = "black") +
    geom_path(aes(x = elevation, y = OCR_mayne), colour = "black") +
    geom_point(aes(x = elevation, y = OCR_mayne2), size = 0.5, colour = "blue") +
    geom_path(aes(x = elevation, y = OCR_mayne2), colour = "blue") +
    coord_flip() +
    xlab("Elevation (mASL)") +
    ylab("OCR") +
    ggtitle("Overconsolidation Ratio") +
    xlim(elev_min, elev_max) +
    ylim(0,10)
  
  return(list("p_ic" = p_ic, "p_fs" = p_fs, "p_qt" = p_qt, 
               "p_pp" = p_pp, "p_su" = p_su, "p_rf" = p_rf, 
               "p_mod" = p_mod, "p_ocr" = p_ocr))
}