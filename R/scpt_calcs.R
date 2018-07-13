#' A calculation function for CPT data
#'
#' This function takes sounding information from a typical ConeTec investigation
#' @param data This is a dataframe containing the sounding information
#' @param sounding_info This is a dataframe containing the header info for the sounding
#' @param unit_weight This is a user entered value. Default = 20 g/cm^3.
#' @param water_table This is a user entered value for the depth to water table. Default = 6 m.
#' @param nu User entered poisson ratio, default = 0.3
#' @param schnaid_g_go Default value of Schaid method g/go ratio, default = 0.3
#' @param schnaid_k Schnaid (2004) method K value, default = 240
#' @param atm_pressure Atmospheric pressure used for calculations, default = 100
#' @param Nkt Mayne (2009) method default Nkt value, default = 15
#' @keywords CPT
#' @export
#' @examples
#' scpt_calcs()

scpt_calcs <- function(data,sounding_info,unit_weight = 20, water_table = 6,
                       nu = 0.3, schnaid_g_go = 0.3, schnaid_k = 240,
                       atm_pressure = 100, Nkt = 15){

  # Elevation (mASL)
  data$elevation <- as.numeric(levels(sounding_info[1,"Elevation:"])) - data$Depth

  # 1. total stress (kPa)
  data$total_stress <- data$Depth * unit_weight

  # 2. hydrostatic pressure (kPa)
  data$hydrostatic <- (data$Depth - water_table) * 9.81
  data[data$hydrostatic <= 0, "hydrostatic"] <- 0

  # 3. effective stress (kPa)
  data$effective_stress <- data$total_stress - data$hydrostatic

  # 4. q_net - net cone resistance (kPa)
  data$q_net <- data$qt * 100 - data$total_stress

  # 5. pp - pore pressure (kPa)
  data$pp <- data$u * 9.81

  # 6. delta_pp - excess pore pressure (kPa)
  data$delta_pp <- data$pp - data$hydrostatic

  # 7. Bq - pore pressure ratio (kPa/kPa)
  data$Bq <- data$delta_pp / data$q_net

  # 8. Q - Normalized cone resistance (kPa / kPa)
  data$Q <- data$q_net / data$effective_stress

  # 9. F - Normalized friction ratio (kPa / kPa, in percent)
  data$F <- data$fs * 100 / data$q_net * 100

  # 10. Rf - Friction ratio (kPa / kPa, in percent)
  data$Rf <- (data$fs / data$qt) * 100

  # 11. Icrw - Simplified Ic, uncorrected (unitless)
  data$Icrw <- sqrt((3.47-log10(data$Q))^2+(1.22+log10(data$F))^2)

  # 12. Ic - Ic, uncorrected (unitless)
  data$Ic <- sqrt((3-log10(data$Q*(1-data$Bq)+1))^2 + (1.5 + 1.3*log10(data$F))^2)

  # remove rows with Inf or NaN
  data <- data %>% filter(is.finite(F) & is.finite(Ic))

  # 13. su - undrained shear strength (kPa)
  data$su <- (data$qt * 100 - data$total_stress) / Nkt

  # 14. alpha_m, and Mayne elastic modulus, based on soil categories
  data$alpha <- 0 # set vector
  ## Ic > 2.2, Q < 14
  for (row in seq(nrow(data))){
    if (data[row,"Ic"] >= 2.2 & data[row,"Q"] < 14){
      data[row,"alpha"] <- data[row,"Q"]
    } else if (data[row,"Ic"] >= 2.2 & data[row,"Q"] > 14) {
      data[row,"alpha"] <- 14
    } else
      data[row,"alpha"] <- 0.0188 * (10 ** (0.55 * data[row,"Ic"]+ 1.68))
  }
  ## Drained modulus estimated from Mayne (2007), CPT guide page 58
  data$mayne_D <- data$alpha * data$q_net
  ## Drained Young's modulus estimated from Mayne(2007)
  data$mayne_E <- (data$mayne_D*(1+nu)*(1-2*nu))/(1-nu)

  # 15. Schnaid (2004) Modulus Estimation
  ## G_o from Schnaid 04 (kPa)
  data$schnaid_go <- schnaid_k * (data$qt * 100 * data$effective_stress * atm_pressure) ** (1/3)
  ## E' from Schnaid 04
  data$schnaid_E <- data$schnaid_go * schnaid_g_go * 2 * (1 + nu)

  # 16. Estimation of Pc', OCR, and OCD from Mayne (2001) (28)
  data$Pc_mayne <- 0.33 * data$q_net
  data$OCR_mayne <- data$Pc_mayne / data$effective_stress
  data$OCD_mayne <- data$effective_stress * (data$OCR_mayne - 1)

  # 17. Estimation of Pc', OCR, and OCD from Mayne (20091 / 2007) (31)
  data$mayne_m <- 0.65 + 1/(800*10 ** (-data$Ic) + 2.5)
  data$Pc_mayne2 <- 0.33 * data$q_net ** data$mayne_m * (atm_pressure/100) ** (1-data$mayne_m)
  data$OCR_mayne2 <- data$Pc_mayne2 / data$effective_stress
  data$OCD_mayne2 <- data$effective_stress * (data$OCR_mayne2 - 1)

  # 18. Estimation of Friction Angle from Kulhawy and Mayne (1990) and Mayne (2006)
  data$phi_sand <-
    data$phi_claysilt

  return(data)
}
