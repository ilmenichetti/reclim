# ICBM accessory functions
# GAI function
# author: Lorenzo Menichetti, on specifications from Martin Bolinder
# Year 2018, August

### Roxygen documentation

#GAI
#' Internal function to calculate the GAI (Green Area Index) based on yields and agronomical data. New version with code optimized for readability.
#' @description  The function distributes the biomass growth (considering its proxy GAI and LAI) according to a gaussian function with parameters defined relatively to the crop.
#' It is used for determining the effect of increased ET due to the presence of plant and the effect of shading on soil temperature.
#'
#' @author Lorenzo Menichetti \email{ilmenichetti@@gmail.com}
#'
#' @param yield annual yields (kg/ha)
#' @param crop crop id, either "spring_small_grains", "spring_oil_seeds","winter_small_grains", "winter_oil_seeds","root_crop", "fodder", "fodder maize" or "ley"
#' @param year sequence of the years to run the simulation for
#' @param variance the variance of the gaussian used to simulate the GAI, in days
#' @param seeding day of seeding (day number of the year)
#' @param harvest day of harvest (day number of the year)
#' @param tillage day of tillage (day number of the year)
#' @param minimum_cover, forcing a minimum GAI cover, useful for leys (dimensionless)
#' @param yield2 OPTIONAL, in case there is more than one harvest per year (kg/ha)
#' @param harvest2 OPTIONAL, in case there is more than one harvest per year (days)
#'
#' @return
#' A data frame of 5 variables: \code{date}, \code{GAI}, \code{crop}, \code{yields_at_harvest} and \code{LAI}.
#' The other dimension of the data frame is as long as the combination of the treatments and the days of the simulation.
#'
#' @details
#' The function relies on an input matrix which must follow a precise format, please refer to the attached \link{aboveground_testdata}.
#'
#' The function is used to simulate the development of crops and their green area index (G.A.I.). The function uses among the inputs a vector of different crops, which will be simulated with different parameters.
#' These are \code{spring_small_grains}, \code{spring_oil_seeds}, \code{winter_small_grains}, \code{winter_oil_seeds},\code{root_crop}, \code{fodder}, \code{fodder maize} and \code{ley}. The function loops in annual steps through the years of the simulation
#' and then runs a nested loop to simulate the crop growth in daily steps. The function initially calls the \link{is.leapyear} function to decide if to use 365 or 366 days in the simulation. Different crops are simulated
#' in different ways.
#' The functions returns also the LAI, calculated as \deqn{0.8 \cdot GAI} until maximum GAI. After maximum GAI, before harvest the LAI is set to never fall below \deqn{0.7 \cdot max(LAI)}, and between harvest and tillage never below \deqn{0.2 \cdot max(LAI)} according to Kätterer & Andrén 2009.
#'
#' First of all the function checks if the crop of that year is not "fodder", "fodder maize" or "ley". if not, then sets the maximum GAI (j is the index used in the main function loop, looping through the simulation years):
#'  \deqn{GAI_{max}=0.0129 \cdot (\frac{yield_j}{1000})^2 + 0.1964 \cdot(\frac{yield_j}{1000})}
#' For root crops the maximum GAI is set differently (see below the specific section)
#' The function then proceeds to simulate the growth according to a gausssian function subsequently modified. The gaussian function is controlled by the parameters defining where it is centered and its variance.
#' Its center is calculated according to seeding and harvest dates, which are in the input data. Then the GAI outside the area covered by such function is either set to zero or to the minimum coverage specified in the input data.
#' The main function used to simulate the crop growth, after first having calculated the center of the gausian with
#' \deqn{middle=seeding_j + \frac{harvest_j-seeding_j}{2}}
#' is the following:
#' \deqn{GAI=GAI_{max} \cdot exp(-\frac{(day-middle)^2)}{(2\cdot variance_j)})}
#' Most crops are considered covering the soil even after being fully mature, except root crops fodder (including silage maize). Please not that this does not imply that such crops are returned as C inputs to the soil in the ICBM model, this
#' concerns just the calculation of the climatic reduction coefficients.
#'
#' ## Exceptions
#' ### \code{root_crops}
#' Root crops have a specific function, which is based on the average yields (yield_vec) and maximum LAI (LAI_max_vec) obtained in the Ultuna experiment during the three years when root crops were planted.
#' The maximum GAI is also calculated with a different function:
#' \deqn{GAI_{max}=min(5.6,\frac{1}{0.8} \cdot mean(\frac{LAI_max_vec}{yield_vec}) \cdot \frac{1}{0.75} \cdot yield_j)}
#'
#' ### \code{fodder}
#' The maximum GAI is calculated according to data for fodder rape
#' (https://www.agronomysociety.org.nz/files/2010_11._Seed_yield_of_forage_rape.pdf)
#' \deqn{GAI_{max}=min(10,0.0004615385 \cdot yield_j)}
#'
#' ### \code{fodder_maize}
#' The maximum GAI is calculated according to data from the Ultuna experiment, where silage maize has been planted since 2000
#' \deqn{GAI_{max}=min(10,\frac{1}{0.8} \cdot 0.000533 \cdot yield_j)}
#'
#' ### \code{ley}
#' Leys are complicated by the fact that there might be two subsequent cuts, so two harvests. The command considers this possibility with 2 optional parameters, harvest2 and yields2, which are otherwise set to NULL.
#' If these two parameters are present another if condition takes care of them when they are not set to zero.
#'
#'
#'@examples
#'data(aboveground_testdata) #load the example dataset
#'
#'selected_aboveground<-aboveground_testdata[aboveground_testdata$treat=="CONVENTIONAL",]
#'
#'GAI_test<-GAI(yield=selected_aboveground$total_dm_kg_ha, crop=selected_aboveground$crop_id,
#'        year=selected_aboveground$year, variance=selected_aboveground$variance,
#'        seeding=selected_aboveground$seeding, harvest=selected_aboveground$harvest,
#'        tillage=selected_aboveground$tillage, minimum_cover=selected_aboveground$minimum_cover)
#'
#'plot(GAI_test$date, GAI_test$GAI, type="l") # plotting the results to test
#'
#' @md
#'
#' @references
#' W. Mazurczyk, Anna Teresa Wierzbicka, Anna Teresa Wierzbicka, C. Trawczyński,2009,. HARVEST INDEX OF POTATO CROP GROWN UNDER DIFFERENT NITROGEN AND WATER SUPPLY.
#'
#' T. Kätterer and O. Andrén, “Predicting daily soil temperature profiles in arable soils in cold temperate regions from air temperature and leaf area index,” Acta Agric. Scand. Sect. B - Plant Soil Sci., vol. 59, no. 1, pp. 77–86, 2009, doi: 10.1080/09064710801920321.
#'
#' @export
GAI_optimized <- function(yield, crop, year, variance, seeding, harvest, tillage, minimum_cover, yield2=NULL, harvest2=NULL) {

  # Helper function to calculate GAImax based on crop type
  calculate_GAImax <- function(yield, crop) {
    if (crop == "root_crop") {
      LAImax_vec <- c(4.9, 6.5, 5.4)
      yield_vec <- c(5317, 8080, 8031)
      return(min(5.6, (1 / 0.8) * mean(LAImax_vec / yield_vec) * (1 / 0.75) * yield))
    }
    if (crop == "fodder") return(min(10, 0.0004615385 * yield))
    if (crop == "fodder_maize") return(min(10, (1 / 0.8) * 0.000533 * yield))
    if (crop == "ley") return(min(10, 0.0018 * yield))
    return(0.0129 * (yield / 1000) ^ 2 + 0.1964 * (yield / 1000))
  }

  # Initialize lists
  GAI_list <- list()
  LAI_list <- list()
  crop_list <- c("spring_small_grains", "spring_oil_seeds", "winter_small_grains", "winter_oil_seeds", "root_crop")

  for (j in 1:length(yield)) {
    if (is.na(year[j])) stop(paste("Problem with year number", j, "it results", year[j]))

    day <- seq(1, ifelse(is.leapyear(year[j]), 366, 365), by=1)
    GAImax <- calculate_GAImax(yield[j], crop[j])
    GAI <- numeric(length(day))

    if (crop[j] %in% crop_list) {
      middle <- seeding[j] + (harvest[j] - seeding[j]) / 2
      for (i in 1:length(day)) {
        if (day[i] >= seeding[j]) {
          GAI[i] <- GAImax * exp(-((day[i] - middle) ^ 2) / (2 * variance[j]))
        } else {
          GAI[i] <- 0
        }
        if (crop[j] == "root_crop" && day[i] > harvest[j]) {
          GAI[i] <- 0
        }
      }
    } else if (crop[j] %in% c("fodder", "fodder_maize", "ley")) {
      for (i in 1:length(day)) {
        if (day[i] > seeding[j]) {
          GAI[i] <- GAImax / (1 + exp(-((day[i] - seeding[j]) - (harvest[j] - seeding[j]) / 2) / 10))
        } else {
          GAI[i] <- 0
        }
        if (day[i] > harvest[j]) {
          GAI[i] <- 0
        }
      }
      if (crop[j] == "ley" && !is.null(harvest2) && harvest2[j] != 0) {
        GAImax2 <- min(10, 0.0018 * yield2[j])
        for (i in harvest[j]:length(day)) {
          if (day[i] < harvest2[j]) {
            GAI[i] <- GAImax2 / (1 + exp(-((day[i] - harvest[j]) - (harvest2[j] - harvest[j]) / 2) / 10))
          } else {
            GAI[i] <- minimum_cover[j]
          }
        }
      }
    }

    # Apply minimum cover only for crops not in crop_list
    if (!(crop[j] %in% crop_list)) {
      for (i in 1:length(GAI)) {
        if (GAI[i] < minimum_cover[j]) {
          GAI[i] <- minimum_cover[j]
        }
      }
    }
    if (!is.na(tillage[j])) {
      for (i in tillage[j]:length(day)) {
        GAI[i] <- 0
      }
    }

    GAI_date <- as.Date(day - 1, origin = paste(year[j], "-01-01", sep=""))
    GAI_list[[j]] <- data.frame(GAI=GAI, GAI_date=GAI_date, crop=crop[j], yield=yield[j])

    LAI <- GAI * 0.8
    if (crop[j] != "missing") {
      for (i in middle:harvest[j]) {
        if (LAI[i] < max(LAI) * 0.7) {
          LAI[i] <- max(LAI) * 0.7
        }
      }
      for (i in harvest[j]:tillage[j]) {
        LAI[i] <- max(LAI) * 0.2
      }
    }
    LAI_list[[j]] <- data.frame(LAI=LAI, GAI_date=GAI_date, crop=crop[j], yield=yield[j])
  }

  GAI_long <- numeric()
  GAI_date_long <- as.Date(character())
  GAI_yield_long <- numeric()
  GAI_crop_long <- character()
  LAI_long <- numeric()

  for (i in 1:length(GAI_list)) {
    GAI_long <- c(GAI_long, GAI_list[[i]]$GAI)
    GAI_date_long <- c(GAI_date_long, GAI_list[[i]]$GAI_date)
    GAI_yield_long <- c(GAI_yield_long, GAI_list[[i]]$yield)
    GAI_crop_long <- c(GAI_crop_long, as.character(GAI_list[[i]]$crop))
    LAI_long <- c(LAI_long, LAI_list[[i]]$LAI)
  }

  GAI_DF <- data.frame(date=GAI_date_long, GAI=GAI_long, crop=GAI_crop_long, yield_at_harvest=GAI_yield_long, LAI=LAI_long)

  return(GAI_DF)
}
