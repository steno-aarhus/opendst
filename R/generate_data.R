#' Generate longitudinal synthetic bef data
#'
#' @param years_generated A vector of years to generate data points for.
#' @param rows_per_year How many individual PNRs to generate data for each year.
#' @param turnover_per_year The number of individual PNRs left out of any given year per year generated.
#'
#' @returns A tibble containing multiple years of a synthetic bef register (featuring a selection of commonly used variables)
#' @export
#'
#' @seealso See the `vignette("register_references")` for the documentation used to create this function
#'
#' @examples
#' generate_bef(
#'   years_generated = 2015:2024,
#'   rows_per_year = 1000,
#'   turnover_per_year = 20
#' )
generate_bef <- function(years_generated = 2015:2024,
                         rows_per_year = 1000,
                         turnover_per_year = 20) {
  #  SETUP POPULATION SIZE

  # Calculate total size of the unique population pool based on inputs
  total_background_rows <- rows_per_year + (length(years_generated) * turnover_per_year)

  # GENERATE BACKGROUND POPULATION (time-stable Variables)
  # P.S. Yes, PNR and KOEN can change over time for a person changing legal sex, but that's beyond the current scope

  # Setup OPR_LAND vector

  opr_country_codes <- c(
    5115, 5902, 5901, 5101, 5126, 5128, 5422, 5607, 5104, 5130,
    5134, 5142, 5150, 5750, 5609, 5611, 5108, 5153, 5140, 5154, 5156,
    5158, 5778, 5752, 5164, 5170, 5120, 5776, 5180, 5174, 5182, 5122,
    5124, 5754, 5184, 5706, 5106, 5105, 5152, 5758, 5761, 5107, 5712,
    5109, 5759, 5111, 5756, 5110, 5700, 5159, 5160, 5757, 5151, 5162,
    5129, 5172, 5704, 5176, 5114, 5132, 5143, 5186, 5199, 5202, 5204,
    5281, 5207, 5294, 5213, 5277, 5276, 5215, 5278, 5279, 5525, 5272,
    5282, 5216, 5214, 5283, 5222, 5228, 5232, 5231, 5233, 5234, 5235,
    5236, 5238, 5242, 5297, 5243, 5244, 5284, 5245, 5240, 5247, 5285,
    5246, 5286, 5287, 5621, 5288, 5298, 5255, 5253, 5289, 5256, 5258,
    5259, 5262, 5260, 5264, 5266, 5292, 5293, 5268, 5269, 5296, 5295,
    5230, 5290, 5299, 5314, 5390, 5397, 5399, 5309, 5302, 5301, 5303,
    5305, 5526, 5307, 5304, 5306, 5394, 5316, 5318, 5322, 5324, 5325,
    5345, 5326, 5328, 5372, 5530, 5529, 5336, 5339, 5528, 5338, 5308,
    5342, 5348, 5352, 5353, 5354, 5346, 5356, 5358, 5364, 5366, 5527,
    5625, 5347, 5311, 5344, 5374, 5376, 5392, 5395, 5319, 5367, 5398,
    5535, 5404, 5708, 5710, 5406, 5410, 5408, 5412, 5416, 5531, 5474,
    5403, 5724, 5428, 5432, 5434, 5498, 5436, 5438, 5442, 5444, 5446,
    5716, 5448, 5720, 5452, 5454, 5456, 5533, 5458, 5457, 5459, 5414,
    5464, 5466, 5468, 5494, 5462, 5472, 5473, 5496, 5478, 5480, 5482,
    5418, 5484, 5486, 5722, 5424, 5492, 5718, 5493, 5714, 5488, 5402,
    5471, 5476, 5499, 5487, 5502, 5516, 5779, 5508, 5506, 5274, 5248,
    5310, 5514, 5534, 5623, 5522, 5505, 5273, 5275, 5532, 5435, 5599,
    5102, 5999, 5103, 5000, 5001, 5800, 5906
  )

  n_foreign <- length(opr_country_codes)
  # Ensure we don't have negative repetition if the pool is smaller than the code list
  if (total_background_rows < n_foreign) {
    stop("Total population is smaller than the number of unique country codes.")
  }
  n_danish <- total_background_rows - n_foreign

  opr_land_vector <- sample(c(opr_country_codes, rep(5100, n_danish)))

  # Generate the stable "Background" Tibble
  background_pop <- tibble(
    # Stable ID
    PNR = replicate(total_background_rows, paste0(sample(0:9, 12, replace = TRUE), collapse = "")),

    # Stable Origin
    OPR_LAND = opr_land_vector,

    # Stable Gender
    KOEN = sample(c(1, 2), total_background_rows, replace = TRUE),

    # Stable Birthday
    # Cap max birthday at start of sequence - 1 year to ensure valid ALDER in all generated years
    FOED_DAG = sample(
      seq(as.Date("1950-01-01"), as.Date(paste0(min(years_generated) - 1, "-12-31")), by = "day"),
      total_background_rows,
      replace = TRUE
    )
  )

  # HELPER FUNCTION: YEARLY SAMPLING

  generate_year_sample <- function(curr_year, population_pool) {
    year_char <- as.character(curr_year)
    civst_codes <- c("D", "E", "F", "G", "L", "O", "P", "U", "9")

    # Sample specific rows for this year
    population_pool |>
      sample_n(rows_per_year, replace = FALSE) |>
      # Add time-varying Variables (Generated at random each year)
      mutate(
        REG = sample(
          c(81, 82, 83, 84, 85),
          n(),
          replace = TRUE,
          prob = c(0.1, 0.22, 0.21, 0.32, 0.15)
        ),
        CIVST = sample(civst_codes, n(), replace = TRUE),
        year = year_char,

        # Calculate ALDER relative to the current year
        ALDER = floor(time_length(
          interval(FOED_DAG, as.Date(paste0(year_char, "-12-31"))),
          unit = "years"
        ))
      ) |>
      # Reorder columns
      select(PNR, KOEN, FOED_DAG, ALDER, REG, CIVST, OPR_LAND, year)
  }

  # EXECUTE MAP & RETURN

  result <- map(years_generated, \(y) generate_year_sample(y, background_pop)) |>
    list_rbind()

  return(result)
}
