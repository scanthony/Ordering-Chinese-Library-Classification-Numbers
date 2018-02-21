library(dplyr)

comparify_cla_num <- function(cla_num) {
  ## Designed for Chinese Library Classification scheme

  library(stringr)
  cla_num1 <- ''
  cla_num2 <- ''

  # Clearing all dots in CLC numbers
  cla_num <- str_replace_all(cla_num, '\\.', "")

  # Clearing all right parenthesis in CLC numbers
  cla_num <- str_replace_all(cla_num, '\\)', "")

  # Replace "-", "(", and "=" subdivision symbol for automated ordering
  # Refer to the ASCII table and your classification number's shelving specifications
  cla_num <- str_replace_all(cla_num, '\\-', '!')
  cla_num <- str_replace_all(cla_num, '\\(', '#')
  cla_num <- str_replace_all(cla_num, '\\=', '$')

  # Handle CLC numbers start with T
  pad_non_T <- function(x) {
    # Input is a CLC number without column
    len <- nchar(x)
    return_value <- ''
    if (str_sub(x, 1, 1) == "T") {
      return_value <- x
    } else {
      part1 <- str_sub(x, 1, 1)
      part2 <- ''
      if (len > 1) {
        part2 <- str_sub(x, 2, len)
      }
      return_value <- str_c(part1, '.', part2)
    }
    return (return_value)
  }

  # Handling CLC numbers with a column ":"
  if (str_detect(cla_num, pattern = ":")) {
    cla_num_sep <- str_split(cla_num, pattern = ":", simplify = T)
    cla_num1 <- cla_num_sep[1]
    cla_num2 <- cla_num_sep[2]
  } else {
    cla_num1 <- cla_num
    cla_num2 <- '.'
  }

  # Run the "T-issue" function
  cla_num1 <- pad_non_T(cla_num1)
  cla_num2 <- pad_non_T(cla_num2)

  # Pad the CLC number 1 & 2
  cla_num1 <- str_pad(cla_num1, width = 20, side="right", pad = " ")
  cla_num2 <- str_pad(cla_num2, width = 20, side="right", pad = " ")
  return (paste(cla_num1, cla_num2, sep=':'))
}

# Testing the "comparified" Chinese Library Classification Number

to_be_ordered <- tibble(clc_orig = c('K825.5=6','K825.5-49','F729','K825.5', 'F729(225)','F729-49','H319.4:I712','B94-49','I561.45','H319.4:D','TS976','TP319'))
to_be_ordered <- mutate(to_be_ordered, clc_comparified = clc_orig)
to_be_ordered$clc_comparified <- sapply(to_be_ordered$clc_comparified, comparify_cla_num)
order_idx <- order(to_be_ordered$clc_comparified, method="radix")
already_ordered <- to_be_ordered[order_idx,]
