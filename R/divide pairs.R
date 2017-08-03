

#' @export
divide_pair <- function(df, list.input) {
  df <- df %>%ungroup()
  temp_df <- list.input %>%
    map( ~ multicoldiv:::do_opertation(df, .x)) %>%
    bind_cols()

  df %>%
    select(bin_id) %>%
    bind_cols(temp_df)%>%
    gather(Category, Value, 2:length(.))

}

do_opertation <- function(df, var.pair) {
  var1 <- var.pair[[1]]
  var2 <- var.pair[[2]]

  new.name.prefix <- quo_name(var1) %>% stringr::str_replace("_EmpPT", "")

  new.name <- glue::glue("{new.name.prefix}_Ratio")

  message(glue::glue("New variable name will be: {new.name}"))

  df %>%
    select(!!!var.pair) %>%
    mutate(temp.var = UQ(var1)/ UQ(var2)) %>%
    select(temp.var) %>%
    mutate(!!new.name := temp.var) %>%
    select(-temp.var)

}
