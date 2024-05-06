library(dplyr)

lubri_date::today()

mtcars |>
    mutate(disp = stringr::str_pad(disp,
        width = 5,
        side = "left",
        pad = "0" ))

mtcars |> pivot_longer(-vs)

