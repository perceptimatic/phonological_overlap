distances <- readr::read_csv(DISTANCES,
                             col_types=cols(TGT_first = col_logical(),
                                            TGT_first_code = col_number(),
                                            language_stimuli_code = col_number(),
                                            .default = col_guess())) |>
  calculate_all_deltas() |>
  clean_discrimination_items() |>
  mutate(`Δ DTW Mel Filterbank`=`fb_dtw_cosine_Δ`)
  
distances_c <- repeated_average(
  distances,
  c(
    "filename",
    "Context",
    "Phone Contrast Asymmetrical (Language)",
    "Phone Contrast (Language)"
  ),
  c("Phone Language (Code)", "Phone Language (Long)"),
  names(distances)[grepl("_Δ$", names(distances))]
) |> rename(`Δ DTW Mel Filterbank`=`fb_dtw_cosine_Δ`)

