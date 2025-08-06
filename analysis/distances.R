distances <- readr::read_csv(DISTANCES,
                             col_types=cols(TGT_first = col_logical(),
                                            TGT_first_code = col_number(),
                                            language_stimuli_code = col_number(),
                                            .default = col_guess())) |>
  calculate_all_deltas() |>
  clean_discrimination_items() |>
  mutate(log_fb_dtw_cosine_Δ=
           log(replaceif0(fb_dtw_cosine_Δ - min(fb_dtw_cosine_Δ), 0.0001))) |>
  mutate(`Spectral Distinctness`=`fb_dtw_cosine_Δ`,
         `Log Spectral Distinctness`=log_fb_dtw_cosine_Δ)
         
distances_c <- repeated_average(
  distances,
  c(
    "filename",
    "Context",
    "Phone Contrast Asymmetrical (Language)",
    "Phone Contrast (Language)"
  ),
  c("Phone Language (Code)", "Phone Language (Long)", "Phone Contrast"),
  names(distances)[grepl("_Δ$", names(distances))]
) |> 
  rename(`Spectral Distinctness`=`fb_dtw_cosine_Δ`) |>
  mutate(`Log Spectral Distinctness`=log(replaceif0(`Spectral Distinctness` - min(`Spectral Distinctness`), 0.0001)))

