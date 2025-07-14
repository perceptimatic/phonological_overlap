discr <- read_csv(
  DISCR_DATA,
  col_types = cols(
    TGT_first = col_logical(),
    correct_answer = col_number(),
    binarized_answer = col_number(),
    nb_stimuli = col_number(),
    TGT_first_code = col_number(),
    language_indiv_code = col_number(),
    language_stimuli_code =
      col_number(),
    .default = col_character()
  ),
  name_repair = "unique_quiet"
) |>
  select(-`...1`) |>
  clean_discrimination_items() |>
  clean_discrimination_responses()

#discr_asc <- repeated_average(
#  discr,
#  c(
#    "filename",
#    "Context",
#    "Phone Contrast Asymmetrical (Language)"
#  ),
#  c("Listener Group", "Phone Language (Code)", "Phone Language (Long)"),
#  c("Accuracy", "Accuracy and Certainty")
#) 

discr_c <- repeated_average(
  discr,
  c(
    "filename",
    "Context",
    "Phone Contrast Asymmetrical (Language)",
    "Phone Contrast (Language)"
  ),
  c("Listener Group", "Phone Language (Code)", "Phone Language (Long)"),
  c("Accuracy", "Accuracy and Certainty")
) 


discr_f <- repeated_average(
  discr,
  "filename",
  c("Listener Group", "filename"),
  c("Accuracy", "Accuracy and Certainty")
) 

discr_c_wide <- discr_c |>
  pivot_wider(names_from=`Listener Group`,
              values_from=c(`Accuracy`, `Accuracy and Certainty`),
              names_glue="{.value} {`Listener Group`}") |>
  mutate(`Accuracy and Certainty Difference`=
           `Accuracy and Certainty French`-
           `Accuracy and Certainty English`,
         `Accuracy Difference`=`Accuracy French`-`Accuracy English`)
