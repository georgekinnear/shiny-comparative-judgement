This implements a Shiny app through which users are asked to compare a selection of "proof summaries" (see [items-to-be-judged.yml](items-to-be-judged.yml)) using different methods:

1. `cj`: Traditional comparative judgement - a series of paired comparisons, with pairs selected randomly each time.
2. `ccj`: Chained comparative judgement - a series of paired comparisons, where successive pairs always have one item in common.
3. `rank`: Multiple ranking - users are repeatedly asked to put 5 randomly-selected items into rank order.

The app randomises users/judges into 1 of 4 groups, with each group completing different combinations of the methods:

| Group | Methods |
|-|-|
|cj_rank| 15 x `cj` then 5 x `rank` |
|rank_cj| 5 x `rank` then 15 x `cj` |
|cj_ccj| 15 x `cj` then 15 x `ccj` |
|ccj_cj| 15 x `ccj` then 15 x `cj` |

After each method, and at the end, the judges are prompted to answer some questions about their experience of judging.

## Database

The app is designed to save data to a MySQL database. The app reads connection details for this database from `dbconfig.yml`, but this file is not provided here because it contains sensitive login details:
https://github.com/georgekinnear/shiny-comparative-judgement/blob/b52a0018adec5bfd861b2f26e3f4ee993622f664/app.R#L10-L18

## Results

A link to the app was sent to a mailing list of professional mathematicians in December 2021.

The results obtained from mathematicians who completed the survey are available in the [results](results) folder.
