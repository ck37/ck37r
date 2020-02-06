TODO:

New functions:
- ggplot2 theme
- Provide different styles of SL libraries as a service to other SL users.
   - E.g. minimal, optimal, safe, etc.
- plot_auc_table - visualize auc_table results.

Documentation
- Document all functions - IN PROGRESS
- Include examples for all functions - IN PROGRESS
- Expanded README.md - IN PROGRESS
- Vignette

Testing
- Add unit tests - IN PROGRESS
- Add separate tests for data.frames, tibbles, and data.tables.

Parallelization
- Convert from foreach to futures.

Factors to indicators
- Figure out how to increase performance. Should we cbind only once at the end?
- Multicore?
- Display progress report every ~X (e.g. 5) minutes with time elapsed, % complete,
  and estimated time remaining.

Impute missing values:
- Work with Date types appropriately.
- Remove columns that exceed a certain missingness threshold?
- Return missingness indicator names.
