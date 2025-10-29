# Project Setup (R + renv)

This repository uses [**renv**](https://rstudio.github.io/renv/) for reproducible R package management. The lockfile is `renv.lock` (not `renv.log`). Restoring from it will install the exact package set into a project-local library, optionally reusing a global cache for speed.

## Quick start

1.  **Open R/RStudio in the project root** (the folder containing `renv.lock` and `renv/`).
2.  **Install `renv` to your *user* library** (once per machine): `r install.packages("renv")`
3. **In most cases, the project auto-activates via renv/activate.R.** If not, run: ```renv::activate()```
4. **Finish with** ```renv::install()``` to install all the packages.
