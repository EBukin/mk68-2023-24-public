# MK68 Empirical Research Methods: Quantitative Research Methods

Lecture slides for the quantitative part (*Statistics and Econometrics*) of
**MK-068-EN (TM) Empirical Research Methods** at the Institute of Agricultural
Policy and Market Research, Justus Liebig University Giessen. Taught by
[Eduard Bukin](https://www.uni-giessen.de/faculties/f09/institutes/agri/policy/team/bukin)
in Winter Semester 2023/24 (as Part 2 of the module) and, revised, in Winter
Semester 2024/25 (as Part 1).

The repository is a [Quarto](https://quarto.org/) website:
**<https://ebukin.github.io/mk68-2023-24-public/>**

## What is where

| Path | Content |
|:--|:--|
| `materials/*.qmd` | Slide sources of the 2024/25 cohort. Seven week decks plus the parts they `{{< include >}}`; see the render list in `_quarto.yml` for which files are decks and which are parts. `mk68-00-part-1-prerequisites.qmd` is the PDF handout source. |
| `prerequisites.qmd` | Site page that includes the prerequisites handout source. |
| `materials/_metadata.yml` | revealjs settings, course subtitle, footer, logo and bibliography for everything in `materials/`. |
| `materials/00-setup.R`, `00-aux-functions.R`, `01-simple-reg-plots.R` | Setup sourced by the decks (packages, knitr options, ggplot theme) and helper code. |
| `materials/data/`, `materials/img/` | Cleaned datasets and images the decks read. |
| `data-raw/` | Scripts that produced the datasets, with source notes. Raw downloads are not committed. |
| `ilias-2023-24/`, `ilias-2024-25/` | The PDF slides exactly as shared with each cohort on Ilias (Git LFS). |
| `index.qmd`, `about.qmd`, `slides.qmd`, `data.qmd`, `resources.qmd`, `cohorts/`, `weeks/` | The website pages. |
| `_freeze/` | Executed results of every deck, committed so that a build needs no R packages. |
| `styles.scss`, `styles-dark.scss` | Site theme. `eddies-theme.scss` is the revealjs theme of the decks. |
| `references.bib` | Bibliography used by the decks and the reading-list page. |
| `scripts/check-site.py` | Post-render check: broken internal links, deck count, LFS pointers. |
| `.github/workflows/build-site.yml` | Renders and deploys the site to the `gh-pages` branch on every push to `main`. |

## The two cohorts

The slide sources were revised between the cohorts, so the site keeps them
apart:

- **WS 2024/25** is the current state of `materials/`. Its decks are rendered
  as interactive revealjs slides and its PDFs are in `ilias-2024-25/`.
- **WS 2023/24** is published from its PDFs in `ilias-2023-24/`. The sources as
  taught that year are at commit
  [`9c5d2e6`](https://github.com/EBukin/mk68-2023-24-public/tree/9c5d2e6/materials)
  (30 January 2024, the last content change before the 2024/25 revision) and
  the cohort page links to them.

## Rendering locally

Requirements: Quarto (1.4 or newer), R 4.x and the packages loaded by
`materials/00-setup.R` plus those the individual decks call. On a fresh R:

```r
install.packages(c(
  "pacman", "here", "tidyverse", "stringi", "glue", "pins", "lubridate",
  "janitor", "knitr", "patchwork", "scales", "ggpubr", "cowplot", "matrixStats",
  "ggstatsplot", "mosaic", "statar", "palmerpenguins", "haven", "labelled",
  "alr4", "plm", "GGally", "report", "ggpmisc", "parameters", "performance",
  "AER", "equatiomatic", "magick", "modelsummary", "synthpop", "dslabs",
  "faux", "psych", "kableExtra", "flextable", "ggthemes", "ggrepel",
  "effectsize", "ggforce", "concaveman", "agridat", "correlation",
  "gridExtra", "ggpp", "broom", "gdtools"
))
remotes::install_github("nicolash2/ggbrace")
```

`mosaic` needs `gdtools` 0.4.4 or newer; if it fails to load, update `gdtools`.

Then, from the repository root:

```sh
quarto render                       # whole site into _site/
python scripts/check-site.py --decks 11
quarto preview                      # local server with live reload
```

`execute: freeze: auto` is on: a deck is re-executed only when its source
changes, and the result is written to `_freeze/`. Commit `_freeze/` together
with the source change so that the CI build, which has no course packages,
keeps working.

To re-render a single deck: `quarto render materials/mk68-04-slides.qmd`.

### Two decks that need private data

Two data files are excluded from the public repository by `.gitignore`
(`*.csv`, `*.sav`), and the decks that read them are therefore commented out of
the render list in `_quarto.yml`:

| Deck | Needs |
|:--|:--|
| `materials/mk68-01-p2-slides.qmd` (Week 01, Part 2) | `materials/data/poverty-population.csv`, columns `Extreme poverty` and `Population` |
| `materials/mk68-03-slides.qmd` (Week 03), via `ceteris-paribus.qmd` | `data-raw/uganda-poject/Uganda_B_E_agri and nutrition 04.06.2018.sav` |

To publish them as interactive decks: put the two files in place, uncomment the
two lines in `_quarto.yml`, render, commit the new `_freeze/materials/...`
folders, and raise `--decks` in `.github/workflows/build-site.yml` to 13. Their
PDFs are published for both cohorts regardless.

### Producing the Ilias PDFs

The PDFs in `ilias-*/` were printed from the rendered revealjs decks with the
browser (print view, `e` inside a deck). The prerequisites handout keeps its
`format: pdf` source (`materials/mk68-00-part-1-prerequisites.qmd`, rendered
with `quarto render materials/mk68-00-part-1-prerequisites.qmd`); the site
publishes the same content as a page through `prerequisites.qmd`, which
includes it.

`materials/mk68-02-slides-aux.qmd` ("Week 02. Auxiliary materials", 2023/24)
is kept for reference but does not render: it uses objects defined only in the
full 2023/24 Week 2 deck it was split from.

## Deployment

`.github/workflows/build-site.yml` checks out the repository with LFS, installs
Quarto and R with only `knitr` and `rmarkdown`, renders the site from the frozen
results, runs `scripts/check-site.py`, and deploys `_site/` to the `gh-pages`
branch. GitHub Pages must be set to serve from that branch.

Every push to `main` downloads the ~100 MB of LFS PDFs once, which counts
against the repository's LFS bandwidth quota.

## Quarto helpers

- <https://www.rstudio.com/blog/6-productivity-hacks-for-quarto/>
