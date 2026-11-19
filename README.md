
## Splicing of HPV16 E6: Code and data for Figs. 8, S9, and S10
<!-- ************************************************************* -->

Code to reproduce the figures in Lim et al. (2026+), *Science
Translational Medicine*.
Manuscript: (DOI not yet available)

Tested on macOS Tahoe, R 4.5.1.

**Note:** Figures may not be pixel-identical to the published versions
in all cases. Some editing/repositioning of text was done by hand with
an SVG-editor.


## Repository structure
<!-- ************************************************************* -->

- `data/`: input data (not modified by scripts)
- `R/`: shared functions and plotting theme
- `scripts/`: one script per step or figure
- `output/`: empty initially - for output figures, logs

All necessary data are in the `<project-root>/data` directory.



## Usage
<!-- ************************************************************* -->

To create and save all figures directly to `output`:
```bash
git clone https://github.com/asw221/lim26-stm-figures.git
cd lim26-stm-figures

Rscript -e 'install.packages("renv"); renv::restore()'
Rscript run-all.R
```


## Figure-to-code map
<!-- ************************************************************* -->

| Figure             | Script | Output |
|:-------------------|:-------|:-------|
| Fig. 8C            | `scripts/figure-8c.R`   | `output/figure-8c.pdf` |
| Fig. 8D-F          | `scripts/figure-8def.R` | `output/figure-8def.pdf` |
| Fig. 8G            | `scripts/figure-8g.R`   | `output/figure-8g.pdf` |
| Fig. 8H            | `scripts/figure-8h.R`   | `output/figure-8h.pdf` |
| Fig. S9B (part 1)  | `scripts/figure-s9.R`   | `output/figure-s9b-cohort1.pdf` |
| Fig. S9B (part 2)  | `scripts/figure-s9.R`   | `output/figure-s9b-cohort2.pdf` |
| Fig. S9B (part 3)  | `scripts/figure-s9.R`   | `output/figure-s9b-pooled.pdf` |
| Fig. S9C           | `scripts/figure-s9.R`   | `output/figure-s9c.pdf` |
| Fig. S9D           | `scripts/figure-s9.R`   | `output/figure-s9d.pdf` |
| Fig. S9E           | `scripts/figure-s9e.R`  | `output/figure-s9e.pdf` |
| Fig. S10A          | `scripts/figure-s10.R`  | `output/figure-s10a.pdf` |
| Fig. S10B          | `scripts/figure-s10.R`  | `output/figure-s10b.pdf` |
| Fig. S10C (part 1) | `scripts/figure-s10.R`  | `output/figure-s10c-left.pdf` |
| Fig. S10C (part 2) | `scripts/figure-s10.R`  | `output/figure-s10c-right.pdf` |
| Fig. S10D (part 1) | `scripts/figure-s10.R`  | `output/figure-s10d-left.pdf` |
| Fig. S10D (part 2) | `scripts/figure-s10.R`  | `output/figure-s10d-right.pdf` |



## Citation
<!-- ************************************************************* -->

See [CITATION.cff], or use GitHub's "Cite this repository" button.


## Contact
<!-- ************************************************************* -->

Andrew Whiteman - awhitem@umich.edu

Yvonne Lim - yvlim@umich.edu



