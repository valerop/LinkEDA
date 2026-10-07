# LinkEDA website

This directory contains the Quarto website for LinkEDA. The current R package and public launcher are also named `LinkEDA`; the source repository URL retains an earlier technical identifier.

## Requirements

- [Quarto](https://quarto.org/) 1.8 or newer

## Preview locally

From the repository root:

```sh
quarto preview website
```

Or from this directory:

```sh
quarto preview .
```

## Render the site

From the repository root:

```sh
quarto render website
```

Or from this directory:

```sh
quarto render .
```

The generated website is written to `website/_site/` and is not source content.

The website's original documentation material is published under CC BY-NC-ND
4.0. Code excerpts remain under GPL version 3 or later. See `license.qmd` and
the repository's `LICENSE-DOCUMENTATION.md` for scope and attribution.

## Editing notes

- Page content lives in the top-level `.qmd` files.
- Shared visual styling is in `assets/styles.css`.
- The real application icon is `assets/LinkEDA.png`.
- Verified native captures and the contextual-menu animation are under `assets/screenshots/`.
- Download buttons point to immutable assets in the GitHub Release tagged
  `v0.0.113R`; installers are not stored in the Git repository.
- The Quarto configuration uses the public repository and GitHub Pages URLs.

## GitHub Pages

The repository workflow in `.github/workflows/publish-website.yml` installs
Quarto, renders this directory and deploys `website/_site/` to GitHub Pages.
