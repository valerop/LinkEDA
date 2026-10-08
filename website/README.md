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

## Editing notes

- Page content lives in the top-level `.qmd` files.
- Shared visual styling is in `assets/styles.css`.
- The real application icon is `assets/LinkEDA.png`.
- Verified native captures and the contextual-menu animation are under `assets/screenshots/`.
- Download buttons point to installer assets attached to GitHub Releases;
  installers are not stored in the Git repository.
- The Quarto configuration sets the repository URL and basic Open Graph metadata. Update `site-url` in `_quarto.yml` when the final public URL is known.

## GitHub Pages

No workflow is added here because the repository’s existing automation must remain independent from the application build. A Pages workflow should install Quarto, run `quarto render website`, and deploy `website/_site/`.
