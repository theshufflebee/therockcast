# The RockCast: University Forecasting Project

A comprehensive bookdown environment for learning and applying forecasting methods using R.

## Overview

This repository contains all data related to a forecasting project on ECB Policy rates.

## Contents

- **Preface**: Introduction and book structure
- **Chapter 1**: tbd
- **Chapter 2**: tbd 
- **Chapter 3**: tbd
- **Chapter 4**: tbd
## Prerequisites

To work with this bookdown project, you'll need R Studio with:

- The following R packages:
  - `bookdown`
  - `rmarkdown` 
  - `knitr`

## Installation

```r
install.packages(c('bookdown', 'rmarkdown', 'knitr'))
```

## Usage

### Building the Book

To build the HTML version of the book:

```bash
# Navigate to project directory
cd therockcast

# Build the book
R -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
```
Set wd to base repo and run comand from there in R Studio consile

The generated book will be available in the `_book/` directory. Open `_book/index.html` in your web browser to check it out.

### Serving the Book Locally


### Building Other Formats

```bash
# Build PDF (requires LaTeX)
R -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"

# Build EPUB
R -e "bookdown::render_book('index.Rmd', 'bookdown::epub_book')"

# Build all formats
R -e "bookdown::render_book('index.Rmd')"
```

## Project Structure

```
therockcast/
├── index.Rmd              # Main entry point and preface
├── 01-intro.Rmd           # Introduction to Forecasting
├── 02-chapter-1.Rmd      # Time Series Fundamentals
├── 03-chapter-2.Rmd         # Forecasting Methods
├── 04-chapter-3.Rmd      # Model Evaluation and Selection
├── _bookdown.yml          # Bookdown configuration
├── _output.yml            # Output format configuration
├── book.bib               # Bibliography file
├── style.css              # Custom CSS styling
├── .gitignore             # Git ignore file
└── _book/                 # Generated book output (after building)
```

## Customization

### Adding New Chapters

1. Create a new `.Rmd` file (e.g., `05-applications.Rmd`)
2. Update `_bookdown.yml` to include the new file:
```yaml
rmd_files: [
  "index.Rmd",
  "01-intro.Rmd",
  "02-chapter-1.Rmd", 
  "03-chapter-2.Rmd",
  "04-chapter-3.Rmd",
  "05-applications.Rmd"
]
```
3. Rebuild the book

### Modifying Styling

Edit `style.css` to customize the appearance of your book.

### Adding Bibliography

Add references to `book.bib` in BibTeX format and cite them in your chapters using `[@reference-key]`.

## Development

### Live Preview

For development with live preview:

```bash
# Install the servr package
R -e "install.packages('servr')"

# Serve with live reload
R -e "bookdown::serve_book()"
```

This will automatically rebuild and refresh the book when you make changes to the source files.

## Troubleshooting

### Common Issues

1. **Pandoc not found**: Ensure Pandoc is installed and in your PATH
2. **R packages missing**: Install required packages using the installation commands above
3. **Permission errors**: Use `sudo` for system-wide package installation on Linux/macOS
4. **Build errors**: Check that all `.Rmd` files have valid YAML frontmatter and R code

### Getting Help

- [Bookdown documentation](https://bookdown.org/yihui/bookdown/)
- [R Markdown documentation](https://rmarkdown.rstudio.com/)
- [RStudio Community](https://community.rstudio.com/)


## License

None
