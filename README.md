# The RockCast: University Forecasting Project

A comprehensive bookdown environment for learning and applying forecasting methods using R.

## Overview

This repository contains a complete bookdown project designed for university-level forecasting education. The book covers fundamental concepts, methodologies, and practical applications of forecasting using R.

## Contents

- **Preface**: Introduction and book structure
- **Chapter 1**: Introduction to Forecasting
- **Chapter 2**: Time Series Fundamentals  
- **Chapter 3**: Forecasting Methods
- **Chapter 4**: Model Evaluation and Selection

## Prerequisites

To work with this bookdown project, you'll need:

- R (version 4.0 or higher)
- Pandoc
- The following R packages:
  - `bookdown`
  - `rmarkdown` 
  - `knitr`

## Installation

### Ubuntu/Debian
```bash
# Install R and Pandoc
sudo apt update
sudo apt install r-base r-base-dev pandoc

# Install R packages (choose one method)

# Method 1: Via apt (recommended)
sudo apt install r-cran-bookdown r-cran-rmarkdown r-cran-knitr

# Method 2: Via R console
R -e "install.packages(c('bookdown', 'rmarkdown', 'knitr'), repos='https://cloud.r-project.org/')"
```

### macOS
```bash
# Install using Homebrew
brew install r pandoc

# Install R packages
R -e "install.packages(c('bookdown', 'rmarkdown', 'knitr'))"
```

### Windows
1. Install R from [CRAN](https://cran.r-project.org/bin/windows/base/)
2. Install Pandoc from [pandoc.org](https://pandoc.org/installing.html)
3. Install R packages:
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

The generated book will be available in the `_book/` directory. Open `_book/index.html` in your web browser.

### Serving the Book Locally

To serve the book with a local web server:

```bash
# Navigate to the book output directory
cd _book

# Start a simple HTTP server (Python 3)
python3 -m http.server 8000

# Or using Python 2
python -m SimpleHTTPServer 8000

# Or using R
R -e "servr::httd('.')"
```

Then visit `http://localhost:8000` in your browser.

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
├── 02-timeseries.Rmd      # Time Series Fundamentals
├── 03-methods.Rmd         # Forecasting Methods
├── 04-evaluation.Rmd      # Model Evaluation and Selection
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
  "02-timeseries.Rmd", 
  "03-methods.Rmd",
  "04-evaluation.Rmd",
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

## Contributing

Feel free to contribute to this project by:

- Adding new content or chapters
- Improving existing explanations
- Fixing typos or errors
- Enhancing the styling
- Adding examples and exercises

## License

This project is open source and available under the [MIT License](LICENSE).
