# HCfunctions

HCfunctions is an R package that provides useful functions for data processing and analysis, including PDF export capabilities and Scrublet for doublet detection in single-cell RNA-seq data.

## Installation

```r
# Install the development version from GitHub
devtools::install_github('HChaoLab/HCfunctions')
```

## Features

- **ExportPDF**: Export plots and data to PDF format with optional TIFF conversion
- **Scrublet**: Doublet detection for single-cell RNA-seq data
- Additional utility functions for common analysis tasks

## Functions

### ExportPDF
```r
# Export plots and data to PDF
ExportPDF_TiffTolocal_withData()
```

### Scrublet
```r
# Run Scrublet for doublet detection
run_scrublet()
run_scrublet_seurat()
```

### Hello
```r
# Example function
hello()
```

## Usage

After installation, load the package with:

```r
library(HCfunctions)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License.
