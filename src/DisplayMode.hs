module DisplayMode where

data DisplayMode =
      GlossWindow
    | GlossExport FilePath
    | SVGExport FilePath

