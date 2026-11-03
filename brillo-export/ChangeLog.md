# Changelog for brillo-export

## 0.2.1.0

* Support brillo and brillo-rendering 3.0
* SVG export: emit a comment for custom `Shader` pictures,
  as they can't be represented in SVG
* SVG export: omit GPU-compressed bitmaps (with a comment)
  instead of misinterpreting their data as RGBA pixels

## 0.1.0.4

* Forked from gloss-export
* Renamed all modules from Graphics.Gloss.Export to Brillo.Export
* Updated to use Brillo types instead of Gloss types
* Changed dependency from gloss-rendering to brillo-rendering
* Updated all function names (withGlossState -> withBrilloState)
* Updated version bounds for modern dependencies

## Original gloss-export

See https://gitlab.com/timo-a/gloss-export for the original library history
