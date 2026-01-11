# Brillo Export Example

This example demonstrates how to export Brillo pictures to various image formats using the `brillo-export` library.


## Features

The example creates several geometric shapes and exports them as:
- **PNG** files (Portable Network Graphics)
- **BMP** files (Bitmap)
- **TGA** files (Targa)
- **TIFF** files (Tagged Image File Format)
- **Animated GIF** (Graphics Interchange Format)


## Running the Example

```bash
stack run brillo-export
```

This will create an `output` directory with the following files:
- `sample.png` - A simple picture with colored shapes
- `sample.bmp` - Same picture in Bitmap format
- `sample.tga` - Same picture in TGA format
- `sample.tiff` - Same picture in TIFF format
- `tree.png` - A fractal tree on a dark background
- `animation.gif` - An animated GIF showing rotating shapes with color cycling


## Implementation Notes

The `brillo-export` library is a fork of `gloss-export` adapted to work directly with Brillo types. You can use regular Brillo constructors and functions without any type conversions.


## Key Functions

- `exportPictureToPNG` - Export a single picture to PNG
- `exportPictureToBitmap` - Export to BMP format
- `exportPictureToTga` - Export to TGA format
- `exportPictureToTiff` - Export to TIFF format
- `exportPicturesToGif` - Export multiple pictures as an animated GIF


## Example Usage

```haskell
import Brillo
import Brillo.Export

-- Create a picture
myPicture :: Picture
myPicture = Color red $ Circle 80

-- Export it to PNG
main = exportPictureToPNG (400, 400) white "output.png" myPicture
```


## Limitations

- The canvas is limited to the screen's resolution
- No screen is displayed during the export process
- The library uses OpenGL internally for rendering
