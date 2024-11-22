# Brillo

Brillo hides the pain of drawing simple vector graphics and raster images
behind a nice data type and a few display functions.

<table>
  <tr>
    <td>
      <img
        src="gloss-examples/picture/Visibility/screenshot.png"
        width="228"
        height="235"
      >
    </td>
    <td>
      <img
        src="gloss-examples/picture/Tree/screenshot.png"
        width="153"
        height="198"
      >
    </td>
    <td>
      <img
        src="gloss-examples/images/perspec.png"
        width="280"
        height="145"
      >
    </td>
  </tr>
</table>


## Example

Getting something on the screen is as easy as:

```hs
import Graphics.Gloss

main = display
  (InWindow "Nice Window" (200, 200) (10, 10))
  white
  (Circle 80)
```

Explore and run the [example projects](./gloss-examples/README.md)
with [stack](http://haskellstack.org):

```sh
stack run gloss-boids
```


## Usage

Once the window is open you can use the following:

- Quit
  - esc-key
- Move Viewport
  - arrow keys
  - left-click drag
- Zoom Viewport
  - page up/down-keys
  - control-left-click drag
  - right-click drag
  - mouse wheel
- Rotate Viewport
  - home/end-keys
  - alt-left-click drag
- Reset Viewport
  - 'r'-key


## More

- Animations and simulations can be constructed similarly
    using the `animate` and `simulate` functions
- Games with user input can be constructed with the `play` function.
- See the [gloss-examples](./gloss-examples/README.md) directory for more.


## Contributing

Contributions are very welcome!

Please test that all the `gloss-examples` still work
before submitting your pull request.
