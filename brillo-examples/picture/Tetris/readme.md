# Tetris

Haskell Tetris game implementation.

Originally implemented at https://github.com/alextcn/tetris by
[Ambartsumyan Vladislav](https://github.com/vladambartsumyan),
[Anton Ermolinsky](https://github.com/OQJAV),
and [Alexander Tkachenko](skyalexx@gmail.com).


## Project structure

- `src/` directory contains source code files (each file is module).
- `app/` directory must contains files to be executable.
    In our case there is only `main :: IO ()`
- `test/` directory doesn't used.
- `tetris.cabal` – package configuration file
    (each project can have multiple packages).
    This file specifies which packages are dependencies.
- `stack.yaml` – project configuration file.
    This file specifies which packages are available to be used
    and which packages to include (`packages` property).
    `extra-deps` property specifies additional dependencies
    that are not in [LTS](https://www.stackage.org/lts-3.17).
