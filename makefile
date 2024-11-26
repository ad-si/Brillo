.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack run brillo-bitmap brillo-examples/picture/Bitmap/lena-101x101.bmp && \
	stack run brillo-boids && \
	stack run brillo-clock && \
	stack run brillo-color && \
	stack run brillo-conway && \
	stack run brillo-draw && \
	stack run brillo-easy && \
	stack run brillo-eden && \
	stack run brillo-flake && \
	stack run brillo-gameevent && \
	stack run brillo-graph && \
	stack run brillo-gravity && \
	stack run brillo-hello && \
	stack run brillo-lifespan && \
	stack run brillo-machina && \
	stack run brillo-occlusion && \
	stack run brillo-styrene && \
	stack run brillo-tree && \
	stack run brillo-visibility && \
	stack run brillo-zen && \
	stack run brillo-render  # Must be last as it can't be closed


.PHONY: format
format:
	fourmolu --mode=inplace .
	find . -iname '*.cabal' | xargs cabal-fmt --inplace


.PHONY: docs
docs:
		stack haddock --haddock-for-hackage


.PHONY: release
release: docs
	stack upload brillo
	stack upload --documentation brillo

	stack upload brillo-algorithms
	stack upload --documentation brillo-algorithms

	stack upload brillo-examples
	# stack upload --documentation brillo-examples

	stack upload brillo-juicy
	stack upload --documentation brillo-juicy

	stack upload brillo-rendering
	stack upload --documentation brillo-rendering


.PHONY: clean
clean:
	-rm -rf .stack-work
