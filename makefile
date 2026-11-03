.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: test
test:
	stack test


.PHONY: test-examples
test-examples:
	stack run brillo-bitmap brillo-examples/picture/Bitmap/lena-101x101.bmp && \
	stack run brillo-compressed-bitmap && \
	stack run brillo-boids && \
	stack run brillo-clock && \
	stack run brillo-color && \
	stack run brillo-conway && \
	stack run brillo-draw && \
	stack run brillo-easy && \
	stack run brillo-eden && \
	stack run brillo-export && \
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
	stack run brillo-truetype && \
	stack run brillo-visibility && \
	stack run brillo-zen && \
	stack run brillo-gui && \
	stack run brillo-pickfiles && \
	stack run brillo-antialiasing && \
	stack run brillo-circle-comparison && \
	stack run brillo-dropfiles && \
	stack run brillo-interaction && \
	stack run brillo-lines && \
	stack run brillo-polygons && \
	stack run brillo-2048 && \
	stack run brillo-tetris && \
	stack run brillo-chess && \
	stack run brillo-tictactoe && \
	stack run brillo-canvas-editor && \
	stack run brillo-fule-layout && \
	stack run brillo-snake && \
	stack run brillo-pacman && \
	stack run brillo-render  # Must be last as it can't be closed


.PHONY: format
format:
	fourmolu --mode=inplace $$(git ls-files '*.hs' | grep -v 'Brillo/Export/Image.hs')
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
	# stack upload --documentation brillo-juicy

	stack upload brillo-rendering
	stack upload --documentation brillo-rendering

	stack upload brillo-export
	# stack upload --documentation brillo-export

	$(MAKE) github-release


VERSION := $(shell awk '/^version:/ {print $$2; exit}' brillo/brillo.cabal)

.PHONY: github-release
github-release:
	@notes=$$(awk -v v="$(VERSION)" \
		'$$0 ~ "^## .* - " v "$$" {f=1; next} /^## / {f=0} f' CHANGELOG.md \
		| perl -0pe 's/\n[ ]+(?!- )(?=\S)/ /g; s/\A\s+|\s+\z//g'); \
	if test -z "$$notes"; \
	then echo "No CHANGELOG.md section for $(VERSION)" >&2; exit 1; \
	fi; \
	gh release create "v$(VERSION)" \
		--verify-tag \
		--title "Brillo $$(echo $(VERSION) | sed 's/\.0$$//')" \
		--notes "$$notes"


.PHONY: clean
clean:
	-rm -rf .stack-work
