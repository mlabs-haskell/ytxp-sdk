.PHONY: usage

usage:
	@echo "usage: make <command>"
	@echo
	@echo "Available commands:"
	@echo ""
	# Code format
	@echo "    format_lint                                                 -- Formats .hs, .cabal, .nix files and auto-refactors code"
	@echo "    format                                                      -- Formats .hs, .cabal, .nix files"
	@echo "    format_check                                                -- Check formatting of .hs, .cabal, .nix files"

	@echo "    format_haskell                                              -- Formats .hs files"
	@echo "    format_check_haskell                                        -- Check formatting of .hs files"
	@echo "    format_nix                                                  -- Formats .nix files"
	@echo "    format_check_nix                                            -- Check formatting of .nix files"
	@echo "    format_cabal                                                -- Formats .cabal files"
	@echo "    format_check_cabal                                          -- Check formatting of .cabal files"
	@echo "    lint                                                        -- Auto-refactors code"
	@echo "    lint_haskell                                                -- Auto-refactors code"
	@echo "    lint_haskell_check                                          -- Run code linting"
	@echo ""
	# Build
	@echo "    build_all                                                   -- Build all"
	@echo ""
	# Test
	@echo "    test_all                                                    -- Run all the tests"
	@echo "    test_all_dev                                                -- Run all the tests ignoring warning"
	@echo ""
	# Check Typos
	@echo "    typos_check                                                 -- Check typos"
	@echo "    typos_fix                                                   -- Fix typos"
	@echo ""
	# Doc
	@echo "    build_doc                                                   -- Build haddock documentation"
	@echo "    lint_markdown_check                                         -- Check markdownlint suggestions"
	@echo "    lint_markdown                                               -- Apply markdownlint suggestions"
	@echo ""

################################################################################
# Code

# Leave the cabal build directory and the legacy code submodule alone
FIND_EXCLUDE_PATH := -not -path '*/dist-*/*'

FIND_HASKELL_SOURCES := find -name '*.hs' $(FIND_EXCLUDE_PATH)
FIND_NIX_SOURCES := find -name '*.nix' $(FIND_EXCLUDE_PATH)
FIND_CABAL_SOURCES := find -name '*.cabal' $(FIND_EXCLUDE_PATH)
FIND_MARKDOWN_SOURCES := find -name '*.md' $(FIND_EXCLUDE_PATH)

# Runs as command on all results of the `find` call at one.
# e.g.
#   foo found_file_1 found_file_2
find_exec_all_fn = $(1) -exec $(2) {} +

# Runs a command on all results of the `find` call one-by-one
# e.g.
#   foo found_file_1
#   foo found_file_2
find_exec_one_by_one_fn = $(1) | xargs -i $(2) {}


.PHONY: format
format: format_haskell format_nix format_cabal
format_check : format_check_haskell format_check_nix format_check_cabal

# Run fourmolu of .hs files
.PHONY: format_haskell
format_haskell: 
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), fourmolu -i)

.PHONY: format_check_haskell
format_check_haskell:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), fourmolu --mode check)

# Run nixpkgs-fmt of .nix files
.PHONY: format_nix
format_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt)

.PHONY: format_check_nix
format_check_nix:
	$(call find_exec_all_fn, $(FIND_NIX_SOURCES), nixpkgs-fmt --check)

# Run cabal-fmt of .cabal files
.PHONY: format_cabal
format_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt -i)

.PHONY: format_check_cabal
format_check_cabal:
	$(call find_exec_all_fn, $(FIND_CABAL_SOURCES), cabal-fmt --check)


# Apply hlint suggestions
.PHONY: lint_haskell
lint_haskell:
	$(call find_exec_one_by_one_fn, $(FIND_HASKELL_SOURCES), hlint -j --refactor --refactor-options="-i")

# Check hlint suggestions
.PHONY: lint_haskell_check
lint_haskell_check:
	$(call find_exec_all_fn, $(FIND_HASKELL_SOURCES), hlint -j)

# Apply lint suggestions
.PHONY: lint
lint: lint_haskell lint_markdown

# Apply format and hlint
.PHONY: format_lint
format_lint: format lint

################################################################################
# Build
CABAL_YTXP_SDK := cd ytxp-sdk && cabal

.PHONY: build_all
build_all:
	$(CABAL_YTXP_SDK) build -j all

################################################################################
# Test
.PHONY: test_all
test_all:
	cabal test -j all --test-show-details=always

.PHONY: test_all_dev
test_all_dev:
	cabal test -j -fdev all --test-show-details=always

################################################################################
# Test
.PHONY: typos_check
typos_check :
	typos -c ./typos.toml ./

.PHONY: typos_fix
typos_fix:
	typos -w -c ./typos.toml ./

################################################################################
# Doc
.PHONY: build_doc
build_doc:
	$(CABAL_YTXP_SDK) haddock --haddock-all --haddock-quickjump

# Check markdownlint suggestions
.PHONY: lint_markdown_check
lint_markdown_check: 
	$(call find_exec_all_fn, $(FIND_MARKDOWN_SOURCES), markdownlint)

# Apply markdownlint suggestions.
# NOTE: there are some warnings that cannot be automatically fixed
.PHONY: lint_markdown
lint_markdown: 
	$(call find_exec_all_fn, $(FIND_MARKDOWN_SOURCES), markdownlint -f)
