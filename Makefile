# Borba CAD - developer workflow. Run `make` or `make help` for the list of targets.
# Every target fails loudly: nothing here hides an error or needs a manual REPL step.

SHELL := /bin/bash
.SHELLFLAGS := -euo pipefail -c
.DEFAULT_GOAL := help

ROOT := $(patsubst %/,%,$(dir $(abspath $(firstword $(MAKEFILE_LIST)))))

# --- Configuration (override on the command line, e.g. `make test SBCL=/opt/sbcl/bin/sbcl`) ---
SBCL               ?= sbcl
QUICKLISP_SETUP    ?= $(HOME)/quicklisp/setup.lisp
QL_DIST_VERSION    := 2026-01-01
DYNAMIC_SPACE_SIZE ?= 4GB
SUITE              ?= ALL
LOG_DIR            ?= $(ROOT)/var/log
# In an MSYS2 MINGW64 shell the program is a Windows one, and its file name ends in .exe.
EXE_SUFFIX         := $(if $(findstring MINGW,$(shell uname -s)),.exe,)
EXECUTABLE         := $(ROOT)/build/borba-cad$(EXE_SUFFIX)
VERSION            := $(shell awk -F'"' '/:version/ {print $$2; exit}' $(ROOT)/borba-cad.asd)
DIST_NAME          := borba-cad-$(VERSION)-linux-x86_64
SCREENSHOT         ?= $(ROOT)/var/screenshots/demo.png
REPL_SYSTEMS       ?= borba-cad borba-cad/platform borba-cad/tests borba-cad/bench
# Options of the program itself: `make run FILE=drawings/a.bcad LOG_LEVEL=debug ARGS="--log-stderr"`.
RUN_ARGS            = $(if $(FILE),--file $(FILE)) $(if $(LOG_LEVEL),--log-level $(LOG_LEVEL)) $(ARGS)
BENCH_ARGS         ?=

# Runtime options must precede toplevel options for SBCL.
SBCL_RUNTIME := $(SBCL) --dynamic-space-size $(DYNAMIC_SPACE_SIZE) --noinform
LISP_LOAD    := --no-sysinit --no-userinit --load $(QUICKLISP_SETUP) --load $(ROOT)/scripts/bootstrap.lisp
LISP_BATCH   := $(SBCL_RUNTIME) --non-interactive $(LISP_LOAD)
# Interactive sessions use rlwrap for line editing when it is installed.
RLWRAP       := $(shell command -v rlwrap 2>/dev/null)

# Lisp snippets: the system is loaded without output, then the entry point runs. The program's own
# options follow --end-toplevel-options.
define run-system
--eval '(borba-cad/scripts:quickload-quietly "$(1)")' --eval '(uiop:symbol-call :$(2) :main)' --end-toplevel-options
endef

# The targets of `check` share build/; running them in parallel would corrupt the compiled files.
.NOTPARALLEL:

.PHONY: help check-env deps compile build lint test test-gui check all \
        run demo debug repl exe run-exe dist dist-check dist-windows \
        bench bench-quick bench-gui bench-baseline \
        logs-tail logs-summary logs-clean example docs version \
        clean distclean \
        ensure-quicklisp ensure-display

##@ Start here

help: ## Show this help
	@awk 'BEGIN {FS = ":.*## "} \
		/^##@ / {printf "\n%s\n", substr($$0, 5); next} \
		/^[a-zA-Z0-9_-]+:.*## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
	@printf '\nOptions: SUITE=name FILE=path LOG_LEVEL=debug ARGS="..." BENCH_ARGS="..." SBCL=path (see the top of the Makefile)\n'

check-env: ## Check SBCL, Quicklisp, native libraries, fonts and the display session
	@$(ROOT)/scripts/check-env.sh "$(QUICKLISP_SETUP)" "$(QL_DIST_VERSION)"

deps: ensure-quicklisp ## Download and compile third-party dependencies, list the versions in use
	$(LISP_BATCH) --eval '(borba-cad/scripts:install-dependencies)'

version: ## Print the project version
	@awk -F'"' '/:version/ {print $$2; exit}' $(ROOT)/borba-cad.asd

##@ Build and verify

compile: ensure-quicklisp ## Strict rebuild of every system: fails on any compiler warning
	$(LISP_BATCH) --eval '(borba-cad/scripts:build-strict)'

build: compile exe ## Strict rebuild, then the standalone executable build/borba-cad

lint: ensure-quicklisp ## Static checks: sources, docstrings of every exported symbol, dependencies
	$(LISP_BATCH) --load $(ROOT)/scripts/lint.lisp --eval '(borba-cad/scripts:lint)'

test: ensure-quicklisp ## Headless correctness tests (SUITE=name for one suite)
	$(LISP_BATCH) --eval '(borba-cad/scripts:run-tests :suite "$(SUITE)")'

test-gui: ensure-quicklisp ## Window-program tests; renderer tests skip with a message without a display
	$(LISP_BATCH) --eval '(borba-cad/scripts:run-platform-tests :suite "$(SUITE)")'

check: compile lint test test-gui ## The whole gate: strict build, lint, headless and window tests

all: check exe ## check, then build the standalone executable

##@ Run and debug

run: ensure-quicklisp ensure-display ## Run the program (FILE=, LOG_LEVEL=, ARGS=)
	$(LISP_BATCH) $(call run-system,borba-cad/platform,borba-cad/platform) $(RUN_ARGS)

demo: ensure-quicklisp ensure-display ## Scripted demo; leaves a screenshot in var/screenshots/
	@mkdir -p "$(dir $(SCREENSHOT))"
	$(LISP_BATCH) $(call run-system,borba-cad/platform,borba-cad/platform) \
		--demo --exit-after-demo --screenshot "$(SCREENSHOT)" $(RUN_ARGS)
	@echo "Screenshot: $(SCREENSHOT)"

debug: ensure-quicklisp ensure-display ## Run with the Lisp debugger on errors and debug logging on stderr
	BORBA_CAD_DEBUG=1 $(RLWRAP) $(SBCL_RUNTIME) $(LISP_LOAD) \
		$(call run-system,borba-cad/platform,borba-cad/platform) \
		--log-level debug --log-stderr $(RUN_ARGS)

repl: ensure-quicklisp ## Interactive REPL with the project loaded (REPL_SYSTEMS="...")
	$(RLWRAP) $(SBCL_RUNTIME) $(LISP_LOAD) --load $(ROOT)/scripts/tools.lisp \
		--eval '(borba-cad/scripts:start-repl :systems (quote ($(foreach s,$(REPL_SYSTEMS),"$(s)"))))'

exe: ensure-quicklisp ## Build the standalone executable build/borba-cad
	$(LISP_BATCH) --load $(ROOT)/scripts/tools.lisp --eval '(borba-cad/scripts:build-executable :output "build/borba-cad$(EXE_SUFFIX)")'
	@ls -lh "$(EXECUTABLE)"

run-exe: exe ensure-display ## Build, then run the standalone executable (FILE=, LOG_LEVEL=, ARGS=)
	"$(EXECUTABLE)" $(RUN_ARGS)

dist: exe ## Linux distribution in dist/: a folder to double-click and its .tar.gz
	$(ROOT)/scripts/dist-linux.sh "$(EXECUTABLE)" "$(ROOT)/dist" "$(VERSION)"

dist-check: dist ensure-display ## Start the distribution in a clean environment and check it uses its own libraries and fonts
	$(ROOT)/scripts/verify-dist.sh "$(ROOT)/dist/$(DIST_NAME)"

dist-windows: exe ## Windows distribution in dist/ (run in an MSYS2 MINGW64 shell; never run on Windows yet)
	$(ROOT)/scripts/dist-windows.sh "$(EXECUTABLE)" "$(ROOT)/dist" "$(VERSION)"

logs-tail: ## Follow the current log file (Ctrl-C to stop)
	@test -f "$(LOG_DIR)/borba-cad.log" || { echo "No log yet at $(LOG_DIR)/borba-cad.log; run the program first."; exit 1; }
	tail -n 40 -F "$(LOG_DIR)/borba-cad.log"

logs-summary: ensure-quicklisp ## Summarize the logs: counts, timings, frames, GC (LOG_DIR=)
	$(LISP_BATCH) --load $(ROOT)/scripts/tools.lisp --eval '(borba-cad/scripts:logs-summary :directory "$(LOG_DIR)")'

logs-clean: ## Delete the log files in LOG_DIR
	rm -f "$(LOG_DIR)"/borba-cad.log "$(LOG_DIR)"/borba-cad.log.*

##@ Performance

bench: ensure-quicklisp ## Headless benchmarks W0-W3 -> bench/reports/ (BENCH_ARGS="--workloads w1,w2")
	$(LISP_BATCH) $(call run-system,borba-cad/bench,borba-cad/bench) $(BENCH_ARGS)

bench-quick: ensure-quicklisp ## Short benchmark run: proves the harness works, numbers are not comparable
	$(LISP_BATCH) $(call run-system,borba-cad/bench,borba-cad/bench) --quick --workloads w0,w1 $(BENCH_ARGS)

bench-gui: ensure-quicklisp ensure-display ## Frame benchmark in a real window (BENCH_ARGS="--workloads w2 --frames 300")
	$(LISP_BATCH) $(call run-system,borba-cad/bench-gui,borba-cad/bench-gui) $(BENCH_ARGS)

bench-baseline: ensure-quicklisp ensure-display ## Record the baseline: full headless and windowed runs into bench/reports/baseline/
	@mkdir -p "$(ROOT)/bench/reports/baseline"
	$(LISP_BATCH) $(call run-system,borba-cad/bench,borba-cad/bench) --output "$(ROOT)/bench/reports/baseline"
	$(LISP_BATCH) $(call run-system,borba-cad/bench-gui,borba-cad/bench-gui) --output "$(ROOT)/bench/reports/baseline"

##@ Documentation and examples

example: ensure-quicklisp ## Regenerate examples/floor-plan.bcad with the program's own demo
	$(LISP_BATCH) --load $(ROOT)/scripts/tools.lisp --eval '(borba-cad/scripts:make-example)'

docs: ensure-quicklisp ## Regenerate docs/reference.md from the docstrings
	$(LISP_BATCH) --load $(ROOT)/scripts/tools.lisp --eval '(borba-cad/scripts:generate-api-reference)'

##@ Cleaning

clean: ## Remove compiled files, the executable (build/) and the distribution (dist/)
	rm -rf "$(ROOT)/build" "$(ROOT)/dist"

distclean: clean ## Also remove logs, screenshots, scratch drawings and non-baseline benchmark reports
	rm -rf "$(ROOT)/var" "$(ROOT)/spikes/out"
	@if [ -d "$(ROOT)/bench/reports" ]; then \
		find "$(ROOT)/bench/reports" -mindepth 1 -maxdepth 1 ! -name baseline ! -name .gitkeep -exec rm -rf {} +; \
	fi

# --- Guards ---

ensure-quicklisp:
	@test -f "$(QUICKLISP_SETUP)" || { \
		echo "ERROR: Quicklisp not found at $(QUICKLISP_SETUP)."; \
		echo "Install it or pass QUICKLISP_SETUP=/path/to/setup.lisp (see docs/development.md)."; \
		exit 1; }

ensure-display:
	@if [ -z "$${WAYLAND_DISPLAY:-}$${DISPLAY:-}" ]; then \
		echo "ERROR: no graphical session (WAYLAND_DISPLAY and DISPLAY are unset)."; \
		echo "This target opens a window; run it inside a desktop session. 'make check-env' lists what is available."; \
		exit 1; \
	fi
