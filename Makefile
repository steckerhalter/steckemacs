# Makefile for trying out steckemacs.el locally.
#
# Targets:
#   make          show this help
#   make run      start Emacs with steckemacs.el and your existing packages
#   make sandbox  start Emacs with a throwaway init dir (full install on first run)
#   make batch    load steckemacs.el in batch mode and print the startup output
#   make check    CI-style batch load; exits non-zero on failure
#   make clean    remove the sandbox and build artifacts
#
# Variables:
#   EMACS=...       Emacs binary to use (default: emacs)
#   EMACS_ARGS=...  extra arguments for `run' and `sandbox'
#   SANDBOX=...     sandbox init directory (default: .sandbox)
#
# To review a backtrace during startup:
#   make run EMACS_ARGS=--debug-init

EMACS      ?= emacs
INIT       ?= steckemacs.el
SANDBOX    ?= .sandbox
BUILD      ?= build
LOG        ?= $(BUILD)/startup.log
EMACS_ARGS ?=

.PHONY: help run sandbox batch check clean

help:
	@echo "Targets:"
	@echo "  make run      Start Emacs with $(INIT) and your existing packages"
	@echo "  make sandbox  Start Emacs with a throwaway init dir ($(SANDBOX))"
	@echo "  make batch    Load $(INIT) in batch mode, log to $(LOG)"
	@echo "  make check    CI-style batch load; exits non-zero on failure"
	@echo "  make clean    Remove $(SANDBOX) and $(BUILD)"
	@echo
	@echo "Variables: EMACS, INIT, SANDBOX, EMACS_ARGS (e.g. EMACS_ARGS=--debug-init)"

run:
	$(EMACS) -Q -l $(INIT) $(EMACS_ARGS)

sandbox:
	@mkdir -p $(SANDBOX)
	$(EMACS) -Q --init-directory=$(SANDBOX) -l $(INIT) $(EMACS_ARGS)

batch:
	@mkdir -p $(BUILD)
	@$(EMACS) -Q --batch \
	  --eval "(setq steckemacs--make-start (float-time))" \
	  -l $(INIT) \
	  --eval "(princ (format \"\ninit file loaded in %.2fs\n\" (- (float-time) steckemacs--make-start)))" \
	  >$(LOG) 2>&1; \
	status=$$?; \
	cat $(LOG); \
	echo; \
	if [ $$status -eq 0 ]; then \
	  echo "startup log saved to $(LOG)"; \
	else \
	  echo "startup FAILED (exit $$status); log saved to $(LOG)"; \
	fi; \
	exit $$status

check: batch
	@echo "batch load succeeded"

clean:
	rm -rf $(SANDBOX) $(BUILD)
