testfn?=main

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

main: ## Run main function
	ghcid

test: ## Run tests in development mode i.e. run tests on each change
	ghcid \
	    --command "stack ghci dicta:lib dicta:dicta-test --test --ghci-options=-fobject-code" \
	    --test="${testfn}"

repl: ## Run in development mode i.e. run tests on each change
	ghcid \
	    --command "stack ghci dicta:lib --ghci-options=-fobject-code"

hoogle: ## Generate local documentation and start local hoogle
	stack hoogle -- generate --local && stack hoogle -- server --local --port=8888

brittany-inline: ## Run formatter
	brittany --write-mode inplace **/*.hs

hlint: ## Run linter
	hlint . --report

.PHONY: test help hoogle main brittany-inline repl hlint
