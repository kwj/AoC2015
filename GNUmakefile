SLN=AoC2015.slnx

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make restore'
	@echo '    restore dependencies'
	@echo '  make build'
	@echo '    compile all programs (with the release configuration)'
	@echo '  make clean'
	@echo '    remove build outputs'

.PHONY: restore
restore:
	@dotnet restore $(SLN)

.PHONY: build
build:
	@dotnet build $(SLN) -c Release --no-restore

.PHONY: clean
clean:
	@dotnet clean $(SLN) -c Release
	@find . -name '*~' -o -name '#*' | xargs -r rm
