
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
	@dotnet restore AoC2015.sln

.PHONY: build
build:
	@dotnet build AoC2015.sln -c Release --no-restore

.PHONY: clean
clean:
	@dotnet clean AoC2015.sln -c Release
	@find . -name '*~' -o -name '#*' | xargs -r rm
