all:
	clear
	cargo build --release

check:
	clear
	cargo clippy -- -W clippy::pedantic

flash:
	./flash.sh
