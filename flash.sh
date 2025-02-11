arm-none-eabi-objcopy -O binary target/thumbv7em-none-eabihf/release/stm32f4-demo target.bin
dfu-util -d 0483:df11 -a 0 -s 0x08000000:leave -D target.bin
