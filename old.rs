#![no_std]
#![no_main]

use panic_halt as _;
use stm32f4::stm32f401::{self, interrupt};
use cortex_m::asm::delay;

#[cortex_m_rt::entry]
fn start() -> ! {
	let device_peripherals = stm32f401::Peripherals::take().unwrap();
	let rcc = &device_peripherals.RCC;
	let gpioc = &device_peripherals.GPIOC;

	rcc.ahb1enr.modify(|_, w| w.gpiocen().enabled());
	gpioc.moder.modify(|_, w| w.moder13().output());

	loop {
		gpioc.bsrr.write(|w| w.bs13().set());

		{
			let mut i = 0;
			while i < 100000 {
				cortex_m::asm::nop();
				i += 1;
			}
		}

		//delay(100);
		gpioc.bsrr.write(|w| w.br13().reset());
		//delay(100);

		{
			let mut i = 0;
			while i < 100000 {
				cortex_m::asm::nop();
				i += 1;
			}
		}
	}
}
