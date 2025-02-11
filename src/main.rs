#![no_main]
#![no_std]

use panic_halt as _;

use keyberon::key_code::KbHidReport;
use rtic::app;
use stm32f4xx_hal::gpio::{self};
use stm32f4xx_hal::otg_fs::{UsbBusType, USB};
use stm32f4xx_hal::prelude::*;
use stm32f4xx_hal::{pac, timer};
use usb_device::bus::UsbBusAllocator;
use usb_device::class::UsbClass as _;
use keyberon::key_code::KeyCode;
use cortex_m::asm::delay;

type UsbClass = keyberon::Class<'static, UsbBusType, Leds>;
type UsbDevice = usb_device::device::UsbDevice<'static, UsbBusType>;

pub struct Leds {
	caps_lock: gpio::gpioc::PC13<gpio::Output<gpio::PushPull>>,
}
impl keyberon::keyboard::Leds for Leds {
	fn caps_lock(&mut self, status: bool) {
		if status {
			self.caps_lock.set_low()
		} else {
			self.caps_lock.set_high()
		}
	}
}

#[app(device = stm32f4xx_hal::pac, peripherals = true)]
mod app {
	use super::*;

	#[shared]
	struct Shared {
		usb_dev: UsbDevice,
		usb_class: UsbClass,
	}

	#[local]
	struct Local {
		myled: stm32f4xx_hal::gpio::Pin<'B', 0, stm32f4xx_hal::gpio::Output>
	}

	#[init(local = [bus: Option<UsbBusAllocator<UsbBusType>> = None, ep_mem: [u32; 1024] = [0; 1024]])]
	fn init(c: init::Context) -> (Shared, Local, init::Monotonics) {
		let rcc = c.device.RCC.constrain();
		let clocks = rcc
			.cfgr
			.use_hse(25.MHz())
			.sysclk(84.MHz())
			.require_pll48clk()
			.freeze();
		let gpioa = c.device.GPIOA.split();
		let gpiob = c.device.GPIOB.split();
		let gpioc = c.device.GPIOC.split();

		let mut myled = gpiob.pb0.into_push_pull_output();
		myled.set_high();

		let mut led = gpioc.pc13.into_push_pull_output();
		led.set_low();
		let leds = Leds { caps_lock: led };

		let usb = USB::new(
			(
				c.device.OTG_FS_GLOBAL,
				c.device.OTG_FS_DEVICE,
				c.device.OTG_FS_PWRCLK,
			),
			(gpioa.pa11, gpioa.pa12),
			&clocks,
		);
		*c.local.bus = Some(UsbBusType::new(usb, c.local.ep_mem));
		let usb_bus = c.local.bus.as_ref().unwrap();

		let usb_class = keyberon::new_class(usb_bus, leds);
		let usb_dev = keyberon::new_device(usb_bus);

		(
			Shared { usb_dev, usb_class },
			Local {
				myled
			},
			init::Monotonics(),
		)
	}

	#[task(binds = OTG_FS, priority = 2, shared = [usb_dev, usb_class])]
	fn usb_tx(c: usb_tx::Context) {
		(c.shared.usb_dev, c.shared.usb_class).lock(|usb_dev, usb_class| {
			if usb_dev.poll(&mut [usb_class]) {
				usb_class.poll();
			}
		})
	}

	#[idle(shared = [usb_class], local = [myled])]
	fn idle(mut c: idle::Context) -> ! {
		loop {
			{
				let mut report: KbHidReport = KbHidReport::default();
				report.pressed(KeyCode::A);
				if c.shared
					.usb_class
					.lock(|k| k.device_mut().set_keyboard_report(report.clone()))
				{
					while let Ok(0) = c.shared.usb_class.lock(|k| k.write(report.as_bytes())) {}
				}

				c.local.myled.set_high();
				delay(1000);
			}

			{
				let mut report: KbHidReport = KbHidReport::default();
				if c.shared
					.usb_class
					.lock(|k| k.device_mut().set_keyboard_report(report.clone()))
				{
					while let Ok(0) = c.shared.usb_class.lock(|k| k.write(report.as_bytes())) {}
				}

				c.local.myled.set_low();
				delay(1000);
			}
		}
	}
}
