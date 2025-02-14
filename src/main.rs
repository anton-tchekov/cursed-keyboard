#![no_main]
#![no_std]

use panic_halt as _;

use keyberon::key_code::KbHidReport;
use rtic::app;
use stm32f4xx_hal::gpio::{self};
use stm32f4xx_hal::otg_fs::{UsbBusType, USB};
use stm32f4xx_hal::prelude::*;
use usb_device::bus::UsbBusAllocator;
use usb_device::class::UsbClass as _;
use keyberon::key_code::KeyCode;
use cortex_m::asm::delay;
use rtic::Mutex;

type UsbClass = keyberon::Class<'static, UsbBusType, Leds>;
type UsbDevice = usb_device::device::UsbDevice<'static, UsbBusType>;

pub struct Leds {
	caps_lock: gpio::gpioc::PC13<gpio::Output<gpio::PushPull>>,
}

impl keyberon::keyboard::Leds for Leds {
	fn caps_lock(&mut self, status: bool) {
		if status {
			self.caps_lock.set_low();
		} else {
			self.caps_lock.set_high();
		}
	}
}

fn shitty_delay_ms(ms: u32) {
	delay(84000 * ms);
}

fn report_gen(kc: KeyCode, m: Option<KeyCode>) -> KbHidReport {
	let mut report = KbHidReport::default();
	report.pressed(kc);
	if let Some(v) = m {
		report.pressed(v);
	}

	report
}

fn german_char(c: char) -> Option<KbHidReport> {
	match c {
		' ' => Some(report_gen(KeyCode::Space, None)),
		'!' => Some(report_gen(KeyCode::Kb1, Some(KeyCode::LShift))),
		'"' => Some(report_gen(KeyCode::Kb2, Some(KeyCode::LShift))),
		'#' => Some(report_gen(KeyCode::Bslash, None)),
		'$' => Some(report_gen(KeyCode::Kb4, Some(KeyCode::LShift))),
		'%' => Some(report_gen(KeyCode::Kb5, Some(KeyCode::LShift))),
		'&' => Some(report_gen(KeyCode::Kb6, Some(KeyCode::LShift))),
		'\'' => Some(report_gen(KeyCode::Bslash, Some(KeyCode::LShift))),
		'(' => Some(report_gen(KeyCode::Kb8, Some(KeyCode::LShift))),
		')' => Some(report_gen(KeyCode::Kb9, Some(KeyCode::LShift))),
		'*' => Some(report_gen(KeyCode::RBracket, Some(KeyCode::LShift))),
		'+' => Some(report_gen(KeyCode::RBracket, None)),
		',' => Some(report_gen(KeyCode::Comma, None)),
		'-' => Some(report_gen(KeyCode::Slash, None)),
		'.' => Some(report_gen(KeyCode::Dot, None)),
		'/' => Some(report_gen(KeyCode::Kb7, Some(KeyCode::LShift))),

		'0' => Some(report_gen(KeyCode::Kb0, None)),
		'1' => Some(report_gen(KeyCode::Kb1, None)),
		'2' => Some(report_gen(KeyCode::Kb2, None)),
		'3' => Some(report_gen(KeyCode::Kb3, None)),
		'4' => Some(report_gen(KeyCode::Kb4, None)),
		'5' => Some(report_gen(KeyCode::Kb5, None)),
		'6' => Some(report_gen(KeyCode::Kb6, None)),
		'7' => Some(report_gen(KeyCode::Kb7, None)),
		'8' => Some(report_gen(KeyCode::Kb8, None)),
		'9' => Some(report_gen(KeyCode::Kb9, None)),

		':' => Some(report_gen(KeyCode::Dot, Some(KeyCode::LShift))),
		';' => Some(report_gen(KeyCode::Comma, Some(KeyCode::LShift))),
		'<' => Some(report_gen(KeyCode::NonUsBslash, None)),
		'=' => Some(report_gen(KeyCode::Kb0, Some(KeyCode::LShift))),
		'>' => Some(report_gen(KeyCode::NonUsBslash, Some(KeyCode::LShift))),
		'?' => Some(report_gen(KeyCode::Minus, Some(KeyCode::LShift))),
		'@' => Some(report_gen(KeyCode::Q, Some(KeyCode::RAlt))),

		'A' => Some(report_gen(KeyCode::A, Some(KeyCode::LShift))),
		'B' => Some(report_gen(KeyCode::B, Some(KeyCode::LShift))),
		'C' => Some(report_gen(KeyCode::C, Some(KeyCode::LShift))),
		'D' => Some(report_gen(KeyCode::D, Some(KeyCode::LShift))),
		'E' => Some(report_gen(KeyCode::E, Some(KeyCode::LShift))),
		'F' => Some(report_gen(KeyCode::F, Some(KeyCode::LShift))),
		'G' => Some(report_gen(KeyCode::G, Some(KeyCode::LShift))),
		'H' => Some(report_gen(KeyCode::H, Some(KeyCode::LShift))),
		'I' => Some(report_gen(KeyCode::I, Some(KeyCode::LShift))),
		'J' => Some(report_gen(KeyCode::J, Some(KeyCode::LShift))),
		'K' => Some(report_gen(KeyCode::K, Some(KeyCode::LShift))),
		'L' => Some(report_gen(KeyCode::L, Some(KeyCode::LShift))),
		'M' => Some(report_gen(KeyCode::M, Some(KeyCode::LShift))),
		'N' => Some(report_gen(KeyCode::N, Some(KeyCode::LShift))),
		'O' => Some(report_gen(KeyCode::O, Some(KeyCode::LShift))),
		'P' => Some(report_gen(KeyCode::P, Some(KeyCode::LShift))),
		'Q' => Some(report_gen(KeyCode::Q, Some(KeyCode::LShift))),
		'R' => Some(report_gen(KeyCode::R, Some(KeyCode::LShift))),
		'S' => Some(report_gen(KeyCode::S, Some(KeyCode::LShift))),
		'T' => Some(report_gen(KeyCode::T, Some(KeyCode::LShift))),
		'U' => Some(report_gen(KeyCode::U, Some(KeyCode::LShift))),
		'V' => Some(report_gen(KeyCode::V, Some(KeyCode::LShift))),
		'W' => Some(report_gen(KeyCode::W, Some(KeyCode::LShift))),
		'X' => Some(report_gen(KeyCode::X, Some(KeyCode::LShift))),
		'Y' => Some(report_gen(KeyCode::Z, Some(KeyCode::LShift))),
		'Z' => Some(report_gen(KeyCode::Y, Some(KeyCode::LShift))),

		'[' => Some(report_gen(KeyCode::Kb8, Some(KeyCode::RAlt))),
		'\\' => Some(report_gen(KeyCode::Minus, Some(KeyCode::RAlt))),
		']' => Some(report_gen(KeyCode::Kb9, Some(KeyCode::RAlt))),
		'^' => Some(report_gen(KeyCode::Grave, None)), // Special case
		'_' => Some(report_gen(KeyCode::Slash, Some(KeyCode::LShift))),
		'`' => Some(report_gen(KeyCode::Equal, Some(KeyCode::LShift))), // Special case

		'a' => Some(report_gen(KeyCode::A, None)),
		'b' => Some(report_gen(KeyCode::B, None)),
		'c' => Some(report_gen(KeyCode::C, None)),
		'd' => Some(report_gen(KeyCode::D, None)),
		'e' => Some(report_gen(KeyCode::E, None)),
		'f' => Some(report_gen(KeyCode::F, None)),
		'g' => Some(report_gen(KeyCode::G, None)),
		'h' => Some(report_gen(KeyCode::H, None)),
		'i' => Some(report_gen(KeyCode::I, None)),
		'j' => Some(report_gen(KeyCode::J, None)),
		'k' => Some(report_gen(KeyCode::K, None)),
		'l' => Some(report_gen(KeyCode::L, None)),
		'm' => Some(report_gen(KeyCode::M, None)),
		'n' => Some(report_gen(KeyCode::N, None)),
		'o' => Some(report_gen(KeyCode::O, None)),
		'p' => Some(report_gen(KeyCode::P, None)),
		'q' => Some(report_gen(KeyCode::Q, None)),
		'r' => Some(report_gen(KeyCode::R, None)),
		's' => Some(report_gen(KeyCode::S, None)),
		't' => Some(report_gen(KeyCode::T, None)),
		'u' => Some(report_gen(KeyCode::U, None)),
		'v' => Some(report_gen(KeyCode::V, None)),
		'w' => Some(report_gen(KeyCode::W, None)),
		'x' => Some(report_gen(KeyCode::X, None)),
		'y' => Some(report_gen(KeyCode::Z, None)),
		'z' => Some(report_gen(KeyCode::Y, None)),

		'{' => Some(report_gen(KeyCode::Kb7, Some(KeyCode::RAlt))),
		'|' => Some(report_gen(KeyCode::NonUsBslash, Some(KeyCode::RAlt))),
		'}' => Some(report_gen(KeyCode::Kb0, Some(KeyCode::RAlt))),
		'~' => Some(report_gen(KeyCode::RBracket, Some(KeyCode::RAlt))),

		'\n' => Some(report_gen(KeyCode::Enter, None)),

		_ => None,
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

	fn send_report(ctx: &mut idle::Context, report: &KbHidReport) {
		if ctx.shared.usb_class.lock(|k| k.device_mut().set_keyboard_report(report.clone())) {
			while let Ok(0) = ctx.shared.usb_class.lock(|k| k.write(report.as_bytes())) {}
		}
	}

	fn type_char(ctx: &mut idle::Context, c: char) {
		let r = german_char(c);
		if let Some(report) = r {
			let times = if c == '^' || c == '`' { 2 } else { 1 };
			for _i in 0..times {
				send_report(ctx, &report);
				shitty_delay_ms(50);
				send_report(ctx, &KbHidReport::default());
				shitty_delay_ms(50);
			}
		}
	}

	fn type_str(ctx: &mut idle::Context, s: &str) {
		for c in s.chars() {
			type_char(ctx, c);
		}
	}

	#[idle(shared = [usb_class], local = [myled])]
	fn idle(mut c: idle::Context) -> ! {
		shitty_delay_ms(3000);
		loop {
			type_str(&mut c, " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n");
			shitty_delay_ms(1000);
			type_str(&mut c, "Hello World!\n");
			shitty_delay_ms(1000);
		}
	}
}
