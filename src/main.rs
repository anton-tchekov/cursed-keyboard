#![no_main]
#![no_std]

use panic_halt as _;

use keyberon::key_code::KbHidReport;
use rtic::app;
use stm32f4xx_hal::i2c;
use stm32f4xx_hal::i2c::{I2c, I2cExt, Mode};
use stm32f4xx_hal::gpio::{self};
use stm32f4xx_hal::otg_fs::{UsbBusType, USB};
use stm32f4xx_hal::prelude::*;
use usb_device::bus::UsbBusAllocator;
use usb_device::class::UsbClass as _;
use keyberon::key_code::KeyCode;
use cortex_m::asm::delay;
use rtic::Mutex;
use stm32f4xx_hal::pac::I2C1;
use stm32f4xx_hal::i2c::Error::*;
use stm32f4xx_hal::i2c::NoAcknowledgeSource;

type UsbClass = keyberon::Class<'static, UsbBusType, Leds>;
type UsbDevice = usb_device::device::UsbDevice<'static, UsbBusType>;

pub struct Leds {
	caps_lock: gpio::gpioc::PC13<gpio::Output<gpio::PushPull>>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Layout {
	US,
	DE
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Sector {
	Center = -1,
	North = 0,
	East = 1,
	South = 2,
	West = 3
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Direction {
	None = -1,
	NW = 0,
	NE = 1,
	EN = 2,
	ES = 3,
	SE = 4,
	SW = 5,
	WS = 6,
	WN = 7
}

static LAYOUT: [[char; 8]; 8] = [
	[ 's', 'd', 'g', '\'',  'S', 'D', 'G', '\"' ], // NW
	[ 'y', 'b', 'p', 'q',   'Y', 'B', 'P', 'Q'  ], // NE
	[ 'a', 'r', 'x', '?',   'A', 'R', 'X', '*'  ], // EN
	[ 'n', 'm', 'f', '!',   'N', 'M', 'F', '!'  ], // ES
	[ 'o', 'u', 'v', 'w',   'O', 'U', 'V', 'W'  ], // SE
	[ 'e', 'l', 'k', '@',   'E', 'L', 'K', '-'  ], // SW
	[ 'i', 'h', 'j', ',',   'I', 'H', 'J', ';'  ], // WS
	[ 't', 'c', 'z', '.',   'T', 'C', 'Z', ':'  ]  // WN
];

static TABLE: [[i8; 4]; 4] = [
	//         N   E   S   W
	/* N */ [  0,  1,  0, -1 ],
	/* E */ [ -1,  0,  1,  0 ],
	/* S */ [  0, -1,  0,  1 ],
	/* W */ [  1,  0, -1,  0 ]
];

struct Circle {
	x: i32,
	y: i32,
	radius: i32
}

impl Circle {
	fn in_circle(&self, x: i32, y: i32) -> bool {
		(x - self.x).pow(2) + (y - self.y).pow(2) <= self.radius.pow(2)
	}
}

struct Zone {
	area: Circle,
	sector: Sector
}

const RADIUS: i32 = 85;
static ZONES: [Zone; 5] = [
	Zone { area: Circle { x: 128, y:   0, radius: RADIUS }, sector: Sector::South  },
	Zone { area: Circle { x:   0, y: 128, radius: RADIUS }, sector: Sector::West   },
	Zone { area: Circle { x: 255, y: 128, radius: RADIUS }, sector: Sector::East   },
	Zone { area: Circle { x: 128, y: 255, radius: RADIUS }, sector: Sector::North  },
	Zone { area: Circle { x: 128, y: 128, radius: 40     }, sector: Sector::Center },
];

fn nunchuck_to_sector(data: &NunchuckReading) -> Option<Sector> {
	let x = data.joystick_x.into();
	let y = data.joystick_y.into();
	for zone in &ZONES {
		if zone.area.in_circle(x, y) {
			return Some(zone.sector);
		}
	}

	None
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

fn us_char(c: char) -> Option<KbHidReport> {
	match c {
		' ' => Some(report_gen(KeyCode::Space, None)),
		'!' => Some(report_gen(KeyCode::Kb1, Some(KeyCode::LShift))),
		'"' => Some(report_gen(KeyCode::Quote, Some(KeyCode::LShift))),
		'#' => Some(report_gen(KeyCode::Kb3, Some(KeyCode::LShift))),
		'$' => Some(report_gen(KeyCode::Kb4, Some(KeyCode::LShift))),
		'%' => Some(report_gen(KeyCode::Kb5, Some(KeyCode::LShift))),
		'&' => Some(report_gen(KeyCode::Kb7, Some(KeyCode::LShift))),
		'\'' => Some(report_gen(KeyCode::Quote, None)),
		'(' => Some(report_gen(KeyCode::Kb9, Some(KeyCode::LShift))),
		')' => Some(report_gen(KeyCode::Kb0, Some(KeyCode::LShift))),
		'*' => Some(report_gen(KeyCode::Kb8, Some(KeyCode::LShift))),
		'+' => Some(report_gen(KeyCode::Equal, Some(KeyCode::LShift))),
		',' => Some(report_gen(KeyCode::Comma, None)),
		'-' => Some(report_gen(KeyCode::Minus, None)),
		'.' => Some(report_gen(KeyCode::Dot, None)),
		'/' => Some(report_gen(KeyCode::Slash, None)),

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

		':' => Some(report_gen(KeyCode::SColon, Some(KeyCode::LShift))),
		';' => Some(report_gen(KeyCode::SColon, None)),
		'<' => Some(report_gen(KeyCode::Comma, Some(KeyCode::LShift))),
		'=' => Some(report_gen(KeyCode::Equal, None)),
		'>' => Some(report_gen(KeyCode::Dot, Some(KeyCode::LShift))),
		'?' => Some(report_gen(KeyCode::Slash, Some(KeyCode::LShift))),
		'@' => Some(report_gen(KeyCode::Kb2, Some(KeyCode::LShift))),

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
		'Y' => Some(report_gen(KeyCode::Y, Some(KeyCode::LShift))),
		'Z' => Some(report_gen(KeyCode::Z, Some(KeyCode::LShift))),

		'[' => Some(report_gen(KeyCode::LBracket, None)),
		'\\' => Some(report_gen(KeyCode::Bslash, None)),
		']' => Some(report_gen(KeyCode::RBracket, None)),
		'^' => Some(report_gen(KeyCode::Kb6, Some(KeyCode::LShift))),
		'_' => Some(report_gen(KeyCode::Minus, Some(KeyCode::LShift))),
		'`' => Some(report_gen(KeyCode::Grave, None)),

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
		'y' => Some(report_gen(KeyCode::Y, None)),
		'z' => Some(report_gen(KeyCode::Z, None)),

		'{' => Some(report_gen(KeyCode::LBracket, Some(KeyCode::LShift))),
		'|' => Some(report_gen(KeyCode::Bslash, Some(KeyCode::LShift))),
		'}' => Some(report_gen(KeyCode::RBracket, Some(KeyCode::LShift))),
		'~' => Some(report_gen(KeyCode::Grave, Some(KeyCode::LShift))),

		'\n' => Some(report_gen(KeyCode::Enter, None)),
		'\x08' => Some(report_gen(KeyCode::BSpace, None)),

		_ => None
	}
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
		'^' => Some(report_gen(KeyCode::Grave, None)),
		'_' => Some(report_gen(KeyCode::Slash, Some(KeyCode::LShift))),
		'`' => Some(report_gen(KeyCode::Equal, Some(KeyCode::LShift))),

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
		'\x08' => Some(report_gen(KeyCode::BSpace, None)),

		_ => None
	}
}

const NUNCHUCK_ADDR: u8 = 0x52;

struct NunchuckReading {
	joystick_x: u8,
	joystick_y: u8,
	accel_x: u16,
	accel_y: u16,
	accel_z: u16,
	c_button_pressed: bool,
	z_button_pressed: bool,
}

fn nunchuck_init<T: i2c::Instance>(i2c: &mut I2c<T>) -> Result<(), i2c::Error> {
	// May need to be changed depending on Nunchuck
	i2c.write(NUNCHUCK_ADDR, &[ 0xF0, 0x55 ])?;
	shitty_delay_ms(10);
	i2c.write(NUNCHUCK_ADDR, &[ 0xFB, 0x00 ])?;
	shitty_delay_ms(10);
	Ok(())
}

fn nunchuck_read<T: i2c::Instance>(i2c: &mut I2c<T>) -> Result<NunchuckReading, i2c::Error> {
	let mut data: [u8; 6] = [0; 6];

	i2c.write(NUNCHUCK_ADDR, &[ 0x00 ])?;
	shitty_delay_ms(10);
	i2c.read(NUNCHUCK_ADDR, &mut data)?;

	Ok(NunchuckReading {
		joystick_x: data[0],
		joystick_y: data[1],
		accel_x: (u16::from(data[2]) << 2) | ((u16::from(data[5]) >> 6) & 0b11),
		accel_y: (u16::from(data[3]) << 2) | ((u16::from(data[5]) >> 4) & 0b11),
		accel_z: (u16::from(data[4]) << 2) | ((u16::from(data[5]) >> 2) & 0b11),
		c_button_pressed: (data[5] & 0b10) == 0,
		z_button_pressed: (data[5] & 0b01) == 0,
	})
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
		myled: stm32f4xx_hal::gpio::Pin<'B', 0, stm32f4xx_hal::gpio::Output>,
		i2c: I2c<I2C1>
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

		let scl = gpiob.pb8;
		let sda = gpiob.pb9;

		let i2c = c.device.I2C1.i2c(
			(scl, sda),
			Mode::Standard {
				frequency: 100.kHz(),
			},
			&clocks,
		);

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
				myled,
				i2c
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

	fn type_char(ctx: &mut idle::Context, layout: Layout, c: char) {
		let r = match layout {
			Layout::US => us_char(c),
			Layout::DE => german_char(c)
		};

		if let Some(report) = r {
			let times = if layout == Layout::DE && (c == '^' || c == '`') { 2 } else { 1 };
			for _i in 0..times {
				send_report(ctx, &report);
				shitty_delay_ms(20);
				send_report(ctx, &KbHidReport::default());
				shitty_delay_ms(20);
			}
		}
	}

	fn type_str(ctx: &mut idle::Context, layout: Layout, s: &str) {
		for c in s.chars() {
			type_char(ctx, layout, c);
		}
	}

	fn get_correct_char(initial_pos: Sector, cnt: i32) -> Option<char> {
		let mut counter = cnt;
		if counter != 0 {
			let sel_array =
			if counter < 0 {
				counter = -counter;
				match initial_pos {
					Sector::North => Direction::NW,
					Sector::South => Direction::SE,
					Sector::East => Direction::EN,
					Sector::West => Direction::WS,
					_ => Direction::None
				}
			}
			else {
				match initial_pos {
					Sector::North => Direction::NE,
					Sector::South => Direction::SW,
					Sector::East => Direction::ES,
					Sector::West => Direction::WN,
					_ => Direction::None
				}
			};

			let chr = LAYOUT[sel_array as usize][((counter - 1) as usize) % LAYOUT[0].len()];
			return Some(chr)
		}

		None
	}

	#[idle(shared = [usb_class], local = [i2c, myled])]
	fn idle(mut c: idle::Context) -> ! {
		shitty_delay_ms(2000);
		// type_str(&mut c, Layout::US, "Started!\n");

		let test = false;
		if test {

			type_str(&mut c, Layout::US, "US Layout Test:\n");
			type_str(&mut c, Layout::US, " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n");
			shitty_delay_ms(1000);

			type_str(&mut c, Layout::US, "You now have 5 seconds to switch the keyboard layout to german\n");
			shitty_delay_ms(5000);

			type_str(&mut c, Layout::DE, "DE Layout Test:\n");
			type_str(&mut c, Layout::DE, " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n");
			shitty_delay_ms(1000);

			loop {
				type_str(&mut c, Layout::DE, "Hello World!\n");
				shitty_delay_ms(1000);
			}
		}
		else {
			match nunchuck_init(c.local.i2c) {
				Ok(()) => {
					// type_str(&mut c, Layout::US, "Nunchuck Initialized!\n");
				},
				Err(e) => {
					let s = match e {
						Overrun => { "Overrun" },
						NoAcknowledge(NoAcknowledgeSource::Address) => { "NoAcknowledge Address" },
						NoAcknowledge(NoAcknowledgeSource::Data) => { "NoAcknowledge Data" },
						NoAcknowledge(NoAcknowledgeSource::Unknown) => { "NoAcknowledge Unknown" },
						Timeout => { "Timeout" },
						Bus => { "Bus" },
						Crc => { "Crc" },
						ArbitrationLoss => { "ArbitrationLoss" },
						_ => { "Unknown" }
					};

					type_str(&mut c, Layout::US, s);
					type_char(&mut c, Layout::US, '\n');
				}
			}

			let mut initial_pos = Sector::Center;
			let mut last_pos = Sector::Center;
			let mut counter: i32 = 0;
			let mut prev_char = '\0';
			loop {
				let d = nunchuck_read(c.local.i2c);
				match d {
					Ok(data) => {
						if data.c_button_pressed {
							type_char(&mut c, Layout::US, '\x08');
							shitty_delay_ms(60);
						}
						else if data.z_button_pressed {
							type_char(&mut c, Layout::US, ' ');
							shitty_delay_ms(60);
						}

						if let Some(cur_pos) = nunchuck_to_sector(&data) {
							/*match cur_pos {
								Sector::Center => {
									type_str(&mut c, Layout::US, "Center\n");
								},
								Sector::North => {
									type_str(&mut c, Layout::US, "North\n");
								},
								Sector::South => {
									type_str(&mut c, Layout::US, "South\n");
								},
								Sector::East => {
									type_str(&mut c, Layout::US, "East\n");
								},
								Sector::West => {
									type_str(&mut c, Layout::US, "West\n");
								}
							};*/

							if initial_pos == Sector::Center {
								// Start einer Bewegung
								initial_pos = cur_pos;
								last_pos = cur_pos;
								counter = 0;
								prev_char = '\0';
							}
							else {
								// Neue Bewegung
								if cur_pos == Sector::Center {
									// Final
									initial_pos = Sector::Center;
								}
								else {
									// Every temporary change
									counter += TABLE[last_pos as usize][cur_pos as usize] as i32;

									if let Some(chr) = get_correct_char(initial_pos, counter) {
										if chr != prev_char {
											type_char(&mut c, Layout::US, '\x08');
											type_char(&mut c, Layout::US, chr);
											prev_char = chr;
										}
									}
								}

								last_pos = cur_pos;
							}
						}
					},
					Err(_) => {
						type_str(&mut c, Layout::US, "Read Error\n");
					}
				}

				shitty_delay_ms(10);
			}
		}
	}
}
