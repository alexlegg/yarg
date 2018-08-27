extern crate sdl2;
extern crate time;

use self::sdl2::event::Event;
use self::sdl2::keyboard::Keycode;
use self::sdl2::pixels::Color;
use self::sdl2::pixels::PixelFormatEnum;
use joypad::JoypadInput;
use std::collections::HashSet;
//use self::time::{Duration, PreciseTime};

use emulator::Emulator;

pub fn init(mut emu: Emulator) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("YARG", 320, 288)
        .opengl()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    let texture_creator = canvas.texture_creator();

    let mut texture = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 160, 144)
        .unwrap();

    // Create a red-green gradient
    texture
        .with_lock(None, |buffer: &mut [u8], pitch: usize| {
            for y in 0..144 {
                for x in 0..160 {
                    let offset = y * pitch + x * 3;
                    buffer[offset] = 0xff;
                    buffer[offset + 1] = y as u8;
                    buffer[offset + 2] = 0;
                }
            }
        })
        .unwrap();

    canvas.set_draw_color(Color::RGB(0, 0, 0));
    canvas.clear();
    canvas.present();

    let mut joypad: JoypadInput = JoypadInput::new();

    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        // get the inputs here
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                Event::KeyDown {
                    keycode: Some(Keycode::Up),
                    ..
                } => joypad.up = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Up),
                    ..
                } => joypad.up = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Down),
                    ..
                } => joypad.down = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Down),
                    ..
                } => joypad.down = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Left),
                    ..
                } => joypad.left = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Left),
                    ..
                } => joypad.left = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    ..
                } => joypad.right = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Right),
                    ..
                } => joypad.right = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Return),
                    ..
                } => joypad.start = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Return),
                    ..
                } => joypad.start = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Backspace),
                    ..
                } => joypad.select = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Backspace),
                    ..
                } => joypad.select = false,
                Event::KeyDown {
                    keycode: Some(Keycode::Z),
                    ..
                } => joypad.button_a = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Z),
                    ..
                } => joypad.button_a = false,
                Event::KeyDown {
                    keycode: Some(Keycode::X),
                    ..
                } => joypad.button_b = true,
                Event::KeyUp {
                    keycode: Some(Keycode::X),
                    ..
                } => joypad.button_b = false,
                _ => {}
            }
        }

        //let keys : HashSet<Keycode> = event_pump.keyboard_state().pressed_scancodes().filter_map(Keycode::from_scancode).collect();
        //println!("{:?}", joypad);

        match emu.emu_loop(joypad) {
            Ok(_) => (),
            Err(s) => {
                println!("{:?}", s);
                break 'running;
            }
        }

        //let start = PreciseTime::now();

        if emu.should_draw() {
            texture.update(None, emu.screen_buffer(), 3 * 160).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        //let duration = start.to(PreciseTime::now());
        //println!("a {:?}", duration);
    }
}
