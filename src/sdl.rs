extern crate sdl2;

use self::sdl2::event::Event;
use self::sdl2::keyboard::Keycode;
use self::sdl2::pixels::Color;
use self::sdl2::pixels::PixelFormatEnum;
use self::sdl2::rect::Rect;
use crate::joypad::JoypadInput;
use std::thread::sleep;
use std::time::{Duration, Instant};

use crate::emulator::Emulator;

pub fn init(emu: &mut Emulator, show_vram: bool) {
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();

  let (screen_width, screen_height) = if !show_vram {
    (320, 288)
  } else {
    (320 + 256, 384)
  };

  let window = video_subsystem
    .window("YARG", screen_width, screen_height)
    .opengl()
    .build()
    .unwrap();

  let mut canvas = window.into_canvas().build().unwrap();

  let texture_creator = canvas.texture_creator();

  let mut texture = texture_creator
    .create_texture_streaming(PixelFormatEnum::RGB24, 160, 144)
    .unwrap();

  let mut vram_texture = texture_creator
    .create_texture_streaming(PixelFormatEnum::RGB24, 128, 192)
    .unwrap();

  canvas.set_draw_color(Color::RGB(0, 0, 0));
  canvas.clear();
  canvas.present();

  let mut joypad: JoypadInput = JoypadInput::new();

  let frame_duration = Duration::from_secs(1) / 60;

  let mut event_pump = sdl_context.event_pump().unwrap();
  'running: loop {
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

    let start = Instant::now();
    while !emu.should_draw() {
      match emu.emu_loop(joypad) {
        Ok(_) => (),
        Err(s) => {
          println!("{:?}", s);
          break 'running;
        }
      }
    }
    texture.update(None, emu.screen_buffer(), 3 * 160).unwrap();
    canvas
      .copy(&texture, None, Some(Rect::new(0, 0, 320, 288)))
      .unwrap();
    canvas.present();
    let duration = Instant::now() - start;
    if duration < frame_duration {
      sleep(frame_duration - duration);
    } else {
      println!("Warning: slow frame ({:?})", duration);
    }

    if let Some(buf) = emu.get_tile_data() {
      vram_texture.update(None, buf.as_ref(), 3 * 128).unwrap();
      canvas
        .copy(&vram_texture, None, Some(Rect::new(320, 0, 256, 384)))
        .unwrap();
      canvas.present();
    }
  }
}
