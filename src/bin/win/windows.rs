#[cfg(windows)]
extern crate winapi;

use self::winapi::shared::minwindef::{BOOL, LPARAM, LRESULT, UINT, WPARAM};
use self::winapi::shared::windef;
use self::winapi::um::d2d1::*;
use self::winapi::um::libloaderapi::GetModuleHandleW;
use self::winapi::um::winnt::SHORT;
use self::winapi::um::winuser::{
  self, AdjustWindowRect, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetAsyncKeyState,
  GetClientRect, LoadCursorW, PeekMessageW, RegisterClassW, TranslateMessage, CS_HREDRAW, CS_OWNDC,
  CS_VREDRAW, CW_USEDEFAULT, MSG, PM_REMOVE, WM_CLOSE, WM_KEYUP, WM_QUIT, WNDCLASSW,
  WS_OVERLAPPEDWINDOW, WS_VISIBLE,
};
use crate::editor;
use direct2d::enums::{AlphaMode, BitmapInterpolationMode, RenderTargetType};
use direct2d::image::bitmap::BitmapBuilder;
use direct2d::math::{RectF, SizeU};
use direct2d::render_target::HwndRenderTarget;
use direct2d::RenderTarget;
use dxgi::Format;
use std::ffi::OsStr;
use std::fs;
use std::iter::once;
use std::mem;
use std::os::windows::ffi::OsStrExt;
use std::ptr::null_mut;
use std::thread::sleep;
use std::time::{Duration, Instant};
use yarg::emulator::Emulator;
use yarg::joypad::JoypadInput;

static mut GLOBAL_RUNNING: bool = false;

const KEYDOWN_MASK: SHORT = 0b1 << 15;

fn win32_string(value: &str) -> Vec<u16> {
  OsStr::new(value).encode_wide().chain(once(0)).collect()
}

unsafe fn make_joypad_input() -> JoypadInput {
  let mut joypad: JoypadInput = JoypadInput::new();
  joypad.up = GetAsyncKeyState(winuser::VK_UP) & KEYDOWN_MASK != 0;
  joypad.down = GetAsyncKeyState(winuser::VK_DOWN) & KEYDOWN_MASK != 0;
  joypad.left = GetAsyncKeyState(winuser::VK_LEFT) & KEYDOWN_MASK != 0;
  joypad.right = GetAsyncKeyState(winuser::VK_RIGHT) & KEYDOWN_MASK != 0;
  joypad.start = GetAsyncKeyState(winuser::VK_RETURN) & KEYDOWN_MASK != 0;
  joypad.button_a = GetAsyncKeyState('Z' as i32) & KEYDOWN_MASK != 0;
  joypad.button_b = GetAsyncKeyState('X' as i32) & KEYDOWN_MASK != 0;
  joypad
}

pub fn init(emu: &mut Emulator, rom: Option<Vec<(u16, String)>>) {
  let name = win32_string("yarg");

  let d2d = direct2d::Factory::new().unwrap();
  let debugger = rom.is_some();
  let mut editor = rom.map(|r| editor::Editor::new(r, 330.0, 5.0, 690.0, 758.0));

  unsafe {
    let hinstance = GetModuleHandleW(null_mut());
    let wnd_class = WNDCLASSW {
      style: CS_OWNDC | CS_HREDRAW | CS_VREDRAW,
      lpfnWndProc: Some(windows_proc),
      hInstance: hinstance,
      lpszClassName: name.as_ptr(),
      cbClsExtra: 0,
      cbWndExtra: 0,
      hIcon: null_mut(),
      hCursor: LoadCursorW(null_mut(), winuser::IDC_ARROW),
      hbrBackground: null_mut(),
      lpszMenuName: null_mut(),
    };

    RegisterClassW(&wnd_class);

    let mut wnd_rect: windef::RECT = if !debugger {
      windef::RECT {
        left: 0,
        top: 0,
        right: 320,
        bottom: 288,
      }
    } else {
      windef::RECT {
        left: 0,
        top: 0,
        right: 1024,
        bottom: 768,
      }
    };
    AdjustWindowRect(
      &mut wnd_rect,
      WS_OVERLAPPEDWINDOW | WS_VISIBLE,
      false as BOOL,
    );

    let handle = CreateWindowExW(
      0,
      name.as_ptr(),
      name.as_ptr(),
      WS_OVERLAPPEDWINDOW | WS_VISIBLE,
      CW_USEDEFAULT, /* x */
      CW_USEDEFAULT, /* y */
      wnd_rect.right - wnd_rect.left,
      wnd_rect.bottom - wnd_rect.top,
      null_mut(),
      null_mut(),
      hinstance,
      null_mut(),
    );

    if handle.is_null() {
      println!("Couldn't create window");
      return;
    }

    let mut client_rect = mem::uninitialized();
    GetClientRect(handle, &mut client_rect);
    let width = (client_rect.right - client_rect.left) as u32;
    let height = (client_rect.bottom - client_rect.top) as u32;

    let mut render_target = HwndRenderTarget::create(&d2d)
      .with_hwnd(handle)
      .with_target_type(RenderTargetType::Default)
      .with_alpha_mode(AlphaMode::Unknown)
      .with_pixel_size(width, height)
      .build()
      .unwrap();

    let frame_duration = Duration::from_secs(1) / 60;

    let mut savestate: Option<Vec<u8>> = None;

    GLOBAL_RUNNING = true;
    while GLOBAL_RUNNING {
      let mut msg: MSG = mem::uninitialized();
      while PeekMessageW(&mut msg, 0 as windef::HWND, 0, 0, PM_REMOVE) != 0 {
        match msg.message {
          WM_QUIT => GLOBAL_RUNNING = false,
          WM_KEYUP => match msg.wParam as i32 {
            winuser::VK_ESCAPE => GLOBAL_RUNNING = false,
            winuser::VK_F1 => {
              savestate = Some(emu.save_state().unwrap());
              fs::write("savestate", emu.save_state().unwrap()).unwrap();
            }
            winuser::VK_F2 => {
              if let Some(state) = &savestate {
                emu.restore_state(state).unwrap();
              } else {
                let data = fs::read("savestate").unwrap();
                savestate = Some(data.clone());
                emu.restore_state(&data).unwrap();
              }
            }
            _ => (),
          },
          _ => (),
        }
        TranslateMessage(&msg as *const MSG);
        DispatchMessageW(&msg as *const MSG);
      }

      let joypad = make_joypad_input();
      let start = Instant::now();
      while !emu.should_draw() {
        match emu.emu_loop(joypad) {
          Ok(_) => (),
          Err(s) => {
            println!("{:?}", s);
            GLOBAL_RUNNING = false;
          }
        }
      }
      let buffer = emu.screen_buffer();

      let width = 160u32;
      let height = 144u32;
      let size = SizeU(D2D1_SIZE_U { width, height });

      let image = BitmapBuilder::new(&render_target)
        .with_raw_data(size, buffer, 160 * 4)
        .with_format(Format::R8G8B8A8Unorm)
        .build()
        .unwrap();

      render_target.begin_draw();
      render_target.clear((0x27_28_22, 0.0));

      render_target.draw_bitmap(
        &image,
        RectF::new(0f32, 0f32, 320f32, 288f32),
        1.0f32,
        BitmapInterpolationMode::NearestNeighbor,
        RectF::new(0f32, 0f32, 160f32, 144f32),
      );

      if let Some(e) = &mut editor {
        if GetAsyncKeyState('J' as i32) & KEYDOWN_MASK != 0 {
          e.scroll_down();
        }
        if GetAsyncKeyState('K' as i32) & KEYDOWN_MASK != 0 {
          e.scroll_up();
        }
        e.draw(&mut render_target);
      }

      render_target.end_draw().unwrap();

      let duration = Instant::now() - start;
      if duration < frame_duration {
        sleep(frame_duration - duration);
      } else {
        println!("Warning: slow frame ({:?})", duration);
      }
    }
  }
}

pub unsafe extern "system" fn windows_proc(
  hwnd: windef::HWND,
  msg: UINT,
  wparam: WPARAM,
  lparam: LPARAM,
) -> LRESULT {
  match msg {
    WM_CLOSE => {
      GLOBAL_RUNNING = false;
    }
    _ => return DefWindowProcW(hwnd, msg, wparam, lparam),
  }
  0 as LRESULT
}
