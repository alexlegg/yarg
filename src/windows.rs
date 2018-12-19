#[cfg(windows)]
extern crate winapi;

use self::winapi::shared::minwindef::{BOOL, LPARAM, LPVOID, LRESULT, UINT, WPARAM};
use self::winapi::shared::windef;
use self::winapi::um::libloaderapi::GetModuleHandleW;
use self::winapi::um::memoryapi::{VirtualAlloc, VirtualFree};
use self::winapi::um::wingdi::{
  StretchDIBits, BITMAPINFO, BITMAPINFOHEADER, BI_RGB, DIB_RGB_COLORS, SRCCOPY,
};
use self::winapi::um::winnt;
use self::winapi::um::winuser::{
  AdjustWindowRect, BeginPaint, CreateWindowExW, DefWindowProcW, DispatchMessageW, EndPaint,
  GetClientRect, GetDC, PeekMessageW, RegisterClassW, TranslateMessage, CS_HREDRAW, CS_OWNDC,
  CS_VREDRAW, CW_USEDEFAULT, MSG, PAINTSTRUCT, PM_REMOVE, WM_CLOSE, WM_PAINT, WM_QUIT, WM_SIZE,
  WNDCLASSW, WS_OVERLAPPEDWINDOW, WS_VISIBLE,
};
use crate::emulator::Emulator;
use crate::joypad::JoypadInput;
use std::ffi::OsStr;
use std::iter::once;
use std::mem;
use std::mem::zeroed;
use std::os::windows::ffi::OsStrExt;
use std::ptr::null_mut;
use std::thread::sleep;
use std::time::{Duration, Instant};

static mut GLOBAL_RUNNING: bool = false;
static mut BITMAP_DATA: LPVOID = 0 as LPVOID;
static mut BITMAP_WIDTH: i32 = 0;
static mut BITMAP_HEIGHT: i32 = 0;

fn win32_string(value: &str) -> Vec<u16> {
  OsStr::new(value).encode_wide().chain(once(0)).collect()
}

pub fn init(emu: &mut Emulator, _show_vram: bool) {
  let name = win32_string("yarg");

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
      hCursor: null_mut(),
      hbrBackground: null_mut(),
      lpszMenuName: null_mut(),
    };

    RegisterClassW(&wnd_class);

    let mut wnd_rect: windef::RECT = windef::RECT {
      left: 0,
      top: 0,
      right: 320,
      bottom: 288,
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

    let device_context = GetDC(handle);

    resize_dib_section(160, 144);

    let mut client_rect = mem::uninitialized();
    GetClientRect(handle, &mut client_rect);

    let joypad: JoypadInput = JoypadInput::new();

    let frame_duration = Duration::from_secs(1) / 60;

    GLOBAL_RUNNING = true;
    while GLOBAL_RUNNING {
      let mut message: MSG = mem::uninitialized();
      while PeekMessageW(&mut message, 0 as windef::HWND, 0, 0, PM_REMOVE) != 0 {
        if message.message == WM_QUIT {
          GLOBAL_RUNNING = false;
        }
        TranslateMessage(&message as *const MSG);
        DispatchMessageW(&message as *const MSG);
      }
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
      BITMAP_DATA.copy_from_nonoverlapping(buffer.as_ptr() as *const _, buffer.len());
      update_window(device_context, client_rect);
      let duration = Instant::now() - start;
      if duration < frame_duration {
        sleep(frame_duration - duration);
      } else {
        println!("Warning: slow frame ({:?})", duration);
      }
    }
  }
}

fn resize_dib_section(width: i32, height: i32) {
  unsafe {
    if BITMAP_DATA != 0 as LPVOID {
      VirtualFree(BITMAP_DATA, 0, winnt::MEM_RELEASE);
    }
    BITMAP_WIDTH = width;
    BITMAP_HEIGHT = height;

    BITMAP_DATA = VirtualAlloc(
      0 as LPVOID,
      (BITMAP_WIDTH * BITMAP_HEIGHT * 4) as usize,
      winnt::MEM_COMMIT,
      winnt::PAGE_READWRITE,
    );
  }
}

fn update_window(device_context: windef::HDC, client_rect: windef::RECT) {
  let window_width = client_rect.right - client_rect.left;
  let window_height = client_rect.bottom - client_rect.top;

  unsafe {
    let mut bitmap_info = zeroed::<BITMAPINFO>();
    bitmap_info.bmiHeader.biSize = ::std::mem::size_of::<BITMAPINFOHEADER>() as u32;
    bitmap_info.bmiHeader.biWidth = BITMAP_WIDTH;
    bitmap_info.bmiHeader.biHeight = -BITMAP_HEIGHT;
    bitmap_info.bmiHeader.biPlanes = 1;
    bitmap_info.bmiHeader.biBitCount = 32;
    bitmap_info.bmiHeader.biCompression = BI_RGB;

    StretchDIBits(
      device_context,
      0,
      0,
      window_width,
      window_height, //X, Y, Width, Height,
      0,
      0,
      BITMAP_WIDTH,
      BITMAP_HEIGHT, //X, Y, Width, Height,
      BITMAP_DATA,
      &bitmap_info,
      DIB_RGB_COLORS,
      SRCCOPY,
    );
  }
}

pub unsafe extern "system" fn windows_proc(
  hwnd: windef::HWND,
  msg: UINT,
  wparam: WPARAM,
  lparam: LPARAM,
) -> LRESULT {
  match msg {
    WM_SIZE => {}
    WM_CLOSE => {
      GLOBAL_RUNNING = false;
    }
    WM_PAINT => {
      let mut ps = zeroed::<PAINTSTRUCT>();
      let hdc = BeginPaint(hwnd, &mut ps);
      let mut rc = zeroed::<windef::RECT>();
      GetClientRect(hwnd, &mut rc);
      update_window(hdc, rc);
      EndPaint(hwnd, &ps);
    }
    _ => return DefWindowProcW(hwnd, msg, wparam, lparam),
  }
  0 as LRESULT
}
