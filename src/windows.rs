#[cfg(windows)] extern crate winapi;
extern crate time;

use self::time::{Duration, PreciseTime};
use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;
use std::iter::once;
use std::mem;
use std::ptr::null_mut;
use std::mem::zeroed;
use self::winapi::um::libloaderapi::GetModuleHandleW;
use self::winapi::shared::windef;
use self::winapi::um::memoryapi::{
  VirtualAlloc,
  VirtualFree,
};
use self::winapi::um::winnt;
use self::winapi::um::winuser::{
  DefWindowProcW,
  RegisterClassW,
  CreateWindowExW,
  PeekMessageW,
  TranslateMessage,
  DispatchMessageW,
  BeginPaint,
  EndPaint,
  GetClientRect,
  GetDC,
  MSG,
  WNDCLASSW,
  CS_OWNDC,
  CS_HREDRAW,
  CS_VREDRAW,
  CW_USEDEFAULT,
  WS_OVERLAPPEDWINDOW,
  WS_VISIBLE,
  PAINTSTRUCT,
  WM_PAINT,
  WM_QUIT,
  WM_SIZE,
  WM_CLOSE,
  PM_REMOVE,
};
use self::winapi::shared::minwindef::{
  LRESULT,
  LPARAM,
  UINT,
  WPARAM,
  LPVOID,
};
use self::winapi::um::wingdi::{
  StretchDIBits,
  BITMAPINFO,
  BITMAPINFOHEADER,
  DIB_RGB_COLORS,
  BI_RGB,
  SRCCOPY,
};

static mut GLOBAL_RUNNING : bool = false;
static mut BITMAP_DATA : LPVOID = 0 as LPVOID;
static mut BITMAP_WIDTH : i32 = 0;
static mut BITMAP_HEIGHT : i32 = 0;

fn win32_string( value : &str ) -> Vec<u16> {
  OsStr::new( value ).encode_wide().chain( once( 0 ) ).collect()
}

pub fn init<F>(mut callback : F) 
where F : FnMut() -> Result<(), String> {
  let name = win32_string("GBA");
  let title = win32_string("GBA");

  unsafe {
    let hinstance = GetModuleHandleW( null_mut() );
    let wnd_class = WNDCLASSW {
      style : CS_OWNDC | CS_HREDRAW | CS_VREDRAW,
      lpfnWndProc : Some(windows_proc),
      hInstance : hinstance,
      lpszClassName : name.as_ptr(),
      cbClsExtra : 0,
      cbWndExtra : 0,
      hIcon: null_mut(),
      hCursor: null_mut(),
      hbrBackground: null_mut(),
      lpszMenuName: null_mut(),
    };

    RegisterClassW( &wnd_class );

    let handle = CreateWindowExW(
      0,
      name.as_ptr(),
      title.as_ptr(),
      WS_OVERLAPPEDWINDOW | WS_VISIBLE,
      CW_USEDEFAULT /* x */,
      CW_USEDEFAULT /* y */,
      320 /* width */,
      288 /* height */,
      null_mut(),
      null_mut(),
      hinstance,
      null_mut() );

    if handle.is_null() {
      println!("Couldn't create window");
      return;
    }

    let device_context = GetDC(handle);

    resize_dib_section(160, 144);

    let mut client_rect = mem::uninitialized();
    GetClientRect(handle, &mut client_rect);

    GLOBAL_RUNNING = true;
    while GLOBAL_RUNNING {
      let mut message : MSG = mem::uninitialized();
      while PeekMessageW(&mut message, 0 as windef::HWND, 0, 0, PM_REMOVE) != 0 {
        if message.message == WM_QUIT {
          GLOBAL_RUNNING = false;
        }
        TranslateMessage( &message as *const MSG );
        DispatchMessageW( &message as *const MSG );
      }
      match callback() {
        Ok(_) => (),
        Err(s) => {
          println!("{:?}", s);
          GLOBAL_RUNNING = false;
        }
      }
      let start = PreciseTime::now();
      update_window(device_context, client_rect);
      let duration = start.to(PreciseTime::now());
      println!("a {:?}", duration);
    }
  }
}

fn resize_dib_section(width : i32, height : i32) {
  unsafe {
    if BITMAP_DATA != 0 as LPVOID {
      VirtualFree(BITMAP_DATA, 0, winnt::MEM_RELEASE);
    }
    BITMAP_WIDTH = width;
    BITMAP_HEIGHT = height;

    BITMAP_DATA = VirtualAlloc(0 as LPVOID, (BITMAP_WIDTH*BITMAP_HEIGHT*4) as usize, winnt::MEM_COMMIT, winnt::PAGE_READWRITE);
  }
}

fn update_window(device_context : windef::HDC, client_rect : windef::RECT) {
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

    StretchDIBits(device_context,
            0, 0, window_width, window_height, //X, Y, Width, Height,
            0, 0, BITMAP_WIDTH, BITMAP_HEIGHT, //X, Y, Width, Height,
            BITMAP_DATA,
            &bitmap_info,
            DIB_RGB_COLORS,
            SRCCOPY);
  }
}

pub unsafe extern "system" fn windows_proc(
  hwnd: windef::HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {

  match msg {
    WM_SIZE => {
    }
    WM_CLOSE => {
      GLOBAL_RUNNING = false;
    }
    WM_PAINT => {
      //println!("paint");
      let mut ps = zeroed::<PAINTSTRUCT>();
      let hdc = BeginPaint(hwnd, &mut ps);
      let mut rc = zeroed::<windef::RECT>();
      GetClientRect(hwnd, &mut rc);
      update_window(hdc, rc);
      EndPaint(hwnd, &ps);
    }
    _ => {
      return DefWindowProcW(hwnd, msg, wparam, lparam)
    }
  }
  0 as LRESULT
}

pub fn draw(x : usize, y : usize, c : u32) {
  unsafe {
    let mut pixel = BITMAP_DATA as *mut u32;
    pixel = pixel.offset(((y * BITMAP_WIDTH as usize) + x) as isize);
    *pixel = c;
  }
}
