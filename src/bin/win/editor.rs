use direct2d::brush::SolidColorBrush;
use direct2d::enums::DrawTextOptions;
use direct2d::math::RoundedRect;
use direct2d::render_target::RenderTarget;
use directwrite;
use winapi::um::d2d1::*;

const LINE_HEIGHT: f32 = 12.0;
const SCROLLBAR_WIDTH: f32 = 6.0;

pub struct Editor {
  dwrite: directwrite::Factory,
  text_format: directwrite::TextFormat,
  pos: (f32, f32),
  width: f32,
  height: f32,
  viewport_start: usize,
  viewport_lines: usize,
  rom: Vec<(u16, String)>,
}

impl Editor {
  pub fn new(rom: Vec<(u16, String)>, x: f32, y: f32, width: f32, height: f32) -> Editor {
    let dwrite = directwrite::Factory::new().unwrap();
    let text_format = directwrite::TextFormat::create(&dwrite)
      .with_family("Consolas")
      .with_size(12.0)
      .build()
      .unwrap();
    Editor {
      dwrite,
      text_format,
      pos: (x, y),
      width,
      height,
      viewport_start: 100,
      viewport_lines: (height / LINE_HEIGHT) as usize,
      rom,
    }
  }

  pub fn scroll_down(&mut self) {
    if self.viewport_start < self.rom.len() - self.viewport_lines {
      self.viewport_start += 1;
    }
  }

  pub fn scroll_up(&mut self) {
    if self.viewport_start > 0 {
      self.viewport_start -= 1;
    }
  }

  pub fn draw<RT>(&self, render_target: &mut RT)
  where
    RT: RenderTarget,
  {
    let text_brush = SolidColorBrush::create(&render_target)
      .with_color(0xf8_f8_f2)
      .build()
      .unwrap();

    let mut y = 0.0;
    for line in self.viewport_start..(self.viewport_start + self.viewport_lines) {
      let text = if line < self.rom.len() {
        format!("{:#06x}  {}", self.rom[line].0, self.rom[line].1)
      } else {
        "".to_string()
      };
      let text_layout = directwrite::TextLayout::create(&self.dwrite)
        .with_text(&text)
        .with_font(&self.text_format)
        .with_width(1e6)
        .with_height(1e6)
        .build()
        .unwrap();

      render_target.draw_text_layout(
        (self.pos.0, self.pos.1 + y),
        &text_layout,
        &text_brush,
        DrawTextOptions::ENABLE_COLOR_FONT,
      );

      y += LINE_HEIGHT;
    }

    // Draw the scrollbar.
    let bg_brush = SolidColorBrush::create(&render_target)
      .with_color(0xff_ff_ff)
      .build()
      .unwrap();

    let fg_brush = SolidColorBrush::create(&render_target)
      .with_color(0x00_00_00)
      .build()
      .unwrap();

    let bg_rect = D2D1_ROUNDED_RECT {
      rect: D2D1_RECT_F {
        left: self.pos.0 + self.width - SCROLLBAR_WIDTH,
        top: self.pos.1,
        right: self.pos.0 + self.width,
        bottom: self.pos.1 + self.height,
      },
      radiusX: 2.0,
      radiusY: 2.0,
    };

    let ratio = self.height / self.rom.len() as f32;
    let fg_top = ratio * self.viewport_start as f32;
    let fg_height = f32::max(ratio * self.viewport_lines as f32, 15.0);
    let fg_rect = D2D1_ROUNDED_RECT {
      rect: D2D1_RECT_F {
        left: self.pos.0 + self.width - SCROLLBAR_WIDTH,
        top: self.pos.1 + fg_top,
        right: self.pos.0 + self.width,
        bottom: self.pos.1 + fg_top + fg_height,
      },
      radiusX: 2.0,
      radiusY: 2.0,
    };

    render_target.fill_rounded_rectangle(RoundedRect(bg_rect), &bg_brush);
    render_target.fill_rounded_rectangle(RoundedRect(fg_rect), &fg_brush);
  }
}
