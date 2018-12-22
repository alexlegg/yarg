use direct2d::brush::SolidColorBrush;
use direct2d::enums::DrawTextOptions;
use direct2d::render_target::RenderTarget;
use directwrite;

const LINE_HEIGHT: f32 = 12.0;

pub struct Editor {
  dwrite: directwrite::Factory,
  text_format: directwrite::TextFormat,
  x: f32,
  y: f32,
}

impl Editor {
  pub fn new(x: f32, y: f32) -> Editor {
    let dwrite = directwrite::Factory::new().unwrap();
    let text_format = directwrite::TextFormat::create(&dwrite)
      .with_family("Consolas")
      .with_size(12.0)
      .build()
      .unwrap();
    Editor {
      dwrite,
      text_format,
      x,
      y,
    }
  }

  pub fn draw<RT>(&mut self, render_target: &mut RT)
  where
    RT: RenderTarget,
  {
    let text0 = directwrite::TextLayout::create(&self.dwrite)
      .with_text("JMP 0x1234")
      .with_font(&self.text_format)
      .with_width(1e6)
      .with_height(1e6)
      .build()
      .unwrap();

    let text1 = directwrite::TextLayout::create(&self.dwrite)
      .with_text("LD a, b")
      .with_font(&self.text_format)
      .with_width(1e6)
      .with_height(1e6)
      .build()
      .unwrap();

    let text_brush = SolidColorBrush::create(&render_target)
      .with_color(0xf8_f8_f2)
      .build()
      .unwrap();

    render_target.draw_text_layout(
      (self.x, self.y),
      &text0,
      &text_brush,
      DrawTextOptions::ENABLE_COLOR_FONT,
    );

    render_target.draw_text_layout(
      (self.x, self.y + LINE_HEIGHT),
      &text1,
      &text_brush,
      DrawTextOptions::ENABLE_COLOR_FONT,
    );
  }
}
