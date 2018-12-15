use crate::cpu::Interrupt;
use crate::cpu::TrapHandler;
use crate::util;

const SCREEN_WIDTH: u64 = 160;
const SCREEN_HEIGHT: u64 = 144;
const PIXEL_SIZE: usize = 3;

const OAM_SEARCH_CYCLES: u64 = 20;
const PIXEL_TRANSFER_CYCLES: u64 = 43;
const H_BLANK_CYCLES: u64 = 51;
const HORIZONTAL_LINE_CYCLES: u64 = OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES + H_BLANK_CYCLES;

const V_BLANK_LINES: u64 = 10;
const VERTICAL_LINE_CYCLES: u64 = HORIZONTAL_LINE_CYCLES * (SCREEN_HEIGHT + V_BLANK_LINES);

#[derive(Copy, Clone, Debug)]
enum Mode {
  HBlank,
  VBlank,
  OamSearch,
  PixelTransfer,
}

/// LCD Control Register
///
/// Bit 7 - LCD Controller Operation Stop Flag (0=Off, 1=On)
/// Bit 6 - Window Code Area Selection Flag (0=9800h-9BFFh, 1=9C00h-9FFFh)
/// Bit 5 - Windowing On Flag (0=Disabled, 1=Enabled)
/// Bit 4 - BG Character Data Selection Flag (0=8800h-97FFh, 1=8000h-8FFFh)
/// Bit 3 - BG Code Area Selection Flag (0=9800h-9BFFh, 1=9C00h-9FFFh)
/// Bit 2 - Sprite Size (0=8x8, 1=8x16)
/// Bit 1 - Sprites Enabled (0=Disabled, 1=Enabled)
/// Bit 0 - BG Enabled (0=Disabled, 1=Enabled)
/// Note that `BG Enabled` is always set on CGBs.
/// (Source: GameBoy Programming Manual)
#[derive(Copy, Clone, Debug, Default)]
struct LcdControl {
  register: u8
}

impl LcdControl {
  fn enabled(&self) -> bool {
    util::bit_bool(7, self.register)
  }

  fn sprite_size(&self) -> u8 {
    if util::bit(2, self.register) == 0 {
      8
    } else {
      16
    }
  }

  fn tileset(&self) -> u8 {
    util::bit(4, self.register)
  }

  fn sprites_enabled(&self) -> bool {
    util::bit_bool(1, self.register)
  }
}

pub struct Ppu {
  bad_timer: u64,
  lcdc: LcdControl,
  bg_palette: u8,
  tiles: Vec<[u8; 16]>,
  bg_tile_map: Vec<[u8; 32]>,
  scroll_x: u8,
  scroll_y: u8,
  sprite_attributes: Vec<SpriteAttributeTable>,
  sprite_palettes: [u8; 2],

  pub screen_buffer: Box<[u8; (SCREEN_WIDTH as usize) * (SCREEN_HEIGHT as usize) * PIXEL_SIZE]>,
  draw_buffer: bool,

  tile_data_dirty: bool,
}

/// OAM Register.
///
/// Byte 0x00 - LCD y-coordinate
/// Byte 0x01 - LCD x-coordinate
/// Byte 0x02 - CHR (tile) code
/// Byte 0x03 - Attribute flag
///
/// Flags:
/// Bit 7 - Display priority flag (0=OBJ, 1=BG)
/// Bit 6 - Vertical flip flag (0=Normal, 1=Flip)
/// Bit 5 - Horizontal flip flag (0=Normal, 1=Flip)
/// Bit 4 - Specifies palette
/// Bit 3 - Specifies character bank (CGB only)
/// Bits 2:0 - Specifies colour palette (CGB only)
/// (Source: GameBoy Programming Manual)
#[derive(Copy, Clone, Debug)]
struct SpriteAttributeTable {
  y_position: u8,
  x_position: u8,
  tile_number: u8,
  flags: u8,
}

impl SpriteAttributeTable {
  fn new() -> SpriteAttributeTable {
    SpriteAttributeTable {
      y_position: 0,
      x_position: 0,
      tile_number: 0,
      flags: 0,
    }
  }

  fn sprite_priority(&self) -> bool {
    util::bit_bool(7, self.flags)
  }

  fn vertical_flip(&self) -> bool {
    util::bit_bool(6, self.flags)
  }

  fn horizonal_flip(&self) -> bool {
    util::bit_bool(5, self.flags)
  }

  fn palette(&self) -> usize {
    usize::from(util::bit(4, self.flags))
  }
}

impl Default for Ppu {
  fn default() -> Ppu {
    Ppu {
      bad_timer: 0,
      lcdc: LcdControl::default(),
      bg_palette: 0,
      tiles: vec![[0; 16]; 384],
      bg_tile_map: vec![[0; 32]; 32],
      scroll_x: 0,
      scroll_y: 0,
      sprite_attributes: vec![SpriteAttributeTable::new(); 40],
      sprite_palettes: [0; 2],
      screen_buffer: Box::new([0xff; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * PIXEL_SIZE]),
      draw_buffer: false,
      tile_data_dirty: false,
    }
  }
}

impl Ppu {
  pub fn new() -> Ppu {
    Ppu::default()
  }

  pub fn should_draw(&mut self) -> bool {
    if self.draw_buffer {
      self.draw_buffer = false;
      true
    } else {
      false
    }
  }


  fn draw_line(&mut self, y: u8) {
    // First draw the background
    let palette = self.bg_palette;
    let mut x: u8 = 0;
    while x < SCREEN_WIDTH as u8 {
      let bg_x = x.wrapping_add(self.scroll_y);
      let bg_y = y.wrapping_add(self.scroll_x);
      let mut tile = self.bg_tile_map[(bg_y >> 3) as usize][(bg_x >> 3) as usize] as usize;
      if self.lcdc.tileset() == 0 && tile < 0x80 {
        tile += 0x100;
      }
      let x_off = bg_x % 8;
      draw_tile_line(
        &self.tiles[tile],
        x as isize,
        y,
        x_off,
        bg_y % 8,
        palette,
        false, /* flip */
        false, /* transparent */
        &mut *self.screen_buffer,
      );
      x += 8 - x_off;
    }

    // Next draw sprites
    if self.lcdc.sprites_enabled() {
      let sprite_height = self.lcdc.sprite_size();
      for sprite in self.sprite_attributes.iter() {
        // TODO: Handle other priority.
        if sprite.sprite_priority() {
          continue;
        }
        if y + 16 < sprite.y_position || y + 16 >= sprite.y_position + sprite_height {
          continue;
        }
        let mut tile_y = (y + 16 - sprite.y_position) % sprite_height;
        if sprite.vertical_flip() {
          tile_y = sprite_height - tile_y - 1;
        }
        let palette = self.sprite_palettes[sprite.palette()];
        let tile = if sprite_height == 16 {
          let top_tile = (sprite.tile_number & !1) as usize;
          if tile_y < 8 {
            &self.tiles[top_tile]
          } else {
            tile_y -= 8;
            &self.tiles[top_tile + 1]
          }
        } else {
          &self.tiles[sprite.tile_number as usize]
        };
        draw_tile_line(
          tile,
          sprite.x_position as isize - 8,
          y,
          0, /* x offset */
          tile_y,
          palette,
          sprite.horizonal_flip(),
          true, /* transparent */
          &mut *self.screen_buffer,
        );
      }
    }
  }

  pub fn get_tile_data(&mut self) -> Option<Box<[u8; 128 * 192 * PIXEL_SIZE]>> {
    if !self.tile_data_dirty {
      return None;
    }
    self.tile_data_dirty = false;
    let mut buffer = Box::new([0; 128 * 192 * PIXEL_SIZE]);
    for tile in 0..384 {
      let row_y = (tile / 16) * 8;
      let col_x = (tile % 16) * 8;
      for ty in (0..16).step_by(2) {
        let data0 = self.tiles[tile][ty];
        let data1 = self.tiles[tile][ty + 1];
        for x in 0..8 {
          let colour_val = (util::bit(7 - x, data1) << 1) | util::bit(7 - x, data0);
          let colour = get_palette_colour(colour_val, self.bg_palette);
          let offset = ((row_y + (ty / 2)) as usize * 128 * PIXEL_SIZE as usize)
            + ((col_x + x as usize) * PIXEL_SIZE);
          buffer[offset] = (colour & 0xff) as u8;
          buffer[offset + 1] = ((colour >> 8) & 0xff) as u8;
          buffer[offset + 2] = ((colour >> 16) & 0xff) as u8;
        }
      }
    }
    Some(buffer)
  }

  fn mode(&self) -> Mode {
    // TODO Probably faster to just store the mode and update in tick().
    if !self.lcdc.enabled() {
      return Mode::HBlank;
    }
    if self.bad_timer > SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES {
      return Mode::VBlank;
    }

    let horizontal_cycles = self.bad_timer % HORIZONTAL_LINE_CYCLES;
    if horizontal_cycles < OAM_SEARCH_CYCLES {
      Mode::OamSearch
    } else if horizontal_cycles - OAM_SEARCH_CYCLES < PIXEL_TRANSFER_CYCLES {
      Mode::PixelTransfer
    } else {
      Mode::HBlank
    }
  }
}

impl TrapHandler for Ppu {
  fn read(&self, addr: u16) -> Result<u8, String> {
    if addr >= 0x8000 && addr <= 0x97ff {
      let tile_num = ((addr - 0x8000) >> 4) as usize;
      let byte_num = (addr % 16) as usize;
      return Ok(self.tiles[tile_num][byte_num]);
    } else if addr >= 0x9800 && addr <= 0x9bff {
      let row = ((addr - 0x9800) >> 5) as usize;
      let col = ((addr - 0x9800) % 32) as usize;
      return Ok(self.bg_tile_map[row][col]);
    }

    match addr {
      0xff40 => Ok(self.lcdc.register),
      0xff41 => {
        // STAT
        let val = match self.mode() {
          Mode::HBlank => 0b00,
          Mode::VBlank => 0b01,
          Mode::OamSearch => 0b10,
          Mode::PixelTransfer => 0b11,
        };
        Ok(val)
      }
      0xff42 => Ok(self.scroll_x),
      0xff43 => Ok(self.scroll_y),
      0xff44 => Ok((self.bad_timer / HORIZONTAL_LINE_CYCLES) as u8),
      0xff47 => Ok(self.bg_palette),
      0xff48 => Ok(self.sprite_palettes[0]),
      0xff49 => Ok(self.sprite_palettes[1]),
      _ => Err(format!("Not implemented: PPU read {:#06x}", addr)),
    }
  }

  fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
    if addr == 0xff40 {
      let previously_enabled = self.lcdc.enabled();
      self.lcdc.register = val;
      if !previously_enabled && self.lcdc.enabled() {
        self.bad_timer = 0;
      }
    } else if addr == 0xff41 {
      println!("write to STAT {:x}", val);
    //unimplemented!("write to stat");
    } else if addr == 0xff42 {
      self.scroll_x = val;
    } else if addr == 0xff43 {
      self.scroll_y = val;
    } else if addr == 0xff47 {
      self.bg_palette = val;
    } else if addr == 0xff48 {
      self.sprite_palettes[0] = val;
    } else if addr == 0xff49 {
      self.sprite_palettes[1] = val;
    } else if addr >= 0xff40 {
      //println!("Write to PPU {:#06x} {:#04x}", addr, val);
    } else if addr >= 0x8000 && addr <= 0x97ff {
      let tile_num = ((addr - 0x8000) >> 4) as usize;
      let byte_num = (addr % 16) as usize;
      self.tiles[tile_num][byte_num] = val;
    } else if addr >= 0x9800 && addr <= 0x9bff {
      let row = ((addr - 0x9800) >> 5) as usize;
      let col = ((addr - 0x9800) % 32) as usize;
      self.bg_tile_map[row][col] = val;
    } else if addr >= 0xfe00 && addr <= 0xfe9f {
      let sprite = (addr as usize - 0xfe00) / 4;
      match (addr - 0xfe00) % 4 {
        0 => self.sprite_attributes[sprite].y_position = val,
        1 => self.sprite_attributes[sprite].x_position = val,
        2 => self.sprite_attributes[sprite].tile_number = val,
        3 => self.sprite_attributes[sprite].flags = val,
        _ => unreachable!("Unreachable in PPU write"),
      };
    }
    Ok(())
  }

  fn tick(&mut self, cycles: u16) -> Result<Option<Interrupt>, String> {
    if !self.lcdc.enabled() {
      return Ok(None);
    }
    let mut interrupt = None;
    for _ in 0..cycles {
      self.bad_timer += 1;
      self.bad_timer %= VERTICAL_LINE_CYCLES;

      // Draw line on last pixel transfer cycle
      if self.bad_timer % HORIZONTAL_LINE_CYCLES == OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES - 1
        && self.bad_timer < SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES
      {
        let y = (self.bad_timer / HORIZONTAL_LINE_CYCLES) as u8;
        self.draw_line(y);
      }

      if self.bad_timer == SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES {
        interrupt = Some(Interrupt::VBlank);
        self.draw_buffer = true;
        self.tile_data_dirty = true;
      }
    }

    Ok(interrupt)
  }
}

fn get_palette_colour(val: u8, palette: u8) -> u32 {
  if val > 4 {
    panic!("Bad value to get_colour");
  }
  match (palette >> (val * 2)) & 0b11 {
    0b00 => 0x00ff_ffff,
    0b01 => 0x00c0_c0c0,
    0b10 => 0x0096_9696,
    0b11 => 0x0000_0000,
    _ => 0x00ff_ffff,
  }
}

fn put_pixel(screen_buffer: &mut [u8], x: u8, y: u8, colour: u32) {
  let offset =
    (y as usize * SCREEN_WIDTH as usize * PIXEL_SIZE as usize) + (x as usize * PIXEL_SIZE);
  screen_buffer[offset] = (colour & 0xff) as u8;
  screen_buffer[offset + 1] = ((colour >> 8) & 0xff) as u8;
  screen_buffer[offset + 2] = ((colour >> 16) & 0xff) as u8;
}

#[allow(clippy::too_many_arguments)]
// TODO: Take care of this lint
fn draw_tile_line(
  tile: &[u8; 16],
  x: isize,
  y: u8,
  x_off: u8,
  line: u8,
  palette: u8,
  flip: bool,
  transparency: bool,
  buf: &mut [u8],
) {
  let data0 = tile[(line << 1) as usize];
  let data1 = tile[(line << 1) as usize + 1];
  for tile_x in x_off..8 {
    let x_pixel = if flip { tile_x } else { 7 - tile_x };
    let colour_val = (util::bit(x_pixel, data1) << 1) | util::bit(x_pixel, data0);
    if transparency && colour_val == 0 {
      continue;
    }
    let colour = get_palette_colour(colour_val, palette);
    let screen_x = x + tile_x as isize - x_off as isize;
    if screen_x < 0 {
      continue;
    } else if screen_x >= SCREEN_WIDTH as isize {
      break;
    }
    put_pixel(buf, screen_x as u8, y, colour);
  }
}
