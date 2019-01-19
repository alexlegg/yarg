use crate::cpu::Interrupt;
use crate::cpu::TrapHandler;
use crate::util;
use serde::{Deserialize, Serialize};

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;
const PIXEL_SIZE: usize = 4;

const OAM_SEARCH_CYCLES: u64 = 20;
const PIXEL_TRANSFER_CYCLES: u64 = 43;
const H_BLANK_CYCLES: u64 = 51;
const HORIZONTAL_LINE_CYCLES: u64 = OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES + H_BLANK_CYCLES;

const V_BLANK_LINES: u64 = 10;
const VERTICAL_LINE_CYCLES: u64 = HORIZONTAL_LINE_CYCLES * (SCREEN_HEIGHT as u64 + V_BLANK_LINES);

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
#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
struct LcdControl {
  register: u8,
}

impl LcdControl {
  fn enabled(self) -> bool {
    util::bit_bool(7, self.register)
  }

  fn sprite_size(self) -> u8 {
    if util::bit(2, self.register) == 0 {
      8
    } else {
      16
    }
  }

  fn tileset(self) -> u8 {
    util::bit(4, self.register)
  }

  fn sprites_enabled(self) -> bool {
    util::bit_bool(1, self.register)
  }
}

#[derive(Copy, Clone, Debug)]
enum Mode {
  HBlank = 0b00,
  VBlank = 0b01,
  OamSearch = 0b10,
  PixelTransfer = 0b11,
}

/// LCD Status Register
///
/// Bit 6 - LYC == LY Interrupt
/// Bit 5 - Mode 2 (OamSearch) Interrupt
/// Bit 4 - Mode 1 (VBlank) Interrupt
/// Bit 3 - Mode 0 (HBlank) Interrupt
/// Bit 2 - Match flag LYC == LCDC LY
/// Bits 1..0 - Mode
/// (Source: GameBoy Programming Manual)
#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
struct LcdStatus {
  register: u8,
}

impl LcdStatus {
  fn write(&mut self, val: u8) {
    self.register = (val & 0xf0) | (self.register & 0x0f);
  }

  fn set_mode(&mut self, mode: Mode) {
    self.register = (self.register & 0xf0) | (mode as u8);
  }

  fn set_coincidence(&mut self, val: bool) {
    if val {
      self.register |= 0b0000_0100;
    } else {
      self.register &= !(0b0000_0100);
    }
  }

  fn coincidence_interrupt_enabled(self) -> bool {
    util::bit_bool(6, self.register)
  }

  fn oam_search_interrupt_enabled(self) -> bool {
    util::bit_bool(5, self.register)
  }

  fn vblank_interrupt_enabled(self) -> bool {
    util::bit_bool(4, self.register)
  }

  fn hblank_interrupt_enabled(self) -> bool {
    util::bit_bool(3, self.register)
  }
}

#[derive(Serialize, Deserialize)]
pub struct Ppu {
  cycles: u64,
  lcdc: LcdControl,
  status: LcdStatus,
  bg_palette: u8,
  tiles: Vec<[u8; 16]>,
  bg_tile_map: Vec<[u8; 32]>,
  scroll_x: u8,
  scroll_y: u8,
  ly_compare: u8,
  sprite_attributes: Vec<SpriteAttributeTable>,
  sprite_palettes: [u8; 2],

  pub screen_buffer: Vec<u8>,
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
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
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

  fn sprite_priority(self) -> bool {
    util::bit_bool(7, self.flags)
  }

  fn vertical_flip(self) -> bool {
    util::bit_bool(6, self.flags)
  }

  fn horizonal_flip(self) -> bool {
    util::bit_bool(5, self.flags)
  }

  fn palette(self) -> usize {
    usize::from(util::bit(4, self.flags))
  }
}

impl Default for Ppu {
  fn default() -> Ppu {
    Ppu {
      cycles: 0,
      lcdc: LcdControl::default(),
      status: LcdStatus::default(),
      bg_palette: 0,
      tiles: vec![[0; 16]; 384],
      bg_tile_map: vec![[0; 32]; 32],
      scroll_x: 0,
      scroll_y: 0,
      ly_compare: 0,
      sprite_attributes: vec![SpriteAttributeTable::new(); 40],
      sprite_palettes: [0; 2],
      screen_buffer: vec![0xff; SCREEN_WIDTH * SCREEN_HEIGHT * PIXEL_SIZE],
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

  fn get_bg_pixel(&self, x: u8, y: u8) -> u8 {
    let bg_x = x.wrapping_add(self.scroll_x);
    let bg_y = y.wrapping_add(self.scroll_y);
    let mut tile = self.bg_tile_map[(bg_y >> 3) as usize][(bg_x >> 3) as usize] as usize;
    if self.lcdc.tileset() == 0 && tile < 0x80 {
      tile += 0x100;
    }
    let line = bg_y as usize % 8;
    let tile_x = 7 - (bg_x % 8);
    let data0 = self.tiles[tile][(line << 1)];
    let data1 = self.tiles[tile][(line << 1) + 1];
    (util::bit(tile_x, data1) << 1) | util::bit(tile_x, data0)
  }

  fn draw_line(&mut self, y: u8) {
    // Construct a buffer of background pixel values.
    let mut buffer = Vec::new();
    for x in 0..SCREEN_WIDTH {
      let colour_val = self.get_bg_pixel(x as u8, y);
      buffer.push((colour_val, self.bg_palette));
    }

    // Draw sprites on top.
    // TODO: Handle priority between sprites (lower x value = higher priority).
    if self.lcdc.sprites_enabled() {
      let sprite_height = self.lcdc.sprite_size();
      for sprite in self.sprite_attributes.iter() {
        if y + 16 < sprite.y_position || y + 16 >= sprite.y_position + sprite_height {
          continue;
        }
        let mut tile_y = (y + 16 - sprite.y_position) % sprite_height;
        if sprite.vertical_flip() {
          tile_y = sprite_height - tile_y - 1;
        }
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
        let data0 = tile[(tile_y as usize) << 1];
        let data1 = tile[((tile_y as usize) << 1) + 1];
        let palette = self.sprite_palettes[sprite.palette()];
        let x = sprite.x_position as isize - 8;
        for tile_x in 0..8 {
          let screen_x = x + tile_x as isize;
          if screen_x < 0 {
            continue;
          } else if screen_x >= SCREEN_WIDTH as isize {
            break;
          }
          let x_pixel = if sprite.horizonal_flip() {
            tile_x
          } else {
            7 - tile_x
          };
          let colour_val = (util::bit(x_pixel, data1) << 1) | util::bit(x_pixel, data0);
          // Apply transparency.
          if colour_val == 0 {
            continue;
          }
          // Apply OBJ-to-BG priority.
          if sprite.sprite_priority() && buffer[screen_x as usize].0 > 0 {
            continue;
          }
          buffer[screen_x as usize] = (colour_val, palette);
        }
      }
    }

    for (x, (val, palette)) in buffer.iter().enumerate() {
      let colour = get_palette_colour(*val, *palette);
      put_pixel(&mut self.screen_buffer, x, y as usize, colour);
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

  fn ly(&self) -> u8 {
    (self.cycles / HORIZONTAL_LINE_CYCLES) as u8
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
      0xff41 => Ok(self.status.register),
      0xff42 => Ok(self.scroll_y),
      0xff43 => Ok(self.scroll_x),
      0xff44 => Ok(self.ly()),
      0xff45 => Ok(self.ly_compare),
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
        self.cycles = 0;
      }
    } else if addr == 0xff41 {
      self.status.write(val);
    } else if addr == 0xff42 {
      self.scroll_y = val;
    } else if addr == 0xff43 {
      self.scroll_x = val;
    } else if addr == 0xff45 {
      self.ly_compare = val;
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
      self.status.set_mode(Mode::HBlank);
      return Ok(None);
    }
    let mut interrupt = None;
    for _ in 0..cycles {
      self.cycles += 1;
      self.cycles %= VERTICAL_LINE_CYCLES;

      // Update mode and trigger interrupts.
      if self.cycles > SCREEN_HEIGHT as u64 * HORIZONTAL_LINE_CYCLES {
        self.status.set_mode(Mode::VBlank);
        if self.status.vblank_interrupt_enabled() {
          interrupt = Some(Interrupt::LCDStat);
        }
      }
      let horizontal_cycles = self.cycles % HORIZONTAL_LINE_CYCLES;
      if horizontal_cycles < OAM_SEARCH_CYCLES {
        self.status.set_mode(Mode::OamSearch);
        if self.status.oam_search_interrupt_enabled() {
          interrupt = Some(Interrupt::LCDStat);
        }
      } else if horizontal_cycles - OAM_SEARCH_CYCLES < PIXEL_TRANSFER_CYCLES {
        self.status.set_mode(Mode::PixelTransfer);
      } else {
        self.status.set_mode(Mode::HBlank);
        if self.status.hblank_interrupt_enabled() {
          interrupt = Some(Interrupt::LCDStat);
        }
      }

      let coincidence = self.ly() == self.ly_compare;
      self.status.set_coincidence(coincidence);
      if coincidence && self.status.coincidence_interrupt_enabled() {
        interrupt = Some(Interrupt::LCDStat);
      }

      // Draw line on last pixel transfer cycle
      if horizontal_cycles == OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES - 1
        && self.cycles < SCREEN_HEIGHT as u64 * HORIZONTAL_LINE_CYCLES
      {
        let ly = self.ly();
        self.draw_line(ly);
      }

      if self.cycles == SCREEN_HEIGHT as u64 * HORIZONTAL_LINE_CYCLES {
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

fn put_pixel(screen_buffer: &mut [u8], x: usize, y: usize, colour: u32) {
  let offset = (y * SCREEN_WIDTH * PIXEL_SIZE) + (x * PIXEL_SIZE);
  screen_buffer[offset] = (colour & 0xff) as u8;
  screen_buffer[offset + 1] = ((colour >> 8) & 0xff) as u8;
  screen_buffer[offset + 2] = ((colour >> 16) & 0xff) as u8;
  screen_buffer[offset + 3] = 0u8;
}
