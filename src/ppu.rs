use cpu::Interrupt;
use cpu::TrapHandler;
use util::bit_mask;

const SCREEN_WIDTH: u64 = 160;
const SCREEN_HEIGHT: u64 = 144;
const PIXEL_SIZE: usize = 3;

const OAM_SEARCH_CYCLES: u64 = 20;
const PIXEL_TRANSFER_CYCLES: u64 = 43;
const H_BLANK_CYCLES: u64 = 51;
const HORIZONTAL_LINE_CYCLES: u64 = OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES + H_BLANK_CYCLES;

const V_BLANK_LINES: u64 = 10;
const VERTICAL_LINE_CYCLES: u64 = HORIZONTAL_LINE_CYCLES * (SCREEN_HEIGHT + V_BLANK_LINES);

const LCDC_DISPLAY_ENABLE: u8 = 1 << 7;
const LCDC_TILE_SELECT: u8 = 1 << 4;
const LCDC_SPRITE_SIZE: u8 = 1 << 2;
const LCDC_SPRITE_ENABLE: u8 = 1 << 1;
/*
const LCDC_WINDOW_TILE_MAP_DISPLAY_SELECT : u8 		= 1 << 6;
const LCDC_WINDOW_DISPLAY_ENABLE: u8 							= 1 << 5;
const LCDC_BG_TILE_MAP_DISPLAY_SELECT : u8				= 1 << 3;
const LCDC_DISPLAY_PRIORITY : u8									= 1 << 0;
*/

const SPRITE_FLAG_PRIORITY: u8 = 1 << 7;
const SPRITE_FLAG_Y_FLIP: u8 = 1 << 6;
const SPRITE_FLAG_X_FLIP: u8 = 1 << 5;
const SPRITE_FLAG_PALETTE: u8 = 1 << 4;
const SPRITE_FLAG_BANK: u8 = 1 << 3;

#[derive(Copy, Clone, Debug)]
enum Mode {
    HBlank,
    VBlank,
    OamSearch,
    PixelTransfer,
}

pub struct Ppu {
    bad_timer: u64,
    lcdc: u8,
    bg_palette: u8,
    tiles: Vec<[u8; 16]>,
    bg_tile_map: Vec<[u8; 32]>,
    scroll_x: u8,
    scroll_y: u8,
    sprite_attributes: Vec<SpriteAttributeTable>,
    sprite_palette0: u8,
    sprite_palette1: u8,

    pub screen_buffer: Box<[u8; (SCREEN_WIDTH as usize) * (SCREEN_HEIGHT as usize) * PIXEL_SIZE]>,
    draw_buffer: bool,

    tile_data_dirty: bool,
}

#[derive(Copy, Clone, Debug)]
struct SpriteAttributeTable {
    y_position: u8,
    x_position: u8,
    tile_number: u8,
    flags: u8,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            bad_timer: 0,
            lcdc: 0,
            bg_palette: 0,
            tiles: vec![[0; 16]; 384],
            bg_tile_map: vec![[0; 32]; 32],
            scroll_x: 0,
            scroll_y: 0,
            sprite_attributes: vec![SpriteAttributeTable::new(); 40],
            sprite_palette0: 0,
            sprite_palette1: 0,
            screen_buffer: Box::new(
                [0xff; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * PIXEL_SIZE],
            ),
            draw_buffer: false,
            tile_data_dirty: false,
        }
    }

    pub fn should_draw(&mut self) -> bool {
        if self.draw_buffer {
            self.draw_buffer = false;
            true
        } else {
            false
        }
    }

    fn sprite_size(&self) -> u8 {
        if self.lcdc & LCDC_SPRITE_SIZE == 0 {
            8
        } else {
            16
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
            if self.lcdc & LCDC_TILE_SELECT == 0 && tile < 0x80 {
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
        if self.lcdc & LCDC_SPRITE_ENABLE > 0 {
            let sprite_height = self.sprite_size();
            for sprite in self.sprite_attributes.iter() {
                // TODO: Handle other priority.
                if bit_mask(SPRITE_FLAG_PRIORITY, sprite.flags) {
                    continue;
                }
                if y + 16 < sprite.y_position || y + 16 >= sprite.y_position + sprite_height {
                    continue;
                }
                let mut tile_y = (y + 16 - sprite.y_position) % sprite_height;
                if bit_mask(SPRITE_FLAG_Y_FLIP, sprite.flags) {
                    tile_y = sprite_height - tile_y - 1;
                }
                let palette: u8 = if !bit_mask(SPRITE_FLAG_PALETTE, sprite.flags) {
                    self.sprite_palette0
                } else {
                    self.sprite_palette1
                };
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
                    bit_mask(SPRITE_FLAG_X_FLIP, sprite.flags),
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
                    let colour_val = (bit(data1, 7 - x) << 1) | bit(data0, 7 - x);
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
        if self.lcdc & LCDC_DISPLAY_ENABLE == 0 {
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
            0xff40 => Ok(self.lcdc),
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
            0xff48 => Ok(self.sprite_palette0),
            0xff49 => Ok(self.sprite_palette1),
            _ => Err(format!("Not implemented: PPU read {:#06x}", addr)),
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
        if addr == 0xff40 {
            if (self.lcdc & (1 << 7) == 0) && (val & (1 << 7) > 0) {
                self.bad_timer = 0;
            }
            self.lcdc = val;
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
            self.sprite_palette0 = val;
        } else if addr == 0xff49 {
            self.sprite_palette1 = val;
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
        if self.lcdc & LCDC_DISPLAY_ENABLE == 0 {
            return Ok(None);
        }
        let mut interrupt = None;
        for _ in 0..cycles {
            self.bad_timer += 1;
            self.bad_timer %= VERTICAL_LINE_CYCLES;

            // Draw line on last pixel transfer cycle
            if self.bad_timer % HORIZONTAL_LINE_CYCLES
                == OAM_SEARCH_CYCLES + PIXEL_TRANSFER_CYCLES - 1
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

impl SpriteAttributeTable {
    fn new() -> SpriteAttributeTable {
        SpriteAttributeTable {
            y_position: 0,
            x_position: 0,
            tile_number: 0,
            flags: 0,
        }
    }
}

fn bit(v: u8, b: u8) -> u8 {
    return (v & (1 << b)) >> b;
}

fn get_palette_colour(val: u8, palette: u8) -> u32 {
    if val > 4 {
        panic!("Bad value to get_colour");
    }
    match (palette >> (val * 2)) & 0b11 {
        0b00 => 0xffffff,
        0b01 => 0xc0c0c0,
        0b10 => 0x969696,
        0b11 => 0x000000,
        _ => 0xffffff,
    }
}

fn put_pixel(screen_buffer: &mut [u8], x: u8, y: u8, colour: u32) {
    let offset =
        (y as usize * SCREEN_WIDTH as usize * PIXEL_SIZE as usize) + (x as usize * PIXEL_SIZE);
    screen_buffer[offset] = (colour & 0xff) as u8;
    screen_buffer[offset + 1] = ((colour >> 8) & 0xff) as u8;
    screen_buffer[offset + 2] = ((colour >> 16) & 0xff) as u8;
}

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
        let colour_val = (bit(data1, x_pixel) << 1) | bit(data0, x_pixel);
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
