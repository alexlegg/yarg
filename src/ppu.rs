use cpu::Interrupt;
use cpu::TrapHandler;

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
    tiles: [[u8; 16]; 384],
    bg_tile_map: [[u8; 32]; 32],
    scroll_x: u8,
    scroll_y: u8,
    sprite_attributes: [SpriteAttributeTable; 40],
    sprite_palette0: u8,
    sprite_palette1: u8,

    pub screen_buffer: [u8; (SCREEN_WIDTH as usize) * (SCREEN_HEIGHT as usize) * PIXEL_SIZE],
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
            tiles: [[0; 16]; 384],
            bg_tile_map: [[0; 32]; 32],
            scroll_x: 0,
            scroll_y: 0,
            sprite_attributes: [SpriteAttributeTable::new(); 40],
            sprite_palette0: 0,
            sprite_palette1: 0,
            screen_buffer: [0xff; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * PIXEL_SIZE],
        }
    }

    pub fn should_draw(&self) -> bool {
        self.bad_timer == SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES
    }

    // TODO Don't do this pixel by pixel (obviously).
    // TODO Refactor this horrible mess
    fn draw_pixel(&mut self, x: u8, y: u8) {
        if self.lcdc & LCDC_SPRITE_ENABLE > 0 {
            let sprite_height = if self.lcdc & LCDC_SPRITE_SIZE == 0 {
                8
            } else {
                16
            };
            let mut colour: Option<u32> = None;
            for sprite in self.sprite_attributes.iter() {
                if sprite.y_position == 0 || sprite.y_position as u64 >= SCREEN_HEIGHT + 16 {
                    continue;
                }
                if sprite.x_position == 0 || sprite.x_position as u64 >= SCREEN_WIDTH + 8 {
                    continue;
                }
                if y + 16 < sprite.y_position || y + 16 >= sprite.y_position + sprite_height {
                    continue;
                }
                if x + 8 < sprite.x_position || x >= sprite.x_position {
                    continue;
                }
                let s_x = x + 8 - sprite.x_position;
                let tile_y = (((y + 16 - sprite.y_position) % 8) << 1) as usize;
                // TODO Handle sprites of height 16.
                let data0 = self.tiles[sprite.tile_number as usize][tile_y];
                let data1 = self.tiles[sprite.tile_number as usize][tile_y + 1];
                let colour_val = (bit(data1, 7 - (s_x % 8)) << 1) | bit(data0, 7 - (s_x % 8));
                // TODO palette
                let palette: u8 = if sprite.flags & (1 << 3) == 0 {
                    self.sprite_palette0
                } else {
                    self.sprite_palette1
                };
                if colour_val != 0 {
                    colour = Some(get_palette_colour(colour_val, palette));
                }
            }
            // If we didn't find a sprite pixel, or it was transparent then fall through to background.
            if let Some(c) = colour {
                self.put_pixel(x, y, c);
                return;
            }
        }
        let bg_x = x.wrapping_add(self.scroll_y);
        let bg_y = y.wrapping_add(self.scroll_x);
        let tile = self.bg_tile_map[(bg_y >> 3) as usize][(bg_x >> 3) as usize];
        let tile_y = ((bg_y % 8) << 1) as usize;
        let mut tile_num = tile as usize;
        if self.lcdc & LCDC_TILE_SELECT == 0 && tile <= 0x80 {
            tile_num += 0x100;
        }
        let data0 = self.tiles[tile_num][tile_y];
        let data1 = self.tiles[tile_num][tile_y + 1];
        let colour_val = (bit(data1, 7 - (bg_x % 8)) << 1) | bit(data0, 7 - (bg_x % 8));
        let colour = get_palette_colour(colour_val, self.bg_palette);
        self.put_pixel(x, y, colour);
    }

    fn put_pixel(&mut self, x: u8, y: u8, colour: u32) {
        let offset =
            (y as usize * SCREEN_WIDTH as usize * PIXEL_SIZE as usize) + (x as usize * PIXEL_SIZE);
        self.screen_buffer[offset] = (colour & 0xff) as u8;
        self.screen_buffer[offset + 1] = ((colour >> 8) & 0xff) as u8;
        self.screen_buffer[offset + 2] = ((colour >> 16) & 0xff) as u8;
    }

    fn mode(&self) -> Mode {
        // TODO Probably faster to just store the mode and update in tick().
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
        match addr {
            0xff40 => Ok(self.lcdc),
            0xff41 => {
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
            0xff44 => Ok((self.bad_timer % (SCREEN_HEIGHT + V_BLANK_LINES)) as u8),
            0xff47 => Ok(self.bg_palette),
            0xff48 => Ok(self.sprite_palette0),
            0xff49 => Ok(self.sprite_palette1),
            _ => Err(format!("Not implemented: PPU read {:#06x}", addr)),
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
        if addr == 0xff40 {
            println!("write to LCDC {:x}", val);
            self.lcdc = val;
        } else if addr == 0xff41 {
            println!("write to STAT {:x}", val);
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
            println!("Write to PPU {:#06x} {:#04x}", addr, val);
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
        self.bad_timer += cycles as u64;
        self.bad_timer %= VERTICAL_LINE_CYCLES;

        if let Mode::PixelTransfer = self.mode() {
            let x = ((self.bad_timer % HORIZONTAL_LINE_CYCLES) - OAM_SEARCH_CYCLES) * 4;
            let y = self.bad_timer / HORIZONTAL_LINE_CYCLES;
            for i in x..(x + 4) {
                if i < SCREEN_WIDTH && y < SCREEN_HEIGHT {
                    self.draw_pixel(i as u8, y as u8);
                }
            }
        }

        if self.bad_timer == SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES {
            return Ok(Some(Interrupt::VBlank));
        }

        Ok(None)
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
