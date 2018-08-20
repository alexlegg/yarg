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
/*
const LCDC_WINDOW_TILE_MAP_DISPLAY_SELECT : u8 		= 1 << 6;
const LCDC_WINDOW_DISPLAY_ENABLE: u8 							= 1 << 5;
const LCDC_BG_AND_WINDOW_TILE_DATA_SELECT : u8		= 1 << 4;
const LCDC_BG_TILE_MAP_DISPLAY_SELECT : u8				= 1 << 3;
const LCDC_SPRITE_SIZE : u8												= 1 << 2;
const LCDC_SPRITE_DISPLAY_ENABLE : u8							= 1 << 1;
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

    pub screen_buffer: [u8; (SCREEN_WIDTH as usize) * (SCREEN_HEIGHT as usize) * PIXEL_SIZE],
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
            screen_buffer: [0xff; SCREEN_WIDTH as usize * SCREEN_HEIGHT as usize * PIXEL_SIZE],
        }
    }

    pub fn should_draw(&self) -> bool {
        self.bad_timer == SCREEN_HEIGHT * HORIZONTAL_LINE_CYCLES
    }

    fn draw_pixel(&mut self, screen_x: u8, screen_y: u8) {
        let x = screen_x.wrapping_add(self.scroll_y);
        let y = screen_y.wrapping_add(self.scroll_x);
        let tile = self.bg_tile_map[(y >> 3) as usize][(x >> 3) as usize];
        let tile_y = ((y % 8) << 1) as usize;
        let data0 = self.tiles[tile as usize][tile_y];
        let data1 = self.tiles[tile as usize][tile_y];
        let colour_val = (bit(data0, 7 - (x % 8)) << 1) | bit(data1, 7 - (x % 8));
        let colour = match colour_val {
            0b00 => 0xffffff,
            0b01 => 0xc0c0c0,
            0b10 => 0x969696,
            0b11 => 0x000000,
            _ => 0xffffff,
        };
        self.put_pixel(screen_x, screen_y, colour);
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
        //println!("Read from PPU {:#06x}", addr);
        match addr {
            0xff40 => Ok(self.lcdc),
            0xff42 => Ok(self.scroll_x),
            0xff43 => Ok(self.scroll_y),
            0xff44 => Ok((self.bad_timer % (SCREEN_HEIGHT + V_BLANK_LINES)) as u8),
            _ => Err(format!("Not implemented: PPU read {:#06x}", addr)),
        }
    }

    fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
        if addr == 0xff40 {
            println!("write to LCDC {:x}", val);
            self.lcdc = val;
        } else if addr == 0xff42 {
            self.scroll_x = val;
        } else if addr == 0xff43 {
            self.scroll_y = val;
        } else if addr == 0xff47 {
            self.bg_palette = val;
        } else if addr >= 0xff40 {
            println!("Write to PPU {:#06x} {:#04x}", addr, val);
        }

        if addr >= 0x8000 && addr <= 0x97ff {
            let tile_num = ((addr - 0x8000) >> 4) as usize;
            let byte_num = (addr % 16) as usize;
            self.tiles[tile_num][byte_num] = val;
        } else if addr >= 0x9800 && addr <= 0x9bff {
            let row = ((addr - 0x9800) >> 5) as usize;
            let col = ((addr - 0x9800) % 32) as usize;
            self.bg_tile_map[row][col] = val;
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
            return (Ok(Some(Interrupt::VBlank)));
        }

        Ok(None)
    }
}

fn bit(v: u8, b: u8) -> u8 {
    return (v & (1 << b)) >> b;
}
