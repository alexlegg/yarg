mod cpu;
mod emulator;
mod opcode;
mod ppu;
mod sdl;

fn main() {
    let emu = emulator::Emulator::new();
    sdl::init(emu);
}
