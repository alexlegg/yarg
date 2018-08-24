mod cpu;
mod emulator;
mod opcode;
mod ppu;
mod sdl;
mod timer;

fn main() {
    let emu = emulator::Emulator::new();
    sdl::init(emu);
}
