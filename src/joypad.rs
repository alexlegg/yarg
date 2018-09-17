use cpu::{Interrupt, TrapHandler};

#[derive(Copy, Clone, Debug)]
pub struct JoypadInput {
  pub up: bool,
  pub down: bool,
  pub left: bool,
  pub right: bool,
  pub start: bool,
  pub select: bool,
  pub button_a: bool,
  pub button_b: bool,
}

impl JoypadInput {
  pub fn new() -> JoypadInput {
    JoypadInput {
      up: false,
      down: false,
      left: false,
      right: false,
      start: false,
      select: false,
      button_a: false,
      button_b: false,
    }
  }
}

pub struct Joypad {
  select_buttons: bool,
  select_directions: bool,
  state: JoypadInput,
}

impl Joypad {
  pub fn new() -> Joypad {
    Joypad {
      select_buttons: false,
      select_directions: false,
      state: JoypadInput::new(),
    }
  }

  pub fn set_state(&mut self, state: JoypadInput) {
    // TODO interrupts
    self.state = state;
  }
}

impl TrapHandler for Joypad {
  fn read(&self, addr: u16) -> Result<u8, String> {
    if addr != 0xff00 {
      return Err(format!("Bad addr on joypad read: {:#06x}", addr));
    }
    let mut val: u8 = 0xff;
    if self.select_buttons {
      val &= !(1 << 5);
    }
    if self.select_directions {
      val &= !(1 << 4);
    }
    if (self.select_buttons && self.state.start) || (self.select_directions && self.state.down) {
      val &= !(1 << 3);
    }
    if (self.select_buttons && self.state.select) || (self.select_directions && self.state.up) {
      val &= !(1 << 2);
    }
    if (self.select_buttons && self.state.button_b) || (self.select_directions && self.state.left) {
      val &= !(1 << 1);
    }
    if (self.select_buttons && self.state.button_a) || (self.select_directions && self.state.right)
    {
      val &= !(1 << 0);
    }
    Ok(val)
  }

  fn write(&mut self, addr: u16, val: u8) -> Result<(), String> {
    if addr != 0xff00 {
      return Err(format!("Bad addr on joypad write: {:#06x}", addr));
    }
    self.select_buttons = val & (1 << 5) == 0;
    self.select_directions = val & (1 << 4) == 0;
    Ok(())
  }

  fn tick(&mut self, _cycles: u16) -> Result<Option<Interrupt>, String> {
    Ok(None)
  }
}
