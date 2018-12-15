pub fn bit(index: u8, val: u8) -> u8 {
  (val & (1 << index)) >> index
}

pub fn bit_bool(index: u8, val: u8) -> bool {
	bit(index, val) > 0
}

pub fn bits(high: u8, low: u8, val: u8) -> u8 {
  (val & ((1 << (high + 1)) - 1)) >> low
}

pub fn bit_mask(mask: u8, val: u8) -> bool {
  val & mask > 0
}
