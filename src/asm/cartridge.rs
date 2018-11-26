const NINTENDO_LOGO: [u8; 48] = [
  0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
  0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
  0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

pub struct Header {
  pub entry_point: [u8; 4],
  pub logo: [u8; 48],
  // Note: On CGB the title contains a manufacturer's code and flag.
  pub title: [u8; 16],
  pub new_licensee: [u8; 2],
  pub sgb_flag: u8,
  pub cartidge_type: u8,
  pub rom_size: u8,
  pub ram_size: u8,
  pub destination: u8,
  pub old_licensee: u8,
  pub rom_version: u8,
  pub header_checksum: u8,
}

impl Header {
  pub fn new() -> Header {
    Header {
      entry_point: [0, 0xc3, 0x50, 0x01], // NOP; JP 0x150;
      logo: NINTENDO_LOGO,
      title: [0; 16],
      new_licensee: [0; 2],
      sgb_flag: 0,
      cartidge_type: 0,
      rom_size: 0,
      ram_size: 0,
      destination: 0,
      old_licensee: 0,
      rom_version: 0,
      header_checksum: 0,
    }
  }

  pub fn serialise(&self) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&self.entry_point);
    out.extend_from_slice(&self.logo);
    out.extend_from_slice(&self.title);
    out.extend_from_slice(&self.new_licensee);
    out.push(self.sgb_flag);
    out.push(self.cartidge_type);
    out.push(self.rom_size);
    out.push(self.ram_size);
    out.push(self.destination);
    out.push(self.old_licensee);
    out.push(self.rom_version);
    out.push(self.header_checksum);
    return out;
  }

  pub fn set_header_checksum(&mut self) {
    let mut checksum: u8 = 0;
    let bytes = self.serialise();
    for b in &bytes[0x34..0x4d] {
      checksum = checksum.wrapping_sub(*b);
      checksum = checksum.wrapping_sub(1);
    }
    self.header_checksum = checksum;
  }
}
