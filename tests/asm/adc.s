// Clear carry
scf
ccf

// Test A + A
ld a, 2
adc a, a
cp 4
jp nz, fail

// Test A + A + carry
ld a, 2
scf
adc a, a
cp 5
jp nz, fail

// Test A + B
ld a, 2
ld b, 3
adc a, b
cp 5
jp nz, fail

// Test A + B + carry
ld a, 2
ld b, 3
scf
adc a, b
cp 6
jp nz, fail

// Test A + C
ld a, 2
ld c, 4
adc a, c
cp 6
jp nz, fail

// Test A + C + carry
ld a, 2
ld c, 4
scf
adc a, c
cp 7
jp nz, fail

// Test A + D
ld a, 2
ld d, 5
adc a, d
cp 7
jp nz, fail

// Test A + D + carry
ld a, 2
ld d, 5
scf
adc a, d
cp 8
jp nz, fail

// Test A + E
ld a, 2
ld e, 6
adc a, e
cp 8
jp nz, fail

// Test A + E + carry
ld a, 2
ld e, 6
scf
adc a, e
cp 9
jp nz, fail

// Test A + H
ld a, 2
ld h, 7
adc a, h
cp 9
jp nz, fail

// Test A + H + carry
ld a, 2
ld h, 7
scf
adc a, h
cp 10
jp nz, fail

// Test A + L
ld a, 2
ld l, 8
adc a, l
cp 10 
jp nz, fail

// Test A + L + carry
ld a, 2
ld h, 8
scf
adc a, l
cp 11
jp nz, fail

// Test A + d8
ld a, 2
adc a, 9
cp 11
jp nz, fail

// Test A + d8 + carry
ld a, 2
scf
adc a, 9
cp 12
jp nz, fail

// Test carry
ld a, 0xf0
ld b, 0x9
adc a, b
jp c, fail
cp 0xf9
jp nz, fail
ld a, 0xf0
ld b, 0x11
adc a, b
jp nc, fail
cp 1
jp nz, fail

// Test zero flag
ld a, 1
adc a, a
jp z, fail
cp 2
jp nz, fail

jp pass

.section 0x300
fail: halt

.section 0x400
pass: halt