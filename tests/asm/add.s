// Test A + A
ld a, 2
add a, a
cp 4
jp nz, fail

// Test A + B
ld a, 2
ld b, 3
add a, b
cp 5
jp nz, fail

// Test A + C
ld a, 2
ld c, 4
add a, c
cp 6
jp nz, fail

// Test A + D
ld a, 2
ld d, 5
add a, d
cp 7
jp nz, fail

// Test A + E
ld a, 2
ld e, 6
add a, e
cp 8
jp nz, fail

// Test A + H
ld a, 2
ld h, 7
add a, h
cp 9
jp nz, fail

// Test A + L
ld a, 2
ld l, 8
add a, l
cp 10 
jp nz, fail

// Test A + d8
ld a, 2
add a, 9
cp 11
jp nz, fail

// Test carry
ld a, 0xf0
ld b, 0x9
add a, b
jp c, fail
cp 0xf9
jp nz, fail
ld a, 0xf0
ld b, 0x11
add a, b
jp nc, fail
cp 1
jp nz, fail

// Test zero flag
ld a, 1
add a, a
jp z, fail
cp 2
jp nz, fail

jp pass

.section 0x300
fail: halt

.section 0x400
pass: halt