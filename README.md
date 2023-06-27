# GBADA

**GBADA is a WIP. At this time, the emulator is fairly useless!**

GBADA is a Gameboy emulator written in the Ada programming language. While
there are many better Gameboy emulators out there, I have not found one written
in Ada and I thought it'd be a good experience to better understand hardware
and devices as well as improve my handiness with Ada.

GBADA features near-accurate CPU and memory emulation, with input and screen
emulation implemented via the SDL2 C Library, interfaced through the SDLADA
binding found [here](https://github.com/Lucretia/sdlada).

Future Features:
- `ncurses` implementation for input and screen rendering in the terminal (for
  terminals which can zoom out far enough)
- Gameboy Color support
- Color palette selection

## Building

Dependencies:
- **SDL2**

Obtain dependencies with **Alire**:
```
alr update
```

Build with **Alire** from the root directory:
```
alr build
```

## Copyright
Copyright (C) 2023, Dylan Eskew
