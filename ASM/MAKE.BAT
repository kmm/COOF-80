REM Build Z80 binaries
REM Outputs images in Intel hex format suitable for burning to PROM.
REM Requires Telemark Assembler 'tasm.exe' in path,
REM not sure if these files will build with other assemblers.

mkdir bin
tasm -80 -b -g0 blinkenlights.asm bin\blinkenlights.bin
tasm -80 -b -g0 z80monitor.asm bin\z80monitor.bin