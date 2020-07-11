# Vectrex Extensions for VecForth

The files in this directory provide support for the Vectrex BIOS API and additional hardware. See also the `include.fs` compiler control file, one directory level up.

## Directory Contents

- **vectrex_equ.fs** - Cross-compilation labels for locations in the Vectrex address space.

- **cartheader.fs** - This file lays down the manadatory Vectrex header block to the start of the memory address space in ROM i.e. starting at $0000. Without this block, the Vectrex BIOS will run the built-in Minestorm game.

- **vecfeverheader.fs** - This file lays down a data block near the start of the address space (just after the Vectrex header block) and is used by the VecFever on boot-up to configure itself. VecForth uses this to trigger the serial port for use as a terminal. If you're not using  a VecFever as a terminal serial port, you can remove or comment out this file from the `include.fs` control file.

- **ym_music_data.fs** - Test music data for the sound synthesizer test words.

- **vector_lists.fs** - Test vector list data for the drawing test words.

- **bios_api.fs** - Assembler code that provides an interface to Forth programs so the can use the standard Vectrex BIOS ROM routines, for use of I/O devices, memory, genral purpose functions etc.

- **bios_api_tests.fs** - Forth test words that invoke (almost) all of the BIOS functions in the BIOS API file.

- **bios_api_tests_old.fs** - Early Forth test words that were written before there was an API. Not really needed, but you might find something useful in it.

- **vecfever_exit.fs** - Assembly code to allow Forth to initiate an exit from VecFever ROM emulation and return control back the VecFever menu interface.
