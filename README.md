# VecForth

Vectrex Forth is a port of Brad Rodriguez' CamelForth/6809 to the Vectrex video game console, with an added Forth to Vectrex BIOS API for easy programming.

1. [Introduction](#Introduction)
1. [What Can You Do With It?](#What-Can-You-Do-With-It)
1. [How About Performance?](#How-About-Performance)
1. [Can I Write My Game In It?](#Can-I-Write-My-Game-In-It)
1. [Repository Contents](#Repository-Contents)
1. [Hardware Requirements](#Hardware-Requirements)
1. [Getting Started](#Getting-Started)
1. [YouTube Videos](#YouTube-Videos)
1. [To-Do List](#To-Do-List)
1. [Links](#Links)
1. [License](#License)

## Introduction

In the late 1990s, I came across a Vectrex at some video game show. I remember thinking it was a cool little device with a vector display that played a sort-of Asteroids game, but I didn't really pay any attention because, you know, I already had actual Asteroids and Star Wars full-size arcade machines in my garage. By comparison, the Vectrex was clearly just a toy.

Fast-forward 20 years, I acquired a non-working Vectrex for free, which I fixed and then wondered what I could do with it, maybe there was more than just a version of Asteroids. Seeing it had a 6809 CPU, I remembered the open source CamelForth for 6809 I'd seen some years previously, which included a PC-based cross-compiler. This was very similar to the professional Z80180 single-board computers I had programmed in Forth in the 1990s.

I thought maybe I could get CamelForth running on the Vectrex? At first sight, the Vectrex presents a few challenges to this scenario, namely 1K of RAM, no serial communications port, keyboard or other usable display. However, the cartridge port is quite fully featured, the computer address space is completely open and all the circuit diagrams are readily available. I also found a thriving community of Vectrex enthusiasts making all sorts of hardware add-ons, new software and all sorts of other stuff. As it turns out, getting CamelForth running on the Vectrex was quite doable.

#### [Back to top](#VecForth)

## What Can You Do With It?

Good question! Lots of people have heard of Forth, but not many know much about it. In the 1970s and 80s, Forth was very popular in microcomputer programming circles as it could provide a complete interactive operating system and programming language on the device itself, with good performance, whilst consuming very few system resources. Since the 1990s, coupled with a PC-based cross-compiler, Forth offers an incredibly efficient development platform for custom hardware, even today.

Now, for the Vectrex specifically, with VecForth you can:

- Use the Vectrex like a 1980s microcomputer, such as the Commodore 64 for example, via a serially-connected PC terminal emulator, entering commands on the keyboard and getting immediate answers. You can write Forth programs on-the-fly.

- Live experiment with the Vectrex hardware directly on the device, e.g. read/write directly to the timers, joysticks, sound generator, vector monitor.

- Write new programs/games in Forth on a PC and - in literally 1 or 2 seconds - cross-compile, transfer to and run on the Vectrex for interactive testing/debugging.

- Use Forth calls to the built-in Vectrex BIOS routines using an API written in assembly.

- Compile a finished application to be completely standalone, just like any other Vectrex ROM image.

- Use the built-in assembler to write machine-language programs or subroutines that can be called from Forth.

- The compiler can be configured to output a symbol file with the target binary that can be loaded into the VIDE emulator/debugger, if you're really want to deep-dive. (Currently removed, will be reinstated soon!)

- Develop games just like they did at Atari Coin-Op in the early 1980's, using pretty much the same setup as the [Blue Box/White box, explained here](https://www.jmargolin.com/vmail/vmail.htm).

#### [Back to top](#VecForth)

## How About Performance?

OK, let's address this head-on, starting with a few Q&A:

- **Does Forth run as fast as native assembly language?**
  
  No, unless you're a bad assembly language coder.

- **Does Forth run as fast as C?**
  
  Yes, if you're using a modern commercial native "Subroutine Threaded Code" (STC) Forth compiler, optimisation technology produces code that runs as fast as optimised C. (See Links section.)

- **Does VecForth run as fast as C on the Vectrex?**
  
  Maybe, but probably not. VecForth is non-commercial "Direct Threaded Code" (DTC) compiler, so code probably will not be as fast as compiled C for the Vectrex, unless you add some optimisations. How much faster or slower, I really don't know yet, I haven't tried it - I'm just making educated guesses. With it's two hardware stacks and opcodes to use them, the 6809 is particularly suitable for Forth, but my money is still on C running faster. I look forward to finding out!

- **Forth has an interactive command prompt, like BASIC. Does it use an interpreter, like BASIC, or does it compile code, like C?**
  
  Yes and yes! Forth has a command interpreter like BASIC, whatever you type gets interpreted and executed when you press Enter. But if you enter the : (colon) command, the interpreter switches to compile mode and compiles everything you type, instead of running it. Entering the ; (semi-colon) command returns Forth back to interpret mode. Once you've compiled some code, you can then use the interpreter to run it. Whilst developing and testing your program, you can cross-compile it to a binary target image at any time and run it at startup, bypassing the command interpreter, just like an assembled binary.

- **Does VecForth run faster than BASIC?**
  
  I haven't done any substantial tests of my own, but if you believe the tag line of the 1980's Jupiter Ace home computer advert, then Forth is "10 times faster and 4 times more compact than BASIC". The Jupiter Ace used a Z80 CPU, which has only one stack, the 6809 CPU has two and significantly more Forth-friendly instructions. In practice, once you start using Forth, it becomes clear that it's way faster than BASIC.

#### [Back to top](#VecForth)

## Can I Write My Game In It?
  
  If you're an assembly guru looking to create the next AAA game title for the Vectrex, to rival "Protector", "Vector Patrol" or "Vectorblade", you're probably not going to use Forth to produce the core game code. However, you could use the Forth environment, because the PC-hosted cross-compiler also includes an assembler and you can prototype with Forth code *really* quickly.
  
  If you're not an assembly wizard and learning C sounds like a lot of hard work, or your design does not demand the ultimate in performance, then Forth might be what you're looking for, it's fast, flexible, easy to learn and *extensible*, meaning that your program is literally extending the Forth language to be specific to your program. To quote Wolf Wejgaard, regarding Forth, "*The program is the compiler*".
  
  Either way, as mentioned above and compared with other languages, using Forth you can get something running really fast. The code/compile/transfer/test cycle from PC to physical hardware is 1 or 2 seconds and, if that's not fast enough, you can also code directly on the Vectrex via the terminal command prompt. With all the time you save, if parts of the code are not fast enough, you can rewrite them in assembler or improve your algorithm.

#### [Back to top](#VecForth)

## Repository Contents

- **CamelForth\_Original** - The original Brad Rodriguez 1995 CamelForth 6809 source code and Neal Crook's 2015 ANS Forth update to allow PC compilation using GForth as a host.

- **VecForth** - A 2020-onwards update based on Neal Crook's 2015 code, converted to use plain text source code files and with several further enhancements.
  
  - **Chromium** - The Forth 6809 target cross-compiler and assembler that is hosted on Gforth and tested under Windows. It should also work on Linux, Mac and probably Android.

  - **CamelForth** - The core ANS-compliant Forth implementation for the 6809 target.

  - **Vectrex** - The Vectrex-specific config files, the Forth to Vectrex BIOS API and associated test/tutorial code for each of the BIOS API calls.

  - ***Your Program*** - One or more directories with your custom program/game code in it.
  
  See also the further readme files in the lower level directories of the repository.

#### [Back to top](#VecForth)

## Hardware Requirements

### Recommended:

- A working Vectrex video game console

- A VecFever multicart with USB connection to PC as storage device *and* a second USB connection to PC for serial communications (e.g. Vectrex Ultimate)

A key feature of Forth is that it has an interactive command-line interpreter that runs on the target via a terminal, allowing you the greatest freedom to experiment with the bare-metal hardware in real time. Indeed, in the early days, this was pretty much the *only* way of using Forth. The VecFever provides a fast code/compile/transfer/run development cycle and provides a software serial port connection to the PC for a terminal. 

### Minimum:

- A working Vectrex video game console

- A non-VecFever multicart or dedicated EPROM emulator, i.e. something that will allow you to use a custom binary file e.g. a VecFlash or VecMulti in developer mode

If you don't have a VecFever with additional serial port, you can compile Forth programs to a regular binary that doesn't bypasses the command-line interpreter and runs just like any other Vectrex binary file. You're missing out the essence of Forth with the command line, but you'll still get your programs written quickly.

### Absolute Minimum:

- A working Vectrex emulator

Really, this is the same as above. You're getting further away from the bare-metal and your retro-coder street-cred is going out the window now. You're only a couple of steps away from writing in 'C', but hey, I'm not judging.

### But you could try...

- Adding a seperate hardware serial port to the Vectrex and using that for serial communications to the PC. Try an M6850/6350 and FT232R or equivalent (which is what the VecFever uses internally), or perhaps an FT485R, which looks even better. Either way, you'd need to tweak the Forth send/receive serial communications driver code.

- Using a VEXTREME, which is roughly the same hardware base as a VecFever, but being developed on GitHub as open source hardware and software. It's not as advanced as the VecFever yet and at time of writing (mid-2020), no-one's implemented the second serial port...maybe you could do it?

- Adapting the communications code in the Berzerk Arena game produced by Alex Herbert to interface to a terminal. Berzerk Arena uses the second controller ports of two Vectrexes to communicate together and make an amazing head-2-head multiplayer game. At this point you've got to know what you're doing.

#### [Back to top](#VecForth)

## Getting Started

This section assumes you know your way around a PC a little bit and how to setup a USB serial interface. If you're in deep enough to attempt to create a target binary, hopefully you can handle it. See also the YouTube video links.

1. **Install Gforth onto your PC**

   The latest Windows binary version is 0.7.0, this is what I use to develop with. The Linux port is newer and should also work. Gforth hosts the Chromium cross\-compiler, which is also written in Forth.

   *Recommended:* Add the path to the Gforth executable to the `path` environment variable, so you can compile from your target Forth code directory.

1. **Install Tera Term terminal emulator and setup to use USB serial communications**

   I use 921,600 Baud, 1-8-N, if you know what that means (see also below). Terminal emulation I use VT100, probably a default.

1. **Download and extract the VecForth source code**

   The file you're reading is part of this.

1. **Setup the `include` file**

   This works a bit like a C make file, it contains links to all the source code to create the target binary. You can add/delete/comment out lines in this file, depending on what you're trying to build.

1. **Setup the Vectrex communications serial port (if you're using the VecFever with additional serial port)**

   1. In the VecFever menu system, change the Serial Port settings to Scores: Off, Bits: 8, Parity: None, Stop: 1, Baud: 921,600.

   1. Connect the second serial port to a separate USB port on the PC and setup Tera Term to use it (see above).

1. **Cross-compile the code to a target binary**

   Assuming you added the Gforth binary to the `path` environment variable, from a command prompt and within the `VecForth\VecForth\` directory, type 

   ```
   gforth include.fs
   ```

   and your target binary should then be compiled to a file. Review the contents of the `include.fs` file, you may want to tweak things in there.

1. **Transfer the binary**

   How you do this depends or your hardware. If you have a VecFever, by far the fastest way is to:

   1. Connect the VecFever to the PC via USB cable

   1. Set the VecFever into RAMDISK mode using "Development" from the user menu, then option "Button & Eject"

   1. Rename the compiled binary to `CART.BIN` and transfer it to the VecFever as a storage device

   1. Press Button 4 on the controller to run the target binary.

   Alternatively, you could simply copy the binary to the VecFever or any other programmable multicart or and run it, just like any other target binary.

1. **Boot the VecForth system**

   If you managed the above steps on the VecFever and the PC Tera Term is setup correctly, VecForth will have already booted up and on the terminal screen there will be a VecForth message and an OK prompt.

   ```
   VecForth v1.0 2020-07-12 Phillip Eaton
   based on 6809 CamelForth v1.1 2016-03-20
   OK-0
   ```

   At the OK prompt on the terminal, type `1 2 +` and hit Enter, if the terminal returns `3` then it's working!

   ```
   OK-0 1 2 + . 3
   OK-0
   ```

   Try typing `words` and hit Enter, for a list of all the words in the dictionary you can run.

**Optional:**

- **Install VIDE for debugging, reference and tools**
  
  One of Forths major advantages is that it provides a user command prompt terminal that runs on the target computer, so having a Vectrex emulator isn't so necessary. However, if you want to poke around inside the Forth target code, you can use the VIDE debugger with the compiled target binary and a corresponding `.cnt` file that is also produced by the cross-compiler. (Currently removed, will be reinstated soon!)

#### [Back to top](#VecForth)

## YouTube Videos

See how VecForth made it to where it is today. More to follow, when time permits.

- [Vectrex Programming with Forth - Part 1 - Intro to Vectrex, Forth and the Project](https://www.youtube.com/watch?v=0_OprEzRG2s)
- [Vectrex Programming with Forth - Part 2 - Working with the Vectrex Memory Map](https://www.youtube.com/watch?v=N6mMw1WpcoE)
- [Vectrex Programming with Forth - Part 3 - It's Alive!](https://www.youtube.com/watch?v=8J-r_Sr9AYY)
- [Vectrex Programming with Forth - Part 4 - Porting a Game - 2048](https://www.youtube.com/watch?v=o89QMOgPuug)

#### [Back to top](#VecForth)

## To Do List

- **VecForth**
  
  - Reintroduce the option to create a `.cnt` symbol file for the VIDE debugger
  
  - Implement `?DO` and the `CASE` statement, which will allow easier portability of programs to run with VecForth
  
  - Improve the number recogniser so that `$-10` ond `-$10` would both be recognised and add a character recogniser
  
  - Reimplement some of the key Vectrex BIOS routines so that the Forth API calls have less of a call overhead
  
  - Implement some optimisers, such as peephole and inlining 

- **Hardware**
  
  - Create alternative options to the VecFever for implementing a serial port for the interactive terminal, perhaps using VEXTREME or a VecMulti and/or dedicated PCB with an FT232R or FT245R

- **Games**
  
  - Write some simple example games to demonstrate how it all works and what's possible

#### [Back to top](#VecForth)

## Links

- <http://vide.malban.de/> - Home of the VIDE development environment, which includes lots of useful tools and tutorials

- <http://www.playvectrex.com/> - Yet more programming tools on the DESIGN IT page

- <https://www.forth.com/starting-forth/> - The original and best tutorial for learning Forth, defacto standard

- <http://turboforth.net/> - A wonderful Forth implementation for the Texas Instruments TI-99/4A home computer and a supporting website that's even better, including some great videos

- <http://www.camelforth.com/> - The website of the original CamelForth that is the basis of VecForth

- <https://www.mpeforth.com/sample-page/about-forth-2/> - Commercial Forth vendor with advanced compilation and community product licensing

- <https://github.com/nealcrook/multicomp6809> - Neal uses CamelForth as the core OS for an FPGA computer with a 6809 core, based on Grant Searle's popular design

- <http://jmargolin.com/vmail/vmail.htm> - Atari Blue Box/White Box Forth-based early arcade game development

- <http://jmargolin.com/vgens/vgens.htm> - "The Secret Life of Vector Generators", some great information about how Atari vector games work and were designed

#### [Back to top](#VecForth)

## License

GPLv3 - essentially you must keep this open, all changes must be disclosed and shared.

Full [LICENSE here](https://github.com/technobly/VEXTREME/blob/master/LICENSE) and [TL;DR summary here](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3))

#### [Back to top](#VecForth)
