# bootloader

A simple bootloader that jumps directly from Real Mode to Long Mode. 
2MB of memory are mapped in a page table located at `0x1000`-`0x4FFF`. Changing this just involves changing the value of `PAGE_TABLE_BUFFER_TEMP` in `constants.inc`, it won't break unless you cross a segment boundary (basically, just make sure you don't make it higher than 0xC000).
Memory detection info from `int 0x15, eax = 0xE820` is located starting at `0xF004`, with an entry count placed at `0xF000` for convenience. Changing this is the same as above.
Syscalls are enabled, and the address of the handler is placed in the `LSTAR` MSR.
Actual memory detection and creating a real page table (i.e. one not hardcoded at 2MB) accordingly will be up to you.
The bootloader copies the address of a label `syscall_handler` into `LSTAR`, and jumps to a label `init` after the switch to Long Mode, which will be up to you to define (i.e. to link in from your C/C++/whatever code - remember to declare your `init` as `extern "C"` if using C++).
Integrating into your kernel only involves creating an `init` function (the signature doesn't matter) of your own, and either linking the object file with your kernel with the provided linker script, or using your own linker script if you know what you're doing.

The bootloader is assembled and linked with a binutils cross-assembler and cross-linker (x86_64-elf), but using some other toolchain shouldn't be much of an issue. Syntax is gas intel syntax, so if you plan on using a different assembler entirely, you'll be replacing some directives and a few syntax oddities (i.e. ljmp). If you're wondering why I even bothered using gas just to use intel syntax, the answer is the toolchain made life simple, and was one I already had on hand. For you fans of ~~wrong~~ AT&T syntax, I'm not sorry ;)
