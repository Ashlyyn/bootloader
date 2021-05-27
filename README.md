# bootloader

A simple bootloader that jumps directly from Real Mode to Long Mode. 
2MB of memory are mapped in a page table located at `0x9000`-`0xCFFF`.
Memory detection info from `int 0x15, eax = 0xE820` is located starting at `0xD000`.
Syscalls are enabled, and the address of the handler is placed in the `LSTAR` MSR.
Actual memory detection and creating a real page table (i.e. one not hardcoded at 2MB) accordingly will be up to you.
The bootloader copies the address of a label `syscall_handler` into `LSTAR`, and jumps to a label `init` after the switch to Long Mode, which will be up to you to either define, or replace with your own.
If you plan on trying to add any code to the boot sector yourself, the boot sector has a whopping 5 bytes free in its current form, so you'll be doing some refactoring. An easy way to free a few bytes would be either eliminating the error strings, or moving them past the boot sector to be loaded on the call to int 0x13.

The bootloader is assembled and linked with a binutils cross-assembler and cross-linker (x86_64-elf), but using some other toolchain shouldn't be much of an issue. Syntax is gas intel syntax, so if you plan on using a different assembler entirely, you'll be replacing some directives and a few syntax oddities (i.e. ljmp). If you're wondering why I even bothered using gas just to use intel syntax, the answer is the toolchain made life simple, and was one I already had on hand. For you fans of ~~wrong~~ AT&T syntax, I'm not sorry ;)
