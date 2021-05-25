AS	 = x86_64-elf-as
ASFLAGS  = --msyntax=intel -mnaked-reg ${ASMFILES} -g -o ${OBJFILE}
ASMFILES = bootloader.asm
OBJFILE	 = boot.o
LD	 = x86_64-elf-ld
LDFLAGS	 = -T linker.ld -g -o ${ELFFILE} ${OBJFILE}
LDFILE	 = linker.ld
ELFFILE  = boot.bin

assemble:
	${AS} ${ASFLAGS}
	${LD} ${LDFLAGS}
