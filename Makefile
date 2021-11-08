AS	 = x86_64-elf-as
ASFLAGS  = --msyntax=intel -mnaked-reg ${ASMFILES} -g -o ${OBJFILE}
ASMFILES = bootloader.asm
OBJFILE	 = ../boot.o

assemble:
	${AS} ${ASFLAGS}
