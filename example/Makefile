CC = x86_64-elf-gcc
CFLAGS = -Wall -Wextra -Wpedantic -std=gnu99 -ffreestanding
OPTFLAGS = -O2
64BITFLAGS = -mno-red-zone -mno-mmx -mno-sse -mno-sse2

AS = x86_64-elf-as
ASFLAGS = -msyntax=intel -mnaked-reg

LINKER = linker.ld
LDFLAGS= -lgcc -nostdlib
# NOTE: in some installs you need to specify the path of the library files
#LDFLAGS += -L/lib/gcc/x86_64-elf/11.2.0/libgcc.a 

QEMUCMD = qemu-system-x86_64
QEMUFLAGS = -drive format=raw,file=

OBJDIR = ./obj
OBJLIST = ./obj/kernel.o ./obj/bootloader.o
BUILDDIR = ./build

BB.bin : $(BUILDDIR) $(OBJLIST)
				$(CC) $(OBJLIST) -o $(BUILDDIR)/BB.bin $(CFLAGS) $(LDFLAGS) $(64BITFLAGS) $(DIRECTIVES) -T $(LINKER) $(OPTFLAGS)

qemu: BB.bin
			  $(QEMUCMD) $(QEMUFLAGS)$(BUILDDIR)/BB.bin

$(OBJDIR)/kernel.o : kernel.c $(OBJDIR)
				$(CC) -c kernel.c -o $(OBJDIR)/kernel.o $(CFLAGS) $(OPTFLAGS) $(DIRECTIVES)

$(OBJDIR)/bootloader.o : bootloader.asm $(OBJDIR)
				$(AS) bootloader.asm -o $(OBJDIR)/bootloader.o $(ASFLAGS)

$(BUILDDIR) : 
				test ! -d $(BUILDDIR) && mkdir $(BUILDDIR) 

$(OBJDIR) :
				test ! -d $(OBJDIR) && mkdir $(OBJDIR) 

clean :
				rm $(OBJLIST)
				rm $(BUILDDIR)/BB.bin
