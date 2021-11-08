.include "constants.inc"

.code16

.section .boot
.global startup
.global init

startup:
	ljmp 0x0000, .flush_CS	# flush cs in case BIOS set it to 0x07C0

.flush_CS:
	cld		
	mov sp, startup

	xor ax, ax
	mov ds, ax
	mov es, ax	# zero ds and es
	mov ss, ax	# intialize stack to 0x0000:0x7C00
			# (directly below bootloader)

	movb [disk], dl	

	mov ax, 0x0241		# ah = 0x02 (read sector function of int 0x13), al = 65 (read 65 sectors)
				# sector count could theoretically be 255, but 65 is the max that can be read
				# without crossing a segment boundary
				# 65 sectors is roughly 33k of disk space, so make sure you have disk drivers
				# up and running before your kernel binary grows beyond this size, else
				# some data will not be loaded
	mov bx, 0x7E00		# es:bx = memory location to copy data into, es already zeroed
	mov cx, 0x0002		# ch = 0x00 (track idx), cl = 0x02 (sector idx to start reading from)
	xor dh, dh		# dh = 0x00 (head idx), dl = drive number (implicitly placed in dl by BIOS on startup)
	int 0x13		# copy data


.check_CPUID:
	pushfd
	pop eax
	mov ecx, eax
	xor eax, (1 << 21)
	push eax
	popfd
	pushfd
	pop eax
	push ecx
	popfd
	xor eax, ecx
	jz .no_CPUID		#check if CPU supports CPUID, abort if not

	mov eax, 0x80000000
	cpuid
	cmp eax, 0x80000001
	jb .no_lmode		

	mov eax, 0x80000001 	#check if CPU supports Long Mode, abort if not
	cpuid
	test edx, (1 << 29)
	jz .no_lmode

	mov ax, 0x2401
	int 0x15		#enable A20 Gate
	
	mov ax, 0x03
	int 0x10		#set VGA text mode to 3

.int15:
	mov eax, 0xE820		# int 0x15 subfunction - get memory map	
	xor ebx, ebx		# clear ebx
	mov ecx, 24 		# request 24 byte entries
	mov edx, 0x0534D4150 	# magic number for interrupt
	mov di, E820_MEMORY_MAP_BUFFER 	# set di to second dword immediately after end of page table
	movb [di + 20], 0x01 	# force a valid ACPI 3.X entry
	xor bp, bp		# zero bp for counter (interrupt clobbers all other registers besides si)
	int 0x15
	jc .int15_failed	# interrupt failed if carry set
	mov edx, 0x0534D4150	# reset edx in case interrupt clobbered it (some BIOSes do)
	cmp eax, edx		# if success, eax = magic number
	jne .int15_failed	# else failed
	test ebx, ebx		# if ebx = 0, list is only 1 entry long - useless
	jz .int15_failed
	jmp .int15_cont
.int15_failed:
	cli
	hlt

.int15_loop:
	mov ax, 0xE820		# ax clobbered by interrupt
	movb [di + 20], 0x01 	# force a valid ACPI 3.X entry
	mov ecx, 24		# request 24-byte entry
	int 0x15
	jc .int15_failed	# if carry set, end of list already reached
	mov edx, 0x0534D4150	# edx potentially clobbered by interrupt
.int15_cont:
	jcxz .skip_entry	# skip entry if length = 0
	cmp cl, 20		# if entry is 24 bytes, entry is ACPI 3.0 type
	jbe .not_ACPI
	testb [di + 20], 0x01 	# 0x01 = ignore data flag
	jz .skip_entry
.not_ACPI:
	mov ecx, [di + 8]	# get lower uint32_t of memory region length
	or ecx,  [di + 12]	# OR" it with upper uint32_t to test for zero
	jz .skip_entry		# if length uint64_t is 0, skip entry
	inc bp			# else got a good entry, increment counter)
	add di, 24		# and set pointer to next entry
.skip_entry:
	test ebx, ebx		# if ebx resets to 0, list is complete
	jnz .int15_loop		# else loop
.int15_finished:
	movw ax, [disk]
	movw [E820_MEMORY_MAP_ENTRY_COUNT], bp 	# store the entry count at begining of list

.switch_lmode:
	mov di, PAGE_TABLE_BUFFER_TEMP
	push di

	mov cx, 0x1000
	xor eax, eax
	cld
	rep stosd		#zero out page table buffer

	pop di

	movw [di], PAGE_TABLE_BUFFER_TEMP + 0x1003	   
	# &PML4T[0] = PAGE_TABLE_BUFFER_TEMP + 0x0000, PML4T[0] = &PDPT | 0x03
	movw [di + 0x1000], PAGE_TABLE_BUFFER_TEMP + 0x2003 
	# &PDPT[0] = PAGE_TABLE_BUFFER_TEMP + 0x1000, PDPT[0] = &PDT | 0x03
	movw [di + 0x2000], PAGE_TABLE_BUFFER_TEMP + 0x3003 
	# &PDT[0] = PAGE_TABLE_BUFFER_TEMP + 0x2000, PDT[0] = &PT | 0x03
	mov di, PAGE_TABLE_BUFFER_TEMP + 0x3000		
	# &PT[0] = PAGE_TABLE_BUFFER_TEMP + 0x3000
	mov ax, 0x03
.pagesLoop:
	# &PT[0] = PAGE_TABLE_BUFFER_TEMP + 0x3000
	#  PT[i] = (0x1000 * i) | 0x03
	mov [di], eax	      # identity map pages
	add eax, 0x1000
	add di, 8
	cmp di, PAGE_TABLE_BUFFER_TEMP + 0x4000
	jl .pagesLoop

	mov al, 0xFF
	out 0x21, al
	out 0xA1, al		#disable IRQs

	lidt [dummy_IDT]	#load dummy dummy_IDT

	mov eax, 0b10100000	
	mov cr4, eax		#enable paging and PAE
	
	mov edi, PAGE_TABLE_BUFFER_TEMP		
	mov cr3, edi		#copy page table pointer to cr3

	mov ecx, EFER
	rdmsr
	or eax, 0x00000101
	wrmsr			#enable system calls (bit 0) and activate long mode (bit 8)

	mov ebx, cr0
	or ebx, 0x80000001
	mov cr0, ebx		#enable protected mode
	
	mov ecx, LSTAR
	mov eax, syscall_handler
	wrmsr			#copy location of syscall handler to LSTAR

	mov ecx, SFMASK
	mov eax, 0xFFFFFFFF
	wrmsr			#set sycall flags mask to 0xFFFFFFFF

	lgdt [GDT_ptr]		#load GDT

	ljmp KERNEL_CODE_SEG, 0x7E00	#jump to long mode

	
.no_CPUID:
	mov si, no_CPUID_str
	call printstr_rmode
	cli
	hlt
.no_lmode:
	mov si, no_lmode_str
	call printstr_rmode
	cli 
	hlt

printstr_rmode:
	lodsb
	test al, al
	jz .done
	mov ah, 0x0E
	int 0x10
	jmp printstr_rmode
.done:
	ret

dummy_IDT: 
	.word 0x00
	.long 0x00	

disk: .byte 0x00

.space 510 - (. - startup), 0
.word 0xAA55

.code64
lmode:
	mov ax, KERNEL_DATA_SEG
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax		# set selector registers

	cli			# disable interrupts until IDT set up

	xor rax, rax
	xor rbx, rbx
	xor rcx, rcx
	xor rdx, rdx
	xor rdi, rdi		# zero GPRs (rsi previously zeroed)

	mov rsp, KERNEL_STACK
	mov rbp, rsp		# initialize stack to top of kernel memory

	call init		# init()

	cli
	hlt

syscall_handler:
	sysretq

GDT:
.null:
	.quad 0x00

.kcode:
	.quad 0x00209A0000000000
	#.byte 0x00		# base 24:31 = 0x0000
	#.byte 0b00100000	# flags = 0x2 (Long Mode flag set), limit 16:19 = 0x0
	#.byte 0b10011010	# access byte = 0x9A (Present bit set, 
				# privilege level = 0, desc. type set, executable set, 
				# RW set, DC and AC cleared)
	#.byte 0x00		# base 16:23 = 0x0000
	#.word 0x0000		# base 0:15  = 0x0000
	#.word 0x0000		# limit 0:15 = 0x0000
.kdata:
	.quad 0x0000920000000000
	#.byte 0x00		# base 24:31 = 0x0000
	#.byte 0x00		# flags = 0x0, limit 16:19 = 0x0
	#.byte 0b10010010	# access byte = 0x92 (Present bit set, 
				# privilege level = 0, desc. type set, 
				# RW set, DC, AC, and executable cleared)
	#.byte 0x00		# base 16:23 = 0x0000
	#.word 0x0000		# base 0:15  = 0x0000
	#.word 0x0000		# limit 0:15 = 0x0000

.ucode:
	.quad 0x0000FA0000000000
	#.byte 0x00		# base 24:31 = 0x0000
	#.byte 0b00100000	# flags = 0x2 (Long Mode flag set), limit 16:19 = 0x0
	#.byte 0b11111010	# access byte = 0xFA (Present bit set, 
				# privilege level = 3, desc. type set, executable set, 
				# RW set, DC and AC cleared)
	#.byte 0x00		# base 16:23 = 0x0000
	#.word 0x0000		# base 0:15  = 0x0000
	#.word 0x0000		# limit 0:15 = 0x0000

.udata:
	.quad 0x0000F20000000000
	#.byte 0x00		# base 24:31 = 0x0000
	#.byte 0x00		# flags = 0x0, limit 16:19 = 0x0
	#.byte 0b11110010	# access byte = 0xF2 (Present bit set, 
				# privilege level = 0, desc. type set, 
				# RW set, DC, AC, and executable cleared)
	#.byte 0x00		# base 16:23 = 0x0000
	#.word 0x0000		# base 0:15  = 0x0000
	#.word 0x0000		# limit 0:15 = 0x0000
GDT_end:

GDT_ptr: 
	.word GDT_end - GDT - 1
	.long GDT

no_CPUID_str: .ascii "ERROR: FATAL: CPU does not support CPUID."
no_lmode_str: .ascii "ERROR: FATAL: CPU does not support Long Mode."
