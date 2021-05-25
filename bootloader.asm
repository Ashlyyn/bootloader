.include "constants.inc"

.code16

.section .boot
.global boot

boot:
	ljmp 0x0000, .flush_CS

.flush_CS:
	cld		
	mov sp, boot

	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax

	movb [disk], dl

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

	mov ax, 0x0217		# ah = 0x02 (read sector function of int 0x13), al = 17 (read sectors 2-18)
	mov bx, 0x7E00		# es:bx = memory location to copy data into, es already zeroed
	mov cx, 0x0002		# ch = 0x00 (track idx), cl = 0x02 (sector idx to start reading from)
	movzxb dx, [disk]	# dh = 0x00 (head idx), dl = drive number
	int 0x13		# copy data

	mov di, PAGE_TABLE_BUFFER

.switch_lmode:
	push di

	mov cx, 0x1400
	xor eax, eax
	cld
	rep stosd		#zero out page table buffer

	pop di

	movw [di], PAGE_TABLE_BUFFER + 0x1003	   
	# &PML4T[0] = PAGE_TABLE_BUFFER + 0x0, PML4T[0] = &PDPT | 0x03
	movw [di + 0x1000], PAGE_TABLE_BUFFER + 0x2003 
	# &PDPT[0] = PAGE_TABLE_BUFFER + 0x1000, PDPT[0] = &PDT | 0x03
	mov di, PAGE_TABLE_BUFFER + 0x2000   	   
	# &PDT[0] = PAGE_TABLE_BUFFER + 0x2000
	mov ax, PAGE_TABLE_BUFFER + 0x3003  	  	
	mov cx, 512
.pageTableLoop:	 		#  PDT[0..511] = &PT[0..511] | 0x03
	mov [di], ax
	add di, 8
	dec cx
	test cx, cx
	jnz .pageTableLoop

	mov di, PAGE_TABLE_BUFFER + 0x3000		
	# &PT[0] = PAGE_TABLE_BUFFER + 0x3000
	mov ax, 0x03
.pagesLoop:
	# &PT[0] = PAGE_TABLE_BUFFER + 0x3000
	#  PT[i] = (0x1000 * i) | 0x03
	mov [di], eax	      # identity map pages
	add eax, 0x1000
	add di, 8
	cmp di, PAGE_TABLE_BUFFER + 0x4000
	jl .pagesLoop

	mov al, 0xFF
	out 0x21, al
	out 0xA1, al		#disable IRQs

	lidt [dummy_IDT]	#load dummy dummy_IDT

	mov eax, 0b10100000	
	mov cr4, eax		#enable paging and PAE
	
	mov edi, PAGE_TABLE_BUFFER		
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
	mov si, .no_CPUID_str
	call .printstr
	cli
	hlt
.no_lmode:
	mov si, .no_lmode_str
	call .printstr
	cli 
	hlt

.printstr:
	lodsb
	test al, al
	jz .done
	mov ah, 0x0E
	int 0x10
	jmp .printstr
.done:
	ret

.no_CPUID_str: .ascii "ERROR: FATAL: CPU does not support CPUID."
.no_lmode_str: .ascii "ERROR: FATAL: CPU does not support Long Mode."


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

GDT_ptr: 
	.word $ - GDT - 1
	.long GDT

dummy_IDT: 
	.word 0x00
	.long 0x00	

disk: .byte 0x00

.code64

.space 510 - (. - boot), 0
.word 0xAA55

copy:
lmode:
	mov ax, KERNEL_DATA_SEG
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax		#set selector registers

	cli			#disable interrupts until IDT set up

	xor rax, rax
	xor rbx, rbx
	xor rcx, rcx
	xor rdx, rdx
	xor rdi, rdi		#zero GPRs (rsi previously zeroed)

	mov rsp, 0x4000000
	mov rbp, rsp		#initialize stack to top of kernel memory

	call init		#init()

	cli
	hlt

init:			# put down here just to keep the assembler happy
syscall_handler:
