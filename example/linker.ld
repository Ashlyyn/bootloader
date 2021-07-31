ENTRY(boot)
OUTPUT_FORMAT("binary")
SECTIONS {
    . = 0x7C00;
    .text :
    {
        *(.boot)
        *(.text)
    }
    .rodata :
    {
        *(.rodata)
    }
    .data :
    {
        *(.data)
    }
    .bss :
    {
        *(.bss)
    }
}
