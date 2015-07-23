# Laboratory Exercise Nios2io
# Assignments on Timer and Parallel I/O
# Written by F Lundevall
# Last modified 2008-11-25
# Copyright abandoned. This file is in the public domain.

# Useful definitions to copy into your init_timer_asm.s file
.equ    timer1_status,        (0x920)         # status
.equ    timer1_control,       (0x924)         # control
.equ    timer1_period_low,    (0x928)         # period low
.equ    timer1_period_high,   (0x92C)         # period high
.equ    timer1_snap_low,      (0x930)         # snapshot low
.equ    timer1_snap_high,     (0x934)         # snapshot high
 
        .data
        .align 2
        .global mytime
mytime: .word 0x5957
 
        .text
        .global main
main:   call    initfix_int     # Call init fixup routine
        call    init_timer      # Initialize timer_1
 
loop:   call check_key3         # Later changed to call check_key3
        bne     r2,r0,noset     # If key is up, do not set time
        
nop1:   call get_toggles        # Later changed to call get_toggles
        movia   r8,mytime       # Address to time
        stw     r2,0(r8)        # Update time
        br      disp            # Go show time immediately
 
noset:  call    checktimer
        bne     r2,r0,notime    # If no timeout yet, skip
 
nop2:   call snaptime           # Later changed to call snaptime
        mov     r16,r2          # Take a snapshot
 
        movia   r4,mytime       # Address to mytime
        call    tick            # Same as in lab nios2time
 
nop3:   call snaptime           # Later changed to call snaptime
        sub     r4,r16,r2       # Calculate elapsed time
nop4:   call putdiff            # later changed to call putdiff
 
disp:   movia   r4,mytime       # Prepare for call to puttime
        call    puttime_hex         # Later changed to call puttime_hex
 
notime:
        br      loop            # Jump to loop
.end
