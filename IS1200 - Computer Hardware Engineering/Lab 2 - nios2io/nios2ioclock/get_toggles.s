        # Define address to PIO-for-toggle-switches.
        .equ    de2_pio_toggles18,0x850

        # Make get_toggles known outside this file.
        .global get_toggles
        
        # Program code follows.
        .text
        # Align, so that our program code is guaranteed
        # to start on a 4-byte boundary.
        .align  2

# Function get_toggles - return input value from
# the 18 toggle-switches of the DE2 board.
# The result value uses the 18 least-significant bits of
# the return-value register.
# The unused 14 most-significant bits are set to zero.
# C-language function prototype:
#   int get_toggles( void );
get_toggles:
        # Set address to PIO-for-toggle-switches into r8.
        movia   r8,de2_pio_toggles18
        ldwio   r2,0(r8)    # Read toggle switches.

        # Set constant to clear unused bits into r8.
        # Constant is too big for movi (without the a),
        # so we use the pseudo-instruction movia. 
        movia   r8,0x3ffff
        and     r2,r2,r8    # Clear bits 31 through 18.

        ret                 # Return to caller.        
.end
