        # Laboratory Exercise 1, Home Assignment 5-7
        # Written by Johan Wennlund, 24 December 2000
        # Edited by F Lundevall, 13 February 2004
        # Edited by J Wennlund, 25 August 2005
        # Edited by F Lundevall, 29 October 2007
        # Edited by F Lundevall, 20 July 2009
        # Copyright abandoned. This file is in the public domain.


        .data                   # Reserve space for time-variable
        .align 2                # Fix possible alignment errors
mytime: .word   0x5957          # give time initial value "59:57"

        .text                   # Instructions follow
        .align 2                # Fix possible alignment errors
        .global main            # Makes "main" globally known

main:   movia   r4,mytime       # parameter for call to puttime
        call    puttime         # present current time

        movia   r4,mytime       # parameter for call to tick
        call    tick            # update current time

        movi	r4,1000         # parameter 1000 to delay
        call    delay           # wait milliseconds, (how many?) 

        br      main            # Branch to main
        .end                    # Marks the end of the program
