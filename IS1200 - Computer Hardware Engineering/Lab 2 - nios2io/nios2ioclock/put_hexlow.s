.data
.align 2
.global put_hexlow
.equ hexbase, 0x9F0
put_hexlow:	movia r8, hexbase 	#Läs in hexbase till r8
			stwio r4, 0(r8)		#Skriv r4 till displayen
			ret