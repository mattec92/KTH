.data
.align 2
.global check_key3
.equ keybase, 0x840

check_key3:	movia r8, keybase	#Ladda in keybase i r8
			ldwio r9, 0(r8)		#Ladda in statusbitarna
			andi r9, r9, 0x4	#Maska fram biten för key3
			bgt r9, r0, case1	#Om key3 inte är intryckt hoppa till case1
case2:		mov r2, r0			#Om intryckt, sätt r2 till nollor
			ret
case1: 		movi r2, -1			#Om inte intryckt, sätt r2 till ettor
			ret
