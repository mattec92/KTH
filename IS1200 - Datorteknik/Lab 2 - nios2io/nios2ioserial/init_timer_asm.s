.data
.align 2
.global init_timer
.equ period, 500000
.equ timerbase, 0x920

init_timer:	movia r8, timerbase	#Flytta timerns adress till r8
			movia r9, period	#Flytta perioden till r9
			stwio r9, 8(r8)		#Skriv till periodlo
			srli r9, r9, 16		#Skifta r9 16 steg åt höger
			stwio r9, 12(r8)	#Skriv till periodhi
			movi r10, 0b0110	#Spara kontroll-inställningar till r10, run continous
			stwio r10, 4(r8)	#Skriv kontrollinställningar
			ret
