.data
.align 2
.global snaptime
.equ timerbase, 0x920

snaptime:	movia r8, timerbase		#Flytta timerbase till r8
			stwio r17, 16(r8)		#Skriv något till snaplo för att ta ett snapshot
			ldwio r9, 16(r8)		#Ladda in snaplo
			ldwio r10, 20(r8)		#Ladda in snaphi
			slli r10, r10, 16		#Skifta snaphi 16 bitar åt vänster
			andi r9, r9, 0xFFFF		#Nollställ de 16 vänstraste bitarna
			or r2, r9, r10			#Slå ihop snaphi och snaplo
			ret
