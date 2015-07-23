.data
.align 2
.global checktimer
.equ timerbase, 0x920

checktimer:	movia r8, timerbase	#Ladda in timerbase i r8
			ldwio r9, 0(r8)		#Ladda in statusbitarna
			andi r9, r9, 0x1	#Maska fram TO-biten
			bgt r9, r0, case1	#Om TO har skett, hoppa till case1
case2:		movi r2, -1			#Om inte, sätt r2 till -1
			ret
case1: 		subi r9, r9, 1		#Om TO är ett, dra bort ett, dvs nollställ TO-biten
			stwio r9, 0(r8)		#Skriv till statusregistret
			mov r2, r0			#Fyll r2 med nollor
			ret
