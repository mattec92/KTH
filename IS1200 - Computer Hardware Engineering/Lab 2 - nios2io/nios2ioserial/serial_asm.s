.data
.align 2
.global out_char, in_charx
.equ uartbase, 0x860

out_char:	movia r8, uartbase		#Ladda in uartbase till r8
outloop:	ldwio r9, 8(r8)			#Läs in statusbitarna
			andi r9, r9, 0x40		#Maska fram OBE, 1 om ingen data
			beq r9, r0, outloop		#Om OBE är större än 0
			stw r4, 4(r8)			#Skriv till output-bitarna
			ret
			
in_charx:	movi r2, -1				#Flytta -1 till r2
			movia r8, uartbase		#Ladda in uartbase till r8
			ldwio r9, 8(r8)			#Hämta statusbitarna 
			andi r9, r9, 0x80		#Maska fram IBF, 1 om ny data
			beq r9, r0, out			#Om ingen data finns att läsa, hoppa ut
			ldwio r2, 0(r8)			#Annars, läs indata
			andi r2, r2, 0xFF		#Maska fram de väsentliga bitarna
out:		ret
