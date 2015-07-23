.macro PUSH reg
  		subi    sp, sp, 4
		stw     \reg, 0(sp)
.endm

.macro POP reg
		ldw     \reg, 0(sp)
		addi    sp, sp, 4
.endm

.macro PUSHALL
		PUSH r4
		PUSH r8
		PUSH r9
		PUSH r10
		PUSH r11
		PUSH r31
.endm

.macro POPALL
		POP r31
		POP r11
		POP r10
		POP r9
		POP r8
		POP r4
.endm


.global puttime
.text
.align 2
puttime:		
ldw r8, 0(r4)			#Spara värdet för tiden som ska skrivas ut
movi r4, 0xA			#Ladda in ascii för new line
PUSHALL					#Spara alla register vi använder innan vi kallar på subrutin
call putchar			#Skriv ut ny line
POPALL					#Ladda alla sparade register
movi r10, 4				#Lägg till värde för att jämföra när man ska skriva ut kolon
movi r11, 12			#Värde för hur mycket man ska shifta talbitarna

loop:
srl r9, r8, r11			#Shifta talbitarna rätt
mov r4, r9				#Ladda in första bit-koden
PUSHALL					#Spara alla register vi använder innan vi kallar på subrutin
call hexasc				#Översätt till ascii-kod
POPALL					#Ladda alla sparade register
mov r4, r2				#Ladda in ascii-koden
PUSHALL					#Spara alla register vi använder innan vi kallar på subrutin
call putchar			#Skriv ut ascii-koden
POPALL					#Ladda alla sparade register
subi r11, r11, 4		#Dra bort 4 från hur många bitar i talbitarna som ska shiftas
beq r10, r11, kolon		#Om 2 tal skrivits ut, hoppa för att skriva ut ett kolon
blt r11, r0, end		#Om 4 tal skrivits ut, hoppa för att sluta
br loop					#Annars skriv ut nästa tal

kolon:
movi r4, 0x3A			#Ladda in ascii-koden för kolon
PUSHALL					#Spara alla register vi använder innan vi kallar på subrutin
call putchar			#Skriv ut komma
POPALL					#Ladda alla sparade register
br loop					#Hoppa tillbaka till loopen för att skriva ut tiden

end:
ret
