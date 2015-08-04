.global hexasc
.text
.align 2
hexasc: movi r8, 9		#Lägg till värdet 9 till r8
andi r4, r4, 0xF		#Om talet är större än 15, ignorera allt utom de 4 sista bitarna
bgt r4, r8, tio			#hoppa till tio om input är större än 9
addi r2, r4, 0x30		#Lägg till 0x30 till output
br slut					#Hoppa till slutet
tio: addi r2, r4, 0x37	#Lägg till 0x37 till output
slut: ret
