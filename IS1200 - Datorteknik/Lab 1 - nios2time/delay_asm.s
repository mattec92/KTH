.global delay
.text
.align 2
.equ delaycount, 0x2D	#Antal varv per millisekund

delay:
mov r8, r4				#Flytta indatan till register r8

outer:
movi r9, delaycount		#Lägg antalet delays för en ms till register r9

inner:
subi r9, r9, 1			#Minska antalet kvarvarande varv i inre loopen
bgt r9, r0, inner		#Om varvräknaren är större än noll, hoppa till inner
subi r8, r8, 1			#Minska antalet kvarvarande varv i yttre loopen
bgt r8, r0, outer		#Om varvräknaren är större än noll, hoppa till outer
ret
