int hex7seg(int i);
void put_hexlow(int i);

void puttime_hex(int* mytimep) {
    int out;
    out = hex7seg((*mytimep & 0xF000) >> 12) << 21;     //Maska fram bitar, skifta till höger,
    out += ((hex7seg((*mytimep & 0x0F00) >> 8)) << 14); //skicka till hex7seg, skifta returbitarna
    out += ((hex7seg((*mytimep & 0x00F0) >> 4)) << 7);  //åt vänster till rätt position
    out += hex7seg(*mytimep & 0x000F);
    put_hexlow(out);                                    //Skriv ut på displayen
}
