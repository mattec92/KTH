volatile unsigned int* uart1 = (unsigned int*) 0x880;
volatile unsigned int* uart0 = (unsigned int*) 0x860;

void send_char(int in) {
    int status = *(uart1 + 8) & 0x40; //Läs av statusregistret, maska fram OBE-biten
    if (status) {
        *(uart1 + 4) = in; //Om OBE är 1, skicka indatan
    }
}

int rec_charx() {
    int status = *(uart1 + 8) & 0x80; //Läs av statusregistret, maska fram IBE-biten
    if (status) {
        return *uart1 & 0xFF; //OM IBE är 1, returna indatan
    }
    else {
     return -1; //Annars returna -1
    }
}
