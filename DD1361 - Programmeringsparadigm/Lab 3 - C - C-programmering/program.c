#include <stdlib.h>
#include <stdio.h>
#include "nod.h"

void skrivMeny() {
    printf("\n*******************  MENY *********************\n\n");
    printf("\t 1 Ladda namn från fil\n");
    printf("\t 2 Ny person\n");
    printf("\t 3 Sök namn\n");
    printf("\t 4 Sök personer med bmi\n");
    printf("\t 5 Skriv ut hela listan\n");
    printf("\t 6 Ta bort person\n");
    printf("\t 0 Avsluta\n\n");
    printf("\t Vad vill du göra? ");
}

int main(int argc, char * argv[]) {

    Post * list = NULL;
    int menyval = 1;
    Post tmp;
    Post * tmp_pek;

    printf("Hej och välkommen till Viktmästarnas meny\n");
    while (menyval > 0 && menyval <= 6) {
        skrivMeny();
        scanf("%d", &menyval);
        printf("\n");

        switch (menyval) {
        case 1:
            load_names("bmi_namn.txt", & list);
            break;
        case 2:
            insert(& list);
            break;
        case 3:
            printf("Vem söker du? ");
            fscanf(stdin, "%s", tmp.name);
            tmp_pek = find((*compare_names), tmp, list);
            if (tmp_pek != NULL) writePost(tmp_pek);
            else printf("Hittade inte namnet '%s'\n", tmp.name);
            break;
        case 4:
			/*Läser in vilket bmi vi vill söka efter. Anropar därefter find
			som hittar första lösningen i listan. Därefter anropas find igen
			på resterande lista tills att hela listan sökts igenom. När lösning
			hittats skrivs den ut direkt. */
	    	printf("Personer med vilket bmi söker du? ");
			fscanf(stdin, "%f", &tmp.bmi);
			tmp_pek = find((*compare_bmi), tmp, list);
			if (tmp_pek != NULL) {
				writePost(tmp_pek);
				tmp_pek = tmp_pek->next;
				while (tmp_pek != NULL) {
					tmp_pek = find((*compare_bmi), tmp, tmp_pek);
					if (tmp_pek != NULL) {
						writePost(tmp_pek);
						tmp_pek = tmp_pek->next;
					}
				}
			}
            else {
				printf("Hittade ingen med bmi '%f'\n", tmp.bmi);
			}
			
            break;
        case 5:
            writeList(list);
            break;
        case 6:
            /*Letar med find efter inmatat namn. Anropar sedan remove_person som tar bort
			givet element från listan.*/
			printf("Vem vill du ta bort? ");
			fscanf(stdin, "%s", tmp.name);
            tmp_pek = find((*compare_names), tmp, list);
            if (tmp_pek != NULL) {
				printf("Tar bort %s från listan.\n", tmp_pek->name);
				remove_person(&list, tmp_pek);
			}
            else {
				printf("Hittade inte namnet '%s'\n", tmp.name);
			}
            break;
        }
        
    }
	//Avallokerar hela listan när programmet stängs ner.
	Post * temp;
	while(list != NULL) {
		temp = list;
		list = list->next;
		free(temp);
	}

    printf("\n\nHej då!\n");
    return 0;
}


