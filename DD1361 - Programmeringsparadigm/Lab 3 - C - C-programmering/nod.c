#include <stdlib.h>
#include <stdio.h>
#include "nod.h"
#include "nod.h" //Testar include guards!

void writePost(Post * p) {
	printf("Namn: %s\nBMI: %.2f\n",p->name, p->bmi);
}
/*Loopar igenom hela listan och kallar på writePost för att skriva ut de enskilda elementen*/
void writeList(Post * p) {
	if (p == NULL) {
		printf("Listan är tom.");
	}
	while(p != NULL) {
		writePost(p);
		p = p->next;
	}
}
/*Loopar igenom listan och jämför givet element med listans element.
Om lika element påträffas returneras toppen av listan, alltså elementet*/
Post * find(int (* compare) (Post *  a, Post * b), Post sought, Post * list) {
	if (list == NULL) {
		printf("Listan är tom.");
		return NULL;
	}
	Post *temp = (Post *) malloc(sizeof(Post));
	*temp = sought;
	while (list != NULL) {
		if ((* compare)(temp, list) == 0) {
			break;
		}
		list = list->next;
	}
	free(temp);
	return list;
}
/*Använder inbyggd funktion för att jämföra strings*/
int compare_names(Post * a, Post * b) {
	return strcmp(a->name, b->name);
}
/*Om lika returnera 0, a större 1, b större -1*/
int compare_bmi(Post * a, Post * b) {
	if (a->bmi == b->bmi) {
		return 0;
	}
	else if (a->bmi > b->bmi) {
		return 1;
	}
	else {
		return -1;
	}
}
/*Läser in data och allokerar nytt element- Datan läggs till elementet och elementet
läggs till i början av listan. Det nya elementets next-pekare pekar på det tidigare
första elementet och toppen av listan pekar nu på det nya elementet, precis som i load_names*/
void insert(Post ** list) {
	Post * temp = (Post *) malloc(sizeof(Post));
	char name[SIZE];
	float length;
	float weight;
	printf("\nVad heter personen? ");
	scanf("%s", &name);
	printf("\nHur lång är personen? (m) ");
	scanf("%f", &length);
	printf("\nHur mycket väger personen? (kg) ");
	scanf("%f", &weight);
	strcpy(temp->name, name);
	temp->bmi = weight / (length * length);

	temp->next = *list;
	*list = temp;
}
/*Tar bort givet element(jämför dock endast namn) från listan. 
De olika fallen beskrivs nedan.*/
void remove_person(Post ** list, Post * toRemove) {
	Post * curr = *list;
	Post * next = curr->next;
	
	if (curr == NULL) {
		printf("Listan är tom.");
		return;
	}
	//Om elementet som ska tas bort ligger först i listan och det inte finns några fler.
	if (next == NULL) {
		if (compare_names(curr, toRemove) == 0) {
			Post *temp = curr;
			*list = NULL;
			free(temp);
			return;
		}
	}
	//Om elementet ligger först i listan och det finns fler element.
	if (compare_names(curr, toRemove) == 0) {
		Post *temp = curr;
		*list = next;
		free(temp);
		return;
	}
	//Om elementet inte ligger först i listan, loopa genom listan och kolla next-elementet.
	while(next != NULL) {
		if (compare_names(next, toRemove) == 0) {
			Post *temp = next;
			curr->next = next->next;
			free(temp);
			return;
		}
		curr = next;
		next = next->next;
	}
}

void load_names(char * filename, Post ** list) {
    char name[SIZE]; /* <- Ful hårdkodning */
    float bmi;
    FILE *fil = fopen(filename, "r");
    Post * p;
    if (fil == NULL) {
	printf("Filen inte funnen.\n");
    } else {
        while (fscanf(fil, "%s %f", name, &bmi) == 2) {
            p = (Post *) malloc(sizeof(Post));
            strcpy(p->name, name);
            p->bmi = bmi;

            p->next = *list;
            *list = p;
        }
    }
	fclose(fil);
}


