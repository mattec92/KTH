#include <stdlib.h>
#include <stdio.h>

struct post {
    char name[29];
    float bmi;
    struct post * next;
};

typedef struct post Post;

void writePost(Post * p) {
    printf("Namn: %s\nbmi: %.2f\n", p->name, p->bmi); 
}

int main(int argc, char * argv[]) {
    int weight;
    float length;
    Post * p = (Post *) malloc(sizeof(Post));
    p -> next = (Post *) malloc(sizeof(Post));

    printf("Vad heter du? ");
    fscanf(stdin, "%s", p->name);

    printf("Hur lång är du (m)? ");
    fscanf(stdin, "%f", length);

    printf("Vad väger du (kg)? ");
    fscanf(stdin, "%d", &weight);

    p -> bmi = weight / (length * length);

    writePost(p);

    free(p);
    free(p->next);

    return 0;
}
