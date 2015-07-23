#ifndef NODH_INCLUDED
#define NODH_INCLUDED

#define SIZE 40

struct post {
    char name[SIZE]; /* <- Ful hårdkodning */
    float bmi;
    struct post * next;
};

typedef struct post Post;

void writePost(Post * p);

void writeList(Post * p);

/* find tar en funktionspekare som första parameter */
Post * find(int (* compare) (Post * a, Post * b), Post sought, Post * list);

int compare_names(Post * a, Post * b);

int compare_bmi(Post * a, Post * b);

void insert(Post ** list);

void load_names(char * filename, Post ** list);
#endif
