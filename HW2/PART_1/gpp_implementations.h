#ifndef _GPP_LISTHASH_H_
#define _GPP_LISTHASH_H_
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int yyerror(char* s);
int yylex();

/*---------------LIST IMPLEMENTATION-----------------------*/
typedef struct
{
	int* arr;
	int size;
	int capacity;
}List;


void *createList(){
    List* list=(List*)malloc(sizeof(List));
    list->arr=(int*)malloc(sizeof(int));
    list->capacity=1;
    list->size=0;
    return list;
}

void reSize(void* lst){
    List* l=lst;
    l->capacity*=2;
    l->arr = (int*)realloc(l->arr, sizeof(int) * l->capacity);
}
void* addList(void* list, int item)
{
	if(list == NULL)
		list = createList();

	List* l = list;

	if(l->size == l->capacity)
		reSize(l);

	l->arr[l->size] = item;
	++l->size;

	return l;
}
void* appendList(void* list, int item)
{
	List* l = createList();
	List* l2 = list;

	addList(l, item);

	if(l2)
		for(int i=0 ; i < l2->size ; ++i)
			addList(l, l2->arr[i]);

	return l;
}
void* concatList(void* list1, void* list2)
{
	List* l = createList();

	if(list1 != NULL)
	{
		List* l1 = list1;

		for(int i=0 ; i < l1->size ; ++i)
			addList(l, l1->arr[i]);
	}

	if(list2 != NULL)
	{
		List* l2 = list2;

		for(int i=0 ; i < l2->size ; ++i)
			addList(l, l2->arr[i]);
	}

	return l;
}
void print(void* list)
{
	if(list == NULL)
	{
		printf("NULL\n");
		return;
	}

	List* l = (List*)list;

	if(l->size == 0)
	{
		printf("()\n");
		return;
	}

	printf("(");
	for(int i=0 ; i < l->size ; ++i)
	{
		printf("%d",l->arr[i]);
		if(i < l->size - 1)
			printf(" ");
	}
	printf(")\n");
}

/*---------------HASHSET IMPLEMENTATION-----------------------*/
typedef struct
{
	char id[16];
	int val;
}Entry;

typedef struct
{
	Entry* entry;
	int size;
	int capacity;
}HashSet;

HashSet* set;

void createSet()
{
	set = (HashSet*)malloc(sizeof(HashSet));
	set->entry = (Entry*)malloc(sizeof(Entry));
    set->size = 0;
	set->capacity = 1;
}

void setReSize()
{
	set->capacity *= 2;
	set->entry = (Entry*)realloc(set->entry, sizeof(Entry) * set->capacity);
}

int setContains(char* id)
{
	for(int i=0 ; i < set->size ; ++i)
		if(strcmp(set->entry[i].id, id) == 0)
			return i;
	return -1;
}

void setAdd(char* id, int val)
{
	int var = setContains(id);

	if(var != -1)
	{
		set->entry[var].val = val;
		return;
	}

	if(set->size == set->capacity)
		setReSize();

	strcpy(set->entry[set->size].id, id);
	set->entry[set->size].val = val;
	++(set->size);
}

int setGet(char* id)
{
	int var = setContains(id);

	if(var == -1)
		return -1;
	return set->entry[var].val;
}

void setFree()
{
	if(set != NULL)
	{
		if(set->entry != NULL)
			free(set->entry);
		free(set);
	}
}

#endif 