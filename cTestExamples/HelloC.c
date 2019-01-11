#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//----------------------------------------------
//----------------------------------------------
// L.List: Case Classes

typedef enum {NIL, CONS} L_LIST;

//----------------------------------------------
// Abstract Class: L.List
typedef struct L_List {
	void* instance;
	L_LIST caseClass;
} AbstractClass_L_List, *L_List; // Object, then pointer to object

//----------------------------------------------
// Case Class: Cons
typedef struct Cons {
	int head;
	L_List tail;
} Cons;

// Case class constructor
L_List l_list_cons(int head, L_List tail) 
{
	Cons* cons = malloc(sizeof(Cons));
    cons->head = head;
    cons->tail = tail;
	
	L_List l_list = malloc(sizeof(AbstractClass_L_List));
    l_list->instance = cons;
    l_list->caseClass = CONS;
    
	return l_list;
}

//~ Cons* inst_cons(L_List l)
//~ {
	//~ void* v = l.instance;
	//~ Cons* c = (Cons *)v;
	//~ int h = c->head;
	//~ printf("%d\n", h); //((Cons *)(l.instance))->head
	//~ return (Cons *)l.instance;
//~ }

#define inst_cons(l)    ((Cons *)l->instance)

//int head(L_List l)
//{
	//return (Cons *)l.instance)->head;
//}

//----------------------------------------------
// Case Class: Nil
typedef struct Nil {} Nil;

// Case class constructor
L_List l_list_nil() 
{
	Nil nil = {};
	L_List l_list = malloc(sizeof(L_List));
	l_list->instance = &nil;
    l_list->caseClass = NIL;
	return l_list;
}

#define inst_nil()    ((Nil *)l->instance)

//~ Nil* inst_nil(L_List l)
//~ {
	//~ return (Nil *)l.instance;
//~ }

//----------------------------------------------
//----------------------------------------------
int head(L_List l) 
{	
	switch(l->caseClass) {
		case CONS:
			return inst_cons(l)->head; // careful naming
		case NIL:
			fprintf(stderr, "%s", "head(Nil)");
			return -1; //error handling
		default:
			printf("Default");
	}
}

//----------------------------------------------

//def head(l: List): Int = {
    //l match {
      //case Cons(h, _) => h
      //case Nil() => error("head(Nil)")
    //}
  //}
  
  //def merge(l1: List, l2: List): List = {
    //l1 match {
      //case Nil() => l2
      //case Cons(h1, t1) =>
        //l2 match {
          //case Nil() => l1
          //case Cons(h2, t2) =>
            //if (h1 <= h2) {
              //Cons(h1, merge(t1, l2))
            //} else {
              //Cons(h2, merge(l1, t2))
            //}
        //}
    //}
  //}
  
  //def split(l: List): LPair = {
    //l match {
      //case Cons(h1, Cons(h2, t)) =>
        //val rec: LPair = split(t);
        //rec match {
          //case LP(rec1, rec2) =>
            //LP(Cons(h1, rec1), Cons(h2, rec2))
        //}
      //case _ =>
        //LP(l, Nil())
    //}
  //}
  
  //def mergeSort(l: List): List = {
    //l match {
      //case Nil() => l
      //case Cons(h, Nil()) => l
      //case l =>
        //split(l) match {
          //case LP(l1, l2) =>
            //merge(mergeSort(l1), mergeSort(l2))
        //}
    //}
  //}
  
  
//----------------------------------------------
//----------------------------------------------
// L.List: Case Classes

typedef enum {LP_c} L_LPAIR;

//----------------------------------------------
// Abstract Class: L.LPair
typedef struct L_LPair {
	void* instance;
	L_LPAIR caseClass;
} AbstractClass_L_LPair, *L_LPair; // Object, then pointer to object

//----------------------------------------------
// Case Class: LP
typedef struct LP {
	L_List l1;
	L_List l2;
} LP;

// Case class constructor
L_LPair l_lpair_lp(L_List l1, L_List l2) 
{
	LP* lp = malloc(sizeof(LP));
    lp->l1 = l1;
    lp->l2 = l2;
	
	L_LPair l_lpair = malloc(sizeof(AbstractClass_L_LPair));
    l_lpair->instance = lp;
    l_lpair->caseClass = LP_c;
    
	return l_lpair;
}

#define inst_lp(l)    ((LP *)l->instance)
  
L_List merge(L_List l1, L_List l2)
{
	switch(l1->caseClass) {
		case NIL:
			return l2;
		case CONS: {
			int h1 = inst_cons(l1)->head;
			L_List t1 = (inst_cons(l1)->tail);
			switch(l2->caseClass) { // get CaseClass // careful naming
				case NIL:
					return l1;
				case CONS: {
					int h2 = inst_cons(l2)->head;
					L_List t2 = (inst_cons(l2)->tail);
					if(h1 <= h2) {
						return l_list_cons(h1, merge(t1, l2));
					} else {
						return l_list_cons(h2, merge(l1, t2));
					}
				}
			}
		}
	}
}

L_LPair split(L_List l)
{
	switch(l->caseClass) {
		case CONS: {
			int h1 = inst_cons(l)->head;
			L_List t1 = inst_cons(l)->tail;
			if(t1->caseClass == CONS) {
				int h2 = inst_cons(inst_cons(l)->tail)->head;
				L_List t = inst_cons(inst_cons(l)->tail)->tail;
				L_LPair rec = split(t);
				switch(rec->caseClass) {
					case LP_c: {
						L_List rec1 = inst_lp(rec)->l1;
						L_List rec2 = inst_lp(rec)->l2;
						return l_lpair_lp(l_list_cons(h1, rec1), l_list_cons(h2, rec2));
					}
				}
			} else {
				return l_lpair_lp(l, l_list_nil());
			}
		}
		default:
			return l_lpair_lp(l, l_list_nil());
	}
}

L_List mergeSort(L_List l)
{
	switch(l->caseClass) {
		case NIL:
			return l;
		case CONS: {
			//int h = inst_cons(l)->head;
			L_List t = inst_cons(l)->tail;
			if(t->caseClass == NIL) {
				return l;
			} else {
				L_LPair lp2 = split(l);
				switch(lp2->caseClass) {
					case LP_c: {
						L_List l1 = inst_lp(lp2)->l1;
						L_List l2 = inst_lp(lp2)->l2;
						return merge(mergeSort(l1), mergeSort(l2));
					}
				}
			}
		}
	}
}
  
//----------------------------------------------
//----------------------------------------------



int main()
{
	// val l_good: L.List = Cons(2, Nil()) translates to
	L_List good = l_list_cons(8, l_list_cons(6, l_list_cons(2, l_list_nil())));
	L_List t = ((inst_cons(good))->tail);
	printf("%d\n", head(good));
	printf("%d test \n", head(t));
	
	L_List bad = l_list_nil();
	printf("%d\n", head(bad));
		
	printf("Merge Sort Baby!\n");
	L_List m = mergeSort(good);
	printf("%d\n", head(m));
	printf("%d\n", head((inst_cons(good))->tail));
	
	//~ Nil n0 = {};
	//~ L_List l0 = {&n0, NIL};
	
	//~ Cons c1 = {4, l0};
	//~ L_List l1 = {&c1, CONS};
	//void* cp= &c;
	//L_List l = {&c};//malloc(sizeof(List));
	//l.instance = cp;
	//l.caseClass = CONS;
	
	
	
	//if(l1.caseClass == CONS) {
		//if(((Cons *)l1.instance)->head == 4) {
			//printf("Bitch, please");
		//}
	//}
	
	
	
	return 0;
}
