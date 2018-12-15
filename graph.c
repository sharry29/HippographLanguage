#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* constants */

int VOIDTYPE = 1;
int INTTYPE  = 2;
int STRTYPE  = 3;
int BOOLTYPE = 4;

/* data structures */

typedef union primitive {
  int i;
  char *s;
  void *v;
} primitive;

typedef struct node node;

typedef struct edge {
  node *src;
  node *dst;
  primitive *w;
  int w_typ;
  struct edge *next;
  int has_val;
} edge;

typedef struct neighbor_list_item {
  edge *edge;
  struct neighbor_list_item *next;
} neighbor_list_item;

typedef struct neighbor_list {
  neighbor_list_item *hd;
} neighbor_list;

struct node {
  primitive *label;
  int label_typ;
  primitive *data;
  int data_typ;
  int has_val;
  neighbor_list *neighbor_list;
  node *next;
};

typedef struct node_list {
  node *hd;
} node_list;

typedef struct edge_list {
  edge *hd;
} edge_list;

typedef struct graph {
  node_list *node_list;
  edge_list *edge_list;
} graph;

/* create primitive of type */

void *create_prim_int(int i) {
  primitive *p = (primitive *) malloc(sizeof(primitive));
  p -> i = i;
  return (void *) p;
}

void *create_prim_str(char *s) {
  primitive *p = (primitive *) malloc(sizeof(primitive));
  p -> s = s;
  return (void *) p;
}

primitive *clone_primitive(primitive *p) {
  if (p == NULL) return NULL;

  primitive *p_cp = (primitive *) malloc(sizeof(primitive));
  memcpy(p_cp, p, sizeof(primitive));
  return p_cp;
}

/* NODES */

void *create_neighbor_list_item(edge *e) {
  neighbor_list_item *nli = (neighbor_list_item *) malloc(sizeof(neighbor_list_item));
  nli -> edge = e;
  nli -> next = NULL;
  return (void *) nli;  
}

void *create_neighbor_list() {
  neighbor_list *nl = (neighbor_list *) malloc(sizeof(neighbor_list));
  nl -> hd = NULL;
  return (void *) nl;
}

void *create_node() {
  node *n = (node *) malloc(sizeof(node));
  n -> label = NULL;
  n -> data = 0;
  n -> has_val = 0;
  n -> neighbor_list = create_neighbor_list();
  n -> neighbor_list -> hd = NULL;
  n -> next = NULL;
  return (void *) n;
}

int cmp_node_label(node *n1, node *n2) {
  int lt = n1 -> label_typ;
  if (lt == INTTYPE || lt == BOOLTYPE) {
    return n1 -> label -> i == n2 -> label -> i;
  } else if (lt == STRTYPE) {
    return strcmp(n1 -> label -> s, n2 -> label -> s);
  } else {
    return -1;
  }
}

node *clone_node(node *n) {
  if (n == NULL) return NULL;

  node *n_cp = (node *) malloc(sizeof(node));
  memcpy(n_cp, n, sizeof(node));
  n_cp -> label = clone_primitive(n -> label);
  n_cp -> data = clone_primitive(n -> data);
  n_cp -> neighbor_list = NULL;
  n_cp -> next = NULL;
  return n_cp;
}

void set_node_label_int(node *n, int i) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_int(i);
  n -> label_typ = INTTYPE;
}

void set_node_label_bool(node *n, int i) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_int(i);
  n -> label_typ = BOOLTYPE;
}

void set_node_label_str(node *n, char *s) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_str(s);
  n -> label_typ = STRTYPE;
}

void set_node_data_int(node *n, int i, int has_val) {
  if (n -> data != NULL) {
    free(n -> data);
  }
  n -> data = create_prim_int(i);
  n -> data_typ = INTTYPE;
  n -> has_val = has_val; //flag
}

void set_node_data_bool(node *n, int i, int has_val) {
  if (n -> data != NULL) {
    free(n -> data);
  }
  n -> data = create_prim_int(i);
  n -> data_typ = BOOLTYPE;
  n -> has_val = has_val; //flag
}

void set_node_data_str(node *n, char *s, int has_val) {
  if (n -> data != NULL) {
    free(n -> data);
  }
  n -> data = create_prim_str(s);
  n -> data_typ = STRTYPE;
  n -> has_val = has_val;
}

void *get_node_label(node *n) {
  int typ = n -> label_typ;
  void *label = NULL;

  if (typ == INTTYPE || typ == BOOLTYPE) {
    label = (void *) &(n -> label -> i);
  } else if (typ == STRTYPE) {
    label = (void *) n -> label -> s;
  } else if (typ == VOIDTYPE) {
    label = (void *) n -> label -> v;
  }
  return label;
}

void *get_node_data(node *n) {
  int typ = n -> data_typ;
  void *data = NULL;

  if (typ == INTTYPE || typ == BOOLTYPE) {
    data = (void *) &(n -> data -> i);
  } else if (typ == STRTYPE) {
    data = (void *) n -> data -> s;
  } else if (typ == VOIDTYPE) {
    data = (void *) n -> data -> v;
  }
  return data;  // not guaranteed to return valid value if not has_val
}

/* EDGES */

int cmp_edge_weight(edge *e1, edge *e2) {
  int lt = e1 -> w_typ;
  if (lt == INTTYPE || lt == BOOLTYPE) {
    return e1 -> w -> i == e2 -> w -> i;
  } else if (lt == STRTYPE) {
    return strcmp(e1 -> w -> s, e2 -> w -> s);
  } else {
    return -1;
  }
}

void set_edge_w_int(edge *e, int i, int has_val) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_int(i);
  e -> has_val = has_val;
  e -> w_typ = INTTYPE;
}

void set_edge_w_bool(edge *e, int i, int has_val) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_int(i);
  e -> has_val = has_val;
  e -> w_typ = BOOLTYPE;
}

void set_edge_w_str(edge *e, char *s, int has_val) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_str(s);
  e -> has_val = has_val;
  e -> w_typ = STRTYPE;
}

node *get_edge_src(edge *e) {
  if (e == NULL) return NULL;

  return e -> src;
}

node *get_edge_dst(edge *e) {
  if (e == NULL) return NULL;

  return e -> dst;
}

int get_edge_w_int(edge *e) {
  if (e == NULL || e -> has_val == 0) return 0;
  return e -> w -> i;
}

char *get_edge_w_str(edge *e) {
  if (e == NULL || e -> has_val == 0) return 0;
  return e -> w -> s;
}

void *create_edge() {
  edge *e = (edge *) malloc(sizeof(edge));
  e -> src = NULL;
  e -> dst = NULL;
  e -> w = NULL;
  e -> next = NULL;
  e -> has_val = 0;
  return e;
}

edge *clone_edge(edge *e) {
  if (e == NULL) return NULL;
  edge *e_cp = (edge *) malloc(sizeof(edge));
  memcpy(e_cp, e, sizeof(edge));
  e_cp -> src = clone_node(e -> src);
  e_cp -> dst = clone_node(e -> dst);
  e_cp -> has_val = e -> has_val;
  e_cp -> w = clone_primitive(e -> w);
  e_cp -> next = NULL;
  return e_cp;
}

/* GRAPHS */

void *create_node_list() {
  node_list *nl = (node_list *) malloc(sizeof(node_list));
  nl -> hd = NULL;
  return (void *) nl;
}

void *create_edge_list() {
  edge_list *el = (edge_list *) malloc(sizeof(edge_list));
  el -> hd = NULL;
  return (void *) el;
}

void *create_graph() {
  graph *g = (graph *) malloc(sizeof(graph));
  g -> node_list = create_node_list();
  g -> edge_list = create_edge_list();
  return (void *) g;
}

/*
  Given a graph, creates a linked list of copies of its nodes.
  Used to enable node iteration (for_node) without side effects.
*/
node *graph_to_node_iterable(graph *g) {
  node *curr_orig = g -> node_list -> hd;
  node *curr_new = clone_node(curr_orig);
  node *hd_new = curr_new;

  while (curr_orig != NULL) {
    curr_new -> next = clone_node(curr_orig -> next);
    curr_orig = curr_orig -> next;
    curr_new = curr_new -> next;
  }

  return hd_new;
}

/*
  Given a graph, creates a linked list of copies of its edges.
  Used to enable edge iteration (for_edge) without side effects.
*/
edge *graph_to_edge_iterable(graph *g) {
  edge *curr_orig = g -> edge_list -> hd;
  edge *curr_new = clone_edge(curr_orig);
  edge *hd_new = curr_new;

  while (curr_orig != NULL) {
    curr_new -> next = clone_edge(curr_orig -> next);
    curr_orig = curr_orig -> next;
    curr_new = curr_new -> next;
  }

  return hd_new;
}

node *get_graph_next_node(node *n) {
  return n -> next;
}

edge *get_graph_next_edge(edge *e) {
  return e -> next;
}

node *get_node_by_label_int(graph *g, int label) {
  node *curr = g -> node_list -> hd;
  while (curr != NULL) {
    if ((curr -> label_typ == INTTYPE || curr -> label_typ == BOOLTYPE) && *(int *) get_node_label(curr) == label) {
      return curr;
    }
    curr = curr -> next;
  }
  return curr;
}

node *get_node_by_label_str(graph *g, char *label) {
  node *curr = g -> node_list -> hd;
  while (curr != NULL) {
    if (curr -> label_typ == STRTYPE && strcmp((char *) get_node_label(curr), label) == 0) {
      return curr;
    }
    curr = curr -> next;
  }
  return curr;
}

edge *get_edge_by_src_and_dst_int(graph *g, int src_label, int dst_label) {
  edge *curr = g -> edge_list -> hd;
  while (curr != NULL) {
    if ((curr -> w_typ == INTTYPE || curr -> w_typ == BOOLTYPE) &&
        get_edge_src(curr) -> label -> i == src_label &&
        get_edge_dst(curr) -> label -> i == dst_label) {
      return curr;
    }
    curr = curr -> next;
  }
  return curr;
}

edge *get_edge_by_src_and_dst_str(graph *g, char *src_label, char *dst_label) {
  edge *curr = g -> edge_list -> hd;
  while (curr != NULL) {
    if (curr -> w_typ == STRTYPE &&
        strcmp((char *) (get_edge_src(curr) -> label -> s), src_label) == 0 &&
        strcmp((char *) (get_edge_dst(curr) -> label -> s), dst_label) == 0) {
      return curr;
    }
    curr = curr -> next;
  }
  return curr;
}

int add_neighbor(node *n, edge *e) {
  if (n != e -> src) return -1;

  if (n -> neighbor_list -> hd == NULL) {
    n -> neighbor_list -> hd = create_neighbor_list_item(e);
  } else if (n -> neighbor_list -> hd -> edge == e) {
    return -1;
  } else {
    neighbor_list_item *curr = n -> neighbor_list -> hd;
    while (curr -> next != NULL) {
      if (curr -> next -> edge == e) return -1;
      curr = curr -> next;
    }
    curr -> next = create_neighbor_list_item(e);
  }
  return 0;
}

int add_edge_to_edge_list(edge *e, edge_list *el) {
  if (el -> hd == NULL) {
    el -> hd = e;
  } else if (el -> hd == e) {
    return -1;
  } else {
    edge *curr = el -> hd;
    while (curr -> next != NULL) {
      if (curr -> next == e) return -1;
      curr = curr -> next;
    }
    curr -> next = e;
  }
  return 0;
}

void *add_edge_int(graph *g, edge *e, int src, int dst) {
  e -> src = get_node_by_label_int(g, src);
  e -> dst = get_node_by_label_int(g, dst);
  e -> next = NULL;

  // add to neighbors
  if (e -> src == NULL || e -> dst == NULL ||
      add_neighbor(e -> src, e) < 0 ||
      add_edge_to_edge_list(e, g -> edge_list) < 0) return NULL;

  return e;
}

void *add_edge_bool(graph *g, edge *e, int src, int dst) {
  return add_edge_int(g, e, src, dst);
}

void *add_edge_str(graph *g, edge *e, char *src, char *dst) {
  e -> src = get_node_by_label_str(g, src);
  e -> dst = get_node_by_label_str(g, dst);
  e -> w = NULL;
  e -> next = NULL;
  e -> has_val = 0;

  // add to neighbors
  if (e -> src == NULL || e -> dst == NULL ||
      add_neighbor(e -> src, e) < 0 ||
      add_edge_to_edge_list(e, g -> edge_list) < 0) return NULL;

  return e;
}

int add_node(graph *g, node *n) {
  if (g -> node_list -> hd == NULL) {
    g -> node_list -> hd = n;
  } else if (cmp_node_label(g -> node_list -> hd, n)) {
    return -1;
  } else {
    node *curr = g -> node_list -> hd;
    while (curr -> next != NULL) {
      if (cmp_node_label(curr -> next, n)) return -1;
      curr = curr -> next;
    }
    curr -> next = n;
  }
  return 0;
}

int graph_set_node(graph *g, node *n) {
  // try adding node; handle if node w/ name already exists in the graph
  if (add_node(g, n) < 0 && n -> has_val) {
    int lt = n -> label_typ;
    int dt = n -> data_typ;
    node *n_in_g;

    // find the node in the graph
    if (lt == INTTYPE || lt == BOOLTYPE) {
      n_in_g = get_node_by_label_int(g, n -> label -> i);
    } else {
      n_in_g = get_node_by_label_str(g, n -> label -> s);
    }

    // set its data to the data of n
    if (dt == INTTYPE || dt == BOOLTYPE) {
      set_node_data_int(n_in_g, n -> data -> i, 1);
    } else if (dt == STRTYPE) {
      set_node_data_str(n_in_g, n -> data -> s, 1);
    }
  }

  return 0;
}

int remove_edge(graph *g, edge *e) {
  if (e == NULL) return -1;

  // remove from edge list
  edge *curr_e = g -> edge_list -> hd;
  if (curr_e != NULL && curr_e == e) {
    g -> edge_list -> hd = curr_e -> next;
  } else {
    edge *prev;
    while (curr_e != NULL && curr_e != e) {
      prev = curr_e; 
      curr_e = curr_e -> next; 
    } 

    if (curr_e == NULL) return -1; 

    prev -> next = curr_e -> next;
    prev = NULL;
  }

  // remove from neighbors
  neighbor_list_item *curr_nl = e -> src -> neighbor_list -> hd;
  if (curr_nl != NULL && curr_nl -> edge == e) {
    e -> src -> neighbor_list -> hd = curr_nl -> next;
  } else {
    neighbor_list_item *prev;
    while (curr_nl != NULL && curr_nl -> edge != e) {
      prev = curr_nl; 
      curr_nl = curr_nl -> next; 
    } 

    if (curr_nl == NULL) return -1; 

    prev -> next = curr_nl -> next;
    free(curr_nl);
    prev = NULL;
  }

  // free
  free(e);
  e = NULL;

  return 0;
}

int graph_set_edge_int_int(graph *g, int src_label, int dst_label, int w) {
  edge *e = get_edge_by_src_and_dst_int(g, src_label, dst_label);
  if (add_edge_int(g, e, src_label, dst_label) == NULL) {
    remove_edge(g, e);
    add_edge_int(g, e, src_label, dst_label);
  }

  return 0;
}

int graph_set_edge_str_int(graph *g, char *src_label, char *dst_label, int w) {
  edge *e = get_edge_by_src_and_dst_str(g, src_label, dst_label);
  if (add_edge_str(g, e, src_label, dst_label) == NULL) {
    remove_edge(g, e);
    add_edge_str(g, e, src_label, dst_label);
  }

  return 0;
}

int graph_set_edge_int_str(graph *g, int src_label, int dst_label, char *w) {
  edge *e = get_edge_by_src_and_dst_int(g, src_label, dst_label);
  if (add_edge_int(g, e, src_label, dst_label) == NULL) {
    remove_edge(g, e);
    add_edge_int(g, e, src_label, dst_label);
  }

  return 0;
}

int remove_all_edges(graph *g, node *n) {
  edge *curr_edge = g -> edge_list -> hd;
  edge *temp;
  while (curr_edge != NULL) {
    temp = curr_edge->next;
    if (n -> label_typ == INTTYPE || n -> label_typ == BOOLTYPE) {
      if (*(int *) get_node_label(curr_edge -> src) == *(int *) get_node_label(n) || *(int *) get_node_label(curr_edge -> dst) == *(int *)get_node_label(n)) {
        remove_edge(g, curr_edge);
      }
    }
    if (n -> label_typ == STRTYPE) {
      if ((char *) get_node_label(curr_edge -> src) == (char *)get_node_label(n) || (char *) get_node_label(curr_edge -> dst) == (char *)get_node_label(n)) {
        remove_edge(g, curr_edge);
      }
    }
    curr_edge = temp;
  }
  return 0;
}


int remove_node(graph *g, node *n) {
  if (n->label_typ == INTTYPE || n->label_typ == BOOLTYPE) {
    node *curr = g -> node_list -> hd;
    while (curr != NULL) {
      if ((curr -> label_typ == INTTYPE || curr -> label_typ == BOOLTYPE) && 
        *(int *) get_node_label(curr->next) == *(int *)get_node_label(n)) {
        curr -> next = curr -> next -> next;
        remove_all_edges(g, n);
        return 1;
      }
      curr = curr -> next;
    }
    return 0;
  }
  else if (n->label_typ == STRTYPE) {
    node *curr = g -> node_list -> hd;
    while (curr != NULL) {
      if (curr -> label_typ == STRTYPE && strcmp((char *) get_node_label(curr), (char *)get_node_label(n)) == 0) {
        curr->next = curr->next->next;
        remove_all_edges(g, n);
        return 1;
      }
      curr = curr -> next;
    }
  }
  return 0;
}

int has_node_int(graph *g, int name) {
  if (get_node_by_label_int(g, name)) {
    return 1;
  }
  return 0;
}

int has_node_str(graph *g, char *name) {
  if (get_node_by_label_str(g, name)) {
    return 1;
  }
  return 0;
}

int are_neighbors_int(graph *g, int from_name, int to_name) {
  edge *e = g -> edge_list -> hd;
  while (e != NULL) {
    if ((*(int *) get_node_label(e -> src) == from_name) && (*(int *) get_node_label(e -> dst) == to_name) ) {
      return 1;
    }
    e = e -> next;
  }
  return 0;
}

int are_neighbors_str(graph *g, char *from_name, char *to_name) {
  edge *e = g -> edge_list -> hd;
  while (e != NULL) {
    if (((char *) get_node_label(e -> src) == from_name) && ((char *) get_node_label(e -> dst) == to_name) ) {
      return 1;
    }
    e = e -> next;
  }
  return 0;
}

int is_empty(graph *g) {
  if (g -> node_list -> hd) {
    return 0;
  }
  return 1;
}

int neighbors_int_name(graph *g, int name, int level, int include_current) {
  return 0;
}

int neighbors_str_name(graph *g, char *name, int level, int include_current) {
  return 0;
}

int find_data_int(graph *g, int data) {
  return 0;
}

int find_data_str(graph *g, char *data) {
  return 0;
}

void print_node(node *n) {
  if (n -> label_typ == INTTYPE) {
    printf("%d:", n -> label -> i);
  } else if (n -> label_typ == BOOLTYPE) {
    if (n -> label -> i == 0) printf("false:");
    else printf("true:");
  } else if (n -> label_typ == STRTYPE) {
    printf("\"%s\":", n -> label -> s);
  }

  if (n -> has_val == 0) {
    printf("null");
  } else if (n -> data_typ == INTTYPE) {
    printf("%d", n -> label -> i);
  } else if (n -> data_typ == BOOLTYPE) {
    if (n -> data -> i == 0) printf("false");
    else printf("true");
  } else if (n -> data_typ == STRTYPE) {
    printf("\"%s\"", n -> data -> s);
  }

}

char *print_edge(edge *e) {
  // char *x = strcat(strcat(strcat(strcat(string_of_node(e -> src), "-"), (char *) e -> w), ">"), print_node(e -> dst));
  // return x;
  return NULL;
}

char *print_graph(graph *g) {
  edge *e = g -> edge_list -> hd;
  char *x;
  while (e) {
    strcat(x, print_edge(e));
    e = e -> next;
  }
  return x;
}
