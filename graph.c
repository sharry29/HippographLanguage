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

void *create_prim_void(void *v) {
  primitive *p = (primitive *) malloc(sizeof(primitive));
  p -> v = v;
  return (void *) p;
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

void set_node_label_int(node *n, int i) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_int(i);
  n -> label_typ = INTTYPE;
}

void set_node_label_str(node *n, char *s) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_str(s);
  n -> label_typ = STRTYPE;
}

void set_node_label_void(node *n, void *v) {
  if (n -> label != NULL) {
    free(n -> label);
  }
  n -> label = create_prim_void(v);
  n -> label_typ = VOIDTYPE;
}

void set_node_data_int(node *n, int i, int has_val) {
  if (n -> data != NULL) {
    free(n -> data);
  }
  n -> data = create_prim_int(i);
  n -> data_typ = INTTYPE;
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

void set_node_data_void(node *n, void *v, int has_val) { //does this need has_val arg?
  if (n -> data != NULL) {
    free(n -> data);
  }
  n -> data = create_prim_void(v);
  n -> data_typ = VOIDTYPE;
  n -> has_val = has_val;
}

void *get_node_label(node *n) {
  int typ = n -> label_typ;
  void *label = NULL;

  if (typ == INTTYPE) {
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

  if (typ == INTTYPE) {
    data = (void *) &(n -> data -> i);
  } else if (typ == STRTYPE) {
    data = (void *) n -> data -> s;
  } else if (typ == VOIDTYPE) {
    data = (void *) n -> data -> v;
  }
  return data;  // not guaranteed to return valid value if not has_val
}

/* EDGES */

void *create_edge(node *src, node *dst) {
  edge *e = (edge *) malloc(sizeof(edge));
  e -> src = src;
  e -> dst = dst;
  e -> w = NULL;
  e -> next = NULL;
  e -> has_val = 0;
  return (void *) e;
}

void set_edge_src_int(edge *e, int i) {
  if (e -> src != NULL) {
    free(e -> src);
  }
  e -> src = create_prim_int(i);
}

void set_edge_src_str(edge *e, char *s) {
  if (e -> src != NULL) {
    free(e -> src);
  }
  e -> src = create_prim_str(s);
}

void set_edge_src_void(edge *e, void *v) {
  if (e -> src != NULL) {
    free(e -> src);
  }
  e -> src = create_prim_void(v);
}

void set_edge_dst_int(edge *e, int i) {
  if (e -> dst != NULL) {
    free(e -> dst);
  }
  e -> dst = create_prim_int(i);
}

void set_edge_dst_str(edge *e, char *s) {
  if (e -> dst != NULL) {
    free(e -> dst);
  }
  e -> dst = create_prim_str(s);
}

void set_edge_dst_void(edge *e, void *v) {
  if (e -> dst != NULL) {
    free(e -> dst);
  }
  e -> dst = create_prim_void(v);
}

void set_edge_w_int(edge *e, int i) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_int(i);
  e -> w_typ = INTTYPE;
}

void set_edge_w_str(edge *e, char *s) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_str(s);
  e -> w_typ = STRTYPE;
}

void set_edge_w_void(edge *e, void *v) {
  if (e -> w != NULL) {
    free(e -> w);
  }
  e -> w = create_prim_void(v);
  e -> w_typ = VOIDTYPE;
}

void *get_edge_w(edge *e) {
  int typ = e -> w_typ;
  void *w = NULL;

  if (typ == INTTYPE) {
    w = (void *) &(e -> w -> i);
  } else if (typ == STRTYPE) {
    w = (void *) e -> w -> s;
  } else if (typ == VOIDTYPE) {
    w = (void *) e -> w -> v;
  }
  return w;
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

edge *get_edge_by_ptr(graph *g, edge *e) {
  edge *curr = g -> edge_list -> hd;
  while (curr != NULL) {
    if (curr == e) {
      return curr;
    }
    curr = curr -> next;
  }
  return NULL;
}

node *get_node_by_ptr(graph *g, node *n) {
  node *curr = g -> node_list -> hd;
  while (curr != NULL) {
    if (curr == n) {
      return curr;
    }
    curr = curr -> next;
  }
  return NULL;
}

node *get_node_by_label_int(graph *g, int label) {
  node *curr = g -> node_list -> hd;
  while (curr != NULL) {
    if (curr -> label_typ == INTTYPE && *(int *) get_node_label(curr) == label) {
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

node *get_node_by_label_void(graph *g, void *label) {
  node *curr = g -> node_list -> hd;
  while (curr != NULL) {
    if (curr -> label_typ == VOIDTYPE && get_node_label(curr) == label) {
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

int add_edge(graph *g, edge *e) {
  // add to neighbors
  node *src = get_node_by_ptr(g, e -> src);
  node *dst = get_node_by_ptr(g, e -> dst);
  if (src == NULL || dst == NULL || add_neighbor(src, e) < 0) return -1;

  // add to edge list
  if (g -> edge_list -> hd == NULL) {
    g -> edge_list -> hd = e;
  } else if (g -> edge_list -> hd == e) {
    return -1;
  } else {
    edge *curr = g -> edge_list -> hd;
    while (curr -> next != NULL) {
      if (curr -> next == e) return -1;
      curr = curr -> next;
    }
    curr -> next = e;
  }
  return 0;
}

int add_node(graph *g, node *n) {
  if (g -> node_list -> hd == NULL) {
    g -> node_list -> hd = n;
  } else if (g -> node_list -> hd == n) {
    return -1;
  } else {
    node *curr = g -> node_list -> hd;
    while (curr -> next != NULL) {
      if (curr -> next == n) return -1;
      curr = curr -> next;
    }
    curr -> next = n;
  }
  return 0;
}

int remove_edge(graph *g, edge *e) {
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
