#include <stdio.h>

struct lsp { 
  int d;
  int g; 
};

int add(int a, int b) {
  return a+b;
}

int main () {
  
  int x;
  int i;
  int z;
  char chr;
  int *p;
  double y;

  
  while (x > 50) {
    x++;
  };
  
  for (i=0; i<50; i++) {
    int f;
    f = x+i;
  };

  if (x < 120) {
    x--; }
  else x = 120;

  lsp.d = x;
  lsp.g = i;
  i = add (x,y);
  
  z = 3;
  
  /* 
     Test des erreurs de typage :
  */

  // x.g = i;
  // x = k;
  // x+2 = i;
  // *x = i;
  // x = sizeof(void);
  // sizeof(struct e);
  // (void) x;
  // lsp = i;
  // lsp++;
  // i = -lsp;
  // i = x % y;
  // i = add(x);
  // i = add (x,z,z+1);
  // i = add (x, lsp);
  // i = ad (x, z);
  // if (lsp) { return x; }
  // return lsp;
 
  return x;
}
