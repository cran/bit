/*
   1-bit boolean vectors for R
   Copyright 2008 Jens Oehlschlägel
*/

#include <R.h>
#include <Rinternals.h>

int BITS;    // number of bits in one word
int LASTBIT; // last bit in word (=BITS-1)

/*
 & bitwise and
 | bitwise or
 ^ bitwise xor
 ~ bitwise not
*/


int *mask0, *mask1;

void bit_init(int   bits){
  BITS = bits;
  LASTBIT = bits - 1;
  mask0 = calloc(BITS, sizeof(int));
  mask1 = calloc(BITS, sizeof(int));
  int b = 1;
  int i;
  for (i=0; i<BITS; i++){
    mask1[i] = b;
    mask0[i] = ~b;
    //Rprintf("i=%d mask0[i]=%d mask1[i]=%d\n", i, mask0[i], mask1[i]);
    b = b << 1;
  }
}

void bit_done(){
  free(mask0);
  free(mask1);
}


SEXP R_bit_init(SEXP bits_){
  int bits = asInteger(bits_);
  bit_init(bits);
  return R_NilValue;
}
SEXP R_bit_done(){
  bit_done();
  return R_NilValue;
}





void bit_set1(int *b, int *l, int n){
  register int i,j,k;
  for (i=0; i<n; i++){
    j = i/BITS;
    k = i%BITS;
    if (l[i])
      b[j] |= mask1[k];
    else
      b[j] &= mask0[k];
  }
}

void bit_get1(int *b, int *l, int n){
  register int i,j,k;
  for (i=0; i<n; i++){
    j = i/BITS;
    k = i%BITS;
    l[i] = b[j] & mask1[k] ? 1 : 0;
  }
}

void bit_set(int *b, int *l, int n){
  register int word;
  register int i=n;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (l[--i])
        word |= mask1[k];
      else
        word &= mask0[k];
      //Rprintf("pre i=%d j=%d k=%d l[i]=%d mask0[k]=%d mask1[k]=%d word=%d\n", i, j, k, l[i], mask0[k], mask1[k], word);
    }
    b[j] = word;
  }
  for (j--; j>=0; j--){
    word = b[j];
    for(k=LASTBIT; k>=0; k--){
      if (l[--i])
        word |= mask1[k];
      else
        word &= mask0[k];
      //Rprintf("main i=%d j=%d k=%d l[i]=%d mask0[k]=%d mask1[k]=%d word=%d\n", i, j, k, l[i], mask0[k], mask1[k], word);
    }
    b[j] = word;
  }
}

void bit_get(int *b, int *l, int n){
  register int word;
  register int i=n;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      l[--i] = word & mask1[k] ? 1 : 0;
    }
  }
  for (j--; j>=0; j--){
    word = b[j];
    for(k=LASTBIT ;k>=0 ;k--){
      l[--i] = word & mask1[k] ? 1 : 0;
    }
  }
}

void bit_which_positive(int *b, int *l, int n, int s){
  register int word;
  register int h=s;
  register int i=n;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (word & mask1[k])
        l[--h] = i;
      i--;
    }
  }
  for (j--; j>=0; j--){
    word = b[j];
    for(k=LASTBIT ;k>=0 ;k--){
      if (word & mask1[k])
        l[--h] = i;
      i--;
    }
  }
}

void bit_which_negative(int *b, int *l, int n, int s){
  register int word;
  register int h=s;
  register int i= -n;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (!(word & mask1[k]))
        l[--h] = i;
      i++;
    }
  }
  for (j--; j>=0; j--){
    word = b[j];
    for(k=LASTBIT ;k>=0 ;k--){
      if (!(word & mask1[k]))
        l[--h] = i;
      i++;
    }
  }
}

void bit_extract(int *b, int *i, int *l, int n){
  register int il, ib, j, k;
  for (il=0; il<n; il++){
    ib = i[il] - 1;
    j = ib/BITS;
    k = ib%BITS;
    l[il] = b[j] & mask1[k] ? 1 : 0;
  }
}

void bit_replace(int *b, int *i, int *l, int n){
  register int il, ib, j, k;
  for (il=0; il<n; il++){
    ib = i[il] - 1;
    j = ib/BITS;
    k = ib%BITS;
    if (l[il])
      b[j] |= mask1[k];
    else
      b[j] &= mask0[k];
  }
}




void bit_not(int *b, int n){
  register int i;
  for (i=0; i<n; i++){
    b[i] = ~b[i];
  }
}


void bit_and(int *b1, int *b2, int *ret, int n){
  register int i;
  for (i=0; i<n; i++){
    ret[i] = b1[i] & b2[i];
  }
}

void bit_or(int *b1, int *b2, int *ret, int n){
  register int i;
  for (i=0; i<n; i++){
    ret[i] = b1[i] | b2[i];
  }
}

void bit_xor(int *b1, int *b2, int *ret, int n){
  register int i;
  for (i=0; i<n; i++){
    ret[i] = b1[i] ^ b2[i];
  }
}

void bit_equal(int *b1, int *b2, int *ret, int n){
  register int i;
  for (i=0; i<n; i++){
    ret[i] = ~(b1[i] ^ b2[i]);
  }
}


int bit_sum(int *b, int n){
  register int word;
  register int s=0;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (word & mask1[k])
        s++;
    }
  }
  for (j--; j>=0; j--){
    word = b[j];
    for(k=LASTBIT ;k>=0 ;k--){
      if (word & mask1[k])
        s++;
    }
  }
  return s;
}


/* == bit_min */
int bit_all(int *b, int n){
  register int word;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (!(word & mask1[k]))
        return 0;
    }
  }
  for (j--; j>=0; j--){
    if(~(b[j]))
      return 0;
  }
  return 1;
}

/* == bit_max */
int bit_any(int *b, int n){
  register int word;
  register int k=n%BITS;
  int j=n/BITS;
  if (k){
    word = b[j];
    for(k--; k>=0; k--){
      if (word & mask1[k])
        return 1;
    }
  }
  for (j--; j>=0; j--){
    if(b[j])
      return 1;
  }
  return 0;
}



SEXP R_bit_set(SEXP b_, SEXP l_){
  int *b = INTEGER(b_);
  int *l = LOGICAL(l_);
  int n = asInteger(getAttrib(b_, install("n")));
  bit_set(b, l, n);
  return(b_);
}

SEXP R_bit_get(SEXP b_, SEXP l_){
  int *b = INTEGER(b_);
  int *l = LOGICAL(l_);
  int n = asInteger(getAttrib(b_, install("n")));
  bit_get(b, l, n);
  return(l_);
}

SEXP R_bit_set_integer(SEXP b_, SEXP l_){
  int *b = INTEGER(b_);
  int *l = INTEGER(l_);
  int n = asInteger(getAttrib(b_, install("n")));
  bit_set(b, l, n);
  return(b_);
}

SEXP R_bit_get_integer(SEXP b_, SEXP l_){
  int *b = INTEGER(b_);
  int *l = INTEGER(l_);
  int n = asInteger(getAttrib(b_, install("n")));
  bit_get(b, l, n);
  return(l_);
}

SEXP R_bit_which(SEXP b_, SEXP s_, SEXP negative_){
  int *b = INTEGER(b_);
  int n = asInteger(getAttrib(b_, install("n")));
  int s = asInteger(s_);
  SEXP ret_;
  int *ret;
  if (asLogical(negative_)){
    // negative return
    PROTECT( ret_ = allocVector(INTSXP,s) );
    ret = INTEGER(ret_);
    bit_which_negative(b, ret, n, s);
  }else{
    // positive return
    PROTECT( ret_ = allocVector(INTSXP,s) );
    ret = INTEGER(ret_);
    bit_which_positive(b, ret, n, s);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_extract(SEXP b_, SEXP i_, SEXP l_){
  int *b = INTEGER(b_);
  int *i = INTEGER(i_);
  int *l = LOGICAL(l_);
  int n = LENGTH(i_);
  bit_extract(b, i, l, n);
  return(l_);
}

SEXP R_bit_replace(SEXP b_, SEXP i_, SEXP l_){
  int *b = INTEGER(b_);
  int *i = INTEGER(i_);
  int *l = LOGICAL(l_);
  int n = LENGTH(i_);
  bit_replace(b, i, l, n);
  return(b_);
}



SEXP R_bit_not(SEXP b_){
  int *b = INTEGER(b_);
  int n = LENGTH(b_);
  bit_not(b, n);
  return(b_);
}

SEXP R_bit_and(SEXP b1_, SEXP b2_, SEXP ret_){
  int *b1 = INTEGER(b1_);
  int *b2 = INTEGER(b2_);
  int *ret = INTEGER(ret_);
  int n = LENGTH(b1_);
  bit_and(b1, b2, ret, n);
  return(ret_);
}

SEXP R_bit_or(SEXP b1_, SEXP b2_, SEXP ret_){
  int *b1 = INTEGER(b1_);
  int *b2 = INTEGER(b2_);
  int *ret = INTEGER(ret_);
  int n = LENGTH(b1_);
  bit_or(b1, b2, ret, n);
  return(ret_);
}

SEXP R_bit_xor(SEXP b1_, SEXP b2_, SEXP ret_){
  int *b1 = INTEGER(b1_);
  int *b2 = INTEGER(b2_);
  int *ret = INTEGER(ret_);
  int n = LENGTH(b1_);
  bit_xor(b1, b2, ret, n);
  return(ret_);
}

SEXP R_bit_equal(SEXP b1_, SEXP b2_, SEXP ret_){
  int *b1 = INTEGER(b1_);
  int *b2 = INTEGER(b2_);
  int *ret = INTEGER(ret_);
  int n = LENGTH(b1_);
  bit_equal(b1, b2, ret, n);
  return(ret_);
}


SEXP R_bit_sum(SEXP b_){
  int *b = INTEGER(b_);
  int n = asInteger(getAttrib(b_, install("n")));
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  INTEGER(ret_)[0] = bit_sum(b, n);
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_all(SEXP b_){
  int *b = INTEGER(b_);
  int n = asInteger(getAttrib(b_, install("n")));
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  LOGICAL(ret_)[0] = bit_all(b, n);
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_any(SEXP b_){
  int *b = INTEGER(b_);
  int n = asInteger(getAttrib(b_, install("n")));
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  LOGICAL(ret_)[0] = bit_any(b, n);
  UNPROTECT(1);
  return(ret_);
}


// performance tests without bit

void filter_getset(int *l1, int *l2, int n){
  int i;
  for (i=0; i<n; i++){
    if (l1[i])
      l2[i] = 1;
    else
      l2[i] = 0;
  }
}


SEXP R_filter_getset(SEXP l1_, SEXP l2_){
  int *l1 = LOGICAL(l1_);
  int *l2 = LOGICAL(l2_);
  int n = LENGTH(l1_);
  filter_getset(l1, l2, n);
  return(l2_);
}

