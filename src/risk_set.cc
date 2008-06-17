#include <iostream>
#include <R.h>

extern "C" {

  void risk_set(int *n, int *lt, int *dim_nrisk, int *dim_nev, double *times,
		int *from, int *to, double *entry, double *exit,
		int *nrisk, int *ncens, int *nev) {
    for (int i=0; i < *lt; ++i) {
      for (int j=0; j < *n; ++j) {
	if (entry[j] < times[i] && exit[j] >= times[i]) {
	  nrisk[i + *lt * (from[j] - 1)] += 1;
	}
	if (exit[j] == times[i]) {
	  if (to[j] == 0) {
	    ncens[i + *lt * (from[j] - 1)] += 1;
	  }
	  else {
	    nev[dim_nev[1] * dim_nev[1]*i + from[j] - 1 + dim_nev[1] * (to[j] - 1)] += 1; 
	  }
	}
      }
    }
  }
}
