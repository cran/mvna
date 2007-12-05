#include <stdio.h> 
extern "C" {

/*******************************************************************/
/* A function to be called by predict.mvna                         */
/*******************************************************************/ 
/* nt : length of na                                               */
/* na : Nelson-Aalen estimates                                     */
/* var1 : Martingal-based variance estimates                       */
/* var2 : Greenwood-type variance estimates                        */ 
/* times : Transition times                                        */
/* giventimes : Timepoints given by the user                       */
/* ngt : length of giventimes                                      */
/* nat : Nelson-Aalen estimates at the given times                 */
/* var1t : Maringale-based variance estimates at the given times   */
/* var2t : Greenwood-type variance estimates at the given times    */
/*******************************************************************/

void pr(int *nt,double *na, double *var1, double *var2,
	double *times, double *giventimes, int *ngt,
 	double *nat, double *var1t, double *var2t)
{
  for (int i=0; i < *ngt; i++) {
    int j=0;
    for (j=0; j< *nt-1; j++) {
      if (giventimes[i] >=0 && giventimes[i] < times[0]) {
	nat[i] = var1t[i] = var2t[i] =0;
	goto aaa;
      }
      if (giventimes[i] >= times[j] &&
	   giventimes[i] < times[j+1]) {
      break;
      }
    }
    if (giventimes[i] < times[j+1]+1e-6 &&
	giventimes[i] > times[j+1]-1e-6) {
      nat[i] = na[j+1];
      var1t[i] = var1[j+1];
      var2t[i] = var2[j+1];
    }
    else {
      nat[i] = na[j];
      var1t[i] = var1[j];
      var2t[i] = var2[j];
    }
  aaa: continue;
  }
}
}
