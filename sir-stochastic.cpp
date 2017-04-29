//sir-stochastic.f90 

#include <iostream>
#include <conio.h> 
#include <math.h> 
#include <stdio.h>
#include <fstream>
#include <stdio.h>      /* NULL */
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */
#define beta 0.001
#define gamma 0.1 

double rate_total, p; 
double t, dt;
double ranval; 
int pops, popi, popr;
int i; 
	
using namespace std; 

void main () 
{
	t = 0.0;
	pops = 190;
	popi = 10;
  popr = 0;

	srand (time(NULL));

	ofstream myfile ("data_sir_stochastic.csv");
  	if (myfile.is_open())
  	{
			myfile << "time, pops, popi, popr\n";
			myfile << t << "," << pops << "," << popi << "," << popr << "\n";

				// for (i = 1; i <= 100; i++)
				// {
				do 
					{
						if(popi == 0){
							t = t+0.1; //step
						}else{
						
							rate_total = beta*pops*popi + gamma*popi;

							// ranval = rand()%2; It's not bernoulli 0 or 1, should be uniform between 0 to 1
							ranval = (rand()) % 1000;
							ranval = ranval/1000 + 0.00001;
						
							 dt = -1.0*log(ranval)/rate_total;
								t = t + dt;
								p = beta*pops*popi/rate_total;
								
								if (ranval < p) {
								   pops = pops - 1;
								   popi = popi + 1;
								}
								
								else {
								   popi = popi - 1;
								   popr = popr + 1;
								}
								
								if (pops <= 0) {
										pops = 0;
								}
								if (popi <= 0){
									popi = 0;

									// myfile << t << "," << pops << "," << popi << "," << popr << "\n";
								}
						}
									
									myfile << t << "," << pops << "," << popi << "," << popr << "\n";
									
									cout << "t: " << t    << ";; rand: " << ranval  << ";; rate_total: " << rate_total     << ";; pops: " << pops  << ";; popi: " << popi  << ";; popr: " << popr << endl;
						
					}while (popi > 0 || t < 120);
				
			// }			 
  	}
  	
  	else {
		cout << "Unable to open file";
		}
			
  myfile.close();

	// can't plot the graph 
  // system("gnuplot -p sir-i.gnu");
  
	cout << "Output csv completed...";

	_getch(); 

} 

