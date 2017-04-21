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
			myfile << "t " << t << " pops " << pops << " popi " << popi;
			myfile << " popr " << popr << "\n"; 

				for (i = 1; i <= 100; i++)
				{
					do 
						{
							rate_total = beta*pops*popi + gamma*popi;

							//random number from ? 
							ranval = rand()%100;
							
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
								
								if (pops <= 0) 
										pops = 0;
										
								if (popi <= 0){
								    popi = 0;
								    myfile << "t " << t << " pops " << pops << " popi " << popi;
								    myfile << " popr " << popr << "\n"; 
								}

								myfile << "t " << t << " pops " << pops << " popi " << popi;
								myfile << " popr " << popr << "\n"; 

				 		}while (t < 100.0);
			}			 
  	}
  	
  	else {
		cout << "Unable to open file";
		}
			
  myfile.close();
	cout << "Output csv completed...";

	_getch(); 

} 

