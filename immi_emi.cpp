//immi_emi.cpp

#include <iostream> 
#include <conio.h>
#include <math.h> 
#include <stdio.h>
#include <fstream>
#include <stdio.h>      /* NULL */
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */
#define alpha 1.0
#define beta 0.6 

double rate_total, p;
double t, dt;
double ranval;
int pop_size;
int i; 

using namespace std;

void main ()
{
	t = 0.0;
	pop_size = 10;
	rate_total = alpha + beta;
	p = alpha/rate_total; 
	
	srand (time(NULL));

	ofstream myfile ("data_sir_immi_emi.csv");
		if (myfile.is_open())
		{
			myfile << "t" << t << "pop_size" << pop_size << "\n";

				for (i = 1; i <= 100; i++)
				{
					do { 
						// rand number ?
						ranval = rand()%100;

						dt = -1.0*log(ranval)/rate_total;
		        t = t + dt;
		        
		        if (ranval < p) 
		        	pop_size = pop_size + 1;
		         
		        else
		        	pop_size = pop_size - 1;
		         		      
		        if (pop_size < 0)
		        	pop_size = 0;
		        	
		      	myfile << "t" << t << "pop_size" << pop_size << "\n";
		      					
						} while (t < 200.0);

				}
		}

		else {
		cout << "Unable to open files";
		
	myfile.close(); 
	cout << "Output csv completed..."; 

	}
	
// unsure about this part
	ofstream myfile2 ("deterministic.dat");
		myfile2 << "0.00" << " 10\n"; 

		for (i = 1; i <= 20; i++) {
			myfile2 << i*10 << " " << 10+(alpha-beta)*i*10 << "\n";
		}
	myfile2.close();

// unsure about this too 
	system("gnuplot -p plot.gnu");

	cout << "\n";
	cout << "#Simulation completed";
		
	_getch(); 
	
}
