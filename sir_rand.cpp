#include<stdio.h>
#include<iostream>
#include<fstream>
#include<conio.h>
#include <new>

// double beta = 0.001;
// double gamma = 0.1; 
	// double tstep = 0.1;
	// double nsteps = 1000;
	double tstep = 0.05;
	double nsteps = 4000;
	
double beta = 0.000095;
double gamma = 0.33333; 
	double ss = 10000;
	double ii = 10;
	double rr = 0;
	double tt = 0;
		
	//MRand
		int nPlace = 2;		
		double* M = new double[nPlace];
		

	  
	void rk4(double *ss, double *ii, double *rr, double *tt, double M[]);
	double derivS(double tdummy, double sdummy, double idummy, double rdummy);
	double derivI(double tdummy, double sdummy, double idummy, double rdummy, double M[]);
	double derivR(double tdummy, double sdummy, double idummy, double rdummy);
	int rpois(double lambda, int seed);
	void Mrand(double seed, double lambda[], double t);
	
using namespace std;


 void main(){
	int j;

	//write file
	ofstream myfile ("data_sir_rand.csv");
	  if (myfile.is_open())
	  {
		//header
		myfile << "time, S, I, R, M0, M1\n";
		
		//Loop #####
			//call rk4 function
			for(j = 1; j <= nsteps; j++){
				 tt = j*tstep;
				 
				//mrand
				double seed = rand();

					double *lambdaPlace = new (nothrow) double[nPlace];	
					//initialize lambdal
					for(int k = 0; k < nPlace; k++){
						lambdaPlace[k] = 20-k; 
					}
				Mrand(seed, lambdaPlace, tt);
				
				//call rk4
				rk4(&ss, &ii, &rr, &tt, M);
				//write
				myfile << tt <<"," << ss << "," << ii << "," << rr << "," << M[0] << "," << M[1] << "\n";
			}
		
		//done
			myfile.close();
			cout << "Output csv completed..." << endl;
			
	  }
	  else
	  {	  
		  cout << "Unable to open file";
	  }
	  _getch();
	  
 }
 
void rk4(double *ss, double *ii, double *rr, double *tt, double M[]){
  
  double s, i, r, t;
  double h;
  double s1, s2, s3, s4;
  double i1, i2, i3, i4;
  double r1, r2, r3, r4;
	
	
  s = *ss;
  i = *ii;
  r = *rr;
  t = *tt;
  
  h = tstep/2.0;
  
  s1 = tstep * derivS(t, s, i, r);
  i1 = tstep * derivI(t, s, i, r, M);
  r1 = tstep * derivR(t, s, i, r);
  
  s2 = tstep * derivS(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0);
  i2 = tstep * derivI(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0, M);
  r2 = tstep * derivR(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0);
  
  s3 = tstep * derivS(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0);
  i3 = tstep * derivI(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0, M);
  r3 = tstep * derivR(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0);
  
  s4 = tstep * derivS(t+tstep, s+s3, i+i3, r+r3);
  i4 = tstep * derivI(t+tstep, s+s3, i+i3, r+r3, M);
  r4 = tstep * derivR(t+tstep, s+s3, i+i3, r+r3);

  s = s + (s1 + (2.0*(s2 + s3)) + s4)/6.0;
  i = i + (i1 + (2.0*(i2 + i3)) + i4)/6.0;
  r = r + (r1 + (2.0*(r2 + r3)) + r4)/6.0;
  
  *ss = s;
  *ii = i;
  *rr = r;
  *tt = t;
  
}




double derivS(double tdummy, double sdummy, double idummy, double rdummy){
	
	return(-1.0*beta*sdummy*idummy ); 
  
}

double derivI(double tdummy, double sdummy, double idummy, double rdummy, double M[]){
	return(beta*sdummy*idummy - gamma*idummy - M[0] + M[1]); ////add random immigration emmigration
}

double derivR(double tdummy, double sdummy, double idummy, double rdummy){
	return(gamma*idummy);
}

void Mrand(double seed, double lambda[], double t){

	for(int j = 0; j < nPlace; j++){
		M[j] = rpois(lambda[j]*exp(-0.05*t), seed);
	}
}

int rpois(double lambda, int seed)
{
  double L, p;
  double result;
  int k;

  srand( seed );
  L = exp(-1.0*lambda);

    k = 0;
    p = 1;
    do {
      k = k + 1;
      p = p * rand()/RAND_MAX;
    } while ( p > L );
    result = k-1;

  return result;
}

