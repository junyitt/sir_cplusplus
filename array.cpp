#include<stdio.h>
#include<iostream>
#include<fstream>
#include<conio.h>

using namespace std;
void addone(int p[]);

void main(){
	int nrowA = 100;
	int *A = new (nothrow) int[nrowA];
	int i;
	
	for(i = 0; i < 100; i++){
		A[i] = i*10;
		
		cout << A[i] << endl;
	}
	addone(A);
	
	for(i = 0; i < 100; i++){
		
		cout << A[i] << endl;
	}
	
	
	
	
	_getch();
}


void addone(int p[]){
	
	for(int i = 0; i < 100; i++){
		
		p[i] = p[i]+1;
	}
}
