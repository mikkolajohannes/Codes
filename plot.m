clear ; clc

file = fopen("data.txt","r");

T={};

rivi=fgets(file);

# read N from data.txt frist line
jasen = strsplit(rivi,' ');
N = str2double(jasen{1,2});
steps = jasen{1,3};
g = jasen{1,4};
Tin = jasen{1,5};

#read temperatures from data.txt to T[]
rivi=fgets(file);
T = [];

for(i=1:N)
	jasen=strsplit(rivi,' ');
	
	for(j=2:N+1)
		T = [T;str2double(jasen{1,j})];
	endfor

	rivi=fgets(file);
	i=i+1;
endfor

#reshape T to N x N matrice

H = [];
H = reshape(T,N,N);
N;
steps;

s = ["iterations: ",steps,", \\gamma = ",g,", inner T(t=1): ",Tin];


#_____________________plot

graphics_toolkit('gnuplot')
  
subplot(10,10,[01 100]);
set(gca,'fontsize',10);
axis([2,N,2,N]);
hold on;
surf(H)
colorbar;
shading interp;
text(0,-0.05*N,s)
hold off;
print('output.jpg','-djpg');
