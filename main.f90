module mdl
	implicit none
	
	contains

	logical function errorcheck(T,Tmin,Tmax)
		implicit none
		real :: T, Tmin, Tmax

		if(isnan(T) .or. T<Tmin .or. T>Tmax) then
	
			errorcheck = .true.

		else 

			errorcheck = .false.

		endif

	end function errorcheck

end module mdl

program main
	use mdl
	implicit none
	real,allocatable :: T(:,:,:)
	real :: g,T1,T2,T3,T4,Tin,Tmax,Tmin
	integer :: N,steps_in,i,j,k,numb_of_steps,a,b,c,apu
	character(len=80) :: fname
	
	call get_command_argument(1,fname)
	open(1,file=fname)

	!input filename as a command argument: 
	!size of grid, number of iterations(if 0, runs until the 
	!statioary state is reached), lower T, right T
	!upper T, left T, T inner, gamma

	read (1,*)N,steps_in,T1,T2,T3,T4,Tin,g
	
	close(1)

	!check if initial values are incorrect:
	!initial temperatures below zero, size of grid less than 3(only walls exist)
	!gamma > 1 or gamma <= 0, number of steps < 0

	if(T1 < 0 .or. T2 < 0 .or. T3 < 0 .or. T4 < 0 .or. Tin < 0 .or. N < 3 .or. g > 1 .or. g <= 0 .or. steps_in < 0) then
		print *,'Error in initial values.'
		stop
	endif

	!pick highest and lowest temperatures for error handling

	Tmax = max(T1,T2,T3,T4,Tin)
	Tmin = min(T1,T2,T3,T4,Tin)

	!create NxN matrice and set initial values

	allocate (T(N,N,2))

	T(2:N-1,2:N-1,1:2) = Tin

	T(1,1:N,1:2) = T1
	T(1:N,1,1:2) = T4
	T(N,1:N,1:2) = T3
	T(1:N,N,1:2) = T2
	
!___________________________


	!calculate temperatures, if given number of iterations = 0
	!the program runs until stationary state is reached
	
	apu = 0
	numb_of_steps = 0
	
	if(steps_in == 0) then

	!code runs until the stationary state is reached

	do while(apu == 0)
		
		numb_of_steps = numb_of_steps+1
		do i=2,N-1
			
			do j=2,N-1
				T(i,j,1)=T(i,j,2)-g*(4*T(i,j,2)-T(i+1,j,2)-T(i,j+1,2)-T(i-1,j,2)-T(i,j-1,2))
				

				!check if T(i,i) is NaN and if T(i,j) 
				!is lower or higher than the initial values of T
				!if true -> stop program and print error

				if(errorcheck(T(i,j,1),Tmin,Tmax)) then
						
					print *,'Error in temperature'
					print *,'stop at t = ',numb_of_steps,' at temperature T(',i,',',j,') = ',T(i,j,1)
					stop
				endif
			end do
		end do

		!checking every tenth round if 
		!temperatures change less than 0.0005K
		!if true --> exit loop

		if (mod(numb_of_steps,10) == 0) then

			c = 0
			do a=2,N-1
				do b=2,N-1
					
					if(abs(T(a,b,1)-T(a,b,2)) < 0.0005) then
						c = c+1
					endif

				end do
			end do

		endif
		
		if(c == (N-2)*(N-2)) then 
			apu = 1
		endif
		

		!save temperatures for next round
		T(2:N-1,2:N-2,2)=T(2:N-1,2:N-1,1)
		
		
	end do	


	else 
	!code runs the given number of iterations
	
	numb_of_steps = steps_in

	do k=1,steps_in
		
		do i=2,N-1

			do j=2,N-1

				T(i,j,1)=T(i,j,2)-g*(4*T(i,j,2)-T(i+1,j,2)-T(i,j+1,2)-T(i-1,j,2)-T(i,j-1,2))
				
				!check if T(a,b) is NaN, and check if T(a,b) 
				!is lower or higher than the initial values of T
				!if true -> stop loop and print error
				
				if(errorcheck(T(i,j,1),Tmin,Tmax)) then	
					print *,'Error in temperature'
					print *,'stop at t = ',k,' at temperature T(',i,',',j,') = ',T(i,j,1)
					stop
				endif
			end do
		end do
		
		!save temperatures for next round
		T(2:N-1,2:N-2,2)=T(2:N-1,2:N-1,1)
		
	end do

	endif


!writing to new file "data.txt"
!N, number of iterations, g, Tin and the final temperatures


	open(2,file = 'data.txt',status='new',action="write")
		
		write(2,*),N,numb_of_steps,g,Tin
		do i=1,N
			write(2,*),T(1:N,i,1)
		end do

	close(2)
	
	call system('octave plot.m')
	call system('rm data.txt')

end program main
