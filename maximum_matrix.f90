!Write a Fortran function that returns the maximum absolute value of the entries of a real N by N matrix A. 
!The type is real*8. The input parameters are N, A. 
!You can assume that the row dimension in main routine is same as N.
implicit real*8 (a-h, o-z)
real*8 :: Amax
integer :: i,j,num
integer :: N=3
dimension A(3,3)
do i=1,N
    do j=1,N
        read*, num
        A(i,j)=num
    end do
end do
write(*,*) Amax(A,N)
stop
end

real*8 function Amax (A, N)
implicit real*8 (a-h, o-z)
dimension A(N,*)
max=-1.
Anum=0.
do i = 1,N
    do j = 1,N
        Anum = abs(A(i,j))
        if (Anum > max) max = Anum
    end do
end do
Amax=max
return
end