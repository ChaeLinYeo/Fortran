!Write a Fortran function that returns the maximum of the diagonal entries of a real N by N matrix A. 
!The type is real*8. 
!The input parameters are N, A.
!You can assume that the row dimension in main routine is same as N. 
!(Diagonal means a(i,j), I = j, e.g, a(2,2), a(3,3) )
implicit real*8 (a-h, o-z)
real*8 Adiag
integer :: i,j,num
integer :: N=3
dimension A(3,3)
do i=1,N
    do j=1,N
        read*, num
        A(i,j)=num
    end do
end do
write(*,*) Adiag(A,N)
stop
end

real*8 function Adiag (A, N)
implicit real*8 (a-h, o-z)
dimension A(N,*)
max=-1.
diagnum=0.
do i = 1,N
    diagnum = A(i,i)
    if (diagnum > max) max = diagnum
end do
Adiag=max
return
end