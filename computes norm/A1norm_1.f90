!Write a Fortran function that computes ||A||_1 norm.
!It is assumed that A is a 2-dimensional matrix. 
!The function header should be 
!real*8 function A1norm(A, M, N)
!real*8 A(M, N)
real*8 function A1norm(A,N)
integer :: N,i,j
real*8 :: A(N,*), sum, rmax
rmax=-1.
do j=1,N
    sum=0.
    do i=1,N
        sum=sum+abs(A(i,j))
    end do
    if(sum>rmax) rmax=sum
end do
A1norm = rmax
return
end


program problem02
integer :: N = 3
integer :: i, j, num
real*8 A(3,3)
do i=1,N
    do j=1,N
        read*, num
        A(i,j)=num
    end do
end do
print*, A1norm(A,N)

stop
end