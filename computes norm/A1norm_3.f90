!Write a Fortran function, A1norm(A, N), which returns the A1-norm of A
! implicit real*8 (a-h, o-z)
! real*8 :: A1norm
! integer :: i,j,num
! integer :: N=3
! dimension A(3,3)
! do i=1,N
!     do j=1,N
!         read*, num
!         A(i,j)=num
!     end do
! end do
! write(*,*) A1norm(A,N)
! stop
! end

! real*8 function A1norm(A,N)
! implicit real*8 (a-h, o-z)
! dimension A(N,*)
! max=-1.
! do j=1,N
!     sum=0.
!     do i=1,N
!         sum=sum+abs(A(i,j))
!     end do
!     if(sum>max) max=sum
! end do
! A1norm = max
! return
! end

implicit real*8 (a-h, o-z)
real*8 :: A1norm
integer :: i,j,num
integer :: N=2
integer :: M=3
dimension A(3,2)
do i=1,M
    do j=1,N
        read*, num
        A(i,j)=num
    end do
end do
write(*,*) A1norm(A,N,M)
stop
end

real*8 function A1norm(A,N,M)
implicit real*8 (a-h, o-z)
dimension A(M,N)
max=-1
do j=1,N
    sum=0.
    do i=1,M
        sum=sum+abs(A(i,j))
    end do
    if(sum>max) max=sum
end do
A1norm = max
return
end