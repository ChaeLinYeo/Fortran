!Write a Fortran function that computes ||A||_1 norm.
!It is assumed that A is a 2-dimensional matrix. 
!The function header should be 
!real*8 function A1norm(A, M, N)
!real*8 A(M, N) 
function A1norm(A,M,N)
integer :: M,N,i,j
real*8  :: A(M,N),C(N),E(N),F,h
! do i=1,M
!     write(*, "(11F21.10)") (A(i,j),j=1,N)
! end do
E=0.
do j=1,N
    do i=1,M
        C(j)=A(i,j)
        E(j)=E(j)+C(j)
    end do
end do
h=maxval(E)
A1norm = h
!print*, "answer : ",h
!A1norm = maximum
return
end function A1norm

program problem02
integer :: M = 3
integer :: N = 3
integer :: i, j, num
real*8 :: A(3,3)
do i=1,M
    do j=1,N
        !A(i,j)=real(j)/real(i)
        read*, num
        A(i,j)=num
    end do
end do
print*, "M*N행렬 : "
do i=1,M
    write(*, "(11F21.10)") (A(i,j),j=1,N)
end do
print*, "각 column의 합계 중 최대값 : "
print*, A1norm(A,M,N)

stop
end program problem02