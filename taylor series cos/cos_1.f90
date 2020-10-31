!Using Taylor series expansion for cos(x), compute cos(0.5) up to 7 decimal digits. 
!In this case for 7 decimal digits terminate if the absolute value of the term is less that 10**(-7).

!     FORTRAN PROGRAM TO COMPUTE COS(X), USING THE SERIES
!         1 - X^2/2! + X^4/4! + ... + (-1)^k X^(2k)/(2k)!
! 
PROGRAM cosseries
  IMPLICIT NONE
  DOUBLE PRECISION :: sum, x, a
  INTEGER :: k
  
  PRINT *, "give x: "
  READ *, x
  
  sum = 0.0d0   
  k = 0
  a = 1.0d0 ! initial value of a is the term for n=0.
  DO 
     sum = sum + a
     !     next term is ...
     k = k+1
     a = a * (-x*x) / (2*k*(2*k-1))
     IF (sum + a == sum) EXIT
  ENDDO

!  PRINT *, "The sum of Taylor series for COS at ", x, " is ", sum
!  PRINT *, "The correct value is ", COS(x)
  write(*, 100) sum
100 format (F9.7)
END PROGRAM cosseries