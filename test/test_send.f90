PROGRAM name
  USE redis_mod, ONLY: array_to_redis
  IMPLICIT NONE

  INTEGER, PARAMETER ::n=200, m=100
  INTEGER            :: dims(2)
  INTEGER            :: f(n,m)
  INTEGER            :: ndims
  INTEGER            :: i, j

  DO j=1,m
     DO i=1,n
        f(i,j) = i
     END DO
  END DO

  ndims = 2
  dims = (/n,m/)

  CALL array_to_redis(f)
END PROGRAM name
