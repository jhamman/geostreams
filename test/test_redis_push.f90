program name
  use redis_mod
  implicit none
  integer, parameter ::n=200, m=100
  integer :: dims(2)
  real*4 :: f4(n,m)
  real*8 :: f8(n,m)
  integer*2 :: i2(n,m)
  integer*4 :: i4(n,m)
  integer ndims
  integer i, j

  type (c_ptr) :: redis

  redis = setup_connection()

  ! initialize arrays
  do j=1,m
    do i=1,n
      f4(i,j) = i + .5
      f8(i,j) = i + .5
      i2(i,j) = i + 1
      i4(i,j) = i + 1
    end do
  end do

  ndims = 2

  call redis_push(redis, 'A-f4', f4)
  call redis_push(redis, 'A-f8', f8)
  call redis_push(redis, 'A-i2', i2)
  call redis_push(redis, 'A-i4', i4)

  call free_connection(redis)
end program name
