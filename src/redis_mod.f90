module redis_mod
  use iso_c_binding
  implicit none

! Test for a little-endian machine
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  character(len=1), parameter :: endian="<"
#else
  character(len=1), parameter :: endian=">"
#endif

  interface
     subroutine redis_test_set(c) bind(c, name='redisTestSet')
       use iso_c_binding
       type(c_ptr), VALUE :: c
     end subroutine redis_test_set

     subroutine iarray_to_redis(f, dims, ndims) bind ( c )
       use iso_c_binding
       integer(c_int) f(*), dims(*), ndims
     end subroutine iarray_to_redis

     type(c_ptr) function setup_connection() bind(c, name='setupConnection')
       use iso_c_binding
     end function setup_connection

     subroutine free_connection(c) bind(c, name='redisFree')
       use iso_c_binding
       type(c_ptr), VALUE :: c
     end subroutine free_connection

     subroutine c_redis_push(c, key, arr, dtype, dims, ndims) &
          bind(c, name='Redis_push')
       use iso_c_binding
       type(c_ptr), VALUE :: c, arr
       character(c_char) :: dtype(*), key(*)
       integer(c_int), VALUE :: ndims
       integer(c_int) :: dims(ndims)
     end subroutine c_redis_push
  end interface

  interface redis_push
    module procedure redis_push_f4, redis_push_f8, redis_push_i2, redis_push_i4
  end interface

contains

  subroutine array_to_redis(f)
    integer(c_int) f(:,:)
    integer(c_int) dims(2), ndims
    integer i

    ndims = 2
    do i=1,ndims
       dims(i) = size(f,i)
    end do

    call iarray_to_redis(f, dims, ndims)

  end subroutine array_to_redis

  subroutine stream_data(f)
    integer(c_int) :: f(:,:)
    integer(c_int) :: n, m
    external publish_to_redis
    ! TODO this is a function stub
    n = size(f, 1)
    m = size(f, 2)

    call publish_to_redis(f, n, m)

  end subroutine stream_data


  subroutine redis_push_f4(c, key, arr)
    use iso_c_binding
    type(c_ptr), intent(in) :: c
    ! This next declaration is hacky way to make a fortran function polymorphic
    real*4, dimension(:), target, intent(in) :: arr
    character(len=*), intent(in) :: key

    character(len=4) :: dtype_c
    character(len=128) :: key_c

    ! array type
    dtype_c = endian//"f4"//CHAR(0)

    ! strings in C need to be null-terminated
    key_c = (trim(key)//CHAR(0))

    call c_redis_push(c, key_c, c_loc(arr), dtype_c, shape(arr), rank(arr))
  end subroutine redis_push_f4


  subroutine redis_push_f8(c, key, arr)
    use iso_c_binding
    type(c_ptr), intent(in) :: c
    ! This next declaration is hacky way to make a fortran function polymorphic
    real*8, dimension(:), target, intent(in) :: arr
    character(len=*), intent(in) :: key

    character(len=4) :: dtype_c
    character(len=128) :: key_c

    ! array type
    dtype_c = endian//"f8"//CHAR(0)

    ! strings in C need to be null-terminated
    key_c = (trim(key)//CHAR(0))

    call c_redis_push(c, key_c, c_loc(arr), dtype_c, shape(arr), rank(arr))
end subroutine redis_push_f8

subroutine redis_push_i2(c, key, arr)
  use iso_c_binding
  type(c_ptr), intent(in) :: c
  ! This next declaration is hacky way to make a fortran function polymorphic
  integer*2, dimension(:), target, intent(in) :: arr
  character(len=*), intent(in) :: key

  character(len=4) :: dtype_c
  character(len=128) :: key_c

  ! array type
  dtype_c = endian//"i2"//CHAR(0)

  ! strings in C need to be null-terminated
  key_c = (trim(key)//CHAR(0))

  call c_redis_push(c, key_c, c_loc(arr), dtype_c, shape(arr), rank(arr))
end subroutine redis_push_i2


subroutine redis_push_i4(c, key, arr)
  use iso_c_binding
  type(c_ptr), intent(in) :: c
  ! This next declaration is hacky way to make a fortran function polymorphic
  integer*4, dimension(:), target, intent(in) :: arr
  character(len=*), intent(in) :: key

  character(len=4) :: dtype_c
  character(len=128) :: key_c

  ! array type
  dtype_c = endian//"i4"//CHAR(0)

  ! strings in C need to be null-terminated
  key_c = (trim(key)//CHAR(0))

  call c_redis_push(c, key_c, c_loc(arr), dtype_c, shape(arr), rank(arr))
end subroutine redis_push_i4

end module redis_mod
