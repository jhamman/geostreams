MODULE redis_mod
  USE iso_c_binding
  IMPLICIT NONE

  ! Test for a little-endian machine
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  CHARACTER(len=1), PARAMETER :: endian="<"
#else
  CHARACTER(len=1), PARAMETER :: endian=">"
#endif

  CHARACTER(len=1), PARAMETER :: nullchar=CHAR(0)

  INTERFACE
     SUBROUTINE redis_test_set(c) bind(c, name='redisTestSet')
       USE iso_c_binding
       ! Arguments
       TYPE(c_ptr), VALUE :: c
     END SUBROUTINE redis_test_set

     SUBROUTINE iarray_to_redis(f, dims, ndims) bind ( c )
       USE iso_c_binding
       ! Arguments
       INTEGER(c_int) f(*), dims(*), ndims
     END SUBROUTINE iarray_to_redis

     TYPE(c_ptr) FUNCTION setup_connection() bind(c, name='setupConnection')
       USE iso_c_binding
     END FUNCTION setup_connection

     SUBROUTINE free_connection(c) bind(c, name='redisFree')
       USE iso_c_binding
       ! Arguments
       TYPE(c_ptr), VALUE :: c
     END SUBROUTINE free_connection

     SUBROUTINE c_redis_push(c, key, arr, dtype, dims, ndims) &
          bind(c, name='Redis_push')
       USE iso_c_binding
       ! Arguments
       TYPE(c_ptr), VALUE    :: c, arr
       CHARACTER(c_char)     :: dtype(*), key(*)
       INTEGER(c_int), VALUE :: ndims
       INTEGER(c_int)        :: dims(ndims)
     END SUBROUTINE c_redis_push
  END INTERFACE

  INTERFACE redis_push
     MODULE PROCEDURE redis_push_f4, redis_push_f8, redis_push_i2, redis_push_i4
  END INTERFACE

CONTAINS

  SUBROUTINE array_to_redis(f)
    ! Arguments
    INTEGER(c_int) :: f(:,:)
    ! Local Variables
    INTEGER(c_int) :: dims(2), ndims
    INTEGER        :: i

    ndims = 2
    DO i=1,ndims
       dims(i) = SIZE(f,i)
    END DO

    CALL iarray_to_redis(f, dims, ndims)

  END SUBROUTINE array_to_redis

  SUBROUTINE stream_data(f)
    INTEGER(c_int) :: f(:,:)
    INTEGER(c_int) :: n, m
    EXTERNAL publish_to_redis
    ! TODO this is a function stub
    n = SIZE(f, 1)
    m = SIZE(f, 2)

    CALL publish_to_redis(f, n, m)

  END SUBROUTINE stream_data


  SUBROUTINE redis_push_f4(c, key, arr)
    USE iso_c_binding
    ! Arguments
    TYPE(c_ptr), INTENT(in)                  :: c
    REAL*4, DIMENSION(:), TARGET, INTENT(in) :: arr
    CHARACTER(len=*), INTENT(in)             :: key
    ! Local Variables
    CHARACTER(len=3)                         :: dtype

    ! array type
    dtype = endian//"f4"

    ! Publish via explicit form
    CALL redis_push_explict(c, key, arr, dtype, SHAPE(arr))
  END SUBROUTINE redis_push_f4


  SUBROUTINE redis_push_f8(c, key, arr)
    USE iso_c_binding
    ! Arguments
    TYPE(c_ptr), INTENT(in) :: c
    REAL*8, DIMENSION(:), TARGET, INTENT(in) :: arr
    CHARACTER(len=*), INTENT(in) :: key
    ! Local Variables
    CHARACTER(len=3) :: dtype

    ! array type
    dtype = endian//"f8"

    ! Publish via explicit form!
    CALL redis_push_explict(c, key, arr, dtype, SHAPE(arr))
  END SUBROUTINE redis_push_f8


  SUBROUTINE redis_push_i2(c, key, arr)
    USE iso_c_binding
    ! Arguments
    TYPE(c_ptr), INTENT(in)                     :: c
    INTEGER*2, DIMENSION(:), TARGET, INTENT(in) :: arr
    CHARACTER(len=*), INTENT(in)                :: key
    ! Local Variables
    CHARACTER(len=3)                            :: dtype

    ! array type
    dtype = endian//"i2"

    ! Publish via explicit form
    CALL redis_push_explict(c, key, arr, dtype, SHAPE(arr))
  END SUBROUTINE redis_push_i2


  SUBROUTINE redis_push_i4(c, key, arr)
    USE iso_c_binding
    ! Arguments
    TYPE(c_ptr), INTENT(in)                     :: c
    INTEGER*4, DIMENSION(:), TARGET, INTENT(in) :: arr
    CHARACTER(len=*), INTENT(in)                :: key
    ! Local Variables
    CHARACTER(len=3)                            :: dtype

    ! array type
    dtype = endian//"i4"

    ! Publish via explicit form
    CALL redis_push_explict(c, key, arr, dtype, SHAPE(arr))
  END SUBROUTINE redis_push_i4


  SUBROUTINE redis_push_explict(c, key, arr, dtype, dims)
    USE iso_c_binding

    TYPE(c_ptr)        :: c
    TYPE(*), TARGET    :: arr(*)  ! hacky way to make a fortran function polymorphic
    CHARACTER(len=*)   :: dtype, key
    INTEGER(c_int)     :: dims(:)

    CHARACTER(len=4)   :: dtype_c  ! e.g. "<f8\0"
    CHARACTER(len=128) :: key_c  ! e.g. "\0"

    ! strings in C need to be null-terminated
    dtype_c = TRIM(dtype)//nullchar
    key_c = TRIM(key)//nullchar

    ! call C interface
    CALL c_redis_push(c, key_c, c_loc(arr), dtype_c, dims, SIZE(dims,1))
  END SUBROUTINE redis_push_explict

END MODULE redis_mod
