program main

!*****************************************************************************80
!
!! MAIN is the main program for MATRIX_EXPONENTIAL_TEST.
!
!  Discussion:
!
!    MATRIX_EXPONENTIAL_TEST tests the MATRIX_EXPONENTIAL library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the MATRIX_EXPONENTIAL library.'
  write ( *, '(a)' ) '  The C8LIB and R8LIB libraries are needed.'
  write ( *, '(a)' ) '  This test needs the TEST_MATRIX_EXPONENTIAL library.'

  call matrix_exponential_test01 ( )
  call matrix_exponential_test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine matrix_exponential_test01 ( )

!*****************************************************************************80
!
!! MATRIX_EXPONENTIAL_TEST01 compares matrix exponential algorithms.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ), allocatable :: a(:,:)
  real ( kind = 8 ), allocatable :: a_exp(:,:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST01:'
  write ( *, '(a)' ) '  EXPM is MATLAB''s matrix exponential function.'
  write ( *, '(a)' ) '  R8MAT_EXPM1 is based on EXPM;'
  write ( *, '(a)' ) '  R8MAT_EXPM2 uses a Taylor series approach;'
  write ( *, '(a)' ) '  R8MAT_EXPM3 relies on an eigenvalue calculation.'

  call r8mat_exp_test_num ( test_num )

  do test = 1, test_num

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Test #', test

    call r8mat_exp_story ( test )

    call r8mat_exp_n ( test, n )

    write ( *, '(a,i4)' ) '  Matrix order N = ', n

    allocate ( a(1:n,1:n) )

    call r8mat_exp_a ( test, n, a )

    call r8mat_print ( n, n, a, '  Matrix:' )

    allocate ( a_exp(1:n,1:n) )

    call r8mat_expm1 ( n, a, a_exp )
    call r8mat_print ( n, n, a_exp, '  EXPM1(A):' )

    call r8mat_expm2 ( n, a, a_exp )
    call r8mat_print ( n, n, a_exp, '  EXPM2(A):' )

!   call r8mat_expm3 ( n, a, a_exp )
!   call r8mat_print ( n, n, a_exp, '  EXPM3(A):' )

    call r8mat_exp_expa ( test, n, a_exp )
    call r8mat_print ( n, n, a_exp, '  Exact Exponential:' )

    deallocate ( a )
    deallocate ( a_exp )

  end do

  return
end
subroutine matrix_exponential_test02 ( )

!*****************************************************************************80
!
!! MATRIX_EXPONENTIAL_TEST02 compares matrix exponential algorithms.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  complex ( kind = 8 ), allocatable :: a(:,:)
  complex ( kind = 8 ), allocatable :: a_exp(:,:)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) test
  integer ( kind = 4 ) test_num

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'MATRIX_EXPONENTIAL_TEST02:'
  write ( *, '(a)' ) '  EXPM is MATLAB''s matrix exponential function.'
  write ( *, '(a)' ) '  R8MAT_EXPM1 is based on EXPM;'
  write ( *, '(a)' ) '  R8MAT_EXPM2 uses a Taylor series approach;'
  write ( *, '(a)' ) '  R8MAT_EXPM3 relies on an eigenvalue calculation.'

  call c8mat_exp_test_num ( test_num )

  do test = 1, test_num

    write ( *, '(a)' ) ' '
    write ( *, '(a,i4)' ) '  Test #', test

    call c8mat_exp_story ( test )

    call c8mat_exp_n ( test, n )

    write ( *, '(a,i4)' ) '  Matrix order N = ', n

    allocate ( a(1:n,1:n) )

    call c8mat_exp_a ( test, n, a )

    call c8mat_print ( n, n, a, '  Matrix:' )

    allocate ( a_exp(1:n,1:n) )

    call c8mat_expm1 ( n, a, a_exp )
    call c8mat_print ( n, n, a_exp, '  EXPM1(A):' )

!   call c8mat_expm2 ( n, a, a_exp )
!   call c8mat_print ( n, n, a_exp, '  EXPM2(A):' )

!   call c8mat_expm3 ( n, a, a_exp )
!   call c8mat_print ( n, n, a_exp, '  EXPM3(A):' )

    call c8mat_exp_expa ( test, n, a_exp )
    call c8mat_print ( n, n, a_exp, '  Exact Exponential:' )

    deallocate ( a )
    deallocate ( a_exp )

  end do

  return
end
