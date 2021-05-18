program main

!*****************************************************************************80
!
!! MAIN is the main program for JACOBI_EIGENVALUE_TEST.
!
!  Discussion:
!
!    JACOBI_EIGENVALUE_TEST tests the JACOBI_EIGENVALUE library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_EIGENVALUE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the JACOBI_EIGENVALUE library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'JACOBI_EIGENVALUE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses a 4x4 test matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
      4.0,  -30.0,    60.0,   -35.0, &
    -30.0,  300.0,  -675.0,   420.0, &
     60.0, -675.0,  1620.0, -1050.0, &
    -35.0,  420.0, -1050.0,   700.0 /), (/ 4, 4 /) )
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) v(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  For a symmetric matrix A,'
  write ( *, '(a)' ) '  JACOBI_EIGENVALUE computes the eigenvalues D'
  write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  it_max = 100

  call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
  write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

  call r8vec_print ( n, d, '  Eigenvalues D:' )

  call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
!
!  Compute eigentest.
!
  call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Frobenius norm error in eigensystem A*V-D*V = ', &
    error_frobenius

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 uses a 4x4 test matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
    4.0, 0.0, 0.0, 0.0, &
    0.0, 1.0, 0.0, 0.0, &
    0.0, 0.0, 3.0, 0.0, &
    0.0, 0.0, 0.0, 2.0 /), (/ 4, 4 /) )
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) v(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  For a symmetric matrix A,'
  write ( *, '(a)' ) '  JACOBI_EIGENVALUE computes the eigenvalues D'
  write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'As a sanity check, input a diagonal matrix.'

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  it_max = 100

  call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
  write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

  call r8vec_print ( n, d, '  Eigenvalues D:' )

  call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
!
!  Compute eigentest.
!
  call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Frobenius norm error in eigensystem A*V-D*V = ', &
    error_frobenius

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 uses a 5x5 test matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 July 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) error_frobenius
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) it_num
  integer ( kind = 4 ) j
  integer ( kind = 4 ) rot_num
  real ( kind = 8 ) v(n,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  For a symmetric matrix A,'
  write ( *, '(a)' ) '  JACOBI_EIGENVALUE computes the eigenvalues D'
  write ( *, '(a)' ) '  and eigenvectors V so that A * V = D * V.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Use the discretized second derivative matrix.'

  do j = 1, n
    do i = 1, n
      if ( i == j ) then
        a(i,j) = -2.0D+00
      else if ( i == j + 1 .or. i == j - 1 ) then
        a(i,j) = 1.0D+00
      else
        a(i,j) = 0.0D+00
      end if
    end do
  end do

  call r8mat_print ( n, n, a, '  Input matrix A:' )

  it_max = 100

  call jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Number of iterations = ', it_num
  write ( *, '(a,i4)' ) '  Number of rotations  = ', rot_num

  call r8vec_print ( n, d, '  Eigenvalues D:' )

  call r8mat_print ( n, n, v, '  Eigenvector matrix V:' )
!
!  Compute eigentest.
!
  call r8mat_is_eigen_right ( n, n, a, v, d, error_frobenius )
  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) &
    '  Frobenius norm error in eigensystem A*V-D*V = ', &
    error_frobenius

  return
end
