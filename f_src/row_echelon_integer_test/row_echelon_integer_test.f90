program main

!*****************************************************************************80
!
!! MAIN is the main program for ROW_ECHELON_INTEGER_TEST.
!
!  Discussion:
!
!    ROW_ECHELON_INTEGER_TEST tests the ROW_ECHELON_INTEGER library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROW_ECHELON_INTEGER_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the ROW_ECHELON_INTEGER library.'

  call i4_gcd_test ( )

  call i4mat_print_test ( )
  call i4mat_print_some_test ( )
  call i4mat_ref_test ( )
  call i4mat_row_swap_test ( )
  call i4mat_rref_test ( )
! call i4mat_rref_solve_binary_test ( )
! call i4mat_rref_solve_binary_nz_test ( )
  call i4mat_rref_system_test ( )
  call i4mat_u_solve_test ( )

  call i4vec_binary_next_test ( )
  call i4vec_identity_row_test ( )
  call i4vec_is_binary_test ( )
  call i4vec_print_test ( )
  call i4vec_red_test ( )
  call i4vec_transpose_print_test ( )

  call ksub_next4_test ( )

  call r8vec_is_integer_test ( )
  call r8vec_print_test ( )
  call r8vec_transpose_print_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'ROW_ECHELON_INTEGER_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine i4_gcd_test ( )

!*****************************************************************************80
!
!! I4_GCD_TEST tests I4_GCD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 September 2005
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: test_num = 7

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_gcd
  integer ( kind = 4 ), dimension(test_num) :: i_test = (/ &
    36, 49, 0, 12, 36, 1, 91 /)
  integer ( kind = 4 ) j
  integer ( kind = 4 ), dimension(test_num) :: j_test = (/ &
    30, -7, 71, 12, 49, 42, 28 /)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4_GCD_TEST'
  write ( *, '(a)' ) '  I4_GCD computes the greatest common factor,'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '         I       J  I4_GCD'
  write ( *, '(a)' ) ''

  do test = 1, test_num
    i = i_test(test)
    j = j_test(test)
    write ( *, '(2x,3i8)') i, j, i4_gcd ( i, j )
  end do

  return
end
subroutine i4mat_print_test ( )

!*****************************************************************************80
!
!! I4MAT_PRINT_TEST tests I4MAT_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_PRINT_TEST'
  write ( *, '(a)' ) '  I4MAT_PRINT prints an I4MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = i * 10 + j
    end do
  end do

  call i4mat_print ( m, n, a, '  The matrix:' )

  return
end
subroutine i4mat_print_some_test ( )

!*****************************************************************************80
!
!! I4MAT_PRINT_SOME_TEST tests I4MAT_PRINT_SOME.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_PRINT_SOME_TEST'
  write ( *, '(a)' ) '  I4MAT_PRINT_SOME prints some of an I4MAT.'

  do j = 1, n
    do i = 1, m
      a(i,j) = i * 10 + j
    end do
  end do

  call i4mat_print_some ( m, n, a, 2, 1, 4, 2, &
    '  The I4MAT, rows 2:4, cols 1:2:' );

  return
end
subroutine i4mat_ref_test ( )

!*****************************************************************************80
!
!! I4MAT_REF_TEST tests I4MAT_REF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det

  a = reshape ( (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /), (/ m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_REF_TEST'
  write ( *, '(a)' ) '  I4MAT_REF computes the '
  write ( *, '(a)' ) '  integer row echelon form (IREF) of an I4MAT.'

  call i4mat_print ( m, n, a, '  Input A:' )

  call i4mat_ref ( m, n, a, det )

  write ( *, '(a)' ) 
  write ( *, '(a,g14.6)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a, '  IREF of A:' )

  return
end
subroutine i4mat_row_swap_test ( )

!*****************************************************************************80
!
!! I4MAT_ROW_SWAP_TEST tests I4MAT_ROW_SWAP.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_ROW_SWAP_TEST'
  write ( *, '(a)' ) '  I4MAT_ROW_SWAP swaps two rows of an I4MAT.'

  do i = 1, m
    do j = 1, n
      a(i,j) = 10 * i + j
    end do
  end do

  call i4mat_print ( m, n, a, '  Input A:' )

  i1 = 2
  i2 = 5
  write ( *, '(a)' ) ''
  write ( *, '(a,i2,a,i2)' ) '  Swap rows ', i1, ' and ', i2

  call i4mat_row_swap ( m, n, a, i1, i2 )

  call i4mat_print ( m, n, a, '  Modified matrix:' )

  return
end
subroutine i4mat_rref_test ( )

!*****************************************************************************80
!
!! I4MAT_RREF_TEST tests I4MAT_RREF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 4
  integer ( kind = 4 ), parameter :: n = 7

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) det

  a = reshape ( (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /), (/ m, n /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_RREF_TEST'
  write ( *, '(a)' ) '  I4MAT_RREF computes the '
  write ( *, '(a)' ) '  integer row reduced echelon form (IRREF) of an I4MAT.'

  call i4mat_print ( m, n, a, '  Input A:' )

  call i4mat_rref ( m, n, a, det )

  write ( *, '(a)' ) 
  write ( *, '(a,g14.6)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a, '  IREF of A:' )

  return
end
subroutine i4mat_rref_system_test ( )

!*****************************************************************************80
!
!! I4MAT_RREF_SYSTEM_TEST tests I4MAT_RREF_SYSTEM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), allocatable :: a1(:,:)
  integer ( kind = 4 ), allocatable :: a2(:,:)
  integer ( kind = 4 ), allocatable :: a3(:,:)
  integer ( kind = 4 ), allocatable :: b2(:)
  integer ( kind = 4 ), allocatable :: b3(:)
  integer ( kind = 4 ) det
  integer ( kind = 4 ), allocatable :: freedom(:)
  integer ( kind = 4 ) freedom_num
  integer ( kind = 4 ), allocatable :: i4rows(:)
  logical incon
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_RREF_SYSTEM_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  I4MAT_RREF_SYSTEM computes the linear system associated'
  write ( *, '(a)' ) '  with an integer reduced row echelon form of an I4MAT.'
!
!  "Wide" matrix.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Look at a "wide" matrix:'

  m = 4
  n = 7

  allocate ( i4rows(1:m*n) )
  i4rows(1:m*n) = (/ &
    1,  3, 0,  2,  6, 3, 1, &
   -2, -6, 0, -2, -8, 3, 1, &
    3,  9, 0,  0,  6, 6, 2, &
   -1, -3, 0,  1,  0, 9, 3 /)

  allocate ( a1(1:m,1:n) )
  call i4rows_to_i4mat ( m, n, i4rows, a1 )
 
  call i4mat_print ( m, n, a1, '  Input A1:' )

  allocate ( a2(1:m,1:n) )
  a2(1:m,1:n) = a1(1:m,1:n)
  call i4mat_rref ( m, n, a2, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a2, '  A2, the IRREF of A1:' )

  allocate ( b2(1:m) )
  b2(1:m-1) = 1
  b2(m) = 0
  call i4vec_print ( m, b2, '  B2, the right hand side:' )

  allocate ( a3(1:n,1:n) )
  allocate ( b3(1:n) )
  allocate ( freedom(1:n) )

  call i4mat_rref_system ( m, n, a2, b2, a3, b3, incon, freedom_num, freedom )

  write ( *, '(a)' ) ''
  if ( incon ) then
    write ( *, '(a)' ) '  The original system is INCONSISTENT.'
  else
    write ( *, '(a)' ) '  The original system is CONSISTENT.'
  end if

  call i4mat_print ( n, n, a3, '  A3, the augmented IRREF:' )
  call i4vec_print ( n, b3, '  B3, the augmented RHS:' )
  call i4vec_print ( freedom_num, freedom, '  Indices of degrees of freedom.' )

  deallocate ( a1 )
  deallocate ( a2 )
  deallocate ( a3 )
  deallocate ( b2 )
  deallocate ( b3 )
  deallocate ( freedom )
  deallocate ( i4rows )
!
!  "Tall" matrix.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Look at a "tall" matrix:'

  m = 7
  n = 4

  allocate ( i4rows(1:m*n) )
  i4rows(1:m*n) = (/ &
    1, -2, 3, -1, &
    3, -6, 9, -3, &
    0,  0, 0,  0, &
    2, -2, 0,  1, &
    6, -8, 6,  0, &
    3,  3, 6,  9, &
    1,  1, 2,  3 /)

  allocate ( a1(1:m,1:n) )
  call i4rows_to_i4mat ( m, n, i4rows, a1 )
 
  call i4mat_print ( m, n, a1, '  Input A1:' )

  allocate ( a2(1:m,1:n) )
  a2(1:m,1:n) = a1(1:m,1:n)
  call i4mat_rref ( m, n, a2, det )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  The pseudo-determinant = ', det

  call i4mat_print ( m, n, a2, '  A2, the IRREF of A1:' )

  allocate ( b2(1:m) )
  b2(1:m) = 1
  call i4vec_print ( m, b2, '  B2, the right hand side:' )

  allocate ( a3(1:n,1:n) )
  allocate ( b3(1:n) )
  allocate ( freedom(1:n) )

  call i4mat_rref_system ( m, n, a2, b2, a3, b3, incon, freedom_num, freedom )

  write ( *, '(a)' ) ''
  if ( incon ) then
    write ( *, '(a)' ) '  The original system is INCONSISTENT.'
  else
    write ( *, '(a)' ) '  The original system is CONSISTENT.'
  end if

  call i4mat_print ( n, n, a3, '  A3, the augmented IRREF:' )
  call i4vec_print ( n, b3, '  B3, the augmented RHS:' )
  call i4vec_print ( freedom_num, freedom, '  Indices of degrees of freedom.' )

  deallocate ( a1 )
  deallocate ( a2 )
  deallocate ( a3 )
  deallocate ( b2 )
  deallocate ( b3 )
  deallocate ( freedom )
  deallocate ( i4rows )

  return
end
subroutine i4mat_u_solve_test ( )

!*****************************************************************************80
!
!! I4MAT_U_SOLVE_TEST tests I4MAT_U_SOLVE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4
!
!  Each row of this definition is a COLUMN of the matrix.
!
  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
    1, 0, 0,  0, &
    2, 3, 0,  0, &
    4, 5, 6,  0, &
    7, 8, 9, 10 /), (/ n, n /) )
  integer ( kind = 4 ), dimension ( n ) :: b = (/ &
    45, 53, 54, 40 /)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) rnorm
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4MAT_U_SOLVE_TEST'
  write ( *, '(a)' ) '  I4MAT_U_SOLVE solves an upper triangular system.'

  call i4mat_print ( n, n, a, '  Input matrix A:' )

  call i4vec_print ( n, b, '  Right hand side b:' )

  call i4mat_u_solve ( n, a, b, x )

  call r8vec_print ( n, x, '  Computed solution x:' )

  r(1:n) = matmul ( a(1:n,1:n), x(1:n) ) - b(1:n)

  rnorm = sqrt ( sum ( r(1:n) ** 2 ) )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  Norm of A*x-b = ', rnorm

  return
end
subroutine i4vec_binary_next_test ( )

!*****************************************************************************80
!
!! I4VEC_BINARY_NEXT_TEST tests I4VEC_BINARY_NEXT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) bvec(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_BINARY_NEXT_TEST'
  write ( *, '(a)' ) '  I4VEC_BINARY_NEXT generates the next binary vector.'
  write ( *, '(a)' ) ''
 
  bvec(1:n) = 0

  do

    call i4vec_transpose_print ( n, bvec, '  ' )

    if ( all ( bvec(1:n) == 1 ) ) then
      exit
    end if

    call i4vec_binary_next ( n, bvec )
 
  end do

  return
end
subroutine i4vec_identity_row_test ( )

!*****************************************************************************80
!
!! I4VEC_IDENTITY_ROW_TEST tests I4VEC_IDENTITY_ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IDENTITY_ROW_TEST'
  write ( *, '(a)' ) '  I4VEC_IDENTITY_ROW returns a row of the identity matrix.'
  write ( *, '(a)' ) ''
  do i = 0, n + 1
    call i4vec_identity_row ( n, i, a )
    write ( *, '(i2,a,5i2)' ) i, ':', a(1:n)
  end do

  return
end
subroutine i4vec_is_binary_test ( )

!*****************************************************************************80
!
!! I4VEC_IS_BINARY_TEST tests I4VEC_IS_BINARY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  logical i4vec_is_binary
  integer ( kind = 4 ) n
  integer ( kind = 4 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_IS_BINARY_TEST'
  write ( *, '(a)' ) '  I4VEC_IS_BINARY is TRUE if an I4VEC only contains'
  write ( *, '(a)' ) '  0 or 1 entries.'

  n = 3
  allocate ( x(1:n) )
  x = (/ 0, 0, 0 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 1, 0, 1 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  n = 3
  allocate ( x(1:n) )
  x = (/ 0, 2, 1 /)
  write ( *, '(a)' ) ''
  call i4vec_transpose_print ( n, x, '  X:' )
  if ( i4vec_is_binary ( n, x ) ) then
    write ( *, '(a)' ) '  X is binary.'
  else
    write ( *, '(a)' ) '  X is NOT binary.'
  end if
  deallocate ( x )

  return
end
subroutine i4vec_print_test ( )

!*****************************************************************************80
!
!! I4VEC_PRINT_TEST tests I4VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 November 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  integer ( kind = 4 ), dimension ( n ) :: a = (/ &
    91, 92, 93, 94 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_PRINT prints an I4VEC'

  call i4vec_print ( n, a, '  The I4VEC:' )

  return
end
subroutine i4vec_red_test ( )

!*****************************************************************************80
!
!! I4VEC_RED_TEST tests I4VEC_RED.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: m = 5
  integer ( kind = 4 ), parameter :: n = 3

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) factor
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_RED_TEST'
  write ( *, '(a)' ) '  I4VEC_RED divides out any common factors in the'
  write ( *, '(a)' ) '  entries of an I4VEC.'

  a = reshape ( (/ &
    12,   4, -12, 30, 0, &
    88,   8,  88, 18, 4, &
     9, 192,  94, 42, 8 /), (/ 5, 3 /) )

  call i4mat_print ( m, n, a, '  Apply I4VEC_RED to each row of this matrix:' )

  do i = 1, m
    call i4vec_red ( n, a(i,1:n), factor )
  end do

  call i4mat_print ( m, n, a, '  Reduced matrix:' )

  return
end
subroutine i4vec_transpose_print_test ( )

!*****************************************************************************80
!
!! I4VEC_TRANSPOSE_PRINT_TEST tests I4VEC_TRANSPOSE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 2006
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'I4VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  I4VEC_TRANSPOSE_PRINT prints an I4VEC'
  write ( *, '(a)' ) '  with 5 entries to a row, and an optional title.'

  do i = 1, n
    a(i) = i
  end do

  call i4vec_print ( n, a, '  Output from I4VEC_PRINT:' )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Call I4VEC_TRANSPOSE_PRINT with a short title:'

  call i4vec_transpose_print ( n, a, '  My array:  ' )

  return
end
subroutine ksub_next4_test ( )

!*****************************************************************************80
!
!! KSUB_NEXT4_TEST tests KSUB_NEXT4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    29 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: k = 3

  integer ( kind = 4 ) a(k)
  logical done
  integer ( kind = 4 ), parameter :: n = 5
  integer ( kind = 4 ) rank

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'KSUB_NEXT4_TEST'
  write ( *, '(a)' ) '  KSUB_NEXT4 generates K subsets of an N set.'
  write ( *, '(a,i8)' ) '  N = ', n
  write ( *, '(a,i8)' ) '  K=  ', k
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Rank    Subset'
  write ( *, '(a)' ) ''

  done = .true.
  rank = 0
 
  do
 
    call ksub_next4 ( n, k, a, done )
 
    if ( done ) then
      exit
    end if

    rank = rank + 1
    write ( *, '(2x,i4,4x,3i4)' ) rank, a(1:k)

  end do
 
  return
end
subroutine r8vec_is_integer_test ( )

!*****************************************************************************80
!
!! R8VEC_IS_INTEGER_TEST tests R8VEC_IS_INTEGER.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n
  logical r8vec_is_integer
  real ( kind = 8 ), allocatable :: x(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_IS_INTEGER_TEST'
  write ( *, '(a)' ) '  R8VEC_IS_INTEGER is TRUE if an R8VEC only contains'
  write ( *, '(a)' ) '  integer entries.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 1: Obviously integer:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Obviously NOT integer:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.5D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 3: Not Integer, Not obvious:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  x(5) = x(5) + 0.0000001D+00
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Example 4: Not Integer, Not obvious:'
  write ( *, '(a)' ) ''
  n = 6
  allocate ( x(1:n) )
  x = (/ 1.0D+00, 2.0D+00, 300000000.2D+00, 4.0D+00, 5.0D+00, 6.0D+00 /)
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, x, '  X:' )
  if ( r8vec_is_integer ( n, x ) ) then
    write ( *, '(a)' ) '  X is integer.'
  else
    write ( *, '(a)' ) '  X is NOT integer.'
  end if
  deallocate ( x )

  return
end
subroutine r8vec_print_test ( )

!*****************************************************************************80
!
!! R8VEC_PRINT_TEST tests R8VEC_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4

  real ( kind = 8 ), dimension ( n ) :: a = (/ &
    123.456D+00, 0.000005D+00, -1.0D+06, 3.14159265D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_PRINT prints an R8VEC.'

  call r8vec_print ( n, a, '  The R8VEC:' )

  return
end
subroutine r8vec_transpose_print_test ( )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT_TEST tests R8VEC_TRANSPOSE_PRINT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 12

  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_TRANSPOSE_PRINT_TEST'
  write ( *, '(a)' ) '  R8VEC_TRANSPOSE_PRINT prints an R8VEC "tranposed",'
  write ( *, '(a)' ) '  that is, placing multiple entries on a line.'

  x = (/ &
    1.1D+00, 2.02D+00, 30.33D+00, 444.44D+00, -0.005D+00, &
    6.6666666666D+00, 7777777.0D+00, 8.0D+00, 99.0D+00, 10.0D+00, &
    11.0D+00, 12.0D+00 /)

  call r8vec_transpose_print ( n, x, '  The vector X:' )

  return
end

