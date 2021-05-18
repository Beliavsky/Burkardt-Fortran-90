program main

!*****************************************************************************80
!
!! MAIN is the main program for LAPLACIAN_TEST.
!
!  Discussion:
!
!    LAPLACIAN_TEST tests the LAPLACIAN library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAPLACIAN_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the LAPLACIAN library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'LAPLACIAN_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests L1DD and similar routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) h
  real ( kind = 8 ) l(n,n)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  A full-storage matrix is returned by:'
  write ( *, '(a)' ) '  L1DD: Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN: Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND: Neumann/Dirichlet BC;'
  write ( *, '(a)' ) '  L1NN: Neumann/Neumann BC;'
  write ( *, '(a)' ) '  L1PP: Periodic BC;'

  do test = 1, 2

    if ( test == 1 ) then
      h = 1.0D+00
    else
      h = 1.0D+00 / real ( n + 1, kind = 8 )
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

    call l1dd ( n, h, l )
    call r8mat_print ( n, n, l, '  L1DD:' )

    call l1dn ( n, h, l )
    call r8mat_print ( n, n, l, '  L1DN:' )

    call l1nd ( n, h, l )
    call r8mat_print ( n, n, l, '  L1ND:' )

    call l1nn ( n, h, l )
    call r8mat_print ( n, n, l, '  L1NN:' )

    call l1pp ( n, h, l )
    call r8mat_print ( n, n, l, '  L1PP:' )

  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests L1DD_APPLY and similar functions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 9

  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  real ( kind = 8 ) lu(n)
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  The Laplacian L is applied to data U by:'
  write ( *, '(a)' ) '  L1DD_APPLY for Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN_APPLY for Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND_APPLY for Neumann/Dirichlet BC;'
  write ( *, '(a)' ) '  L1NN_APPLY for Neumann/Neumann BC;'
  write ( *, '(a)' ) '  L1PP_APPLY for Periodic BC;'

  do i = 1, n
    x(i) = real ( i, kind = 8 ) / real ( n + 1, kind = 8 )
  end do
  h = 1.0D+00 / real ( n + 1, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

  u(1:n) = x(1:n) * ( 1.0D+00 - x(1:n) )

  call r8vec_print ( n, u, '  Vector U:' );

  call l1dd_apply ( n, h, u, lu )
  call r8vec_print ( n, lu, '  L1DD(U):' )

  call l1dn_apply ( n, h, u, lu )
  call r8vec_print ( n, lu, '  L1DN(U):' )

  call l1nd_apply ( n, h, u, lu )
  call r8vec_print ( n, lu, '  L1ND(U):' )

  call l1nn_apply ( n, h, u, lu )
  call r8vec_print ( n, lu, '  L1NN(U):' )

  call l1pp_apply ( n, h, u, lu )
  call r8vec_print ( n, lu, '  L1PP(U):' )
  
  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests L1DD_EIGEN and similar routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) err
  real ( kind = 8 ) h
  real ( kind = 8 ) lambda(n)
  integer ( kind = 4 ) test
  real ( kind = 8 ) v(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Compute eigen information for the Laplacian:'
  write ( *, '(a)' ) '  L1DD_EIGEN for Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN_EIGEN for Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND_EIGEN for Neumann/Dirichlet BC;'
  write ( *, '(a)' ) '  L1NN_EIGEN for Neumann/Neumann BC;'
  write ( *, '(a)' ) '  L1PP_EIGEN for Periodic BC;'

  do test = 1, 2

    if ( test == 1 ) then
      h = 1.0D+00
    else
      h = 1.0D+00 / real ( n + 1, kind = 8 )
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

    call l1dd ( n, h, a )
    call l1dd_eigen ( n, h, v, lambda )
    call r8vec_print ( n, lambda, '  L1DD Eigenvalues:' )
    call r8mat_print ( n, n, v, '  L1DD Eigenvectors:' )
    call eigen_error ( n, n, a, v, lambda, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DD eigenerror = ', err

    call l1dn ( n, h, a )
    call l1dn_eigen ( n, h, v, lambda )
    call r8vec_print ( n, lambda, '  L1DN Eigenvalues:' )
    call r8mat_print ( n, n, v, '  L1DN Eigenvectors:' )
    call eigen_error ( n, n, a, v, lambda, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DN eigenerror = ', err

    call l1nd ( n, h, a )
    call l1nd_eigen ( n, h, v, lambda )
    call r8vec_print ( n, lambda, '  L1ND Eigenvalues:' )
    call r8mat_print ( n, n, v, '  L1ND Eigenvectors:' )
    call eigen_error ( n, n, a, v, lambda, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1ND eigenerror = ', err

    call l1nn ( n, h, a )
    call l1nn_eigen ( n, h, v, lambda )
    call r8vec_print ( n, lambda, '  L1NN Eigenvalues:' )
    call r8mat_print ( n, n, v, '  L1NN Eigenvectors:' )
    call eigen_error ( n, n, a, v, lambda, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1NN eigenerror = ', err

    call l1pp ( n, h, a )
    call l1pp_eigen ( n, h, v, lambda )
    call r8vec_print ( n, lambda, '  L1PP Eigenvalues:' )
    call r8mat_print ( n, n, v, '  L1PP Eigenvectors:' )
    call eigen_error ( n, n, a, v, lambda, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1PP eigenerror = ', err

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests L1DD_INVERSE and similar routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) err
  real ( kind = 8 ) h
  real ( kind = 8 ) l(n,n)
  real ( kind = 8 ) linv(n,n)
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  The inverse of a full-storage matrix is returned by:'
  write ( *, '(a)' ) '  L1DD_INVERSE: Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN_INVERSE: Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND_INVERSE: Neumann/Dirichlet BC;'

  do test = 1, 2

    if ( test == 1 ) then
      h = 1.0D+00
    else
      h = 1.0D+00 / real ( n + 1, kind = 8 )
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

    call l1dd ( n, h, l )
    call r8mat_print ( n, n, l, '  L1DD:' )
    call l1dd_inverse ( n, h, linv )
    call r8mat_print ( n, n, linv, '  L1DD_INVERSE:' )
    call inverse_error ( n, l, linv, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DD inverse error = ', err

    call l1dn ( n, h, l )
    call r8mat_print ( n, n, l, '  L1DN:' )
    call l1dn_inverse ( n, h, linv )
    call r8mat_print ( n, n, linv, '  L1DN_INVERSE:' )
    call inverse_error ( n, l, linv, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DN inverse error = ', err

    call l1nd ( n, h, l )
    call r8mat_print ( n, n, l, '  L1ND:' )
    call l1nd_inverse ( n, h, linv )
    call r8mat_print ( n, n, linv, '  L1ND_INVERSE:' )
    call inverse_error ( n, l, linv, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1ND inverse error = ', err

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests L1DD_CHOLESKY and similar routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 October 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) c(n,n)
  real ( kind = 8 ) err
  real ( kind = 8 ) h
  integer ( kind = 4 ) test

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Compute upper Cholesky factors for the Laplacian:'
  write ( *, '(a)' ) '  L1DD_CHOLESKY for Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN_CHOLESKY for Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND_CHOLESKY for Neumann/Dirichlet BC;'
  write ( *, '(a)' ) '  L1NN_CHOLESKY for Neumann/Neumann BC;'
  write ( *, '(a)' ) '  L1PP_CHOLESKY for Periodic BC;'

  do test = 1, 2

    if ( test == 1 ) then
      h = 1.0D+00
    else
      h = 1.0D+00 / real ( n + 1, kind = 8 )
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

    call l1dd ( n, h, a )
    call l1dd_cholesky ( n, h, c )
    call r8mat_print ( n, n, c, '  L1DD Cholesky factor:' )
    call cholesky_upper_error ( n, a, c, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DD Cholesky error = ', err

    call l1dn ( n, h, a )
    call l1dn_cholesky ( n, h, c )
    call r8mat_print ( n, n, c, '  L1DN Cholesky factor:' )
    call cholesky_upper_error ( n, a, c, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DN Cholesky error = ', err

    call l1nd ( n, h, a )
    call l1nd_cholesky ( n, h, c )
    call r8mat_print ( n, n, c, '  L1ND Cholesky factor:' )
    call cholesky_upper_error ( n, a, c, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1ND Cholesky error = ', err

    call l1nn ( n, h, a )
    call l1nn_cholesky ( n, h, c )
    call r8mat_print ( n, n, c, '  L1NN Cholesky factor:' )
    call cholesky_upper_error ( n, a, c, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1NN Cholesky error = ', err

    call l1pp ( n, h, a )
    call l1pp_cholesky ( n, h, c )
    call r8mat_print ( n, n, c, '  L1PP Cholesky factor:' )
    call cholesky_upper_error ( n, a, c, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1PP Cholesky error = ', err

  end do

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests L1DD_LU and similar routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 5

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) err
  real ( kind = 8 ) h
  real ( kind = 8 ) l(n,n)
  integer ( kind = 4 ) test
  real ( kind = 8 ) u(n,n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  Compute LU factors for the Laplacian:'
  write ( *, '(a)' ) '  L1DD_LU for Dirichlet/Dirichlet BC;'
  write ( *, '(a)' ) '  L1DN_LU for Dirichlet/Neumann BC;'
  write ( *, '(a)' ) '  L1ND_LU for Neumann/Dirichlet BC;'
  write ( *, '(a)' ) '  L1NN_LU for Neumann/Neumann BC;'
  write ( *, '(a)' ) '  L1PP_LU for Periodic BC;'

  do test = 1, 2

    if ( test == 1 ) then
      h = 1.0D+00
    else
      h = 1.0D+00 / real ( n + 1, kind = 8 )
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  Using spacing H = ', h

    call l1dd ( n, h, a )
    call l1dd_lu ( n, h, l, u )
    call r8mat_print ( n, n, l, '  L1DD L factor:' )
    call r8mat_print ( n, n, u, '  L1DD U factor:' )
    call lu_error ( n, a, l, u, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DD LU error = ', err

    call l1dn ( n, h, a )
    call l1dn_lu ( n, h, l, u )
    call r8mat_print ( n, n, l, '  L1DN L factor:' )
    call r8mat_print ( n, n, u, '  L1DN U factor:' )
    call lu_error ( n, a, l, u, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1DN LU error = ', err

    call l1nd ( n, h, a )
    call l1nd_lu ( n, h, l, u )
    call r8mat_print ( n, n, l, '  L1ND L factor:' )
    call r8mat_print ( n, n, u, '  L1ND U factor:' )
    call lu_error ( n, a, l, u, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1ND LU error = ', err

    call l1nn ( n, h, a )
    call l1nn_lu ( n, h, l, u )
    call r8mat_print ( n, n, l, '  L1NN L factor:' )
    call r8mat_print ( n, n, u, '  L1NN U factor:' )
    call lu_error ( n, a, l, u, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1NN LU error = ', err

    call l1pp ( n, h, a )
    call l1pp_lu ( n, h, l, u )
    call r8mat_print ( n, n, l, '  L1PP L factor:' )
    call r8mat_print ( n, n, u, '  L1PP U factor:' )
    call lu_error ( n, a, l, u, err )
    write ( *, '(a)' ) ' '
    write ( *, '(a,g14.6)' ) '  L1PP LU error = ', err

  end do

  return
end
