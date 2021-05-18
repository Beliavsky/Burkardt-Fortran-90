program main

!*****************************************************************************80
!
!! MAIN is the main program for CG_RC_TEST.
!
!  Discussion:
!
!    CG_RC_TEST tests the CG_RC library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CG_RC_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the CG_RC library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CG_RC_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 uses CG_RC for the simple 1, -2, 1 matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 21

  real ( kind = 8 ) angle
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) bnrm2
  real ( kind = 8 ) err
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  real ( kind = 8 ) p(n)
  real ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real ( kind = 8 ) q(n)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) rnrm2
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_exact(n)
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use CG_RC on the 1, -2, 1 matrix.'
!
!  In order to specify the right hand side, pick an exact solution,
!  and multiply by the matrix.
!
  do i = 1, n
    angle = 2.0D+00 * pi * real ( i - 1, kind = 8 ) / real ( n - 1, kind = 8 )
    x_exact(i) = sin ( angle )
  end do

  b(1:n) = - 2.0D+00 * x_exact(1:n)
  b(1:n-1) = b(1:n-1) + x_exact(2:n)
  b(2:n) = b(2:n) + x_exact(1:n-1)
!
!  Here is the initial guess for the solution.
!
  x(1:n) = 0.0D+00
!
!  Parameters for the stopping test.
!
  it = 0
  it_max = 30
  tol = 1.0D-05
  bnrm2 = sqrt ( sum ( b(1:n)**2 ) )
!
!  Set parameters for CG_RC.
!
  job = 1
!
!  Repeatedly call CG_RC, and on return, do what JOB tells you.
!
  do

    call cg_rc ( n, b, x, r, z, p, q, job )
!
!  Compute q = A * p.
!
    if ( job == 1 ) then

      q(1:n)   = - 2.0D+00 * p(1:n)
      q(1:n-1) = q(1:n-1)  + p(2:n)
      q(2:n)   = q(2:n)    + p(1:n-1)
!
!  Solve M * z = r.
!
    else if ( job == 2 ) then

      z(1:n) = r(1:n) / ( - 2.0D+00 )
!
!  Compute r = r - A * x.
!
    else if ( job == 3 ) then

      r(1:n)   = r(1:n) + 2.0D+00 * x(1:n)
      r(1:n-1) = r(1:n-1) - x(2:n)
      r(2:n)   = r(2:n) - x(1:n-1)
!
!  Stopping test on R.
!
    else if ( job == 4 ) then

      rnrm2 = sqrt ( sum ( r(1:n)**2 ) )

      if ( bnrm2 == 0.0D+00 ) then
        if ( rnrm2 <= tol ) then
          exit
        end if
      else
        if ( rnrm2 <= tol * bnrm2 ) then
          exit
        end if
      end if

      it = it + 1

      if ( it_max <= it ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Iteration limit exceeded.'
        write ( *, '(a)' ) '  Terminating early.'
        exit
      end if

    end if

    job = 2

  end do
  
  write ( *, '(a)' ) ' '
  write ( *, '(a,i5)' ) '  Number of iterations was ', it
  write ( *, '(a,g14.6)' ) '  Estimated error is ', rnrm2
  err = maxval ( abs ( x_exact(1:n) - x(1:n) ) )
  write ( *, '(a,g14.6)' ) '  Loo error is ', err

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X(I)         X_EXACT(I)        B(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), x_exact(i), b(i)
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests CG_RC with the Wathen matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 January 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 79

  real ( kind = 8 ) a(n,n)
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) bnrm2
  real ( kind = 8 ) err
  integer ( kind = 4 ) i
  integer ( kind = 4 ) it
  integer ( kind = 4 ) it_max
  integer ( kind = 4 ) job
  integer ( kind = 4 ) nx
  integer ( kind = 4 ) ny
  real ( kind = 8 ) p(n)
  real ( kind = 8 ) q(n)
  real ( kind = 8 ) r(n)
  real ( kind = 8 ) rnrm2
  integer ( kind = 4 ) seed
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) x_exact(n)
  real ( kind = 8 ) z(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Use CG_RC to solve a linear system'
  write ( *, '(a)' ) '  involving the Wathen matrix.'

  nx = 5
  ny = 4
 
  write ( *, '(a)' ) ' '
  write ( *, '(a,i6)' ) '  NX = ', nx
  write ( *, '(a,i6)' ) '  NY = ', ny
  write ( *, '(a,i6)' ) '  N  = ', n

  call wathen ( nx, ny, n, a )

  seed = 123456789
  call r8vec_uniform_01 ( n, seed, x_exact )

  b(1:n) = matmul ( a(1:n,1:n), x_exact(1:n) )

  x(1:n) = 0.0D+00
!
!  Parameters for the stopping test.
!
  it = 0
  it_max = 30
  tol = 1.0D-05
  bnrm2 = sqrt ( sum ( b(1:n)**2 ) )
!
!  Set parameters for the CG_RC code.
!
  job = 1
!
!  Repeatedly call the CG_RC code, and on return, do what JOB tells you.
!
  do

    call cg_rc ( n, b, x, r, z, p, q, job )
!
!  Compute q = A * p.
!
    if ( job == 1 ) then

      q(1:n) = matmul ( a(1:n,1:n), p(1:n) )
!
!  Solve M * z = r.
!
    else if ( job == 2 ) then

      do i = 1, n
        z(i) = r(i) / a(i,i)
      end do
!
!  Compute r = r - A * x.
!
    else if ( job == 3 ) then

      r(1:n) = r(1:n) - matmul ( a(1:n,1:n), x(1:n) )
!
!  Stopping test.
!
    else if ( job == 4 ) then

      rnrm2 = sqrt ( sum ( r(1:n)**2 ) )

      if ( bnrm2 == 0.0D+00 ) then
        if ( rnrm2 <= tol ) then
          exit
        end if
      else
        if ( rnrm2 <= tol * bnrm2 ) then
          exit
        end if
      end if

      it = it + 1

      if ( it_max <= it ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Iteration limit exceeded.'
        write ( *, '(a)' ) '  Terminating early.'
        exit
      end if

    end if

    job = 2

  end do
  
  write ( *, '(a)' ) ' '
  write ( *, '(a,i5)' ) '  Number of iterations was ', it
  write ( *, '(a,g14.6)' ) '  Estimated error is ', rnrm2
  err = maxval ( abs ( x_exact(1:n) - x(1:n) ) )
  write ( *, '(a,g14.6)' ) '  Loo error is ', err

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '     I      X(I)         X_EXACT(I)        B(I)'
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      i, x(i), x_exact(i), b(i)
  end do

  return
end

